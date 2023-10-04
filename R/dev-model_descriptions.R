
# GENERAL MODEL DESCRIPTIONS -----

#' Get model descriptions for parsnip, workflows & modeltime objects
#'
#'
#' @param object Parsnip or workflow objects
#' @param upper_case Whether to return upper or lower case model descriptions
#' @param indicate_training Whether or not to indicate if the model has been trained
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#' library(parsnip)
#'
#' # Model Specification ----
#'
#' arima_spec <- arima_reg() %>%
#'     set_engine("auto_arima")
#'
#' get_model_description(arima_spec, indicate_training = TRUE)
#'
#' # Fitted Model ----
#'
#' m750 <- m4_monthly %>% filter(id == "M750")
#'
#' arima_fit <- arima_spec %>%
#'     fit(value ~ date, data = m750)
#'
#' get_model_description(arima_fit, indicate_training = TRUE)
#'
#'
#' @export
get_model_description <- function(object, indicate_training = FALSE, upper_case = TRUE) {
    UseMethod("get_model_description", object)
}

#' @export
get_model_description.default <- function(object, indicate_training = FALSE, upper_case = TRUE) {
    cli::cli_abort("No method for class {.obj_type_friendly {object}}. Expecting an object of class 'workflow', 'model_spec', or 'model_fit'.")
}

#' @export
get_model_description.model_fit <- function(object, indicate_training = FALSE, upper_case = TRUE) {

    x <- object

    desc <- tryCatch({
        x$fit$desc
    }, error = function(e) {
        NULL
    })

    if (is.null(desc)) {
        desc <- x$spec$engine[1]
        if (is.null(desc)) {
            desc <- class(x$fit)[1]
        }
    }

    if (indicate_training) {
        desc <- stringr::str_c(desc, " (Trained)")
    }

    if (upper_case) {
        desc <- toupper(desc)
    } else {
        desc <- tolower(desc)
    }

    return(desc)
}

#' @export
get_model_description.model_spec <- function(object, indicate_training = FALSE, upper_case = TRUE) {

    spec <- object

    # Try to get engine
    desc <- spec$engine[1]

    # Get class of spec
    if (is.null(desc)) {
        desc <- class(spec)[1]
    }

    if (indicate_training) {
        desc <- stringr::str_c(desc, " (Not Trained)")
    }

    if (upper_case) {
        desc <- toupper(desc)
    } else {
        desc <- tolower(desc)
    }

    return(desc)
}

#' @export
get_model_description.workflow <- function(object, indicate_training = FALSE, upper_case = TRUE) {

    x <- object

    # Fitted Modeltime - Try to grab model description
    desc <- tryCatch({
        x$fit$fit$fit$desc
    }, error = function(e) {
        NULL
    })

    # Fitted Workflow - Try to grab engine from spec
    if (is.null(desc)) {
        desc <- tryCatch({
            x$fit$fit$spec$engine[1]
        }, error = function(e) {
            NULL
        })
    }

    # Fitted Workflow - Try to grab class from model
    if (is.null(desc)) {
        if (!is.null(x$fit$fit$fit)) {
            desc <- class(x$fit$fit$fit)[1]
        }
    }

    # Un-Fitted Workflow - Try to grab class from model engine
    if (is.null(desc)) {
        if (!is.null(x$fit$actions$model$spec)) {
            desc <- class(x$fit$actions$model$spec)[1]
        }
    }

    if (indicate_training) {

        if (x$trained) {
            desc <- stringr::str_c(desc, " (Trained)")
        } else {
            desc <- stringr::str_c(desc, " (Not Trained)")
        }

    }

    if (upper_case) {
        desc <- toupper(desc)
    } else {
        desc <- tolower(desc)
    }

    return(desc)
}


#' @export
get_model_description.recursive <- function(object, indicate_training = FALSE, upper_case = TRUE) {

    class(object) <- class(object)[3:length(class(object))]

    desc <- get_model_description(object, indicate_training = FALSE, upper_case = TRUE)

    desc <- paste("RECURSIVE", desc)

    if (upper_case) {
        desc <- toupper(desc)
    } else {
        desc <- tolower(desc)
    }

    return(desc)

}

#' @export
get_model_description.recursive_panel <- function(object, indicate_training = FALSE, upper_case = TRUE) {

    class(object) <- class(object)[3:length(class(object))]

    desc <- get_model_description(object, indicate_training = FALSE, upper_case = TRUE)

    desc <- paste("RECURSIVE", desc)

    if (upper_case) {
        desc <- toupper(desc)
    } else {
        desc <- tolower(desc)
    }

    return(desc)

}

#' @export
get_model_description.NULL <- function(object, indicate_training = FALSE, upper_case = TRUE) {

    "NULL"

}


# ARIMA Model Descriptions ----

#' Get model descriptions for Arima objects
#'
#' @param object Objects of class `Arima`
#' @param padding Whether or not to include padding
#'
#' @source
#' - Forecast R Package, `forecast:::arima.string()`
#'
#' @examples
#' library(forecast)
#'
#' arima_fit <- forecast::Arima(1:10)
#'
#' get_arima_description(arima_fit)
#'
#'
#' @export
get_arima_description <- function(object, padding = FALSE) {
    UseMethod("get_arima_description", object)
}

#' @export
get_arima_description.default <- function(object, padding = FALSE) {
    cli::cli_abort("No method for class {.obj_type_friendly {object}}. Expecting an object of class 'Arima'.")
}

#' @export
get_arima_description.Arima <- function(object, padding = FALSE) {

    order <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
    m <- order[7]
    result <- paste("ARIMA(", order[1], ",", order[2], ",", order[3], ")", sep = "")

    if (m > 1 && sum(order[4:6]) > 0) {
        result <- paste(result, "(", order[4], ",", order[5], ",", order[6], ")[", m, "]", sep = "")
    }

    if (padding && m > 1 && sum(order[4:6]) == 0) {
        result <- paste(result, "         ", sep = "")
        if (m <= 9) {
            result <- paste(result, " ", sep = "")
        } else if (m <= 99) {
            result <- paste(result, "  ", sep = "")
        } else {
            result <- paste(result, "   ", sep = "")
        }
    }

    if (!is.null(object$xreg)) {
        if (NCOL(object$xreg) == 1 && is.element("drift", names(object$coef))) {
            result <- paste(result, "with drift        ")
        } else {
            result <- paste("Regression with", result, "errors")
        }
    } else {
        if (is.element("constant", names(object$coef)) || is.element("intercept", names(object$coef))) {
            result <- paste(result, "with non-zero mean")
        } else if (order[2] == 0 && order[5] == 0) {
            result <- paste(result, "with zero mean    ")
        } else {
            result <- paste(result, "                  ")
        }
    }

    if (!padding) {
        # Strip trailing spaces
        result <- gsub("[ ]*$", "", result)
    }

    return(result)
}

# TBATS Model Descriptions ----

#' Get model descriptions for TBATS objects
#'
#' @param object Objects of class `tbats`
#'
#' @source
#' - Forecast R Package, `forecast:::as.character.tbats()`
#'
#' @export
get_tbats_description <- function(object) {

    if (!rlang::inherits_any(object, c("tbats", "bats"))) {
        cli::cli_abort("No method for class {.obj_type_friendly {object}}. Expecting an object of class 'bats' or 'tbats'.")
    }

    as.character(object)
}
