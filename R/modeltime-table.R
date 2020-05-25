#' Scale forecast analysis with a Modeltime Table
#'
#' Designed to perform forecasts at scale using models created with
#' `modeltime`, `parsnip`, `workflows`, and regression modeling extensions
#' in the `tidymodels` ecosystem.
#'
#' @param ... Fitted model or workflow objects
#'
#' @examples
#' library(tidyverse)
#' library(lubridate)
#' library(timetk)
#' library(workflows)
#' library(parsnip)
#' library(recipes)
#' library(rsample)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.9)
#'
#' # --- MODELS ---
#'
#' # Model 1: auto_arima ----
#' model_fit_no_boost <- arima_reg() %>%
#'     set_engine(engine = "auto_arima") %>%
#'     fit(value ~ date, data = training(splits))
#'
#' # Model 2: arima_boost ----
#' model_fit_boosted <- arima_boost(
#'     min_n = 2,
#'     learn_rate = 0.015
#' ) %>%
#'     set_engine(engine = "auto_arima_xgboost") %>%
#'     fit(value ~ date + as.numeric(date) + month(date, label = TRUE),
#'         data = training(splits))
#'
#' # ---- COMPARE ----
#' models_tbl <- modeltime_table(
#'     model_fit_no_boost,
#'     model_fit_boosted
#' )
#'
#' models_tbl %>%
#'     modeltime::modeltime_accuracy(new_data = testing(splits))
#'
#' @export
#' @name modeltime_table
modeltime_table <- function(...) {

    ret <- tibble::tibble(
        .model = list(...)
    ) %>%
        tibble::rowid_to_column(var = ".model_id")

    # CHECKS
    validate_model_classes(ret)

    ret <- ret %>%
        dplyr::mutate(.model_desc = purrr::map_chr(.model, .f = function(x) {

            if (inherits(x, "model_fit")) {
                desc <- tryCatch({
                    x$fit$desc
                }, error = function(e) {
                    NULL
                })

                if (is.null(desc)) {
                    desc <- toupper(x$spec$engine[1])
                    if (is.null(desc)) {
                        desc <- class(x$fit)[1]
                    }
                }

            } else if (inherits(x, "workflow")) {

                desc <- tryCatch({
                    x$fit$fit$fit$desc
                }, error = function(e) {
                    NULL
                })

                if (is.null(desc)) {
                    desc <- toupper(x$fit$fit$spec$engine[1])
                    if (is.null(desc)) {
                        desc <- class(x$fit$fit$fit)[1]
                    }
                }
            } else {
                rlang::abort("Object(s) must be fitted parsnip models or fitted workflows.")
            }
            return(desc)

        }))

    class(ret) <- c("mdl_time_tbl", class(ret))

    return(ret)
}

#' @export
print.mdl_time_tbl <- function(x, ...) {
    cat("# Modeltime Table\n")
    class(x) <- class(x)[!(class(x) %in% c("mdl_time_tbl"))]
    print(x, ...)
}




