#' Scale forecast analysis with a Modeltime Table
#'
#' Designed to perform forecasts at scale using models created with
#' `modeltime`, `parsnip`, `workflows`, and regression modeling extensions
#' in the `tidymodels` ecosystem.
#'
#' @param ... Fitted `parsnip` model or `workflow` objects
#' @param .l A list containing fitted `parsnip` model or `workflow` objects
#'
#' @details
#'
#' `modeltime_table()`:
#'
#' 1. Creates a table of models
#' 2. Validates that all objects are models (parsnip or workflows objects) and
#'  all models have been fitted (trained)
#' 3. Provides an ID and Description of the models
#'
#' `as_modeltime_table()`:
#'
#' Converts a `list` of models to a modeltime table. Useful if programatically creating
#' Modeltime Tables from models stored in a `list`.
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#' library(parsnip)
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
#' # Model 1: prophet ----
#' model_fit_prophet <- prophet_reg() %>%
#'     set_engine(engine = "prophet") %>%
#'     fit(value ~ date, data = training(splits))
#'
#'
#' # ---- MODELTIME TABLE ----
#'
#' # Make a Modeltime Table
#' models_tbl <- modeltime_table(
#'     model_fit_prophet
#' )
#'
#' # Can also convert a list of models
#' list(model_fit_prophet) %>%
#'     as_modeltime_table()
#'
#' # ---- CALIBRATE ----
#'
#' calibration_tbl <- models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits))
#'
#' # ---- ACCURACY ----
#'
#' calibration_tbl %>%
#'     modeltime_accuracy()
#'
#' # ---- FORECAST ----
#'
#' calibration_tbl %>%
#'     modeltime_forecast(
#'         new_data    = testing(splits),
#'         actual_data = m750
#'     )
#'
#' @export
#' @name modeltime_table
modeltime_table <- function(...) {
    as_modeltime_table(list(...))
}

#' @export
print.mdl_time_tbl <- function(x, ...) {
    cat("# Modeltime Table\n")
    class(x) <- class(x)[!(class(x) %in% c("mdl_time_tbl"))]
    print(x, ...)
}

#' @export
#' @rdname modeltime_table
as_modeltime_table <- function(.l) {

    ret <- tibble::tibble(
        .model = .l
    ) %>%
        tibble::rowid_to_column(var = ".model_id")

    # CHECKS
    validate_model_classes(ret, accept_classes = c("model_fit", "workflow", "mdl_time_ensemble"))
    validate_models_are_trained(ret)

    # CREATE MODELTIME OBJECT
    ret <- ret %>%
        dplyr::mutate(.model_desc = purrr::map_chr(.model, .f = get_model_description))

    class(ret) <- c("mdl_time_tbl", class(ret))

    return(ret)
}


