#' Interactive Plotting for One or More Time Series
#'
#' A workhorse time-series plotting function that generates interactive `plotly` plots,
#' consolidates 20+ lines of `ggplot2` code, and scales well to many time series.
#'
#' @inheritParams timetk::plot_time_series
#' @param .data A `tibble` or `data.frame` with '.id', '.index', and .value' columns
#' @param ... Additional arguments passed to [timetk::plot_time_series()].
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot containing a forecast
#'
#' @details
#'
#' TODO
#'
#'
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(rsample)
#' library(timetk)
#' library(modeltime)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#' m750
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.8)
#'
#' # Model Spec
#' model_spec <- arima_reg(
#'         period                   = 12,
#'         non_seasonal_ar          = 3,
#'         non_seasonal_differences = 1,
#'         non_seasonal_ma          = 3,
#'         seasonal_ar              = 1,
#'         seasonal_differences     = 0,
#'         seasonal_ma              = 1
#'     ) %>%
#'     set_engine("forecast")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#'
#' # --- VISUALIZE FORECAST ---
#'
#' # Combining forecast with actual values
#' model_fit %>%
#'     modeltime_forecast(h = "3 years", actual_data = training(splits)) %>%
#'     plot_modeltime_forecast(.interactive = FALSE)
#'
#' @export
plot_modeltime_forecast <- function(.data,
                                    .title = "Forecast Plot", .x_lab = "", .y_lab = "",
                                    .color_lab = "Legend",
                                    .interactive = TRUE,
                                    ...) {

    # Checks
    if (!all(c(".id", ".index", ".value") %in% names(.data))) {
        rlang::abort("Expecting the following names to be in the data frame: .id, .index, .value. Try using 'modeltime_forecast()' to return a data frame in the appropriate structure.")
    }

    timetk::plot_time_series(
        .data         = .data,
        .date_var     = .index,
        .value        = .value,
        .color_var    = .id,

        .smooth       = FALSE,

        .title        = .title,
        .x_lab        = .x_lab,
        .y_lab        = .y_lab,
        .color_lab    = .color_lab,
        .interactive  = .interactive,
        ...
    )
}
