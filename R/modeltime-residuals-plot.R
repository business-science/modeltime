#' Interactive Residuals Visualization
#'
#' This is a wrapper for examining residuals using:
#' - Time Plot: [plot_time_series()]
#' - ACF Plot: [plot_acf_diagnostics()]
#' - Seasonality Plot: [plot_seasonal_diagnostics()]
#'
#'
#' @inheritParams timetk::plot_time_series
#' @param .data A `tibble` that is the output of [modeltime_residuals()]
#' @param .type One of "timeplot", "acf", or "seasonality". The default is "timeplot".
#' @param .legend_show Logical. Whether or not to show the legend.
#'  Can save space with long model descriptions.
#' @param .legend_max_width Numeric. The width of truncation to apply to the legend text.
#' @param ... Additional arguments passed to:
#' - Time Plot: [plot_time_series()]
#' - ACF Plot: [plot_acf_diagnostics()]
#' - Seasonality Plot: [plot_seasonal_diagnostics()]
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot containing residuals vs time
#'
#'
#' @examples
#' library(tidyverse)
#' library(lubridate)
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
#' # Model 1: auto_arima ----
#' model_fit_arima <- arima_reg() %>%
#'     set_engine(engine = "auto_arima") %>%
#'     fit(value ~ date, data = training(splits))
#'
#'
#' # ---- MODELTIME TABLE ----
#'
#' models_tbl <- modeltime_table(
#'     model_fit_arima
#' )
#'
#' # ---- RESIDUALS ----
#'
#' models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits)) %>%
#'     modeltime_residuals() %>%
#'     plot_modeltime_residuals(.interactive = FALSE)
#'
#' @export
plot_modeltime_residuals <- function(.data,
                                     .type = c("timeplot", "acf", "seasonality"),
                                     .legend_show = TRUE,
                                     .legend_max_width = 40,
                                     .y_intercept = 0,
                                     .y_intercept_color = "#3366FF",
                                     .title = "Residuals Plot", .x_lab = "", .y_lab = "",
                                     .color_lab = "Legend",
                                     .interactive = TRUE,
                                     ...) {

    # Checks
    if (!inherits(.data, "data.frame")) {
        glubort("No method for {class(.data)[1]}. Expecting the output of 'modeltime_forecast()'.")
    }

    if (!all(c(".model_id", ".model_desc", ".type", ".index", ".actual", ".prediction", ".residuals") %in% names(.data))) {
        rlang::abort("Expecting the following names to be in the data frame: .index, .actual, .prediction, and .residuals. Try using 'modeltime_residuals()' to return a data frame in the appropriate structure.")
    }


    data_prepared <- .data %>%
        # dplyr::ungroup() %>%
        dplyr::mutate(.model_desc = ifelse(!is.na(.model_id), stringr::str_c(.model_id, "_", .model_desc), .model_desc)) %>%
        # dplyr::mutate(.model_desc = ifelse(is.na(.value), stringr::str_c("(ERROR) ", .model_desc), .model_desc)) %>%
        dplyr::mutate(.model_desc = .model_desc %>% stringr::str_trunc(width = .legend_max_width)) %>%
        dplyr::mutate(.model_desc = forcats::as_factor(.model_desc))


    .type <- tolower(.type[[1]])

    if (.type == "timeplot") {
        timetk::plot_time_series(
            .data          = data_prepared,
            .date_var      = .index,
            .value         = .residuals,
            .color_var     = .model_desc,

            .smooth            = FALSE,
            .y_intercept       = .y_intercept,
            .y_intercept_color = .y_intercept_color,

            .title         = .title,
            .x_lab         = .x_lab,
            .y_lab         = .y_lab,
            .color_lab     = .color_lab,
            .interactive   = .interactive,
            ...
        )
    } else if (.type == "acf") {

        timetk::plot_acf_diagnostics(
            .data          = dplyr::group_by(data_prepared, .model_desc),
            .date_var      = .index,
            .value         = .residuals,

            .title         = .title,
            .x_lab         = .x_lab,
            .y_lab         = .y_lab,
            .interactive   = .interactive,
            ...
        )

    } else if (.type == "seasonality") {

        timetk::plot_seasonal_diagnostics(
            .data          = dplyr::group_by(data_prepared, .model_desc),
            .date_var      = .index,
            .value         = .residuals,

            .title         = .title,
            .x_lab         = .x_lab,
            .y_lab         = .y_lab,
            .interactive   = .interactive,
            ...
        )
    }


}

