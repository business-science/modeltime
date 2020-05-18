#' Interactive Forecast Visualization
#'
#' This is a wrapper for [plot_time_series()] that generates an interactive (`plotly`) or static
#' (`ggplot2`) plot with the forecasted data.
#'
#' @inheritParams timetk::plot_time_series
#' @param .data A `tibble` or `data.frame` with '.id', '.index', and .value' columns
#' @param .include_conf_interval Logical. Whether or not to include the confidence interval as a ribbon.
#' @param ... Additional arguments passed to [timetk::plot_time_series()].
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot containing a forecast
#'
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
#'     set_engine("Arima")
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
#'     mutate(.value = exp(.value)) %>%
#'     plot_modeltime_forecast(.interactive = FALSE)
#'
#' @export
plot_modeltime_forecast <- function(.data,
                                    .include_conf_interval = TRUE,
                                    .title = "Forecast Plot", .x_lab = "", .y_lab = "",
                                    .color_lab = "Legend",
                                    .interactive = TRUE, .plotly_slider = FALSE,
                                    ...) {

    # Checks
    if (!all(c(".id", ".index", ".value") %in% names(.data))) {
        rlang::abort("Expecting the following names to be in the data frame: .id, .index, .value. Try using 'modeltime_forecast()' to return a data frame in the appropriate structure.")
    }

    if (.include_conf_interval) {
        if (!all(c(".conf_lo", ".conf_hi") %in% names(.data))) {
            rlang::abort("Expecting the following names to be in the data frame: .conf_hi, .conf_lo. Try using '.include_conf_interval = FALSE' to visualize the forecast without confidence intervals.")
        }
    }

    if (!.include_conf_interval) {

        g <- timetk::plot_time_series(
            .data         = .data,
            .date_var     = .index,
            .value        = .value,
            .color_var    = .id,

            .smooth       = FALSE,

            .title        = .title,
            .x_lab        = .x_lab,
            .y_lab        = .y_lab,
            .color_lab    = .color_lab,
            .interactive  = FALSE,
            ...
        )
    } else {
        g <- timetk::plot_time_series(
            .data         = .data,
            .date_var     = .index,
            .value        = .value,
            .color_var    = .id,

            .smooth       = FALSE,

            .title        = .title,
            .x_lab        = .x_lab,
            .y_lab        = .y_lab,
            .color_lab    = .color_lab,
            .interactive  = FALSE,
            ...
        )

        # Add ribbon
        g <- g +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = .conf_lo, ymax = .conf_hi), alpha = 0.2)

        # Reorder Ribbon to 1st level
        layers_start <- g$layers

        g$layers[[1]] <- layers_start[[2]]
        g$layers[[2]] <- layers_start[[1]]

    }

    # INTERACTIVE

    if (.interactive) {

        p <- plotly::ggplotly(g, dynamicTicks = TRUE)

        if (.plotly_slider) {
            p <- p %>%
                plotly::layout(
                    xaxis = list(
                        rangeslider = list(type = "date")
                    )
                )
        }

        return(p)
    } else {
        return(g)
    }

}
