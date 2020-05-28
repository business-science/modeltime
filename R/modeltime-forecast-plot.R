#' Interactive Forecast Visualization
#'
#' This is a wrapper for [plot_time_series()] that generates an interactive (`plotly`) or static
#' (`ggplot2`) plot with the forecasted data.
#'
#' @inheritParams timetk::plot_time_series
#' @param .data A `tibble` that is the output of [modeltime_forecast()]
#' @param .include_legend Logical. Whether or not to show the legend.
#'  Can save space with long model descriptions.
#' @param .include_conf_interval Logical. Whether or not to include the confidence interval as a ribbon.
#' @param .conf_interval_fill Fill color for the confidence interval
#' @param .conf_interval_alpha Fill opacity for the confidence interval. Range (0, 1).
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
#'     set_engine("arima")
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
#'     plot_modeltime_forecast(.include_conf_interval = TRUE, .interactive = FALSE)
#'
#' @export
plot_modeltime_forecast <- function(.data,
                                    .include_conf_interval = TRUE,
                                    .conf_interval_fill = "grey20",
                                    .conf_interval_alpha = 0.20,
                                    .include_legend = TRUE,
                                    .title = "Forecast Plot", .x_lab = "", .y_lab = "",
                                    .color_lab = "Legend",
                                    .interactive = TRUE, .plotly_slider = FALSE,
                                    ...) {

    # Checks
    if (!all(c(".key", ".index", ".value") %in% names(.data))) {
        rlang::abort("Expecting the following names to be in the data frame: .key, .index, .value. Try using 'modeltime_forecast()' to return a data frame in the appropriate structure.")
    }

    if (.include_conf_interval) {
        if (!all(c(".conf_lo", ".conf_hi") %in% names(.data))) {
            .include_conf_interval <- FALSE
            rlang::warn("Expecting the following names to be in the data frame: .conf_hi, .conf_lo. \nProceeding with '.include_conf_interval = FALSE' to visualize the forecast without confidence intervals.\nAlternatively, try using `modeltime_calibrate()` before forecasting to add confidence intervals.")
        }
    }

    # PASS SINGLE / MULTI

    # Output of a scalable modeltime_table() that has been forecasted
    multi <- FALSE
    if (all(c(".model_id", ".model_desc") %in% names(.data))) {
        multi <- TRUE
    }

    if (multi) {
        g <- plot_modeltime_forecast_multi(
            .data                  = .data,
            .include_conf_interval = .include_conf_interval,
            .conf_interval_fill    = .conf_interval_fill,
            .conf_interval_alpha   = .conf_interval_alpha,
            .include_legend        = .include_legend,
            .title                 = .title,
            .x_lab                 = .x_lab,
            .y_lab                 = .y_lab,
            .color_lab             = .color_lab,
            .interactive           = .interactive,
            .plotly_slider         = .plotly_slider,
            ...
        )
    } else {
        g <- plot_modeltime_forecast_single(
            .data                  = .data,
            .include_conf_interval = .include_conf_interval,
            .conf_interval_fill    = .conf_interval_fill,
            .conf_interval_alpha   = .conf_interval_alpha,
            .include_legend        = .include_legend,
            .title                 = .title,
            .x_lab                 = .x_lab,
            .y_lab                 = .y_lab,
            .color_lab             = .color_lab,
            .interactive           = .interactive,
            .plotly_slider         = .plotly_slider,
            ...
        )
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


plot_modeltime_forecast_single <- function(.data,
                                           .include_conf_interval = TRUE,
                                           .conf_interval_fill = "grey20",
                                           .conf_interval_alpha = 0.20,
                                           .include_legend = TRUE,
                                           .title = "Forecast Plot", .x_lab = "", .y_lab = "",
                                           .color_lab = "Legend",
                                           .interactive = TRUE, .plotly_slider = FALSE,
                                           ...) {



    g <- timetk::plot_time_series(
        .data         = .data,
        .date_var     = .index,
        .value        = .value,
        .color_var    = .key,

        .smooth       = FALSE,

        .title        = .title,
        .x_lab        = .x_lab,
        .y_lab        = .y_lab,
        .color_lab    = .color_lab,
        .interactive  = FALSE,
        ...
    )

    if (.include_conf_interval) {

        # Add ribbon
        g <- g +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = .conf_lo, ymax = .conf_hi, color = .key),
                                 fill = .conf_interval_fill,
                                 alpha = .conf_interval_alpha,
                                 # color = .conf_interval_fill,
                                 linetype = 0)

        # Reorder Ribbon to 1st level
        layers_start <- g$layers

        g$layers[[1]] <- layers_start[[2]]
        g$layers[[2]] <- layers_start[[1]]

    }

    if (!.include_legend) {
        g <- g +
            ggplot2::theme(legend.position = "none")
    }

    return(g)

}


plot_modeltime_forecast_multi <- function(.data,
                                          .include_conf_interval = TRUE,
                                          .conf_interval_fill = "grey20",
                                          .conf_interval_alpha = 0.20,
                                          .include_legend = TRUE,
                                          .title = "Forecast Plot", .x_lab = "", .y_lab = "",
                                          .color_lab = "Legend",
                                          .interactive = TRUE, .plotly_slider = FALSE,
                                          ...) {


    # Data prep
    data_prepared <- .data %>%
        # dplyr::ungroup() %>%
        dplyr::mutate(.model_desc = ifelse(!is.na(.model_id), stringr::str_c(.model_id, "_", .model_desc), .model_desc)) %>%
        dplyr::mutate(.model_desc = ifelse(is.na(.value), stringr::str_c("(ERROR) ", .model_desc), .model_desc)) %>%
        dplyr::mutate(.model_desc = forcats::as_factor(.model_desc))


    g <- timetk::plot_time_series(
        .data         = data_prepared,
        .date_var     = .index,
        .value        = .value,
        .color_var    = .model_desc,

        .smooth       = FALSE,

        .title        = .title,
        .x_lab        = .x_lab,
        .y_lab        = .y_lab,
        .color_lab    = .color_lab,
        .interactive  = FALSE,
        ...
    )

    # Add ribbon
    if (.include_conf_interval) {

        # Add ribbon
        g <- g +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = .conf_lo, ymax = .conf_hi, color = .model_desc),
                                 fill = .conf_interval_fill,
                                 alpha = .conf_interval_alpha,
                                 # color = .conf_interval_fill,
                                 linetype = 0)

        # Reorder Ribbon to 1st level
        layers_start <- g$layers

        g$layers[[1]] <- layers_start[[2]]
        g$layers[[2]] <- layers_start[[1]]

    }

    if (!.include_legend) {
        g <- g +
            ggplot2::theme(legend.position = "none")
    }

    return(g)

}
