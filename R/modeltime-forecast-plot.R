#' Interactive Forecast Visualization
#'
#' This is a wrapper for [plot_time_series()] that generates an interactive (`plotly`) or static
#' (`ggplot2`) plot with the forecasted data.
#'
#' @inheritParams timetk::plot_time_series
#' @param .data A `tibble` that is the output of [modeltime_forecast()]
#' @param .legend_show Logical. Whether or not to show the legend.
#'  Can save space with long model descriptions.
#' @param .legend_max_width Numeric. The width of truncation to apply to the legend text.
#' @param .conf_interval_show Logical. Whether or not to include the confidence interval as a ribbon.
#' @param .conf_interval_fill Fill color for the confidence interval
#' @param .conf_interval_alpha Fill opacity for the confidence interval. Range (0, 1).
#' @param ... Additional arguments passed to [timetk::plot_time_series()].
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot containing a forecast
#'
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
#' # Model 1: prophet ----
#' model_fit_prophet <- prophet_reg() %>%
#'     set_engine(engine = "prophet") %>%
#'     fit(value ~ date, data = training(splits))
#'
#'
#' # ---- MODELTIME TABLE ----
#'
#' models_tbl <- modeltime_table(
#'     model_fit_prophet
#' )
#'
#' # ---- FORECAST ----
#'
#' models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits)) %>%
#'     modeltime_forecast(
#'         new_data    = testing(splits),
#'         actual_data = m750
#'     ) %>%
#'     plot_modeltime_forecast(.interactive = FALSE)
#'
#' @export
plot_modeltime_forecast <- function(.data,
                                    .conf_interval_show = TRUE,
                                    .conf_interval_fill = "grey20",
                                    .conf_interval_alpha = 0.20,
                                    .smooth = FALSE,
                                    .legend_show = TRUE,
                                    .legend_max_width = 40,
                                    .title = "Forecast Plot", .x_lab = "", .y_lab = "",
                                    .color_lab = "Legend",
                                    .interactive = TRUE, .plotly_slider = FALSE,
                                    ...) {

    # Checks
    if (!inherits(.data, "data.frame")) {
        glubort("No method for {class(.data)[1]}. Expecting the output of 'modeltime_forecast()'.")
    }

    if (!all(c(".model_id", ".model_desc", ".key", ".index", ".value") %in% names(.data))) {
        rlang::abort("Expecting the following names to be in the data frame: .key, .index, .value. Try using 'modeltime_forecast()' to return a data frame in the appropriate structure.")
    }

    if (.conf_interval_show) {
        if (!all(c(".conf_lo", ".conf_hi") %in% names(.data))) {
            .conf_interval_show <- FALSE
            rlang::warn("Expecting the following names to be in the data frame: .conf_hi, .conf_lo. \nProceeding with '.conf_interval_show = FALSE' to visualize the forecast without confidence intervals.\nAlternatively, try using `modeltime_calibrate()` before forecasting to add confidence intervals.")
        }
    }


    g <- plot_modeltime_forecast_multi(
        .data                  = .data,
        .conf_interval_show    = .conf_interval_show,
        .conf_interval_fill    = .conf_interval_fill,
        .conf_interval_alpha   = .conf_interval_alpha,
        .smooth                = .smooth,
        .legend_show           = .legend_show,
        .legend_max_width      = .legend_max_width,
        .title                 = .title,
        .x_lab                 = .x_lab,
        .y_lab                 = .y_lab,
        .color_lab             = .color_lab,
        .interactive           = .interactive,
        .plotly_slider         = .plotly_slider,
        ...
    )


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



plot_modeltime_forecast_multi <- function(.data,
                                          .conf_interval_show = TRUE,
                                          .conf_interval_fill = "grey20",
                                          .conf_interval_alpha = 0.20,
                                          .smooth = FALSE,
                                          .legend_show = TRUE,
                                          .legend_max_width = 40,
                                          .title = "Forecast Plot", .x_lab = "", .y_lab = "",
                                          .color_lab = "Legend",
                                          .interactive = TRUE, .plotly_slider = FALSE,
                                          ...) {


    # Data prep
    data_prepared <- .data %>%
        dplyr::arrange(.key, .model_id, .index) %>%
        # dplyr::ungroup() %>%
        dplyr::mutate(.model_desc = ifelse(!is.na(.model_id), stringr::str_c(.model_id, "_", .model_desc), .model_desc)) %>%
        dplyr::mutate(.model_desc = ifelse(is.na(.value), stringr::str_c("(ERROR) ", .model_desc), .model_desc)) %>%
        dplyr::mutate(.model_desc = .model_desc %>% stringr::str_trunc(width = .legend_max_width)) %>%
        dplyr::mutate(.model_desc = forcats::as_factor(.model_desc))

    # Isolate just the forecast data
    data_prepared_forecast_only <- data_prepared %>%
        dplyr::filter(.model_desc != "ACTUAL")

    # Check for only 1 forecast
    #  if 1 forecast, n_forecast_timestamps == 1
    n_forecast_timestamps <- data_prepared_forecast_only %>%
        dplyr::ungroup() %>%
        dplyr::pull(.index) %>%
        unique() %>%
        length()

    # Make the plot
    g <- timetk::plot_time_series(
        .data         = data_prepared,
        .date_var     = .index,
        .value        = .value,
        .color_var    = .model_desc,

        .smooth       = .smooth,

        .title        = .title,
        .x_lab        = .x_lab,
        .y_lab        = .y_lab,
        .color_lab    = .color_lab,
        .interactive  = FALSE
        ,
        ...
    )

    # If forecast timestamps are 1, add geom_point()
    if (n_forecast_timestamps == 1) {
        g <- g +
            ggplot2::geom_point(
                ggplot2::aes(color = .model_desc),
                data = . %>% dplyr::filter(.model_desc != "ACTUAL")
            )
    }

    # Add ribbon
    if (.conf_interval_show) {

        if (n_forecast_timestamps > 1) {

            # Add ribbon
            g <- g +
                ggplot2::geom_ribbon(
                    ggplot2::aes(
                        ymin = .conf_lo,
                        ymax = .conf_hi,
                        # group = .model_desc
                        # ,
                        color = .model_desc
                    ),
                    fill     = .conf_interval_fill,
                    alpha    = .conf_interval_alpha,
                    # color    = .conf_interval_fill,
                    # na.rm    = TRUE, # causes error
                    # data = . %>% dplyr::filter(.model_desc != "ACTUAL"),
                    linetype = 0
                )


            # Reorder Ribbon to 1st level
            layers_start <- g$layers

            g$layers[[1]] <- layers_start[[2]]
            g$layers[[2]] <- layers_start[[1]]

        }

    }

    if (!.legend_show) {
        g <- g +
            ggplot2::theme(legend.position = "none")
    }

    return(g)

}
