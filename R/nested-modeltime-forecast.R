

#' Modeltime Nested Forecast
#'
#' Make a new forecast from a Nested Modeltime Table.
#'
#'
#'
#' @export
modeltime_nested_forecast <- function(object, h = NULL, include_actual = TRUE, conf_interval = 0.95,
                                      control = control_nested_forecast()) {


    UseMethod("modeltime_nested_forecast", object)



}

#' @export
modeltime_nested_forecast.nested_mdl_time <- function(object, h, include_actual, conf_interval,
                                                      control) {

    # New h is not used, return logged forecasts
    if (is.null(h)) {

        fit_column <- attr(object, "fit_column")

        if (fit_column == ".actual_data") {
            if (control$verbose) message("Returning logged future forecast. If new predictions are needed, set `h` for horizon.")
            return(extract_nested_future_forecast(object))
        } else {
            if (control$verbose) message("Returning logged test forecast. If new predictions are needed, set `h` for horizon.")
            return(extract_nested_test_forecast(object))
        }
    }

    # Parallel or Sequential
    if ((control$cores > 1) && control$allow_par) {
        ret <- modeltime_nested_forecast_parallel(
            object         = object,
            # new_data       = new_data,
            h              = h,
            conf_interval  = conf_interval,
            include_actual = include_actual,
            # id             = id,
            control        = control
        )
    } else {
        ret <- modeltime_nested_forecast_sequential(
            object         = object,
            # new_data       = new_data,
            h              = h,
            conf_interval  = conf_interval,
            include_actual = include_actual,
            # id             = id,
            control        = control
        )
    }

    return(ret)

}

modeltime_nested_forecast_sequential <- function(object, h, include_actual, conf_interval,
                                                 control) {

    t1 <- Sys.time()

    # HANDLE INPUTS ----

    id_text <- attr(object, "id")

    object <- object %>%
        dplyr::select(dplyr::one_of(id_text), ".actual_data", ".future_data", ".splits", ".modeltime_tables")

    id_expr <- rlang::sym(id_text)

    n_ids   <- nrow(object)

    x_expr  <- rlang::sym(".modeltime_tables")

    d_expr  <- rlang::sym(".actual_data")

    f_expr  <- rlang::sym(".future_data")


    # SETUP PROGRESS

    logging_env <- rlang::env(
        error_tbl = tibble::tibble()
    )

    if (!control$verbose) cli::cli_progress_bar("Forecast predictions...", total = nrow(object), .envir = logging_env)

    # LOOP LOGIC ----

    nested_modeltime <- object %>%
        tibble::rowid_to_column(var = '..rowid') %>%
        dplyr::mutate(
            .forecast = purrr::pmap(.l = list(x = !! x_expr, d = !! d_expr, f = !! f_expr, id = !! id_expr, i = ..rowid), .f = function(x, d, f, id, i) {


                if (control$verbose) cli::cli_alert_info(stringr::str_glue("[{i}/{n_ids}] Starting Forecast: ID {id}..."))

                # Future Forecast ----
                fcast_tbl <- NULL
                if (!include_actual) d <- NULL
                suppressMessages({
                    suppressWarnings({

                        tryCatch({

                            # print(conf_interval)

                            fcast_tbl <- modeltime_forecast(
                                object        = x,
                                h             = h,
                                # new_data      = f,
                                actual_data   = d,
                                conf_interval = conf_interval
                            ) %>%
                                tibble::add_column(!! id_text := id, .before = 1)

                        }, error=function(e){

                            err <- capture.output(e)

                            error_tbl <- tibble::tibble(
                                !! id_text := id,
                                .error_desc = ifelse(is.null(err), NA_character_, err)
                            )

                            logging_env$error_tbl <- dplyr::bind_rows(logging_env$error_tbl, error_tbl)
                        })


                    })
                })

                # Finish ----

                if (control$verbose) {
                    if (!is.null(fcast_tbl)) {
                        cli::cli_alert_success(stringr::str_glue("[{i}/{n_ids}] Finished Forecasting: ID {id}"))
                    } else {
                        cli::cli_alert_danger(stringr::str_glue("[{i}/{n_ids}] Forecasting Failed: ID {id}"))
                    }

                }
                if (control$verbose) cat("\n")

                if (!control$verbose) cli::cli_progress_update(.envir = logging_env)

                return(fcast_tbl)
            })
        ) %>%
        dplyr::select(-..rowid)

    if (!control$verbose) cli::cli_progress_done(.envir = logging_env)

    # FINALIZE RESULTS ----

    ret <- nested_modeltime %>%
        dplyr::select(.forecast) %>%
        tidyr::unnest(.forecast)

    # FINISH TIMING ----

    t2 <- Sys.time()

    time_elapsed <- difftime(t2, t1, units = "auto") %>%
        utils::capture.output() %>%
        stringr::str_remove("Time difference of ")

    if (control$verbose) cli::cli_inform(stringr::str_glue("Finished in: {time_elapsed}."))

    # STRUCTURE ----

    error_tbl <- logging_env$error_tbl
    if (nrow(error_tbl) > 1) {
        error_tbl <- error_tbl %>%
            tidyr::drop_na(.error_desc)
    }

    class(ret) <- c("mdl_forecast_tbl", class(ret))

    attr(ret, "conf_interval")       <- conf_interval
    attr(ret, "error_tbl")           <- error_tbl
    attr(ret, "time_elapsed")        <- time_elapsed

    if (nrow(attr(nested_modeltime, "error_tbl")) > 0) {
        rlang::warn("Some modeltime tables had errors during forecasting. Use `extract_nested_error_report()` to review errors.")
    }

    return(ret)

}
