

#' Modeltime Nested Forecast
#'
#' Make a new forecast from a Nested Modeltime Table.
#'
#' @inheritParams modeltime_forecast
#' @param object A Nested Modeltime Table
#' @param h The forecast horizon. Extends the "trained on" data "h" periods
#'  into the future.
#' @param include_actual Whether or not to include the ".actual_data" as part of the forecast.
#'  If FALSE, just returns the forecast predictions.
#' @param id_subset A sequence of ID's from the modeltime table to subset the forecasting process.
#'  This can speed forecasts up.
#' @param control Used to control verbosity and parallel processing. See [control_nested_forecast()].
#'
#' @details
#'
#' This function is designed to help users that want to make new forecasts other than those
#' that are created during the logging process as part of the Nested Modeltime Workflow.
#'
#' ## Logged Forecasts
#'
#' The logged forecasts can be extracted using:
#'
#' - [extract_nested_future_forecast()]: Extracts the future forecast created after refitting with `modeltime_nested_refit()`.
#' - [extract_nested_test_forecast()]: Extracts the test forecast created after initial fitting with `modeltime_nested_fit()`.
#'
#' The problem is that these forecasts are static. The user would need to redo the fitting, model selection,
#' and refitting process to obtain new forecasts. This is why `modeltime_nested_forecast()` exists. So you can create
#' a new forecast without retraining any models.
#'
#' ## Nested Forecasts
#'
#' The main arguments is
#' `h`, which is a horizon that specifies how far into the future to make the new forecast.
#'
#' - If `h = NULL`, a logged forecast will be returned
#' - If `h = 12`, a new forecast will be generated that extends each series 12-periods into the future.
#' - If `h = "2 years"`, a new forecast will be generated that extends each series 2-years into the future.
#'
#' Use the `id_subset` to filter the Nested Modeltime Table `object` to just the time series of interest.
#'
#' Use the `conf_interval` to override the logged confidence interval.
#' Note that this will have no effect if `h = NULL` as logged forecasts are returned.
#' So be sure to provide `h` if you want to update the confidence interval.
#'
#' Use the `control` argument to apply verbosity during the forecasting process and to run forecasts in parallel.
#' Generally, parallel is better if many forecasts are being generated.
#'
#' @export
modeltime_nested_forecast <- function(object, h = NULL, include_actual = TRUE,
                                      conf_interval = 0.95, conf_method = "conformal_default",
                                      id_subset = NULL,
                                      control = control_nested_forecast()) {

    # Checks
    fit_column <- attr(object, "fit_column")
    if (fit_column != ".actual_data") {
        rlang::abort("The Modeltime Object must be trained on `.actual_data`. Try using `modeltime_nested_refit()`.")
    }

    UseMethod("modeltime_nested_forecast", object)

}

#' @export
modeltime_nested_forecast.nested_mdl_time <- function(object, h = NULL, include_actual = TRUE,
                                                      conf_interval = 0.95, conf_method = "conformal_default",
                                                      id_subset = NULL,
                                                      control = control_nested_forecast()) {


    # New h is not used, return logged forecasts
    if (is.null(h)) {

        if (control$verbose) message("Returning logged future forecast. If new predictions are needed, set `h` for horizon.")
        return(extract_nested_future_forecast(object, .include_actual = include_actual, .id_subset = id_subset))

        # fit_column <- attr(object, "fit_column")
        #
        # if (fit_column == ".actual_data") {
        #     if (control$verbose) message("Returning logged future forecast. If new predictions are needed, set `h` for horizon.")
        #     return(extract_nested_future_forecast(object))
        # } else {
        #     if (control$verbose) message("Returning logged test forecast. If new predictions are needed, set `h` for horizon.")
        #     return(extract_nested_test_forecast(object))
        # }
    }

    if (!is.null(id_subset)) {

        id_text <- attr(object, "id")
        id_expr <- rlang::sym(id_text)

        object <- object %>%
            dplyr::filter(!! id_expr %in% id_subset)
    }

    # Parallel or Sequential
    if ((control$cores > 1) && control$allow_par) {
        ret <- modeltime_nested_forecast_parallel(
            object         = object,
            # new_data       = new_data,
            h              = h,
            conf_interval  = conf_interval,
            conf_method    = conf_method,
            include_actual = include_actual,
            control        = control
        )
    } else {
        ret <- modeltime_nested_forecast_sequential(
            object         = object,
            # new_data       = new_data,
            h              = h,
            conf_interval  = conf_interval,
            conf_method    = conf_method,
            include_actual = include_actual,
            control        = control
        )
    }

    return(ret)

}

modeltime_nested_forecast_sequential <- function(object, h, include_actual, conf_interval, conf_method,
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
                                conf_interval = conf_interval,
                                conf_method   = conf_method
                            ) %>%
                                tibble::add_column(!! id_text := id, .before = 1)

                        }, error=function(e){

                            err <- utils::capture.output(e)

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

    class(ret) <- c("mdl_nested_forecast_tbl", class(ret))

    attr(ret, "conf_interval")       <- conf_interval
    attr(ret, "conf_method")         <- conf_method
    attr(ret, "error_tbl")           <- error_tbl
    attr(ret, "time_elapsed")        <- time_elapsed

    if (nrow(attr(ret, "error_tbl")) > 0) {
        rlang::warn("Some .modeltime_tables had errors during forecasting. Use `extract_nested_error_report()` to review errors.")
    }

    return(ret)

}


modeltime_nested_forecast_parallel <- function(object, h, include_actual, conf_interval, conf_method,
                                               control) {

    t1 <- Sys.time()

    # Parallel Detection
    is_par_setup <- foreach::getDoParWorkers() > 1

    # If parallel processing is not set up, set up parallel backend
    par_setup_info <- setup_parallel_processing(control, is_par_setup, t1)
    clusters_made  <- par_setup_info$clusters_made
    cl             <- par_setup_info$cl

    # Setup Foreach
    `%op%` <- get_operator(allow_par = control$allow_par)


    # HANDLE INPUTS ----

    id_text <- attr(object, "id")

    object <- object %>%
        dplyr::select(dplyr::one_of(id_text), ".actual_data", ".future_data", ".splits", ".modeltime_tables")

    n_ids   <- nrow(object)

    # SETUP ITERABLES ----

    model_list  = object$.modeltime_tables

    splits_list = object$.splits

    actual_list = object$.actual_data

    id_vec      = object[[id_text]]

    # BEGIN LOOP -----
    if (control$verbose) {
        t <- Sys.time()
        message(stringr::str_glue(" Beginning Parallel Loop | {round(t-t1, 3)} seconds"))
    }

    ret <- foreach::foreach(
        x                   = model_list,
        d                   = actual_list,
        id                  = id_vec,
        .inorder            = TRUE,
        .packages           = control$packages,
        # .export             = c("id_text", "model_list", "n_ids", "safe_fit"),
        .verbose            = FALSE
    ) %op% {

        # Future Forecast ----
        fcast_tbl <- NULL
        error_tbl <- NULL
        err       <- NULL
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
                        conf_interval = conf_interval,
                        conf_method   = conf_method
                    ) %>%
                        tibble::add_column(!! id_text := id, .before = 1)

                }, error=function(e){

                    err <- utils::capture.output(e)

                })
            })
        })

        if(is.null(fcast_tbl)) err <- "Forecast Failed" else err <- NA_character_

        error_tbl <- tibble::tibble(
            !! id_text := id,
            .error_desc = err
        )

        return(list(fcast_tbl = fcast_tbl, error_list = error_tbl))

    } # END LOOP | returns ret

    # CONSOLIDATE RESULTS

    fcast_list    <- ret %>% purrr::map(purrr::pluck("fcast_tbl"))
    error_list    <- ret %>% purrr::map(purrr::pluck("error_list"))


    # FINALIZE RESULTS ----
    fcast_tbl <- fcast_list %>% dplyr::bind_rows()

    error_tbl <- error_list %>% dplyr::bind_rows()

    if (nrow(error_tbl) > 0) {
        error_tbl <- error_tbl %>%
            tidyr::drop_na(.error_desc)
    }

    # Finish Parallel Backend ----
    #   Close clusters if we set up internally.
    finish_parallel_processing(control, clusters_made, cl, t1)

    # FINISH TIMING ----

    t2 <- Sys.time()

    time_elapsed <- difftime(t2, t1, units = "auto") %>%
        utils::capture.output() %>%
        stringr::str_remove("Time difference of ")

    if (control$verbose) cli::cli_inform(stringr::str_glue("Finished in: {time_elapsed}."))

    # STRUCTURE ----

    ret <- fcast_tbl

    class(ret) <- c("mdl_nested_forecast_tbl", class(ret))

    attr(ret, "conf_interval")       <- conf_interval
    attr(ret, "conf_method")         <- conf_method
    attr(ret, "error_tbl")           <- error_tbl
    attr(ret, "time_elapsed")        <- time_elapsed

    if (nrow(attr(ret, "error_tbl")) > 0) {
        rlang::warn("Some .modeltime_tables had errors during forecasting. Use `extract_nested_error_report()` to review errors.")
    }

    return(ret)

}

#' @export
print.mdl_nested_forecast_tbl <- function(x, ...) {

    # Collect inputs
    fit_col <- attr(x, 'fit_column')
    n_models_with_errors <- attr(x, "error_tbl") %>%
        nrow()
    conf_interval <- attr(x, 'conf_interval')
    conf_method   <- attr(x, 'conf_method')
    if (is.null(conf_method)) {conf_method <- "conformal_default"}

    cat("# Nested Forecast Results\n")
    cat("  ")
    cli::cli_text(cli::col_grey("Trained on: {fit_col} | Forecast Errors: [{n_models_with_errors}] | Conf Method: {conf_method} | Conf Interval: {conf_interval}"))
    # cli::cli_rule()
    class(x) <- class(x)[!(class(x) %in% c("mdl_nested_forecast_tbl"))]
    print(x, ...)
}
