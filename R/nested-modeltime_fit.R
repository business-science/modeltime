

#' Fit Tidymodels Workflows to Nested Time Series
#'
#' @description
#' Fits one or more `tidymodels` workflow objects to nested time series data using the following process:
#'
#' 1. Models are iteratively fit to training splits.
#' 2. Accuracy is calculated on testing splits and is logged.
#'   Accuracy results can be retrieved with [extract_nested_test_accuracy()]
#' 3. Any model that returns an error is logged.
#'   Error logs can be retrieved with [extract_nested_error_report()]
#' 4. Forecast is predicted on testing splits and is logged.
#'   Forecast results can be retrieved with [extract_nested_test_forecast()]
#'
#' @inheritParams modeltime_accuracy
#' @inheritParams modeltime_forecast
#' @param nested_data Nested time series data
#' @param ... Tidymodels `workflow` objects that will be fit to the nested time series data.
#' @param model_list Optionally, a `list()` of Tidymodels `workflow` objects can be provided
#' @param control Used to control verbosity and parallel processing. See [control_nested_fit()].
#'
#' @details
#'
#'
#' ## Preparing Data for Nested Forecasting
#'
#' Use [extend_timeseries()], [nest_timeseries()], and [split_nested_timeseries()] for preparing
#' data for Nested Forecasting. The structure must be a nested data frame, which is suppplied in
#' `modeltime_nested_fit(nested_data)`.
#'
#' ## Fitting Models
#'
#' Models must be in the form of `tidymodels workflow` objects. The models can be provided in two ways:
#'
#' 1. Using `...` (dots): The workflow objects can be provided as dots.
#'
#' 2. Using `model_list` parameter: You can supply one or more workflow objects that are wrapped in a `list()`.
#'
#' ## Controlling the fitting process
#'
#' A `control` object can be provided during fitting to adjust the verbosity and parallel processing.
#' See [control_nested_fit()].
#'
#' @export
modeltime_nested_fit <- function(nested_data, ...,
                                 model_list = NULL,
                                 metric_set = default_forecast_accuracy_metric_set(),
                                 conf_interval = 0.95,
                                 conf_method = "conformal_default",
                                 control = control_nested_fit()) {


    # CHECKS ----
    # TODO:
    # - Nested Data Structure
    # - Requires .splits column
    # - dots ... are all workflows

    # Check model_list
    if (!is.null(model_list)) {
        first_model_list_class <- class(model_list)[1]
        if (!first_model_list_class == "list") {
            rlang::abort("`model_list` must be a list. Try using a `list()` of tidymodels workflow objects.")
        }
    }

    # Parallel or Sequential
    if ((control$cores > 1) && control$allow_par) {
        ret <- modeltime_nested_fit_parallel(
            nested_data,
            ...,
            model_list    = model_list,
            metric_set    = metric_set,
            conf_interval = conf_interval,
            conf_method   = conf_method,
            control       = control
        )
    } else {
        ret <- modeltime_nested_fit_sequential(
            nested_data,
            ...,
            model_list    = model_list,
            metric_set    = metric_set,
            conf_interval = conf_interval,
            conf_method   = conf_method,
            control       = control
        )
    }

    return(ret)

}

modeltime_nested_fit_parallel <- function(nested_data, ...,
                                          model_list = NULL,
                                          metric_set = default_forecast_accuracy_metric_set(),
                                          conf_interval = 0.95,
                                          conf_method = "conformal_default",
                                          control = control_nested_fit()) {


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

    nested_data <- nested_data %>%
        dplyr::select(1, ".actual_data", ".future_data", ".splits")


    # SETUP EXPORTS ----

    id_text <- names(nested_data)[[1]]

    model_list <- append(list(...), model_list)
    model_list_tbl <- tibble::tibble(
        .model_id = 1:length(model_list),
        .model    = model_list
    )

    n_ids   <- nrow(nested_data)

    safe_fit <- purrr::safely(mdl_time_refit, otherwise = NULL, quiet = TRUE)


    # SETUP ITERABLES ----

    splits_list = nested_data$.splits

    actual_list = nested_data$.actual_data

    id_vec      = nested_data[[id_text]]


    # BEGIN LOOP -----
    if (control$verbose) {
        t <- Sys.time()
        message(stringr::str_glue(" Beginning Parallel Loop | {round(t-t1, 3)} seconds"))
    }

    ret <- foreach::foreach(
        x                   = splits_list,
        d                   = actual_list,
        id                  = id_vec,
        .inorder            = TRUE,
        .packages           = control$packages,
        # .export             = c("id_text", "model_list", "n_ids", "safe_fit"),
        .verbose            = FALSE
    ) %op% {

        # Safe fitting for each workflow in model_list ----
        .l <- model_list %>%
            purrr::imap(.f = function (mod, i) {

                suppressMessages({
                    suppressWarnings({
                        fit_list <- safe_fit(mod, data = dplyr::slice(d, x$idx_train))
                    })
                })

                res <- fit_list %>% purrr::pluck("result")

                err <- fit_list %>% purrr::pluck("error", 1)

                error_tbl <- tibble::tibble(
                    !! id_text := id,
                    .model_id   = i,
                    .model_desc = get_model_description(mod),
                    .error_desc = ifelse(is.null(err), NA_character_, err)
                )

                return(list(
                    res = res,
                    err = error_tbl
                ))
            })

        # * Extract models and errors ----
        model_list_trained <- sapply(.l, function(l) l[1])
        error_list         <- sapply(.l, function(l) l[2]) %>% dplyr::bind_rows()

        # Convert to Modeltime Table -----
        ret <- tibble::tibble(
            .model = model_list_trained
        ) %>%
            tibble::rowid_to_column(var = ".model_id") %>%
            dplyr::mutate(.model_desc = purrr::map_chr(.model, .f = get_model_description)) %>%

            # Simplify Naming
            dplyr::mutate(.model_desc = gsub("[[:punct:][:digit:][:cntrl:]]", "", .model_desc)) %>%
            dplyr::mutate(.model_desc = gsub(" WITH.*$", "", .model_desc))

        class(ret) <- c("mdl_time_tbl", class(ret))

        # Calibration ----
        suppressMessages({
            suppressWarnings({
                ret0 <- ret

                tryCatch({
                    co <- utils::capture.output({
                        # Use invisible to suppress print when model fails
                        ret <- ret %>%
                            modeltime_calibrate(dplyr::slice(d, x$idx_test))
                    })

                }, error=function(e){
                    # Return original modeltime table
                    ret <- ret0
                })
            })
        })

        # Test Accuracy ----
        acc_tbl <- NULL
        suppressMessages({
            suppressWarnings({

                tryCatch({
                    co <- utils::capture.output({
                        # Use invisible to suppress print when model fails
                        acc_tbl <- modeltime_accuracy(ret, metric_set = metric_set) %>%
                            tibble::add_column(!! id_text := id, .before = 1)
                    })

                }, error=function(e) {

                    # Do nothing

                })
            })
        })

        if (is.null(acc_tbl)) {
            acc_tbl <- tibble::tibble(
                !! id_text := id,
                .model_id   = ret$.model_id,
                .model_desc = "NULL"
            )
        }

        # Test Forecast ----
        fcast_tbl <- NULL
        suppressMessages({
            suppressWarnings({

                tryCatch({
                    fcast_tbl <- modeltime_forecast(
                        object        = ret,
                        new_data      = dplyr::slice(d, x$idx_test),
                        actual_data   = d,
                        conf_interval = conf_interval,
                        conf_method   = conf_method
                    ) %>%
                        tibble::add_column(!! id_text := id, .before = 1)

                }, error=function(e){

                    # Return nothing
                })

            })
        })

        return(list(
            mdl_time_tbl = ret,
            error_list   = error_list,
            acc_tbl      = acc_tbl,
            fcast_tbl    = fcast_tbl
        ))

    } # END LOOP | returns ret

    # CONSOLIDATE RESULTS

    mdl_time_list <- ret %>% purrr::map(purrr::pluck("mdl_time_tbl"))
    error_list    <- ret %>% purrr::map(purrr::pluck("error_list"))
    acc_list      <- ret %>% purrr::map(purrr::pluck("acc_tbl"))
    fcast_list    <- ret %>% purrr::map(purrr::pluck("fcast_tbl"))

    # FORMAT RESULTS ----

    modeltime_tbls   <- tibble::tibble(.modeltime_tables = mdl_time_list)
    nested_modeltime <- nested_data %>% dplyr::bind_cols(modeltime_tbls)

    error_tbl <- error_list %>% dplyr::bind_rows()

    if (nrow(error_tbl) > 0) {
        error_tbl <- error_tbl %>%
            tidyr::drop_na(.error_desc)
    }

    acc_tbl   <- acc_list %>% dplyr::bind_rows()
    fcast_tbl <- fcast_list %>% dplyr::bind_rows()

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

    class(nested_modeltime) <- c("nested_mdl_time", class(nested_modeltime))

    attr(nested_modeltime, "id")                  <- id_text
    attr(nested_modeltime, "model_list_tbl")      <- model_list_tbl
    attr(nested_modeltime, "conf_interval")       <- conf_interval
    attr(nested_modeltime, "conf_method")         <- conf_method
    attr(nested_modeltime, "metric_set")          <- metric_set
    attr(nested_modeltime, "error_tbl")           <- error_tbl
    attr(nested_modeltime, "accuracy_tbl")        <- acc_tbl
    attr(nested_modeltime, "test_forecast_tbl")   <- fcast_tbl
    attr(nested_modeltime, "best_selection_tbl")  <- NULL
    attr(nested_modeltime, "future_forecast_tbl") <- NULL
    attr(nested_modeltime, "fit_column")          <- ".splits"
    attr(nested_modeltime, "time_elapsed")        <- time_elapsed


    if (nrow(attr(nested_modeltime, "error_tbl")) > 0) {
        rlang::warn("Some models had errors during fitting. Use `extract_nested_error_report()` to review errors.")
    }


    return(nested_modeltime)

}

modeltime_nested_fit_sequential <- function(nested_data, ...,
                                            model_list = NULL,
                                            metric_set = default_forecast_accuracy_metric_set(),
                                            conf_interval = 0.95,
                                            conf_method = "conformal_default",
                                            control = control_nested_fit()) {

    t1 <- Sys.time()

    # HANDLE INPUTS ----

    nested_data <- nested_data %>%
        dplyr::select(1, ".actual_data", ".future_data", ".splits")

    id_text <- names(nested_data)[[1]]

    id_expr <- rlang::sym(id_text)

    n_ids   <- nrow(nested_data)

    x_expr  <- rlang::sym(".splits")

    d_expr  <- rlang::sym(".actual_data")

    model_list <- append(list(...), model_list)

    model_list_tbl <- tibble::tibble(
        .model_id = 1:length(model_list),
        .model    = model_list
    )


    # SETUP LOGGING ENV ----
    logging_env <- rlang::env(
        acc_tbl   = tibble::tibble(),
        fcast_tbl = tibble::tibble(),
        error_tbl = tibble::tibble()

    )

    # SETUP PROGRESS

    if (!control$verbose) cli::cli_progress_bar("Fitting models on training data...", total = nrow(nested_data), .envir = logging_env)

    # LOOP LOGIC ----

    nested_modeltime <- nested_data %>%
        tibble::rowid_to_column(var = '..rowid') %>%
        dplyr::mutate(
            .modeltime_tables = purrr::pmap(.l = list(x = !! x_expr, d = !! d_expr, id = !! id_expr, i = ..rowid), .f = function(x, d, id, i) {


                if (control$verbose) cli::cli_alert_info(stringr::str_glue("[{i}/{n_ids}] Starting Modeltime Table: ID {id}..."))

                safe_fit <- purrr::safely(mdl_time_refit, otherwise = NULL, quiet = TRUE)

                # Safe fitting for each workflow in model_list ----
                .l <- model_list %>%
                    purrr::imap(.f = function (mod, i) {

                        suppressMessages({
                            suppressWarnings({
                                fit_list <- safe_fit(mod, data = dplyr::slice(d, x$idx_train))
                            })
                        })

                        res <- fit_list %>% purrr::pluck("result")

                        err <- fit_list %>% purrr::pluck("error", 1)

                        error_tbl <- tibble::tibble(
                            !! id_text := id,
                            .model_id   = i,
                            .model_desc = get_model_description(mod),
                            .error_desc = ifelse(is.null(err), NA_character_, err)
                        )

                        if (control$verbose) {
                            if (!is.null(err)) {
                                cli::cli_alert_danger("Model {i} Failed {error_tbl$.model_desc}: {err}")
                            } else {
                                cli::cli_alert_success("Model {i} Passed {error_tbl$.model_desc}.")
                            }
                        }


                        logging_env$error_tbl <- dplyr::bind_rows(logging_env$error_tbl, error_tbl)

                        return(res)
                    })

                # Convert to Modeltime Table -----
                ret <- tibble::tibble(
                    .model = .l
                ) %>%
                    tibble::rowid_to_column(var = ".model_id") %>%
                    dplyr::mutate(.model_desc = purrr::map_chr(.model, .f = get_model_description)) %>%

                    # Simplify Naming
                    dplyr::mutate(.model_desc = gsub("[[:punct:][:digit:][:cntrl:]]", "", .model_desc)) %>%
                    dplyr::mutate(.model_desc = gsub(" WITH.*$", "", .model_desc))

                class(ret) <- c("mdl_time_tbl", class(ret))


                # Calibration ----
                suppressMessages({
                    suppressWarnings({
                        ret0 <- ret

                        tryCatch({
                            co <- utils::capture.output({
                                # Use invisible to suppress print when model fails
                                ret <- ret %>%
                                    modeltime_calibrate(dplyr::slice(d, x$idx_test))
                            })

                        }, error=function(e){
                            # Return original modeltime table
                            ret <- ret0
                        })


                    })
                })


                # Test Accuracy ----
                suppressMessages({
                    suppressWarnings({

                        tryCatch({
                            co <- utils::capture.output({
                                # Use invisible to suppress print when model fails
                                acc_tbl <- modeltime_accuracy(ret, metric_set = metric_set) %>%
                                    tibble::add_column(!! id_text := id, .before = 1)
                            })

                            logging_env$acc_tbl <- dplyr::bind_rows(logging_env$acc_tbl, acc_tbl)

                        }, error=function(e) {

                            # Return just the id

                            acc_tbl <- tibble::tibble(
                                !! id_text := id,
                                .model_id   = ret$.model_id,
                                .model_desc = "NULL"
                            )

                            logging_env$acc_tbl <- dplyr::bind_rows(logging_env$acc_tbl, acc_tbl)


                        })


                    })
                })

                # Test Forecast ----
                suppressMessages({
                    suppressWarnings({

                        tryCatch({
                            fcast_tbl <- modeltime_forecast(
                                object        = ret,
                                new_data      = d %>% dplyr::slice(x$idx_test),
                                actual_data   = d,
                                conf_interval = conf_interval,
                                conf_method   = conf_method
                            ) %>%
                                tibble::add_column(!! id_text := id, .before = 1)

                            logging_env$fcast_tbl <- dplyr::bind_rows(logging_env$fcast_tbl, fcast_tbl)

                        }, error=function(e){

                            # Return nothing
                        })


                    })
                })

                # Finish ----

                if (control$verbose) cli::cli_alert_success(stringr::str_glue("[{i}/{n_ids}] Finished Modeltime Table: ID {id}"))
                if (control$verbose) cat("\n")


                if (!control$verbose) cli::cli_progress_update(.envir = logging_env)

                return(ret)
            })
        ) %>%
        dplyr::select(-..rowid)

    if (!control$verbose) cli::cli_progress_done(.envir = logging_env)

    t2 <- Sys.time()

    time_elapsed <- difftime(t2, t1, units = "auto") %>%
        utils::capture.output() %>%
        stringr::str_remove("Time difference of ")

    if (control$verbose) cli::cli_inform(stringr::str_glue("Finished in: {time_elapsed}."))

    # STRUCTURE ----

    class(nested_modeltime) <- c("nested_mdl_time", class(nested_modeltime))

    error_tbl <- logging_env$error_tbl
    if (nrow(error_tbl) > 0) {
        error_tbl <- error_tbl %>%
            tidyr::drop_na(.error_desc)
    }

    attr(nested_modeltime, "id")                  <- id_text
    attr(nested_modeltime, "model_list_tbl")      <- model_list_tbl
    attr(nested_modeltime, "conf_interval")       <- conf_interval
    attr(nested_modeltime, "conf_method")         <- conf_method
    attr(nested_modeltime, "metric_set")          <- metric_set
    attr(nested_modeltime, "error_tbl")           <- error_tbl
    attr(nested_modeltime, "accuracy_tbl")        <- logging_env$acc_tbl
    attr(nested_modeltime, "test_forecast_tbl")   <- logging_env$fcast_tbl
    attr(nested_modeltime, "best_selection_tbl")  <- NULL
    attr(nested_modeltime, "future_forecast_tbl") <- NULL
    attr(nested_modeltime, "fit_column")          <- ".splits"
    attr(nested_modeltime, "time_elapsed")        <- time_elapsed


    if (nrow(attr(nested_modeltime, "error_tbl")) > 0) {
        rlang::warn("Some models had errors during fitting. Use `extract_nested_error_report()` to review errors.")
    }

    return(nested_modeltime)

}

#' @export
print.nested_mdl_time <- function(x, ...) {

    # Collect inputs
    fit_col <- attr(x, 'fit_column')
    n_models_with_errors <- attr(x, "error_tbl") %>%
        dplyr::pull(.model_id) %>%
        unique() %>%
        length()
    conf_interval <- attr(x, 'conf_interval')
    conf_method   <- attr(x, 'conf_method')
    if (is.null(conf_method)) {conf_method <- "conformal_default"}

    cat("# Nested Modeltime Table\n")
    cat("  ")
    cli::cli_text(cli::col_grey("Trained on: {fit_col} | Forecast Errors: [{n_models_with_errors}] | Conf Method: {conf_method} | Conf Interval: {conf_interval}"))
    # cli::cli_rule()
    class(x) <- class(x)[!(class(x) %in% c("nested_mdl_time"))]
    print(x, ...)


}






