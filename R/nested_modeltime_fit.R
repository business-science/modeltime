

#' Fit Tidymodels Workflows to Nested Time Series
#'
#' @description
#' Fits one or more `tidymodels` workflow objects to nested time series data.
#'
#' 1. Models are iteratively fit to training splits.
#' 2. Accuracy is calculated on testing splits and is logged.
#' 3. Any model that returns an error is logged.
#' 4. Forecast is predicted on testing splits and is logged.
#'
#' @inheritParams modeltime_accuracy
#' @inheritParams modeltime_forecast
#' @param nested_data Nested time series data
#' @param ... Tidymodels `workflow` objects that will be fit to the nested time series data.
#' @param control Used to control verbosity and parallel processing. See [control_nested_fit()].
#'
#' @export
modeltime_nested_fit <- function(nested_data, ...,
                                 metric_set = default_forecast_accuracy_metric_set(),
                                 conf_interval = 0.95,
                                 control = control_nested_fit()) {

    t1 <- Sys.time()

    # CHECKS ----
    # TODO:
    # - Nested Data Structure
    # - Requires .splits column
    # - dots ... are all workflows

    # HANDLE INPUTS ----

    nested_data <- nested_data %>%
        dplyr::select(1, ".actual_data", ".future_data", ".splits")

    id_text <- names(nested_data)[[1]]

    id_expr <- rlang::sym(id_text)

    n_ids   <- nrow(nested_data)

    x_expr  <- rlang::sym(".splits")

    d_expr  <- rlang::sym(".actual_data")


    # SETUP LOGGING ENV ----
    logging_env <- rlang::env(
        acc_tbl   = tibble(),
        fcast_tbl = tibble(),
        error_tbl = tibble()

    )

    # SETUP PROGRESS

    if (!control$verbose) cli::cli_progress_bar("Fitting models on training data...", total = nrow(nested_data), .envir = logging_env)

    # LOOP LOGIC ----

    nested_modeltime <- nested_data %>%
        tibble::rowid_to_column(var = '..rowid') %>%
        dplyr::mutate(
            .modeltime_tables = purrr::pmap(.l = list(x = !! x_expr, d = !! d_expr, id = !! id_expr, i = ..rowid), .f = function(x, d, id, i) {

                tryCatch({

                    if (control$verbose) cli::cli_alert_info(stringr::str_glue("[{i}/{n_ids}] Starting Modeltime Table: ID {id}..."))

                    model_list <- list(...)

                    safe_fit <- purrr::safely(mdl_time_refit, otherwise = NULL, quiet = TRUE)

                    # Safe fitting for each workflow in model_list ----
                    .l <- model_list %>%
                        purrr::imap(.f = function (mod, i) {

                            suppressMessages({
                                suppressWarnings({
                                    fit_list <- safe_fit(mod, data = rsample::training(x))
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
                            ret <- ret %>%
                                modeltime_calibrate(rsample::testing(x))
                        })
                    })


                    # Accuracy ----
                    acc_tbl <- modeltime_accuracy(ret, metric_set = metric_set) %>%
                        tibble::add_column(!! id_text := id, .before = 1)

                    logging_env$acc_tbl <- dplyr::bind_rows(logging_env$acc_tbl, acc_tbl)

                    # Test Forecast ----
                    suppressMessages({
                        suppressWarnings({
                            fcast_tbl <- modeltime_forecast(
                                object        = ret,
                                new_data      = rsample::testing(x),
                                actual_data   = d,
                                conf_interval = conf_interval
                            ) %>%
                                tibble::add_column(!! id_text := id, .before = 1)

                            logging_env$fcast_tbl <- dplyr::bind_rows(logging_env$fcast_tbl, fcast_tbl)
                        })
                    })

                    # Finish ----

                    if (control$verbose) cli::cli_alert_success(stringr::str_glue("[{i}/{n_ids}] Finished Modeltime Table: ID {id}"))
                    if (control$verbose) cat("\n")

                }, error = function(e) {

                    cli::cli_alert_danger(stringr::str_glue("Modeltime Table (Failed): ID {id}"))
                    cli::cli_inform(e)
                    cat("\n")

                    error_tbl <- tibble::tibble(
                        !! id_text := id,
                        .model_id   = "ALL MODELS",
                        .model_desc = "ALL MODELS FAILED",
                        .error_desc = as.character(e)
                    )

                    logging_env$error_tbl <- dplyr::bind_rows(logging_env$error_tbl, error_tbl)

                    ret <- NULL

                })

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

    attr(nested_modeltime, "id")                  <- id_text
    attr(nested_modeltime, "error_tbl")           <- logging_env$error_tbl %>% tidyr::drop_na()
    attr(nested_modeltime, "accuracy_tbl")        <- logging_env$acc_tbl
    attr(nested_modeltime, "test_forecast_tbl")   <- logging_env$fcast_tbl
    attr(nested_modeltime, "best_selection_tbl")  <- NULL
    attr(nested_modeltime, "future_forecast_tbl") <- NULL
    attr(nested_modeltime, "fit_column")          <- ".splits"
    attr(nested_modeltime, "time_elapsed")        <- time_elapsed


    if (nrow(attr(nested_modeltime, "error_tbl")) > 0) {
        rlang::warn("Some models had errors during fitting. Use `modeltime_nested_error_report()` to review errors.")
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

    cat("# Nested Modeltime Table\n")
    cat("  ")
    cli::cli_text(cli::col_grey("Trained on: {fit_col} | Model Errors: [{n_models_with_errors}]"))
    # cli::cli_rule()
    class(x) <- class(x)[!(class(x) %in% c("nested_mdl_time"))]
    print(x, ...)
}

# CONTROL ----

#' Control aspects of the `modeltime_nested_fit()` process.
#'
#' @param allow_par Logical to allow parallel computation. Default: `FALSE` (single threaded).
#' @param cores Number of cores for computation. If -1, uses all available physical cores.
#'  Default: `-1`.
#' @param packages An optional character string of additional R package names that should be loaded
#'  during parallel processing.
#'
#'  - Packages in your namespace are loaded by default
#'
#'  - Key Packages are loaded by default: `tidymodels`, `parsnip`, `modeltime`, `dplyr`, `stats`, `lubridate` and `timetk`.
#'
#' @param verbose Logical to control printing.
#'
#' @return
#' A List with the control settings.
#'
#'
#' @seealso
#' [modeltime_nested_fit()]
#'
#' @examples
#'
#' # No parallel processing
#' control_nested_fit()
#'
#' # With parallel processing
#' control_nested_fit(allow_par = TRUE)
#'
#' @export
control_nested_fit <- function(verbose = FALSE,
                               allow_par = FALSE,
                               cores = -1,
                               packages = NULL) {

    ret <- control_modeltime_objects(
        verbose   = verbose,
        allow_par = allow_par,
        cores     = cores,
        packages  = packages,
        func      = "control_refit"
    )

    class(ret) <- c("control_nested_fit")

    return(ret)
}

#' Control aspects of the `modeltime_nested_refit()` process.
#'
#' @param allow_par Logical to allow parallel computation. Default: `FALSE` (single threaded).
#' @param cores Number of cores for computation. If -1, uses all available physical cores.
#'  Default: `-1`.
#' @param packages An optional character string of additional R package names that should be loaded
#'  during parallel processing.
#'
#'  - Packages in your namespace are loaded by default
#'
#'  - Key Packages are loaded by default: `tidymodels`, `parsnip`, `modeltime`, `dplyr`, `stats`, `lubridate` and `timetk`.
#'
#' @param verbose Logical to control printing.
#'
#' @return
#' A List with the control settings.
#'
#'
#' @seealso
#' [modeltime_nested_refit()]
#'
#' @examples
#'
#' # No parallel processing
#' control_nested_refit()
#'
#' # With parallel processing
#' control_nested_refit(allow_par = TRUE)
#'
#' @export
control_nested_refit <- control_nested_fit

#' @export
print.control_nested_fit <- function(x, ...) {
    pretty_print_list(x, header = "nested fit control object")
    invisible(x)
}



# NESTED TEST ACCURACY / TEST FORECAST / ERROR REPORTING ----

#' Logging Functions for Modeltime Nested Tables
#'
#' @param object A nested modeltime table
#'
#' @name modeltime_nested_tables

#' @export
#' @rdname modeltime_nested_tables
modeltime_nested_accuracy <- function(object) {
    attr(object, "accuracy_tbl")
}

#' @export
#' @rdname modeltime_nested_tables
modeltime_nested_test_forecast <- function(object) {
    attr(object, "test_forecast_tbl")
}

#' @export
#' @rdname modeltime_nested_tables
modeltime_nested_error_report <- function(object) {
    attr(object, "error_tbl")
}

#' @export
#' @rdname modeltime_nested_tables
modeltime_nested_future_forecast <- function(object) {
    attr(object, "future_forecast_tbl")
}

#' @export
#' @rdname modeltime_nested_tables
modeltime_nested_best_model_report <- function(object) {
    attr(object, "best_selection_tbl")
}
