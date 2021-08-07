
# NESTED REFIT ----

#' Refits a Nested Modeltime Table
#'
#' @description
#' Refits a Nested Modeltime Table to actual data using the following process:
#'
#' 1. Models are iteratively refit to .actual_data.
#' 2. Any model that returns an error is logged.
#'  Errors can be retrieved with [extract_nested_error_report()]
#' 3. Forecast is predicted on future_data and is logged.
#'  Forecast can be retrieved with [extract_nested_future_forecast()]
#'
#' @inheritParams modeltime_accuracy
#' @inheritParams modeltime_forecast
#' @param object A Nested Modeltime Table
#' @param control Used to control verbosity and parallel processing. See [control_nested_refit()].
#'
#' @export
modeltime_nested_refit <- function(object, conf_interval = 0.95, control = control_nested_refit()) {

    t1 <- Sys.time()

    # CHECKS ----
    # TODO:
    # - Nested Data Structure
    # - Requires .splits column
    # - dots ... are all workflows

    # HANDLE INPUTS ----

    id_text <- attr(object, "id")

    object <- object %>%
        dplyr::select(dplyr::one_of(id_text), ".actual_data", ".future_data", ".splits", ".modeltime_tables")

    id_expr <- rlang::sym(id_text)

    n_ids   <- nrow(object)

    x_expr <- rlang::sym(".modeltime_tables")

    d_expr <- rlang::sym(".actual_data")

    f_expr <- rlang::sym(".future_data")


    # SETUP LOGGING ENV ----
    logging_env <- rlang::env(
        fcast_tbl = tibble(),
        error_tbl = tibble()

    )

    # SETUP PROGRESS

    if (!control$verbose) cli::cli_progress_bar("Fitting models on training data...", total = nrow(object), .envir = logging_env)

    # LOOP LOGIC ----

    nested_modeltime <- object %>%
        tibble::rowid_to_column(var = '..rowid') %>%
        dplyr::mutate(
            .modeltime_tables = purrr::pmap(.l = list(x = !! x_expr, d = !! d_expr, f = !! f_expr, id = !! id_expr, i = ..rowid), .f = function(x, d, f, id, i) {

                # Save current model descriptions
                # model_desc_user_vec          <- x$.model_desc
                # model_desc_modeltime_old_vec <- x$.model %>% purrr::map_chr(get_model_description)

                ..model_id <- x$.model_id


                tryCatch({

                    if (control$verbose) cli::cli_alert_info(stringr::str_glue("[{i}/{n_ids}] Starting Modeltime Table: ID {id}..."))

                    model_list <- x$.model

                    safe_fit <- purrr::safely(mdl_time_refit, otherwise = NULL, quiet = TRUE)

                    # Safe fitting for each workflow in model_list ----
                    .l <- model_list %>%
                        purrr::imap(.f = function (mod, i) {

                            suppressMessages({
                                suppressWarnings({
                                    fit_list <- safe_fit(mod, data = d)
                                })
                            })

                            res <- fit_list %>% purrr::pluck("result")

                            err <- fit_list %>% purrr::pluck("error", 1)

                            error_tbl <- tibble::tibble(
                                !! id_text := id,
                                .model_id   = ..model_id,
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
                        dplyr::mutate(.model_id = ..model_id) %>%
                        dplyr::mutate(.model_desc = purrr::map_chr(.model, .f = get_model_description)) %>%

                        # Simplify Naming
                        dplyr::mutate(.model_desc = gsub("[[:punct:][:digit:][:cntrl:]]", "", .model_desc)) %>%
                        dplyr::mutate(.model_desc = gsub(" WITH.*$", "", .model_desc))

                    # Add calibration
                    ret <- ret %>%
                        dplyr::bind_cols(x[c(".type", ".calibration_data")])

                    # Update class
                    class(ret) <- c("mdl_time_tbl", class(ret))

                    # ret <- ret %>%
                    #     dplyr::mutate(.model_desc_user = model_desc_user_vec) %>%
                    #     dplyr::mutate(.model_desc_old  = model_desc_modeltime_old_vec) %>%
                    #     dplyr::mutate(.model_desc_new  = purrr::map_chr(.model, .f = get_model_description)) %>%
                    #
                    #     # Description Logic
                    #     dplyr::mutate(.model_desc = ifelse(
                    #         .model_desc_old == .model_desc_new,
                    #         # TRUE - Let User Choice Alone
                    #         .model_desc_user,
                    #         # FALSE - Model Algorithm Parameters Have Changed
                    #         # - Reflect Updated Model Params in Description
                    #         paste0("UPDATE: ", .model_desc_new)
                    #         )
                    #     ) %>%
                    #
                    #     # Clean up columns
                    #     dplyr::select(-.model_desc_user, -.model_desc_old, -.model_desc_new)

                    # Future Forecast ----

                    suppressMessages({
                        suppressWarnings({
                            fcast_tbl <- modeltime_forecast(
                                object        = ret,
                                new_data      = f,
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

    # attr(nested_modeltime, "id")                  <- id_text
    attr(nested_modeltime, "error_tbl")           <- logging_env$error_tbl %>% tidyr::drop_na()
    # attr(nested_modeltime, "accuracy_tbl")        <- logging_env$acc_tbl
    # attr(nested_modeltime, "test_forecast_tbl")   <- logging_env$fcast_tbl
    # attr(nested_modeltime, "best_selection_tbl")  <- NULL
    attr(nested_modeltime, "future_forecast_tbl") <- logging_env$fcast_tbl
    attr(nested_modeltime, "fit_column")          <- ".actual_data"
    attr(nested_modeltime, "time_elapsed")        <- time_elapsed


    if (nrow(attr(nested_modeltime, "error_tbl")) > 0) {
        rlang::warn("Some models had errors during fitting. Use `modeltime_nested_error_report()` to review errors.")
    }


    return(nested_modeltime)

}

# CONTROL ----

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
