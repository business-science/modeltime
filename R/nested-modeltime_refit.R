
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
#' @param object A Nested Modeltime Table
#' @param control Used to control verbosity and parallel processing. See [control_nested_refit()].
#'
#' @export
modeltime_nested_refit <- function(object, control = control_nested_refit()) {

    UseMethod("modeltime_nested_refit", object)

}

#' @export
modeltime_nested_refit.nested_mdl_time <- function(object, control = control_nested_refit()) {

    # Parallel or Sequential
    if ((control$cores > 1) && control$allow_par) {
        ret <- modeltime_nested_refit_parallel(
            object        = object,
            control       = control
        )
    } else {
        ret <- modeltime_nested_refit_sequential(
            object        = object,
            control       = control
        )
    }

}

# *** PARALLEL *** ----

modeltime_nested_refit_parallel <- function(object, control) {

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

    conf_interval <- attr(object, "conf_interval")

    conf_method <- attr(object, "conf_method")
    if (is.null(conf_method)) {conf_method <- "conformal_default"}

    # SETUP ITERABLES ----

    model_list  = object$.modeltime_tables

    actual_list = object$.actual_data

    future_list = object$.future_data

    id_vec      = object[[id_text]]


    # BEGIN LOOP -----
    safe_fit <- purrr::safely(mdl_time_refit, otherwise = NULL, quiet = TRUE)

    if (control$verbose) {
        t <- Sys.time()
        message(stringr::str_glue(" Beginning Parallel Loop | {round(t-t1, 3)} seconds"))
    }

    ret <- foreach::foreach(
        x                   = model_list,
        d                   = actual_list,
        f                   = future_list,
        id                  = id_vec,
        .inorder            = TRUE,
        .packages           = control$packages,
        .verbose            = FALSE
    ) %op% {

        ..model_id   <- x$.model_id
        ..model_list <- x$.model

        # Safe fitting for each workflow in model_list ----
        .l <- purrr::map2(..model_list, ..model_id, .f = function (mod, mod_id) {

            suppressMessages({
                suppressWarnings({
                    fit_list <- safe_fit(mod, data = d)
                })
            })

            res <- fit_list %>% purrr::pluck("result")

            err <- fit_list %>% purrr::pluck("error", 1)

            error_tbl <- tibble::tibble(
                !! id_text := id,
                .model_id   = mod_id,
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
            dplyr::mutate(.model_id = ..model_id) %>%
            dplyr::mutate(.model_desc = purrr::map_chr(.model, .f = get_model_description)) %>%

            # Simplify Naming
            dplyr::mutate(.model_desc = gsub("[[:punct:][:digit:][:cntrl:]]", "", .model_desc)) %>%
            dplyr::mutate(.model_desc = gsub(" WITH.*$", "", .model_desc))


        # Add calibration
        tryCatch({
            ret <- ret %>%
                dplyr::bind_cols(x[c(".type", ".calibration_data")])
        }, error = function(e) {
            # If calibration does not exist, do nothing
        })

        # Update class
        class(ret) <- c("mdl_time_tbl", class(ret))

        # Future Forecast ----
        fcast_tbl <- NULL
        suppressMessages({
            suppressWarnings({

                tryCatch({

                    fcast_tbl <- modeltime_forecast(
                        object        = ret,
                        new_data      = f,
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

        # return(list(model_list_trained = model_list_trained, error_list = error_list))
        return(list(
            mdl_time_tbl = ret,
            error_list   = error_list,
            fcast_tbl    = fcast_tbl
        ))

    } # END LOOP | returns ret

    # CONSOLIDATE RESULTS

    mdl_time_list <- ret %>% purrr::map(purrr::pluck("mdl_time_tbl"))
    error_list    <- ret %>% purrr::map(purrr::pluck("error_list"))
    fcast_list    <- ret %>% purrr::map(purrr::pluck("fcast_tbl"))

    # FORMAT RESULTS ----

    nested_modeltime <- object %>%
        dplyr::mutate(.modeltime_tables = mdl_time_list)

    error_tbl <- error_list %>% dplyr::bind_rows()
    if (nrow(error_tbl) > 0) {
        error_tbl <- error_tbl %>%
            tidyr::drop_na(.error_desc)
    }

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

    attr(nested_modeltime, "error_tbl")           <- error_tbl
    attr(nested_modeltime, "future_forecast_tbl") <- fcast_tbl
    attr(nested_modeltime, "fit_column")          <- ".actual_data"
    attr(nested_modeltime, "time_elapsed")        <- time_elapsed


    if (nrow(attr(nested_modeltime, "error_tbl")) > 0) {
        rlang::warn("Some models had errors during fitting. Use `extract_nested_error_report()` to review errors.")
    }


    return(nested_modeltime)



    return(ret)





}

# *** SEQUENTIAL *** ----
modeltime_nested_refit_sequential <- function(object, control) {

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

    conf_interval <- attr(object, "conf_interval")

    conf_method <- attr(object, "conf_method")
    if (is.null(conf_method)) {conf_method <- "conformal_default"}


    # SETUP LOGGING ENV ----
    logging_env <- rlang::env(
        fcast_tbl = tibble::tibble(),
        error_tbl = tibble::tibble()

    )

    # SETUP PROGRESS

    if (!control$verbose) cli::cli_progress_bar("Fitting models on actual data...", total = nrow(object), .envir = logging_env)

    # LOOP LOGIC ----

    nested_modeltime <- object %>%
        tibble::rowid_to_column(var = '..rowid') %>%
        dplyr::mutate(
            .modeltime_tables = purrr::pmap(.l = list(x = !! x_expr, d = !! d_expr, f = !! f_expr, id = !! id_expr, i = ..rowid), .f = function(x, d, f, id, i) {

                # Save current model descriptions
                # model_desc_user_vec          <- x$.model_desc
                # model_desc_modeltime_old_vec <- x$.model %>% purrr::map_chr(get_model_description)

                ..model_id <- x$.model_id

                if (control$verbose) cli::cli_alert_info(stringr::str_glue("[{i}/{n_ids}] Starting Modeltime Table: ID {id}..."))

                model_list <- x$.model

                safe_fit <- purrr::safely(mdl_time_refit, otherwise = NULL, quiet = TRUE)

                # Safe fitting for each workflow in model_list ----
                .l <- purrr::map2(model_list, ..model_id, .f = function (mod, mod_id) {

                    suppressMessages({
                        suppressWarnings({
                            fit_list <- safe_fit(mod, data = d)
                        })
                    })

                    res <- fit_list %>% purrr::pluck("result")

                    err <- fit_list %>% purrr::pluck("error", 1)

                    error_tbl <- tibble::tibble(
                        !! id_text := id,
                        .model_id   = mod_id,
                        .model_desc = get_model_description(mod),
                        .error_desc = ifelse(is.null(err), NA_character_, err)
                    )

                    if (control$verbose) {
                        if (!is.null(err)) {
                            cli::cli_alert_danger("Model {mod_id} Failed {error_tbl$.model_desc}: {err}")
                        } else {
                            cli::cli_alert_success("Model {mod_id} Passed {error_tbl$.model_desc}.")
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
                tryCatch({
                    ret <- ret %>%
                        dplyr::bind_cols(x[c(".type", ".calibration_data")])
                }, error = function(e) {
                    # If calibration does not exist, do nothing
                })



                # Update class
                class(ret) <- c("mdl_time_tbl", class(ret))

                # Future Forecast ----

                suppressMessages({
                    suppressWarnings({

                        tryCatch({

                            fcast_tbl <- modeltime_forecast(
                                object        = ret,
                                new_data      = f,
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

    attr(nested_modeltime, "error_tbl")           <- logging_env$error_tbl %>% tidyr::drop_na(.error_desc)
    attr(nested_modeltime, "future_forecast_tbl") <- logging_env$fcast_tbl
    attr(nested_modeltime, "fit_column")          <- ".actual_data"
    attr(nested_modeltime, "time_elapsed")        <- time_elapsed


    if (nrow(attr(nested_modeltime, "error_tbl")) > 0) {
        rlang::warn("Some models had errors during fitting. Use `extract_nested_error_report()` to review errors.")
    }


    return(nested_modeltime)
}


