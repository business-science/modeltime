
# CHECKS ----

check_classes <- function(data, col, accept_classes = c("model_fit", "workflow")) {

    .col <- rlang::enquo(col)

    # Class Check
    ret_1 <- data %>%
        dplyr::mutate(last_class = purrr::map_chr(!! .col, .f = function(obj) {
            class(obj)[length(class(obj))]
        })) %>%
        dplyr::mutate(first_class = purrr::map_chr(!! .col, .f = function(obj) {
            class(obj)[1]
        })) %>%
        dplyr::mutate(fail_check = purrr::map_lgl(!! .col, .f = function(obj) {
            !inherits(obj, accept_classes)
        }))

    return(ret_1)

}

check_ncols <- function(data, col, accept_ncol = 3) {

    .col <- rlang::enquo(col)

    # Class Number of Columns
    ret_1 <- data %>%
        dplyr::mutate(ncol = purrr::map_dbl(!! .col, .f = function(obj) {
            ncol(obj)
        })) %>%
        dplyr::mutate(fail_check = ifelse(ncol != accept_ncol, TRUE, FALSE))

    return(ret_1)

}

check_models_are_trained <- function(data) {

    # Class Check
    ret_1 <- data %>%
        dplyr::mutate(fail_check = purrr::map_lgl(.model, .f = function(obj) {
            !is_trained(obj)
        }))

    return(ret_1)

}

check_models_are_not_null <- function(data) {

    # Class Check
    ret_1 <- data %>%
        dplyr::mutate(fail_check = purrr::map_lgl(.model, .f = is.null))

    return(ret_1)

}

check_type_not_missing <- function(data) {

    # Class Check
    ret_1 <- data %>%
        dplyr::mutate(fail_check = is.na(.type))

    return(ret_1)

}


check_non_bad_class_data <- function(data, bad_classes = c("character")) {

    # Bad Class Check
    ret_1 <- data %>%
        purrr::map_dfr(~ inherits(., bad_classes)) %>%
        tidyr::gather(key = "key", value = "bad_class", dplyr::everything()) %>%
        dplyr::mutate(fail_check = ifelse(bad_class == 1, TRUE, FALSE))

    # Class Description
    ret_2 <- data %>%
        purrr::map_dfr(~ class(.) %>% stringr::str_c(collapse = ", ")) %>%
        tidyr::gather(key = "key", value = "class_desc", dplyr::everything())

    return(dplyr::left_join(ret_1, ret_2, by = "key"))

}

check_non_unique_contrasts <- function(data) {

    ret <- data %>%
        purrr::map_dfr(~ length(unique(.))) %>%
        tidyr::gather(key = "key", value = "unique_count", dplyr::everything()) %>%
        dplyr::mutate(fail_check = ifelse(unique_count == 1, TRUE, FALSE))

    ret

}

check_unused_factor_levels <- function(data) {
    ret_factor_count <- data %>%
        purrr::map_dfr(.f = function(x) {
            if (is.factor(x)) {
                length(levels(x))
            } else {
                0
            }
        }) %>%
        tidyr::gather(key = "key", value = "factor_count", dplyr::everything())

    ret_unique_count <- check_non_unique_contrasts(data) %>%
        dplyr::select(-fail_check)

    ret <- dplyr::left_join(ret_factor_count, ret_unique_count, by = "key") %>%
        dplyr::mutate(fail_check = ifelse(factor_count > unique_count, TRUE, FALSE))

    ret
}

# VALIDATIONS ----

validate_no_overlapping_dates <- function(data, abort_message) {

    date_var_expr <- rlang::sym(timetk::tk_get_timeseries_variables(data)[1])
    idx <- data %>% dplyr::pull(!! date_var_expr)
    if (length(idx) != length(unique(idx)) ) {
        rlang::abort(abort_message)
    }

}

validate_model_classes <- function(data, accept_classes = c("model_fit", "workflow")) {

    result_tbl <- check_classes(data, .model, accept_classes) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        bad_models <- result_tbl$.model_id
        bad_values <- glue::single_quote(result_tbl$first_class)
        bad_msg    <- glue::glue("- Model {bad_models}: Is class {bad_values}")
        bad_msg    <- glue::glue_collapse(bad_msg, sep = "\n")

        rlang::abort(glue::glue(
            "All objects must be fitted workflow or parsnip models inheriting class 'workflow' or 'model_fit'. The following are not:",
            "\n",
            "{bad_msg}")
        )
    }

}


validate_modeltime_table_classes <- function(data, accept_classes = c("mdl_time_tbl")) {

    result_tbl <- check_classes(data, .model_table, accept_classes) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        bad_tables <- result_tbl$.id
        bad_values <- glue::single_quote(result_tbl$first_class)
        bad_msg    <- glue::glue("- Model Table {bad_tables}: Is class {bad_values}")
        bad_msg    <- glue::glue_collapse(bad_msg, sep = "\n")

        rlang::abort(glue::glue(
            "All objects must be Modeltime Tables inheriting class 'mdl_time_tbl'. The following are not:",
            "\n",
            "{bad_msg}")
        )
    }

}

validate_ncols <- function(data, accept_ncol = 3) {

    result_tbl <- check_ncols(data, .model_table, accept_ncol) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        bad_tables <- result_tbl$.id
        bad_values <- glue::single_quote(result_tbl$ncol)
        bad_msg    <- glue::glue("- Model Table {bad_tables}: Has {bad_values} and should have {accept_ncol}")
        bad_msg    <- glue::glue_collapse(bad_msg, sep = "\n")

        rlang::abort(glue::glue(
            "All objects must be Modeltime Tables with 3 columns. The following are not:",
            "\n",
            "{bad_msg}",
            "\n",
            "This problem may have occurred if some tables are calibrated.")
        )
    }

}

validate_models_are_trained <- function(data) {

    result_tbl <- check_models_are_trained(data) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        bad_models <- result_tbl$.model_id
        bad_msg    <- glue::glue("- Model {bad_models}: Is not trained. Try using `fit()` to train the model.")
        bad_msg    <- glue::glue_collapse(bad_msg, sep = "\n")

        cli::cli_abort(c(
            "All objects must be fitted workflow or parsnip models. The following are not:",
            "{bad_msg}"
        ))
    }

}

validate_models_are_not_null <- function(data, type = c("none", "warn", "error"), msg_main = "Models failed Modeltime Refit") {

    type <- type[1]

    result_tbl <- check_models_are_not_null(data) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        bad_models <- result_tbl$.model_id
        bad_msg    <- glue::glue("- Model {bad_models}: Is NULL.")
        bad_msg    <- glue::glue_collapse(bad_msg, sep = "\n")

        message("\n")
        message(cli::rule("Model Failure Report", width = 60))

        bad_data <- data %>%
            dplyr::right_join(
                result_tbl %>% dplyr::select(.model_id),
                by = ".model_id"
            )
        print(bad_data)

        msg <- stringr::str_glue(
            "\n\n{msg_main}:",
            "\n",
            "{bad_msg}",
            "\n\n",
            "Action: Review any error messages.\n",
            "{cli::rule('End Model Failure Report', width = 60)}",
            "\n\n"
        )
        cli::cli_inform(msg)


        if (type == "warn") {
            rlang::warn(msg_main)
        } else if (type == "error") {
            rlang::abort(msg_main)
        }

    }

}

alert_modeltime_calibration <- function(data) {

    result_tbl <- check_type_not_missing(data) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        bad_models <- result_tbl$.model_id
        bad_msg    <- glue::glue("- Model {bad_models}: Failed Calibration.")
        bad_msg    <- glue::glue_collapse(bad_msg, sep = "\n")

        message("\n")
        message(cli::rule("Model Calibration Failure Report", width = 60))
        print(result_tbl)
        msg <- stringr::str_glue(
            "\nThe following models had errors:",
            "\n",
            "{bad_msg}",
            "\n\n",
            "Potential Solution: Check the Error/Warning Messages for clues as to why your model(s) failed calibration.\n",
            "{cli::rule('End Model Calibration Failure Report', width = 60)}",
            "\n\n"
        )
        message(msg)
    }

}

validate_modeltime_calibration <- function(data) {

    # This happens when only some of the models fail
    if (!".calibration_data" %in% names(data)) {

        bad_models <- data$.model_id
        bad_msg    <- glue::glue("- Model {bad_models}: Failed Calibration.")
        bad_msg    <- glue::glue_collapse(bad_msg, sep = "\n")

        message("\n")
        message(cli::rule("Model Calibration Failure Report", width = 60))
        print(data)
        msg <- stringr::str_glue(
            "\nAll models failed Modeltime Calibration:",
            "\n",
            "{bad_msg}",
            "\n\n",
            "Potential Solution: Use `modeltime_calibrate(quiet = FALSE)` AND Check the Error/Warning Messages for clues as to why your model(s) failed calibration.\n",
            "{cli::rule('End Model Calibration Failure Report', width = 60)}",
            "\n\n"
        )
        message(msg)
        rlang::abort("All models failed Modeltime Calibration.")
    }

}

validate_non_bad_class_data <- function(data, bad_classes = c("character")) {

    result_tbl <- check_non_bad_class_data(data, bad_classes) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        bad_cols   <- glue::single_quote(result_tbl$key)
        bad_values <- glue::single_quote(result_tbl$class_desc)
        bad_msg    <- glue::glue("{bad_cols}: Is class {bad_values}")
        bad_msg    <- glue::glue_collapse(bad_msg, sep = "\n")

        cli::cli_abort(c(
            "All variables must be categorical (factor) or date-like, but the following are not:",
            "{bad_msg}"))
    }

}

validate_non_unique_contrasts <- function(data) {

    result_tbl <- check_non_unique_contrasts(data) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        bad_cols   <- glue::single_quote(result_tbl$key)
        bad_values <- purrr::map(result_tbl$unique_count, glue_quote_collapse)
        bad_msg    <- glue::glue("{bad_cols}: {bad_values} unique value")
        bad_msg    <- glue::glue_collapse(bad_msg, sep = "\n")

        rlang::abort(glue::glue(
            "All variables must have more than one unique value, but the following do not:",
            "\n",
            "{bad_msg}")
        )
    }

}

validate_unused_factor_levels <- function(data) {

    result_tbl <- check_unused_factor_levels(data) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        bad_cols     <- glue::single_quote(result_tbl$key)
        bad_values_1 <- purrr::map(result_tbl$factor_count, glue_quote_collapse)
        bad_values_2 <- purrr::map(result_tbl$unique_count, glue_quote_collapse)
        bad_msg      <- glue::glue("{bad_cols}: levels {bad_values_1} > levels used {bad_values_2}")
        bad_msg      <- glue::glue_collapse(bad_msg, sep = "\n")

        rlang::abort(glue::glue(
            "All factor variables must use all levels, but the following do not:",
            "\n",
            "{bad_msg}")
        )
    }

}

# HELPERS ----

is_trained <- function(x) {

    trained <- FALSE

    if (inherits(x, "model_fit")) {
        trained <- TRUE
    }

    if (inherits(x, "workflow")) {
        trained <- x$trained
    }

    if (inherits(x, "mdl_time_ensemble")) {
        trained <- TRUE
    }

    return(trained)
}

glue_quote_collapse <- function(x) {
    glue::glue_collapse(glue::single_quote(x), sep = ", ")
}


