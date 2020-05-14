# UTILITIES -----

prep_xreg_matrix_from_df <- function(xreg_df) {
    xreg_matrix <- NULL
    if (ncol(xreg_df) > 0) {

        # Checks
        validate_non_bad_class_data(xreg_df, bad_classes = c("character"))
        validate_non_unique_contrasts(xreg_df)
        validate_unused_factor_levels(xreg_df)

        xreg_model_frame   <- hardhat::model_frame(~ ., xreg_df)
        xreg_model_matrix  <- hardhat::model_matrix(xreg_model_frame$terms, xreg_model_frame$data)

        xreg_matrix <- xreg_model_matrix %>%
            drop_columns_with_single_value() %>%
            as.matrix()

    }
}


drop_columns_with_single_value <- function(data) {

    results_tbl <- check_non_unique_contrasts(data)

    names_failed <- results_tbl %>%
        dplyr::filter(fail_check) %>%
        dplyr::pull(key)

    data %>%
        dplyr::select(-dplyr::one_of(names_failed))

}


# CHECKS ----

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

validate_non_bad_class_data <- function(data, bad_classes = c("character")) {

    result_tbl <- check_non_bad_class_data(data, bad_classes) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        bad_cols   <- glue::single_quote(result_tbl$key)
        bad_values <- glue::single_quote(result_tbl$class_desc)
        bad_msg    <- glue::glue("{bad_cols}: Is class {bad_values}")
        bad_msg    <- glue::glue_collapse(bad_msg, sep = "\n")

        rlang::abort(glue::glue(
            "All variables must be categorical (factor) or date-like, but the following are not:",
            "\n",
            "{bad_msg}")
        )
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

glue_quote_collapse <- function(x) {
    glue::glue_collapse(glue::single_quote(x), sep = ", ")
}


