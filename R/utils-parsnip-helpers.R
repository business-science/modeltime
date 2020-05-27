# PARSNIP HELPERS ----

find_formula <- function(object) {

    check_formula_tbl <- object %>%
        purrr::map_dfr(~ rlang::is_formula(.)) %>%
        tidyr::gather() %>%
        dplyr::filter(value)

    formula_found <- FALSE
    if (nrow(check_formula_tbl) == 1) {
        formula_found <- TRUE
    }

    form <- NULL
    if (formula_found) {
        form <- object[[check_formula_tbl$key]]
    }

    return(form)
}

find_formula_lhs <- function(object) {

    check_formula_tbl <- object %>%
        purrr::map_dfr(~ rlang::is_formula(.)) %>%
        tidyr::gather() %>%
        dplyr::filter(value)

    formula_found <- FALSE
    if (nrow(check_formula_tbl) == 1) {
        formula_found <- TRUE
    }

    lhs <- NULL
    if (formula_found) {
        lhs <- rlang::f_lhs(object[[check_formula_tbl$key]])
    }

    return(lhs)
}


