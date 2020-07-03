# PARSNIP HELPERS ----

find_formula <- function(object) {

    form <- NULL

    if (isS4(object)) {
        # S4 Class

        slots <- slotNames(object)

        for (slot_name in slots) {
            sl <- slot(object, slot_name)
            if (rlang::is_formula(sl)) {
                form <- stats::formula(sl)
            }
        }
    } else {
        # S3 Class

        check_formula_tbl <- object %>%
            purrr::map_dfr(~ rlang::is_formula(.)) %>%
            tidyr::gather() %>%
            dplyr::filter(value)

        formula_found <- FALSE
        if (nrow(check_formula_tbl) == 1) {
            formula_found <- TRUE
        }

        if (formula_found) {
            form <- stats::formula(object[[check_formula_tbl$key]])
        }
    }

    return(form)
}

find_formula_lhs <- function(object) {

    form <- NULL
    lhs  <- NULL

    form <- find_formula(object)
    if (!is.null(form)) {
        lhs <- rlang::f_lhs(form)
    }

    return(lhs)
}


