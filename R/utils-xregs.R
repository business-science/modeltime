# XREG MATRIX HELPERS ----

prep_xreg_matrix_from_df_fit <- function(xreg_df) {
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
    return(xreg_matrix)
}

prep_xreg_matrix_from_df_predict <- function(xreg_df, xreg_terms) {
    xreg_matrix <- NULL
    if (ncol(xreg_df) > 0) {

        xreg_model_frame   <- hardhat::model_frame(~ ., xreg_df)
        xreg_model_matrix  <- hardhat::model_matrix(xreg_model_frame$terms, xreg_model_frame$data)

        xreg_matrix <- xreg_model_matrix %>%
            as.matrix()

        xreg_matrix <- xreg_matrix[,xreg_terms]

        if (length(xreg_matrix) == 0) {
            xreg_matrix <- NULL
        } else if (ncol(xreg_matrix) == 0) {
            xreg_matrix <- NULL
        }

    }
    return(xreg_matrix)
}


drop_columns_with_single_value <- function(data) {

    results_tbl <- check_non_unique_contrasts(data)

    names_failed <- results_tbl %>%
        dplyr::filter(fail_check) %>%
        dplyr::pull(key)

    data %>%
        dplyr::select(-dplyr::one_of(names_failed))

}
