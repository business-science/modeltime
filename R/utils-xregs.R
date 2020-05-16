# XREG HELPERS ----

prepare_xreg_recipe_from_predictors <- function(data, prepare = TRUE) {

    xregs <- TRUE
    if (ncol(data) == 1) {
        possible_idx <- dplyr::pull(data, 1)
        if (timetk::is_date_class(possible_idx)) {
            xregs <- FALSE
        }
    }

    if (xregs) {

        tryCatch({

            # Create recipe for dummy variables
            recipe_spec <- recipes::recipe(~ ., data = data)

            # Convert any ordered factors to factors
            names_ordered <- data %>%
                dplyr::select_if(is.ordered) %>%
                names()

            if (length(names_ordered) > 0) {
                recipe_spec <- recipe_spec %>%
                    recipes::step_mutate_at(names_ordered,
                                            fn = ~ factor(., ordered = FALSE))
            }

            # Convert factors to dummies
            names_factor <- data %>%
                dplyr::select_if(is.factor)%>%
                names()

            names_character <- data %>%
                dplyr::select_if(is.character)%>%
                names()

            if (length(c(names_factor, names_character)) > 0) {
                recipe_spec <- recipe_spec %>%
                    recipes::step_dummy(recipes::all_nominal(), one_hot = FALSE)
            }

            # Drop any date features
            names_date <- data %>%
                dplyr::select_if(timetk::is_date_class) %>%
                names()

            if (length(c(names_date)) > 0) {
                recipe_spec <- recipe_spec %>%
                    recipes::step_rm(names_date)
            }

            # Remove any zero variance predictors
            recipe_spec <- recipe_spec %>%
                recipes::step_zv(recipes::all_predictors())

            if (prepare) {
                recipe_spec <- recipe_spec %>%
                    recipes::prep()
            }

        }, error = function(e) {
            warning(call. = FALSE, "Failed to return valid external regressors. Processing without regressors.")
            recipe_spec <- NULL
        })

    } else {
        recipe_spec <- NULL
    }

    return(recipe_spec)

}

juice_xreg_recipe <- function(xreg_recipe, format = c("tbl", "matrix")) {

    format <- format[1]

    if (!is.null(xreg_recipe)) {
        xreg_juiced <- xreg_recipe %>% recipes::juice()

        if (format == "tbl") {
            xreg_juiced <- tibble::as_tibble(xreg_juiced)
        }
        if (format == "matrix") {
            xreg_juiced <- as.matrix(xreg_juiced)
        }

    } else {
        xreg_juiced <- NULL
    }

    return(xreg_juiced)
}

bake_xreg_recipe <- function(xreg_recipe, new_data, format = c("tbl", "matrix")) {

    format <- format[1]

    if (!is.null(xreg_recipe)) {
        xreg_baked <- xreg_recipe %>% recipes::bake(new_data)

        if (format == "tbl") {
            xreg_baked <- tibble::as_tibble(xreg_baked)
        }
        if (format == "matrix") {
            xreg_baked <- as.matrix(xreg_baked)
        }

    } else {
        xreg_baked <- NULL
    }

    return(xreg_baked)
}


# OLD ----

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
