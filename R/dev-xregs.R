#' Developer Tools for preparing XREGS (Regressors)
#'
#' These functions are designed to assist developers in extending the `modeltime`
#' package. `create_xregs_recipe()` makes it simple to automate conversion
#' of raw un-encoded features to machine-learning ready features.
#'
#' @param data A data frame
#' @param prepare Whether or not to run `recipes::prep()` on the final recipe.
#'  Default is to prepare. User can set this to FALSE to return an un prepared recipe.
#' @param clean_names Uses `janitor::clean_names()` to process the names and improve robustness
#' to failure during dummy (one-hot) encoding step.
#' @param dummy_encode Should `factors` (categorical data) be
#' @param one_hot If `dummy_encode = TRUE`, should the encoding return
#'  one column for each feature or one less column than each feature. Default is `FALSE`.
#'
#' @return A `recipe` in either prepared or un-prepared format.
#'
#' @details
#'
#' The default recipe contains steps to:
#'
#' 1. Remove date features
#' 2. Clean the column names removing spaces and bad characters
#' 3. Convert ordered factors to regular factors
#' 4. Convert factors to dummy variables
#' 5. Remove any variables that have zero variance
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#' library(recipes)
#' library(lubridate)
#'
#' predictors <- m4_monthly %>%
#'     filter(id == "M750") %>%
#'     select(-value) %>%
#'     mutate(month = month(date, label = TRUE))
#' predictors
#'
#' # Create default recipe
#' xreg_recipe_spec <- create_xreg_recipe(predictors, prepare = TRUE)
#'
#' # Extracts the preprocessed training data from the recipe (used in your fit function)
#' juice_xreg_recipe(xreg_recipe_spec)
#'
#' # Applies the prepared recipe to new data (used in your predict function)
#' bake_xreg_recipe(xreg_recipe_spec, new_data = predictors)
#'
#' @export
create_xreg_recipe <- function(data, prepare = TRUE,
                               clean_names = TRUE,
                               dummy_encode = TRUE,
                               one_hot = FALSE) {
    UseMethod("create_xreg_recipe", data)
}

#' @export
create_xreg_recipe.default <- function(data, prepare = TRUE,
                                       clean_names = TRUE,
                                       dummy_encode = TRUE,
                                       one_hot = FALSE) {
    rlang::abort(paste0("No method for class", class(data)[1]))
}

#' @export
create_xreg_recipe.data.frame <- function(data, prepare = TRUE,
                                          clean_names = TRUE,
                                          dummy_encode = TRUE,
                                          one_hot = FALSE) {
    prepare_xreg_recipe_from_predictors(
        data         = data,
        prepare      = prepare,
        clean_names  = clean_names,
        dummy_encode = dummy_encode,
        one_hot      = one_hot)
}


#' Developer Tools for processing XREGS (Regressors)
#'
#' Wrappers for using `recipes::bake` and `recipes::juice` to process data
#' returning data in either `data frame` or `matrix` format (Common formats needed
#' for machine learning algorithms).
#'
#' @param recipe A prepared recipe
#' @param new_data Data to be processed by a recipe
#' @param format One of:
#'  - `tbl`: Returns a tibble (data.frame)
#'  - `matrix`: Returns a matrix
#'
#' @return Data in either the `tbl` (data.frame) or `matrix` formats
#'
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#' library(recipes)
#' library(lubridate)
#'
#' predictors <- m4_monthly %>%
#'     filter(id == "M750") %>%
#'     select(-value) %>%
#'     mutate(month = month(date, label = TRUE))
#' predictors
#'
#' # Create default recipe
#' xreg_recipe_spec <- create_xreg_recipe(predictors, prepare = TRUE)
#'
#' # Extracts the preprocessed training data from the recipe (used in your fit function)
#' juice_xreg_recipe(xreg_recipe_spec)
#'
#' # Applies the prepared recipe to new data (used in your predict function)
#' bake_xreg_recipe(xreg_recipe_spec, new_data = predictors)
#'
#'
#' @name recipe_helpers
NULL

#' @export
#' @rdname recipe_helpers
juice_xreg_recipe <- function(recipe, format = c("tbl", "matrix")) {

    xreg_recipe <- recipe
    format      <- format[1]

    if (!is.null(xreg_recipe)) {

        xreg_juiced <- tryCatch({
            xreg_juiced <- xreg_recipe %>% recipes::juice()
        }, error = function(e) {
            warning(call. = FALSE, "Failed to process regressors. Proceeding without regressors.")
            xreg_juiced <- NULL
            return(xreg_juiced)
        })

    } else {
        xreg_juiced <- NULL
    }

    if (!is.null(xreg_juiced)) {
        if (format == "tbl") {
            xreg_juiced <- tibble::as_tibble(xreg_juiced)
        }
        if (format == "matrix") {
            xreg_juiced <- as.matrix(xreg_juiced)
        }
    }

    return(xreg_juiced)
}

#' @export
#' @rdname recipe_helpers
bake_xreg_recipe <- function(recipe, new_data, format = c("tbl", "matrix")) {

    xreg_recipe <- recipe
    format      <- format[1]

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


# XREG HELPERS ----

prepare_xreg_recipe_from_predictors <- function(data, prepare = TRUE,
                                                clean_names = TRUE,
                                                dummy_encode = TRUE,
                                                one_hot = FALSE) {

    xregs <- TRUE
    if (ncol(data) == 1) {
        possible_idx <- dplyr::pull(data, 1)
        if (timetk::is_date_class(possible_idx)) {
            xregs <- FALSE
        }
    }

    # Make a copy of the data to ensure names align if cleaned
    data_copy <- data
    if (clean_names) {
        data_copy <- janitor::clean_names(data)
    }

    if (xregs) {

        recipe_spec <- tryCatch({

            # Create recipe for dummy variables
            recipe_spec <- recipes::recipe(~ ., data = data)

            # Clean names
            if (clean_names) {
                recipe_spec <- recipe_spec %>%
                    recipes::step_rename_at(dplyr::everything(), fn = janitor::make_clean_names)

            }

            # Convert any ordered factors to factors
            names_ordered <- names(data_copy)[purrr::map_lgl(data_copy, is.ordered)]

            if (length(names_ordered) > 0) {
                recipe_spec <- recipe_spec %>%
                    recipes::step_mutate_at(dplyr::all_of(names_ordered),
                                            fn = ~ factor(., ordered = FALSE))
            }

            # Convert factors to dummies
            names_factor <- names(data_copy)[purrr::map_lgl(data_copy, is.factor)]

            names_character <- names(data_copy)[purrr::map_lgl(data_copy, is.character)]

            if (length(c(names_factor, names_character)) > 0 && dummy_encode) {
                recipe_spec <- recipe_spec %>%
                    recipes::step_dummy(recipes::all_nominal(), one_hot = one_hot)
            }

            # Drop any date features
            names_date <- names(data_copy)[purrr::map_lgl(data_copy, timetk::is_date_class)]

            if (length(c(names_date)) > 0) {
                recipe_spec <- recipe_spec %>%
                    recipes::step_rm(dplyr::all_of(names_date))
            }

            # Remove any zero variance predictors
            recipe_spec <- recipe_spec %>%
                recipes::step_zv(recipes::all_predictors())

            if (prepare) {
                recipe_spec <- recipe_spec %>%
                    recipes::prep()
            }

        }, error = function(e) {
            cli::cli_warn(c(
                "Failed to return valid external regressors. Proceeding without regressors.",
                "---",
                "What most likely happened:",
                i = "If all of the regressors have zero variance (meaning they add no predictive value to the model), they are removed leaving no valid regressors."
                ))
            # recipe_spec
            return(NULL)
        })

    } else {
        recipe_spec <- NULL
    }

    return(recipe_spec)

}




# OLD ----

# prep_xreg_matrix_from_df_fit <- function(xreg_df) {
#     xreg_matrix <- NULL
#     if (ncol(xreg_df) > 0) {
#
#         # Checks
#         validate_non_bad_class_data(xreg_df, bad_classes = c("character"))
#         validate_non_unique_contrasts(xreg_df)
#         validate_unused_factor_levels(xreg_df)
#
#         xreg_model_frame   <- hardhat::model_frame(~ ., xreg_df)
#         xreg_model_matrix  <- hardhat::model_matrix(xreg_model_frame$terms, xreg_model_frame$data)
#
#         xreg_matrix <- xreg_model_matrix %>%
#             drop_columns_with_single_value() %>%
#             as.matrix()
#
#     }
#     return(xreg_matrix)
# }
#
# prep_xreg_matrix_from_df_predict <- function(xreg_df, xreg_terms) {
#     xreg_matrix <- NULL
#     if (ncol(xreg_df) > 0) {
#
#         xreg_model_frame   <- hardhat::model_frame(~ ., xreg_df)
#         xreg_model_matrix  <- hardhat::model_matrix(xreg_model_frame$terms, xreg_model_frame$data)
#
#         xreg_matrix <- xreg_model_matrix %>%
#             as.matrix()
#
#         xreg_matrix <- xreg_matrix[,xreg_terms]
#
#         if (length(xreg_matrix) == 0) {
#             xreg_matrix <- NULL
#         } else if (ncol(xreg_matrix) == 0) {
#             xreg_matrix <- NULL
#         }
#
#     }
#     return(xreg_matrix)
# }
#
#
# drop_columns_with_single_value <- function(data) {
#
#     results_tbl <- check_non_unique_contrasts(data)
#
#     names_failed <- results_tbl %>%
#         dplyr::filter(fail_check) %>%
#         dplyr::pull(key)
#
#     data %>%
#         dplyr::select(-dplyr::one_of(names_failed))
#
# }
