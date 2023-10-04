# RECURSIVE ----

#' Create a Recursive Time Series Model from a Parsnip or Workflow Regression Model
#'
#' @param object An object of `model_fit` or a fitted `workflow` class
#' @param transform A transformation performed on `new_data` after
#' each step of recursive algorithm.
#'
#' * __Transformation Function:__ Must have one argument `data` (see examples)
#'
#' @param train_tail A tibble with tail of training data set.
#' In most cases it'll be required to create some variables
#' based on dependent variable.
#' @param ... Not currently used.
#' @param id (Optional) An identifier that can be provided to perform a panel forecast.
#'  A single quoted column name (e.g. `id = "id"`).
#' @param chunk_size The size of the smallest lag used in `transform`. If the
#' smallest lag necessary is n, the forecasts can be computed in chunks of n,
#' which can dramatically improve performance. Defaults to 1. Non-integers are
#' coerced to integer, e.g. `chunk_size = 3.5` will be coerced to integer via
#' `as.integer()`.
#'
#' @return An object with added `recursive` class
#'
#' @details
#'
#' __What is a Recursive Model?__
#'
#' A _recursive model_ uses predictions to generate
#' new values for independent features. These features are typically
#' lags used in autoregressive models. It's important to understand that
#' a recursive model is only needed when the __Lag Size < Forecast Horizon.__
#'
#'
#' __Why is Recursive needed for Autoregressive Models with Lag Size < Forecast Horizon?__
#'
#' When the lag length is less than the forecast horizon,
#' a problem exists were missing values (`NA`) are
#' generated in the future data. A solution that `recursive()` implements
#' is to iteratively fill these missing values in with values generated
#' from predictions.
#'
#' __Recursive Process__
#'
#' When producing forecast, the following steps are performed:
#'
#' 1. Computing forecast for first row of new data.
#' The first row cannot contain NA in any required column.
#' 2. Filling i-th place of the dependent variable column with
#' already computed forecast.
#' 3. Computing missing features for next step, based on
#' already calculated prediction. These features are computed
#' with on a tibble object made from binded `train_tail` (i.e. tail of
#' training data set) and `new_data` (which is an argument of predict function).
#' 4. Jumping into point 2., and repeating rest of steps till the for-loop is ended.
#'
#' __Recursion for Panel Data__
#'
#' Panel data is time series data with multiple groups identified by an ID column.
#' The `recursive()` function can be used for Panel Data with the following modifications:
#'
#' 1. Supply an `id` column as a quoted column name
#'
#' 2. Replace [tail()] with [panel_tail()] to use tails for each time series group.
#'
#' @seealso
#' - [panel_tail()] - Used to generate tails for multiple time series groups.
#'
#' @examples
#'
#' \donttest{
#' # Libraries & Setup ----
#' library(tidymodels)
#' library(dplyr)
#' library(tidyr)
#' library(timetk)
#' library(slider)
#'
#' # ---- SINGLE TIME SERIES (NON-PANEL) -----
#'
#' m750
#'
#' FORECAST_HORIZON <- 24
#'
#' m750_extended <- m750 %>%
#'     group_by(id) %>%
#'     future_frame(
#'         .length_out = FORECAST_HORIZON,
#'         .bind_data  = TRUE
#'     ) %>%
#'     ungroup()
#'
#' # TRANSFORM FUNCTION ----
#' # - Function runs recursively that updates the forecasted dataset
#' lag_roll_transformer <- function(data){
#'     data %>%
#'         # Lags
#'         tk_augment_lags(value, .lags = 1:12) %>%
#'         # Rolling Features
#'         mutate(rolling_mean_12 = lag(slide_dbl(
#'             value, .f = mean, .before = 12, .complete = FALSE
#'         ), 1))
#' }
#'
#' # Data Preparation
#' m750_rolling <- m750_extended %>%
#'     lag_roll_transformer() %>%
#'     select(-id)
#'
#' train_data <- m750_rolling %>%
#'     drop_na()
#'
#' future_data <- m750_rolling %>%
#'     filter(is.na(value))
#'
#' # Modeling
#'
#' # Straight-Line Forecast
#' model_fit_lm <- linear_reg() %>%
#'     set_engine("lm") %>%
#'     # Use only date feature as regressor
#'     fit(value ~ date, data = train_data)
#'
#' # Autoregressive Forecast
#' model_fit_lm_recursive <- linear_reg() %>%
#'     set_engine("lm") %>%
#'     # Use date plus all lagged features
#'     fit(value ~ ., data = train_data) %>%
#'     # Add recursive() w/ transformer and train_tail
#'     recursive(
#'         transform  = lag_roll_transformer,
#'         train_tail = tail(train_data, FORECAST_HORIZON)
#'     )
#'
#' model_fit_lm_recursive
#'
#' # Forecasting
#' modeltime_table(
#'     model_fit_lm,
#'     model_fit_lm_recursive
#' ) %>%
#'     update_model_description(2, "LM - Lag Roll") %>%
#'     modeltime_forecast(
#'         new_data    = future_data,
#'         actual_data = m750
#'     ) %>%
#'     plot_modeltime_forecast(
#'         .interactive        = FALSE,
#'         .conf_interval_show = FALSE
#'     )
#'
#' # MULTIPLE TIME SERIES (PANEL DATA) -----
#'
#' m4_monthly
#'
#' FORECAST_HORIZON <- 24
#'
#' m4_extended <- m4_monthly %>%
#'     group_by(id) %>%
#'     future_frame(
#'         .length_out = FORECAST_HORIZON,
#'         .bind_data  = TRUE
#'     ) %>%
#'     ungroup()
#'
#' # TRANSFORM FUNCTION ----
#' # - NOTE - We create lags by group
#' lag_transformer_grouped <- function(data){
#'     data %>%
#'         group_by(id) %>%
#'         tk_augment_lags(value, .lags = 1:FORECAST_HORIZON) %>%
#'         ungroup()
#' }
#'
#' m4_lags <- m4_extended %>%
#'     lag_transformer_grouped()
#'
#' train_data <- m4_lags %>%
#'     drop_na()
#'
#' future_data <- m4_lags %>%
#'     filter(is.na(value))
#'
#' # Modeling Autoregressive Panel Data
#' model_fit_lm_recursive <- linear_reg() %>%
#'     set_engine("lm") %>%
#'     fit(value ~ ., data = train_data) %>%
#'     recursive(
#'         id         = "id", # We add an id = "id" to specify the groups
#'         transform  = lag_transformer_grouped,
#'         # We use panel_tail() to grab tail by groups
#'         train_tail = panel_tail(train_data, id, FORECAST_HORIZON)
#'     )
#'
#' modeltime_table(
#'     model_fit_lm_recursive
#' ) %>%
#'     modeltime_forecast(
#'         new_data    = future_data,
#'         actual_data = m4_monthly,
#'         keep_data   = TRUE
#'     ) %>%
#'     group_by(id) %>%
#'     plot_modeltime_forecast(
#'         .interactive = FALSE,
#'         .conf_interval_show = FALSE
#'     )
#'
#' }
#'
#' @export
recursive <- function(object, transform, train_tail, id = NULL, chunk_size = 1, ...){
    UseMethod("recursive")
}

#' @export
recursive.model_fit <- function(object, transform, train_tail, id = NULL, chunk_size = 1, ...) {

    dot_list <- list(...)

    .class_obj <- if(!is.null(id)){"recursive_panel"} else {"recursive"}

    if (!is.numeric(chunk_size) || chunk_size < 1){
        rlang::abort("'chunk_size' must be an integer >= 1.")
    }

    object$spec[["forecast"]]   <- .class_obj
    object$spec[["transform"]]  <- if(!is.null(id)){.prepare_panel_transform(transform)} else {.prepare_transform(transform)}
    object$spec[["train_tail"]] <- train_tail
    object$spec[["id"]]         <- id
    object$spec[["chunk_size"]] <- as.integer(chunk_size)

    # Workflow: Need to pass in the y_var
    object$spec[["y_var"]]      <- dot_list$y_var # Could be NULL or provided by workflow

    .class <- class(object)

    class(object) <- c(.class[1], .class_obj, .class[2])

    object
}

#' @export
recursive.workflow <- function(object, transform, train_tail, id = NULL, chunk_size = 1, ...) {

    # object$fit$fit$fit$spec[["forecast"]] <- "recursive"
    # object$fit$fit$fit$spec[["transform"]] <- .prepare_transform(transform)
    # object$fit$fit$fit$spec[["train_tail"]] <- train_tail

    mld         <- object %>% workflows::extract_mold()
    y_var       <- names(mld$outcomes)

    if (!is.numeric(chunk_size) || chunk_size < 1){
        rlang::abort("'chunk_size' must be an integer >= 1.")
    }

    if (is.null(id)){

        object$fit$fit <- recursive(
            object     = object$fit$fit,
            transform  = transform,
            train_tail = train_tail,
            chunk_size = as.integer(chunk_size),
            y_var      = y_var
        )
        .class <- class(object)
        class(object) <- c("recursive", .class)
    } else {

        object$fit$fit <- recursive(
            object     = object$fit$fit,
            transform  = transform,
            train_tail = train_tail,
            id         = id,
            chunk_size = as.integer(chunk_size),
            y_var      = y_var
        )
        .class        <- class(object)
        class(object) <- c("recursive_panel", .class)

    }


    object
}



#' @export
print.recursive <- function(x, ...) {

    if (inherits(x, "model_fit")) {
        cat("Recursive [parsnip model]\n\n")
    } else if (inherits(x, "workflow")) {
        cat("Recursive [workflow]\n\n")
    } else {
        cat("Recursive [modeltime ensemble]\n\n")
    }

    y <- x
    class(y) <- class(y)[class(y) %>% stringr::str_detect("recursive", negate = TRUE)]
    print(y)
    invisible(x)
}

#' @export
print.recursive_panel <- function(x, ...) {

    if (inherits(x, "model_fit")) {
        cat("Recursive [parsnip model]\n\n")
    } else if (inherits(x, "workflow")) {
        cat("Recursive [workflow]\n\n")
    } else {
        cat("Recursive [modeltime ensemble]\n\n")
    }

    y <- x
    class(y) <- class(y)[class(y) %>% stringr::str_detect("recursive_panel", negate = TRUE)]
    print(y)
    invisible(x)
}


#' @export
predict.recursive <- function(object, new_data, type = NULL, opts = list(), ...) {

    if (inherits(object, "model_fit")) {
        # print("Recursive Model fit")
        ret <- predict_recursive_model_fit(object, new_data, type = NULL, opts = list(), ...)
    }

    if (inherits(object, "workflow")) {
        # print("Recursive Workflow")
        ret <- predict_recursive_workflow(object, new_data, type = NULL, opts = list(), ...)
    }

    return(ret)

}


#' @export
predict.recursive_panel <- function(object, new_data, type = NULL, opts = list(), ...) {

    if (inherits(object, "model_fit")) {
        # print("Recursive Model fit")
        ret <- predict_recursive_panel_model_fit(object, new_data, type = NULL, opts = list(), ...)
    }

    if (inherits(object, "workflow")) {
        # print("Recursive Workflow")
        ret <- predict_recursive_panel_workflow(object, new_data, type = NULL, opts = list(), ...)
    }

    return(ret)

}

# SINGLE TIME SERIES DISPATCH ----

predict_recursive_model_fit <- function(object, new_data, type = NULL, opts = list(), ...) {

    # SETUP ----
    y_var <- object$spec$y_var
    if (is.null(y_var)) {
        y_var      <- object$preproc$y_var
    }
    pred_fun   <- parsnip::predict.model_fit
    .transform <- object$spec[["transform"]]
    train_tail <- object$spec$train_tail
    chunk_size <- object$spec$chunk_size

    idx_sets <- split(x = seq_len(nrow(new_data)),
                      f = (seq_len(nrow(new_data)) - 1) %/% chunk_size)

    # print({
    #     list(
    #         object,
    #         y_var,
    #         class(object),
    #         new_data,
    #         train_tail
    #     )
    # })


    # LOOP LOGIC ----
    .preds <- tibble::tibble(.pred = numeric(nrow(new_data)))

    .first_slice <- new_data %>%
        dplyr::slice_head(n = chunk_size)

    .preds[idx_sets[[1]],] <- new_data[idx_sets[[1]], y_var] <-
        pred_fun(
            object,
            new_data = .first_slice,
            type     = type,
            opts     = opts,
            ...
        )

     .temp_new_data <- dplyr::bind_rows(
         train_tail,
         new_data
     )

     n_train_tail <- nrow(train_tail)

     if (length(idx_sets) > 1){
         for (i in 2:length(idx_sets)) {

             transform_window_start <- min(idx_sets[[i]])
             transform_window_end   <- max(idx_sets[[i]]) + n_train_tail

             #.nth_slice <- .transform(.temp_new_data[transform_window_start:transform_window_end,], nrow(new_data), idx_sets[[i]])
             .nth_slice <- .transform(.temp_new_data[transform_window_start:transform_window_end,], length(idx_sets[[i]]))

             .preds[idx_sets[[i]],] <- .temp_new_data[idx_sets[[i]] + n_train_tail, y_var] <- pred_fun(
                 object, new_data = .nth_slice[names(.first_slice)],
                 type = type, opts = opts, ...
             )
         }
     }

    return(.preds)

}

predict_recursive_workflow <- function(object, new_data, type = NULL, opts = list(), ...) {
    workflow <- object

    if (!workflow$trained) {
        rlang::abort("Workflow has not yet been trained. Do you need to call `fit()`?")
    }

    # blueprint <- workflow$pre$mold$blueprint
    preprocessor <- workflows::extract_preprocessor(workflow)
    mld          <- hardhat::mold(preprocessor, preprocessor$template)
    forged       <- hardhat::forge(new_data, mld$blueprint)
    new_data     <- forged$predictors

    fit <- workflow$fit$fit

    # print(fit)

    predict.recursive(fit, new_data, type = type, opts = opts, ...)
}

# PANEL DISPATCH ----

predict_recursive_panel_model_fit <- function(object, new_data, type = NULL, opts = list(), ...) {

    # SETUP ----
    y_var <- object$spec$y_var

    if (is.null(y_var)) {
        y_var <- object$preproc$y_var
    }

    pred_fun   <- parsnip::predict.model_fit
    .transform <- object$spec[["transform"]]
    train_tail <- object$spec$train_tail
    id         <- object$spec$id
    chunk_size <- object$spec$chunk_size

    .id <- dplyr::ensym(id)

    unique_id_new_data <- new_data %>% dplyr::select(!! .id) %>% unique() %>% dplyr::pull()

    unique_id_train_tail <- train_tail %>% dplyr::select(!! .id) %>% unique() %>% dplyr::pull()

    if (length(dplyr::setdiff(unique_id_train_tail, unique_id_new_data)) >= 1){
        train_tail <- train_tail %>% dplyr::filter(!! .id %in% unique_id_new_data)
    }

    n_groups <- dplyr::n_distinct(new_data[[id]])
    group_size <- max(table(new_data[[id]]))

    idx_sets <- split(x = seq_len(group_size),
                      f = (seq_len(group_size) - 1) %/% chunk_size)

    # #  Comment this out ----
    # print("here")
    # obj <<- object
    # print({
    #     list(
    #         object,
    #         y_var,
    #         class(object),
    #         new_data,
    #         train_tail
    #     )
    # })

    # LOOP LOGIC ----
    .preds <- tibble::tibble(.id = new_data %>% dplyr::select(!! .id) %>% purrr::as_vector(),
                             .pred = numeric(nrow(new_data))) %>%
        dplyr::group_by(.id) %>%
        dplyr::mutate(rowid.. = dplyr::row_number()) %>%
        dplyr::ungroup()

    new_data <- new_data %>%
        dplyr::group_by(!! .id) %>%
        dplyr::mutate(rowid.. = dplyr::row_number()) %>%
        dplyr::ungroup()

    .first_slice <- new_data %>%
        dplyr::group_by(!! .id) %>%
        dplyr::slice_head(n = chunk_size) %>%
        dplyr::ungroup()

    # Fix - When ID is dummied
    if (!is.null(object$spec$remove_id)) {
        if (object$spec$remove_id) {
            .first_slice <- .first_slice %>%
                dplyr::select(-(!! .id))
        }
    }

    if ("rowid.." %in% names(.first_slice)) {
        .first_slice <- .first_slice %>% dplyr::select(-rowid..)
    }

    .preds[.preds$rowid.. %in% idx_sets[[1]], 2] <- new_data[new_data$rowid.. %in% idx_sets[[1]], y_var] <- pred_fun(object,
                                                                                         new_data = .first_slice,
                                                                                         type = type,
                                                                                         opts = opts,
                                                                                         ...)

    .groups <- new_data %>%
        dplyr::group_by(!! .id) %>%
        dplyr::count(!! .id) %>%
        dim() %>%
        .[1]

    new_data_size <- nrow(.preds)/.groups

    .temp_new_data <- dplyr::bind_rows(train_tail, new_data)

    n_train_tail <- max(table(train_tail[[id]]))

    if (length(idx_sets) > 1){
        for (i in 2:length(idx_sets)) {

            transform_window_start <- min(idx_sets[[i]])
            transform_window_end   <- max(idx_sets[[i]]) + n_train_tail

            .nth_slice <- .transform(.temp_new_data %>%
                                         dplyr::group_by(!! .id) %>%
                                         dplyr::slice(transform_window_start:transform_window_end),
                                     idx_sets[[i]], id)

            # Fix - When ID is dummied
            if (!is.null(object$spec$remove_id)) {
                if (object$spec$remove_id) {
                    .nth_slice <- .nth_slice %>%
                        dplyr::select(-(!! .id))
                }
            }

            if ("rowid.." %in% names(.nth_slice)) {
                .nth_slice <- .nth_slice %>% dplyr::select(-rowid..)
            }

            .nth_slice <- .nth_slice[names(.first_slice)]


            .preds[.preds$rowid.. %in% idx_sets[[i]], 2] <- .temp_new_data[.temp_new_data$rowid.. %in% idx_sets[[i]], y_var] <- pred_fun(object,
                                                                                                                                         new_data = .nth_slice,
                                                                                                                                         type = type,
                                                                                                                                         opts = opts,
                                                                                                                                         ...)
        }
    }

    return(.preds[,2])

}

predict_recursive_panel_workflow <- function(object, new_data, type = NULL, opts = list(), ...) {
    workflow <- object

    # Fix - When ID is dummied
    id <- workflow$fit$fit$spec$id
    df_id = new_data %>% dplyr::select(dplyr::all_of(id))

    if (!workflow$trained) {
        rlang::abort("Workflow has not yet been trained. Do you need to call `fit()`?")
    }

    preprocessor <- workflows::extract_preprocessor(workflow)
    mld          <- hardhat::mold(preprocessor, preprocessor$template)
    forged       <- hardhat::forge(new_data, mld$blueprint)
    new_data     <- forged$predictors

    # Fix - When ID is dummied
    if (!is.null(id)) {
        if (!id %in% names(new_data)) {
            new_data <- new_data %>%
                dplyr::bind_cols(df_id)
            workflow$fit$fit$spec$remove_id <- TRUE
        }
    }

    # print(new_data)

    fit <- workflow$fit$fit

    # print(fit)

    predict.recursive_panel(fit, new_data, type = type, opts = opts, ...)
}


# PANEL TAIL ----

#' Filter the last N rows (Tail) for multiple time series
#'
#' @param data A data frame
#' @param id An "id" feature indicating which column differentiates the time series panels
#' @param n The number of rows to filter
#'
#' @return
#' A data frame
#'
#' @seealso
#' - [recursive()] - used to generate recursive autoregressive models
#'
#' @examples
#' library(timetk)
#'
#' # Get the last 6 observations from each group
#' m4_monthly %>%
#'     panel_tail(id = id, n = 6)
#'
#' @export
panel_tail <- function(data, id, n){

    id <- dplyr::ensym(id)

    ret <- data %>%
        tibble::rowid_to_column(var = "..row_id") %>%
        dplyr::group_by(!! id) %>%
        dplyr::slice_tail(n = n) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(..row_id) %>%
        dplyr::select(-..row_id)

    return(ret)

}


# HELPERS ----

#' Prepare Recursive Transformations
#'
#' @param .transform A transformation function
#'
#' @return A function that applies a recursive transformation
#'
#' @rdname dot_prepare_transform
#' @keywords internal
#' @export
.prepare_transform <- function(.transform) {

    if (inherits(.transform, "recipe")) {

        .recipe <- .transform

        if (!is_prepped_recipe(.recipe)) {
            .recipe <- recipes::prep(.recipe)
        }

        .derived_features <- .recipe$term_info %>%
            dplyr::filter(source == "derived") %>%
            .$variable

        .transform_fun <- function(temp_new_data, chunk_size){
            temp_new_data <- temp_new_data %>%
                dplyr::select(-!!.derived_features)

            recipes::bake(.recipe, new_data = temp_new_data) %>%
                dplyr::slice_tail(n = as.integer(chunk_size))
                #dplyr::slice_tail(n = as.integer(new_data_size)) %>%
                #.[slice_idx, ]
        }
    } else if (inherits(.transform, "function")){
        .transform_fun <- function(temp_new_data, chunk_size){
            .transform(temp_new_data) %>%
                dplyr::slice_tail(n = as.integer(chunk_size))
                #dplyr::slice_tail(n = as.integer(new_data_size)) %>%
                #.[slice_idx, ]
        }
    }
    .transform_fun
}

#' @rdname dot_prepare_transform
#' @export
.prepare_panel_transform <- function(.transform) {

    if (inherits(.transform, "function")) {

        .transform_fun <- function(temp_new_data, slice_idx, id) {

            id_chr <- as.character(id)
            ..id   <- dplyr::ensym(id_chr)

            # print(.transform(temp_new_data))

            .transform(temp_new_data) %>%

                tibble::rowid_to_column(var = "..row_id") %>%

                dplyr::group_by(!! ..id) %>%
                dplyr::group_split() %>%
                purrr::map(function(x){

                    dplyr::filter(x,rowid.. %in% slice_idx)

                    #dplyr::slice_tail(x,n = as.integer(chunk_size))

                    #dplyr::slice_tail(x, n = as.integer(round(new_data_size))) %>%
                    #    .[slice_idx, ]

                }) %>%
                dplyr::bind_rows() %>%

                dplyr::arrange(..row_id) %>%
                dplyr::select(-..row_id)
        }
    } else if (inherits(.transform, "recipe")) {
        rlang::abort("Recursive Panel Data cannot use a recipe. Please use a transform function.")
    }

    .transform_fun
}

is_prepped_recipe <- function(recipe) {
    is_prepped <- FALSE
    if ("orig_lvls" %in% names(recipe)) {
        is_prepped <- TRUE
    }
    return(is_prepped)
}
