# RECURSIVE ----

#' Create a Recursive Time Series Model from a Parsnip or Workflow Regression Model
#'
#' @param object An object of `model_fit` or a fitted `workflow` class
#' @param transform A transformation performed on new_data after
#' each step of recursive algorithm. It can be an object of types:
#'
#' * __Method 1, `recipe`:__ A recipe generates lagged or sliding features (see examples)
#' * __Method 2, `function`:__ Must have one argument `data` (see examples)
#'
#' @param train_tail A tibble with tail of training data set.
#' In most cases it'll be required to create some variables
#' based on dependent variable.
#' @param ... Not currently used.
#' @param id (Optional) An identifier that can be provided to perform a panel forecast.
#'  A single quoted column name (e.g. `id = "id"`).
#'
#' @return An object with added `recursive` class
#'
#' @details
#' Recursive model can be used if some of the features used for training
#' is based of dependent variable we already are trying to forecast.
#' Typically, among these features we can find lags (e.g. created with `step_lag()`)
#' or variables crated with sliding window.
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
#' @examples
#' # Libraries & Setup ----
#' library(modeltime)
#' library(tidymodels)
#' library(tidyverse)
#' library(lubridate)
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
#'     filter(!is.na(value)) %>%
#'     drop_na()
#'
#' future_data <- m750_rolling %>%
#'     filter(is.na(value))
#'
#' # Modeling
#' model_fit_lm <- linear_reg() %>%
#'     set_engine("lm") %>%
#'     fit(value ~ date, data = train_data)
#'
#' model_fit_lm_recursive <- linear_reg() %>%
#'     set_engine("lm") %>%
#'     fit(value ~ ., data = train_data) %>%
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
#' # - NOTE - We create lags by group inside
#' lag_transformer <- function(data){
#'     data %>%
#'         group_by(id) %>%
#'         # Lags
#'         tk_augment_lags(value, .lags = 1:24) %>%
#'         ungroup()
#' }
#'
#' m4_lags <- m4_extended %>%
#'     lag_transformer()
#'
#' train_data <- m4_lags %>%
#'     filter(!is.na(value)) %>%
#'     drop_na()
#'
#' future_data <- m4_lags %>%
#'     filter(is.na(value))
#'
#' # NOTES - recursive() for Panel Data
#' # - We add an id = "id" to specify the groups
#' # - We sue a panel_tail() function to grab tail by groups
#' model_fit_lm_recursive <- linear_reg() %>%
#'     set_engine("lm") %>%
#'     fit(value ~ ., data = train_data) %>%
#'     recursive(
#'         id         = "id",
#'         transform  = lag_transformer,
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
#'
#' @export
recursive <- function(object, transform, train_tail, id = NULL, ...){
    UseMethod("recursive")
}

#' @export
recursive.model_fit <- function(object, transform, train_tail, id = NULL, ...) {

    dot_list <- list(...)

    .class_obj <- if(!is.null(id)){"recursive_panel"} else {"recursive"}

    object$spec[["forecast"]]   <- .class_obj
    object$spec[["transform"]]  <- if(!is.null(id)){.prepare_panel_transform(transform)} else {.prepare_transform(transform)}
    object$spec[["train_tail"]] <- train_tail
    object$spec[["id"]]         <- id

    # Workflow: Need to pass in the y_var
    object$spec[["y_var"]]      <- dot_list$y_var # Could be NULL or provided by workflow

    .class <- class(object)

    class(object) <- c(.class[1], .class_obj, .class[2])

    object
}

#' @export
recursive.workflow <- function(object, transform, train_tail, id = NULL, ...) {

    # object$fit$fit$fit$spec[["forecast"]] <- "recursive"
    # object$fit$fit$fit$spec[["transform"]] <- .prepare_transform(transform)
    # object$fit$fit$fit$spec[["train_tail"]] <- train_tail

    mld         <- object %>% workflows::pull_workflow_mold()
    y_var       <- names(mld$outcomes)

    if (is.null(id)){

        object$fit$fit <- recursive(
            object     = object$fit$fit,
            transform  = transform,
            train_tail = train_tail,
            y_var      = y_var
        )
        .class <- class(object)
        class(object) <- c("recursive", .class)
    } else {

        object$fit$fit <- recursive(
            object     = object$fit$fit,
            transform  = transform,
            train_tail = train_tail,
            y_var      = y_var,
            id         = id
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
    } else {
        cat("Recursive [workflow]\n\n")
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
    } else {
        cat("Recursive [workflow]\n\n")
    }

    y <- x
    class(y) <- class(y)[class(y) %>% stringr::str_detect("recursive_panel", negate = TRUE)]
    print(y)
    invisible(x)
}

#' Recursive Model Predictions
#'
#' Make predictions from a recursive model.
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @details
#'
#' Refer to [recursive()] for further details and examples.
#'
#' @return
#' Numeric values for the recursive panel prediction
#'
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

#' Recursive Model Predictions
#'
#' Make predictions from a recursive model.
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @details
#'
#' Refer to [recursive()] for further details and examples.
#'
#' @return
#' Numeric values for the recursive panel prediction
#'
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
        dplyr::slice_head(n = 1)


    .preds[1,] <- new_data[1, y_var] <-
        pred_fun(
            object,
            new_data = .first_slice,
            type     = type,
            opts     = opts,
            ...
        )



    for (i in 2:nrow(.preds)) {

        .temp_new_data <- dplyr::bind_rows(
            train_tail,
            new_data
        )

        .nth_slice <- .transform(.temp_new_data, nrow(new_data), i)

        .preds[i,] <- new_data[i, y_var] <- pred_fun(
            object, new_data = .nth_slice,
            type = type, opts = opts, ...
        )
    }

    return(.preds)

}

predict_recursive_workflow <- function(object, new_data, type = NULL, opts = list(), ...) {
    workflow <- object

    if (!workflow$trained) {
        rlang::abort("Workflow has not yet been trained. Do you need to call `fit()`?")
    }

    blueprint <- workflow$pre$mold$blueprint
    forged    <- hardhat::forge(new_data, blueprint)
    new_data  <- forged$predictors

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

    .id <- dplyr::ensym(id)

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
        dplyr::mutate(rowid = dplyr::row_number()) %>%
        dplyr::ungroup()

    new_data <- new_data %>%
        dplyr::group_by(!! .id) %>%
        dplyr::mutate(rowid = dplyr::row_number()) %>%
        dplyr::ungroup()

    .first_slice <- new_data %>%
        dplyr::group_by(!! .id) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()


    .preds[.preds$rowid == 1, 2] <- new_data[new_data$rowid == 1, y_var] <- pred_fun(object,
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

    for (i in 2:new_data_size) {

        .temp_new_data <- dplyr::bind_rows(train_tail, new_data)

        .nth_slice <- .transform(.temp_new_data, new_data_size, i, id)

        .preds[.preds$rowid == i, 2] <- new_data[new_data$rowid == i, y_var] <- pred_fun(object,
                                                                                         new_data = .nth_slice,
                                                                                         type = type,
                                                                                         opts = opts,
                                                                                         ...)
    }

    return(.preds[,2])

}

predict_recursive_panel_workflow <- function(object, new_data, type = NULL, opts = list(), ...) {
    workflow <- object

    if (!workflow$trained) {
        rlang::abort("Workflow has not yet been trained. Do you need to call `fit()`?")
    }

    blueprint <- workflow$pre$mold$blueprint
    forged    <- hardhat::forge(new_data, blueprint)
    new_data  <- forged$predictors

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

.prepare_transform <- function(.transform) {

    if (inherits(.transform, "recipe")) {

        .recipe <- .transform

        if (!is_prepped_recipe(.recipe)) {
            .recipe <- recipes::prep(.recipe)
        }

        .derived_features <- .recipe$term_info %>%
            dplyr::filter(source == "derived") %>%
            .$variable

        .transform_fun <- function(temp_new_data, new_data_size, slice_idx){
            temp_new_data <- temp_new_data %>%
                dplyr::select(-!!.derived_features)

            recipes::bake(.recipe, new_data = temp_new_data) %>%
                dplyr::slice_tail(n = new_data_size) %>%
                .[slice_idx, ]
        }
    } else if (inherits(.transform, "function")){
        .transform_fun <- function(temp_new_data, new_data_size, slice_idx){
            .transform(temp_new_data) %>%
                dplyr::slice_tail(n = new_data_size) %>%
                .[slice_idx, ]
        }
    }
    .transform_fun
}

.prepare_panel_transform <- function(.transform) {

    if (inherits(.transform, "function")) {

        .transform_fun <- function(temp_new_data, new_data_size, slice_idx, id) {

            id <- as.character(id)
            id <- dplyr::ensym(id)

            .transform(temp_new_data) %>%

                tibble::rowid_to_column(var = "..row_id") %>%

                dplyr::group_by(!! id) %>%
                dplyr::group_split() %>%
                purrr::map(function(x){

                    dplyr::slice_tail(x, n = new_data_size) %>%
                        .[slice_idx, ]

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




