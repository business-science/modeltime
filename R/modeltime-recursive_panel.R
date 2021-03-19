# RECURSIVE PANEL----

#' Create a Recursive Time Series Model from a Parsnip or Workflow Regression Model
#'
#' @param object An object of `model_fit` or a fitted `workflow` class
#' @param transform A transformation performed on new_data after
#' each step of recursive algorithm. It can be an object of types:
#'
#' * __Method 1, `function`:__ Must have one argument `data` and and you
#' must group by the variable `id`, perform the transformations and
#' finally ungroup. (see examples)
#'
#' @param train_tail A tibble with tail by group of training data set.
#' In most cases it'll be required to create some variables
#' based on dependent variable. You can use the function
#' modeltime::panel_data() (see examples)
#' @param ... Not currently used.
#'
#' @return An object with added `recursive_panel` class
#'
#' @details
#' Recursive model can be used if some of the features used for training
#' is based of dependent variable we already are trying to forecast.
#' Typically, among these features we can find lags (e.g. created with `tk_augment_lags()`)
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
#'
#' library(modeltime)
#' library(tidymodels)
#' library(tidyverse)
#' library(lubridate)
#' library(timetk)
#' library(slider)
#'
#' # METHOD 1: TRANSFORM FUNCTION ----
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
#'
#' model_fit_lm_recursive <- linear_reg() %>%
#'     set_engine("lm") %>%
#'     fit(value ~ ., data = train_data) %>%
#'     recursive_panel(
#'         transform  = lag_transformer,
#'         train_tail = modeltime::panel_tail(train_data, id, FORECAST_HORIZON)
#'     )
#'
#' modeltime_table(model_fit_lm_recursive) %>%
#'     modeltime_forecast(new_data = future_data,
#'                        actual_data = m4_monthly,
#'                        keep_data = TRUE) %>%
#'     group_by(id) %>%
#'     plot_modeltime_forecast(
#'         .interactive = FALSE,
#'         .conf_interval_show = FALSE
#'     )
#'
#'
#' @export
recursive_panel <- function(object, transform, train_tail, ...){
    UseMethod("recursive_panel")
}

#' @export
recursive_panel.model_fit <- function(object, transform, train_tail, ...) {

    dot_list <- list(...)

    object$spec[["forecast"]]   <- "recursive_panel"
    object$spec[["transform"]]  <- .prepare_panel_transform(transform)
    object$spec[["train_tail"]] <- train_tail

    # Workflow: Need to pass in the y_var
    object$spec[["y_var"]]      <- dot_list$y_var # Could be NULL or provided by workflow

    .class <- class(object)
    class(object) <- c(.class[1], "recursive_panel", .class[2])
    object
}

#' @export
recursive_panel.workflow <- function(object, transform, train_tail, ...) {

    # object$fit$fit$fit$spec[["forecast"]] <- "recursive"
    # object$fit$fit$fit$spec[["transform"]] <- .prepare_transform(transform)
    # object$fit$fit$fit$spec[["train_tail"]] <- train_tail

    mld         <- object %>% workflows::pull_workflow_mold()
    y_var       <- names(mld$outcomes)

    object$fit$fit <- recursive(
        object     = object$fit$fit,
        transform  = transform,
        train_tail = train_tail,
        y_var      = y_var
    )
    .class <- class(object)
    class(object) <- c("recursive_panel", .class)
    object
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
#' Refer to [recursive_panel()] for further details and examples.
#'
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

#' @export
predict_recursive_panel_model_fit <- function(object, new_data, type = NULL, opts = list(), ...) {

    # SETUP ----
    y_var <- object$spec$y_var

    if (is.null(y_var)) {
        y_var <- object$preproc$y_var
    }

    pred_fun <- parsnip::predict.model_fit
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
    .preds <- tibble::tibble(.id = new_data$id,
                             .pred = numeric(nrow(new_data))) %>%
        dplyr::group_by(.id) %>%
        dplyr::mutate(rowid = dplyr::row_number()) %>%
        dplyr::ungroup()

    new_data <- new_data %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(rowid = dplyr::row_number()) %>%
        dplyr::ungroup()

    .first_slice <- new_data %>%
        dplyr::group_by(id) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()


    .preds[.preds$rowid == 1, 2] <- new_data[new_data$rowid == 1, y_var] <- pred_fun(object,
                                                                                     new_data = .first_slice,
                                                                                     type = type,
                                                                                     opts = opts,
                                                                                     ...)

    .groups <- new_data %>%
               dplyr::group_by(id) %>%
               dplyr::count(id) %>%
               dim() %>%
               .[1]

    new_data_size <- nrow(.preds)/.groups

    for (i in 2:new_data_size) {

        .temp_new_data <- dplyr::bind_rows(train_tail, new_data)

        .nth_slice <- .transform(.temp_new_data, new_data_size, i)

        .preds[.preds$rowid == i, 2] <- new_data[new_data$rowid == i, y_var] <- pred_fun(object,
                                                                                         new_data = .nth_slice,
                                                                                         type = type,
                                                                                         opts = opts,
                                                                                         ...)
    }

    return(.preds[,2])

}

#' @export
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


# HELPERS ----
#' @export
.prepare_panel_transform <- function(.transform) {

    if (inherits(.transform, "function")) {

        .transform_fun <- function(temp_new_data, new_data_size, slice_idx) {

            .transform(temp_new_data) %>%
                dplyr::group_by(id) %>%
                dplyr::group_split() %>%
                purrr::map(function(x){

                    dplyr::slice_tail(x, n = new_data_size) %>%
                        .[slice_idx, ]

                }) %>%
                dplyr::bind_rows()
        }
    }

    .transform_fun
}

#' @export
panel_tail <- function(train_data, id, n){

    id <- dplyr::enquo(id)

    train_data <- train_data %>%
        dplyr::group_by(!! id) %>%
        dplyr::group_split() %>%
        purrr::map(~tail(.x, n = n)) %>%
        dplyr::bind_rows()

    return(train_data)

}






