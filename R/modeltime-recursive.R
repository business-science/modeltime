#' Create a recursive time series model from arbitrary parsnip regression model
#'
#' @param object An object of model_fit class
#' @param transform A transformation performed on new_data after
#' each step of recursive algorithm. It can be an object of types:
#'
#' * prepped `recipe`: The recipe generates lagged or sliding features
#' * `function` with two argument: `temp_new_data` and `slice_idx`
#'
#' @param train_tail A tibble with tail of training data set.
#' In most cases it'll be required to create some variables
#' based on dependent variable.
#' @param ... Not currently used.
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
#' library(tidymodels)
#' library(modeltime)
#' library(slider)
#' library(tidyverse)
#'
#'
#' # METHOD 1: RECIPE METHOD ----
#' # - Add lags or sliding features
#'
#' dax_stock <- as_tibble(EuStockMarkets) %>%
#'    select(DAX) %>%
#'    bind_rows(tibble(DAX = rep(NA, 30))) # Adding new data
#'
#' recipe_dax_stock <- recipe(DAX ~ ., data = dax_stock) %>%
#'    step_lag(all_outcomes(), lag = 1:10) %>%
#'    prep()
#'
#' dax_stock_m <- juice(recipe_dax_stock)
#'
#' dax_stock_m
#'
#' # SPLIT INTO TRAIN / FORECAST DATA ----
#' # - Divide set into training data and future forecast
#'
#' train_data <- dax_stock_m %>%
#'    filter(!is.na(DAX)) %>%
#'    na.omit()
#'
#' new_data <- dax_stock_m %>%
#'    filter(is.na(DAX))
#'
#' # RECURSIVE MODELING ----
#'
#' model_linear <- linear_reg() %>%
#'    set_engine("lm") %>%
#'    fit(DAX ~ ., data = train_data)
#'
#' recursive_linear <- model_linear %>%
#'    recursive(
#'        transform  = recipe_dax_stock,
#'        train_tail = tail(train_data, 10)
#'    )
#'
#' pred <- recursive_linear %>%
#'    predict(new_data)
#'
#' pred
#'
#' # METHOD 2: TRANSFORM METHOD  ----
#' # - Using function as transform
#'
#' dax_stock <-
#'   as_tibble(EuStockMarkets) %>%
#'   select(DAX) %>%
#'   bind_rows(tibble(DAX = rep(NA, 30))) # Adding new data
#'
#' # Transform Function can be used to make complex lagged transformations
#' #  beyond the available recipes
#' transform_fun <- function(data, slice_idx){
#'    data %>%
#'        mutate(moving_sum = lag(slide_dbl(
#'            DAX, .f = mean, .before = 4L
#'        ), 1))
#'  }
#'
#' # Make Full Data
#' dax_stock_m <- dax_stock %>%
#'   mutate(moving_sum = lag(slider::slide_dbl(
#'        DAX, .f = mean, .before = 4L
#'   ), 1))
#'
#' dax_stock_m
#'
#' # Get Train Data
#' train_data <- dax_stock_m %>%
#'   filter(!is.na(DAX)) %>%
#'   na.omit()
#'
#' # Get Future Data
#' new_data <- dax_stock_m %>%
#'   filter(is.na(DAX))
#'
#' # Make Fitted Model
#' model_linear <- linear_reg() %>%
#'    set_engine("lm") %>%
#'    fit(DAX ~ ., data = train_data)
#'
#' # Convert to Recursive Model
#' recursive_linear <- model_linear %>%
#'    recursive(transform_fun,
#'              train_tail = tail(train_data, 10))
#'
#' # Make predictions
#' pred <- recursive_linear %>%
#'    predict(new_data)
#'
#' pred
#'
#' @export
recursive <- function(object, transform, train_tail, ...){
    object$spec[["forecast"]] <- "recursive"
    object$spec[["transform"]] <- .prepare_transform(transform)
    object$spec[["train_tail"]] <- train_tail
    .class <- class(object)
    class(object) <- c(.class[1], "recursive", .class[2])
    object
}


.prepare_transform <- function(.transform){
    if (inherits(.transform, "recipe")) {

        .recipe <- .transform

        .derived_features <-
            .recipe$term_info %>%
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
            .transform(temp_new_data, slice_idx) %>%
                dplyr::slice_tail(n = new_data_size) %>%
                .[slice_idx, ]
        }
    }
    .transform_fun
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
#'
#'
#' @export
predict.recursive <- function(object, new_data, type = NULL, opts = list(), ...){

    .transform <- object$spec[["transform"]]

    .preds <- tibble::tibble(.pred = numeric(nrow(new_data)))

    .first_slice <- new_data %>%
        dplyr::slice_head(n = 1)

    .preds[1,] <- new_data[1, object$preproc$y_var] <-
        parsnip::predict.model_fit(
            object, new_data = .first_slice,
            type = type, opts = opts, ...
        )

    for (i in 2:nrow(.preds)) {

        .temp_new_data <- dplyr::bind_rows(
                object$spec$train_tail,
                new_data
            )

        .nth_slice <- .transform(.temp_new_data, nrow(new_data), i)

        .preds[i,] <- new_data[i, object$preproc$y_var] <-
            parsnip::predict.model_fit(
                object, new_data = .nth_slice,
                type = type, opts = opts, ...
            )
    }
    .preds
}

