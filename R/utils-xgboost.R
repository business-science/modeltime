# XGBOOST UTILITIES ----


#' Wrapper for parsnip::xgb_train
#'
#'
#' @inheritParams parsnip::xgb_train
#' @param validation A positive number. If on `[0, 1)` the value, `validation`
#' is a random proportion of data in `x` and `y` that are used for performance
#' assessment and potential early stopping. If 1 or greater, it is the _number_
#' of training set samples use for these purposes.
#' @param early_stop An integer or `NULL`. If not `NULL`, it is the number of
#' training iterations without improvement before stopping. If `validation` is
#' used, performance is base on the validation set; otherwise the training set
#' is used.
#'
#' @keywords internal
#' @export
xgboost_impl <- function(x, y,
                         max_depth = 6, nrounds = 15, eta  = 0.3, colsample_bynode = NULL,
                         colsample_bytree = NULL, min_child_weight = 1, gamma = 0, subsample = 1,
                         validation = 0, early_stop = NULL, objective = NULL, counts = TRUE,
                         event_level = c("first", "second"), ...) {

    if (!is.null(colsample_bytree)) {
        if (colsample_bytree == 1) {
            if (counts) {
                rlang::warn("`colsample_bytree = 1` with `counts = TRUE` will only sample a single column.
                            Set `counts = FALSE` to use a proportion (100% of columns).")
            }
        }
    }
    if (!is.null(colsample_bynode)) {
        if (colsample_bynode == 1) {
            if (counts) {
                rlang::warn("`colsample_bynode = 1` with `counts = TRUE` will only sample a single column.
                            Set `counts = FALSE` to use a proportion (100% of columns).")
            }
        }
    }

    parsnip::xgb_train(x, y,
                       max_depth = max_depth,
                       nrounds = nrounds,
                       eta  = eta,
                       colsample_bynode = colsample_bynode,
                       colsample_bytree = colsample_bytree,
                       min_child_weight = min_child_weight,
                       gamma = gamma,
                       subsample = subsample,
                       validation = validation,
                       early_stop = early_stop,
                       objective = objective,
                       counts = counts,
                       event_level = event_level,
                       ...)

}

#' Wrapper for xgboost::predict
#'
#' @inheritParams stats::predict
#' @param newdata New data to be predicted
#'
#' @keywords internal
#' @export
xgboost_predict <- function(object, newdata, ...) {
    if (!inherits(newdata, "xgb.DMatrix")) {
        newdata <- as.matrix(newdata)
        newdata <- xgboost::xgb.DMatrix(data = newdata, missing = NA)
    }

    res <- stats::predict(object, newdata, ...)

    x = switch(
        object$params$objective,
        "reg:linear" = , "reg:logistic" = , "binary:logistic" = res,
        "binary:logitraw" = stats::binomial()$linkinv(res),
        "multi:softprob" = matrix(res, ncol = object$params$num_class, byrow = TRUE),
        res
    )
    x
}
