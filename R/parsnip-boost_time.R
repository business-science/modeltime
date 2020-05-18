
# FIT BRIDGE ----

#' Bridge ARIMA-XGBoost Modeling function
#'
#' @inheritParams forecast::auto.arima
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period A seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param max.p The maximum order of the non-seasonal auto-regressive (AR) terms.
#' @param max.d The maximum order of integration for non-seasonal differencing.
#' @param max.q The maximum order of the non-seasonal moving average (MA) terms.
#' @param max.P The maximum order of the seasonal auto-regressive (SAR) terms.
#' @param max.D The maximum order of integration for seasonal differencing.
#' @param max.Q The maximum order of the seasonal moving average (SMA) terms.
#' @param max_depth An integer for the maximum depth of the tree.
#' @param nrounds An integer for the number of boosting iterations.
#' @param eta A numeric value between zero and one to control the learning rate.
#' @param colsample_bytree Subsampling proportion of columns.
#' @param min_child_weight A numeric value for the minimum sum of instance
#'  weights needed in a child to continue to split.
#' @param gamma A number for the minimum loss reduction required to make a
#'  further partition on a leaf node of the tree
#' @param subsample Subsampling proportion of rows.
#' @param validation A positive number. If on `[0, 1)` the value, `validation`
#' is a random proportion of data in `x` and `y` that are used for performance
#' assessment and potential early stopping. If 1 or greater, it is the _number_
#' of training set samples use for these purposes.
#' @param early_stop An integer or `NULL`. If not `NULL`, it is the number of
#' training iterations without improvement before stopping. If `validation` is
#' used, performance is base on the validation set; otherwise the training set
#' is used.
#' @param ... Other options to pass to `xgb.train`.
#' @param ... Additional arguments passed to `xgboost::xgb.train`
#'
#'
#' @export
#' @importFrom stats frequency
auto_arima_xgboost_fit_impl <- function(x, y, period = "auto",
                                        max.p = 5, max.d = 2, max.q = 5,
                                        max.P = 2, max.D = 1, max.Q = 2,

                                        max.order = 5, d = NA, D = NA,
                                        start.p = 2,
                                        start.q = 2,
                                        start.P = 1,
                                        start.Q = 1,
                                        stationary = FALSE,
                                        seasonal = TRUE,
                                        ic = c("aicc", "aic", "bic"),
                                        stepwise = TRUE,
                                        nmodels = 94,
                                        trace = FALSE,
                                        approximation = (length(x) > 150 | frequency(x) > 12),
                                        method = NULL,
                                        truncate = NULL,
                                        test = c("kpss", "adf", "pp"),
                                        test.args = list(),
                                        seasonal.test = c("seas", "ocsb", "hegy", "ch"),
                                        seasonal.test.args = list(),
                                        allowdrift = TRUE,
                                        allowmean = TRUE,
                                        lambda = NULL,
                                        biasadj = FALSE,
                                        # stats::arima
                                        # SSinit = c("Gardner1980", "Rossignol2011"),
                                        # optim.method = "BFGS",
                                        # optim.control = list(), kappa = 1e6,

                                        # xgboost params
                                        max_depth = 6, nrounds = 15, eta  = 0.3,
                                        colsample_bytree = 1, min_child_weight = 1,
                                        gamma = 0, subsample = 1,
                                        validation = 0, early_stop = NULL,
                                        ...) {

    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x

    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)
    period    <- parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    # XREGS
    # Clean names, get xreg recipe, process predictors
    predictor   <- janitor::clean_names(predictor)
    xreg_recipe <- prepare_xreg_recipe_from_predictors(predictor, prepare = TRUE)
    xreg_tbl    <- juice_xreg_recipe(xreg_recipe, format = "tbl")

    # FIT
    outcome <- stats::ts(outcome, frequency = period)

    # auto.arima
    fit_arima   <- forecast::auto.arima(outcome,
                                        max.p = max.p, max.d = max.d, max.q = max.q,
                                        max.P = max.P, max.D = max.D, max.Q = max.Q,
                                        max.order = max.order, d = d, D = D,
                                        start.p = start.p, start.q = start.q,
                                        start.P = start.P, start.Q = start.Q,
                                        stationary = stationary, seasonal = seasonal,
                                        ic = ic, stepwise = stepwise,
                                        nmodels = nmodels, trace = trace,
                                        approximation = approximation,
                                        method = method, truncate = truncate,
                                        test = test, test.args = test.args,
                                        seasonal.test = seasonal.test, seasonal.test.args = seasonal.test.args,
                                        allowdrift = allowdrift, allowmean = allowmean,
                                        lambda = lambda, biasadj = biasadj
                                        )

    arima_residuals <- as.numeric(fit_arima$residuals)
    arima_fitted    <- as.numeric(fit_arima$fitted)

    # xgboost
    if (!is.null(xreg_tbl)) {
        fit_xgboost <- xgboost_impl(x = xreg_tbl, y = arima_residuals,
                                    max_depth = max_depth, nrounds = nrounds, eta  = eta,
                                    colsample_bytree = colsample_bytree,
                                    min_child_weight = min_child_weight, gamma = gamma,
                                    subsample = subsample, validation = validation,
                                    early_stop = early_stop, ...)
        xgboost_fitted    <- xgboost_predict(fit_xgboost, newdata = xreg_tbl)
    } else {
        fit_xgboost       <- NULL
        xgboost_fitted    <- rep(0, length(arima_residuals))
    }

    # GET FITTED & RESIDUALS

    # RETURN
    new_modeltime_bridge(
        class = "auto_arima_xgboost_fit_impl",

        # Models
        models = list(
            model_1 = fit_arima,
            model_2 = fit_xgboost
        ),

        # Data
        data = tibble::tibble(
                !! idx_col  := idx,
                .value      =  y
            ) %>%
            dplyr::mutate(
                .fitted =  arima_fitted + xgboost_fitted,
                .resid  = .value - .fitted
            ),

        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            xreg_recipe = xreg_recipe
        )
    )

}

#' @export
print.auto_arima_xgboost_fit_impl <- function(x, ...) {
    cat("---\n")
    cat("Model 1: Auto ARIMA:\n\n")
    print(x$models$model_1)
    cat("\n---\n")
    cat("Model 2: XGBoost ARIMA Errors:\n\n")
    print(x$models$model_2$call)
    invisible(x)
}

# PREDICT BRIDGE ----

#' @export
predict.auto_arima_xgboost_fit_impl <- function(object, new_data, ...) {
    auto_arima_xgboost_predict_impl(object, new_data, ...)
}



#' Bridge prediction Function for ARIMA-XGBoost Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `predict.xgb.Booster()`
#'
#' @export
auto_arima_xgboost_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    arima_model   <- object$models$model_1
    xgboost_model <- object$models$model_2
    idx_train     <- object$data %>% timetk::tk_index()
    xreg_recipe   <- object$extras$xreg_recipe
    h_horizon     <- nrow(new_data)

    # XREG
    new_data    <- janitor::clean_names(new_data)
    xreg_tbl    <- bake_xreg_recipe(xreg_recipe, new_data, format = "tbl")

    # PREDICTIONS

    # arima
    preds_arima <- forecast::forecast(arima_model, h = h_horizon) %>%
        tibble::as_tibble() %>%
        purrr::pluck(1) %>%
        as.numeric()

    # xgboost
    if (!is.null(xreg_tbl)) {
        preds_xgboost <- xgboost_predict(xgboost_model, newdata = xreg_tbl, ...)
    } else {
        preds_xgboost <- rep(0, h_horizon)
    }

    # Return predictions as numeric vector
    preds <- preds_arima + preds_xgboost

    return(preds)

}


# XGBOOST UTILITIES ----

xgboost_impl <- function(x, y,
                         max_depth = 6, nrounds = 15, eta  = 0.3, colsample_bytree = 1,
                         min_child_weight = 1, gamma = 0, subsample = 1, validation = 0,
                         early_stop = NULL, ...) {

    parsnip::xgb_train(x, y,
                       max_depth = max_depth, nrounds = nrounds, eta  = eta, colsample_bytree = colsample_bytree,
                       min_child_weight = min_child_weight, gamma = gamma, subsample = subsample, validation = validation,
                       early_stop = early_stop, ...)

}


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
