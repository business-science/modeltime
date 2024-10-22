# ARIMA BOOST ----

#' General Interface for "Boosted" ARIMA Regression Models
#'
#' `arima_boost()` is a way to generate a _specification_ of a time series model
#'  that uses boosting to improve modeling errors (residuals) on Exogenous Regressors.
#'  It works with both "automated" ARIMA (`auto.arima`) and standard ARIMA (`arima`).
#'  The main algorithms are:
#'  - Auto ARIMA + XGBoost Errors (engine = `auto_arima_xgboost`, default)
#'  - ARIMA + XGBoost Errors (engine = `arima_xgboost`)
#'
#'
#' @inheritParams parsnip::boost_tree
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param sample_size  number for the number (or proportion) of data that is exposed to the fitting routine.
#' @param seasonal_period A seasonal frequency. Uses "auto" by default.
#'  A character phrase of "auto" or time-based phrase of "2 weeks"
#'  can be used if a date or date-time variable is provided.
#'  See Fit Details below.
#' @param non_seasonal_ar The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.
#' @param non_seasonal_differences The order of integration for non-seasonal differencing. Often denoted "d" in pdq-notation.
#' @param non_seasonal_ma The order of the non-seasonal moving average (MA) terms. Often denoted "q" in pdq-notation.
#' @param seasonal_ar The order of the seasonal auto-regressive (SAR) terms. Often denoted "P" in PDQ-notation.
#' @param seasonal_differences The order of integration for seasonal differencing. Often denoted "D" in PDQ-notation.
#' @param seasonal_ma The order of the seasonal moving average (SMA) terms. Often denoted "Q" in PDQ-notation.
#' @param stop_iter The number of iterations without improvement before
#'   stopping  (`xgboost` only).
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `arima_boost()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "auto_arima_xgboost" (default) - Connects to [forecast::auto.arima()] and
#'   [xgboost::xgb.train]
#'  - "arima_xgboost" - Connects to [forecast::Arima()] and
#'   [xgboost::xgb.train]
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the __ARIMA model__ are:
#'
#'  - `seasonal_period`: The periodic nature of the seasonality. Uses "auto" by default.
#'  - `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `non_seasonal_differences`: The order of integration for non-seasonal differencing.
#'  - `non_seasonal_ma`: The order of the non-seasonal moving average (MA) terms.
#'  - `seasonal_ar`: The order of the seasonal auto-regressive (SAR) terms.
#'  - `seasonal_differences`: The order of integration for seasonal differencing.
#'  - `seasonal_ma`: The order of the seasonal moving average (SMA) terms.
#'
#' The main arguments (tuning parameters) for the model __XGBoost model__ are:
#'
#'  - `mtry`: The number of predictors that will be
#'   randomly sampled at each split when creating the tree models.
#'  - `trees`: The number of trees contained in the ensemble.
#'  - `min_n`: The minimum number of data points in a node
#'   that are required for the node to be split further.
#'  - `tree_depth`: The maximum depth of the tree (i.e. number of
#'  splits).
#'  - `learn_rate`: The rate at which the boosting algorithm adapts
#'   from iteration-to-iteration.
#'  - `loss_reduction`: The reduction in the loss function required
#'   to split further.
#'  - `sample_size`: The amount of data exposed to the fitting routine.
#'  - `stop_iter`: The number of iterations without improvement before
#'   stopping.
#'
#'
#' These arguments are converted to their specific names at the
#'  time that the model is fit.
#'
#' Other options and argument can be
#'  set using `set_engine()` (See Engine Details below).
#'
#' If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#'
#' @section Engine Details:
#'
#' The standardized parameter names in `modeltime` can be mapped to their original
#' names in each engine:
#'
#' Model 1: ARIMA:
#'
#' ```{r echo = FALSE}
#' # parsnip::convert_args("arima_reg")
#' tibble::tribble(
#'     ~ "modeltime", ~ "forecast::auto.arima", ~ "forecast::Arima",
#'     "seasonal_period", "ts(frequency)", "ts(frequency)",
#'     "non_seasonal_ar, non_seasonal_differences, non_seasonal_ma", "max.p(5), max.d(2), max.q(5)", "order = c(p(0), d(0), q(0))",
#'     "seasonal_ar, seasonal_differences, seasonal_ma", "max.P(2), max.D(1), max.Q(2)", "seasonal = c(P(0), D(0), Q(0))"
#' ) %>% knitr::kable()
#' ```
#'
#' Model 2: XGBoost:
#'
#' ```{r echo = FALSE}
#' # parsnip::convert_args("arima_boost")
#' tibble::tribble(
#'     ~ "modeltime", ~ "xgboost::xgb.train",
#'     "tree_depth", "max_depth (6)",
#'     "trees", "nrounds (15)",
#'     "learn_rate", "eta (0.3)",
#'     "mtry", "colsample_bynode (1)",
#'     "min_n", "min_child_weight (1)",
#'     "loss_reduction", "gamma (0)",
#'     "sample_size", "subsample (1)",
#'     "stop_iter", "early_stop"
#' ) %>% knitr::kable()
#' ```
#'
#'
#' Other options can be set using `set_engine()`.
#'
#' __auto_arima_xgboost (default engine)__
#'
#' Model 1: Auto ARIMA (`forecast::auto.arima`):
#' ```{r echo = FALSE}
#' str(forecast::auto.arima)
#' ```
#'
#' Parameter Notes:
#' - All values of nonseasonal pdq and seasonal PDQ are maximums.
#'  The `auto.arima` will select a value using these as an upper limit.
#' - `xreg` - This should not be used since XGBoost will be doing the regression
#'
#' Model 2: XGBoost (`xgboost::xgb.train`):
#' ```{r echo = FALSE}
#' str(xgboost::xgb.train)
#' ```
#'
#' Parameter Notes:
#' - XGBoost uses a `params = list()` to capture.
#'  Parsnip / Modeltime automatically sends any args provided as `...` inside of `set_engine()` to
#'  the `params = list(...)`.
#'
#'
#'
#' @section Fit Details:
#'
#' __Date and Date-Time Variable__
#'
#' It's a requirement to have a date or date-time variable as a predictor.
#' The `fit()` interface accepts date and date-time features and handles them internally.
#'
#' - `fit(y ~ date)`
#'
#' _Seasonal Period Specification_
#'
#' The period can be non-seasonal (`seasonal_period = 1`) or seasonal (e.g. `seasonal_period = 12` or `seasonal_period = "12 months"`).
#' There are 3 ways to specify:
#'
#' 1. `seasonal_period = "auto"`: A period is selected based on the periodicity of the data (e.g. 12 if monthly)
#' 2. `seasonal_period = 12`: A numeric frequency. For example, 12 is common for monthly data
#' 3. `seasonal_period = "1 year"`: A time-based phrase. For example, "1 year" would convert to 12 for monthly data.
#'
#'
#' __Univariate (No xregs, Exogenous Regressors):__
#'
#' For univariate analysis, you must include a date or date-time feature. Simply use:
#'
#'  - Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.
#'  - XY Interface: `fit_xy(x = data[,"date"], y = data$y)` will ignore xreg's.
#'
#' __Multivariate (xregs, Exogenous Regressors)__
#'
#'  The `xreg` parameter is populated using the `fit()` or `fit_xy()` function:
#'
#'  - Only `factor`, `ordered factor`, and `numeric` data will be used as xregs.
#'  - Date and Date-time variables are not used as xregs
#'  - `character` data should be converted to factor.
#'
#'  _Xreg Example:_ Suppose you have 3 features:
#'
#'  1. `y` (target)
#'  2. `date` (time stamp),
#'  3. `month.lbl` (labeled month as a ordered factor).
#'
#'  The `month.lbl` is an exogenous regressor that can be passed to the `arima_boost()` using
#'  `fit()`:
#'
#'  - `fit(y ~ date + month.lbl)` will pass `month.lbl` on as an exogenous regressor.
#'  - `fit_xy(data[,c("date", "month.lbl")], y = data$y)` will pass x, where x is a data frame containing `month.lbl`
#'   and the `date` feature. Only `month.lbl` will be used as an exogenous regressor.
#'
#'  Note that date or date-time class values are excluded from `xreg`.
#'
#'
#'
#' @seealso `fit.model_spec()`, `set_engine()`
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(lubridate)
#' library(parsnip)
#' library(rsample)
#' library(timetk)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.9)
#'
#' # MODEL SPEC ----
#'
#' # Set engine and boosting parameters
#' model_spec <- arima_boost(
#'
#'     # ARIMA args
#'     seasonal_period = 12,
#'     non_seasonal_ar = 0,
#'     non_seasonal_differences = 1,
#'     non_seasonal_ma = 1,
#'     seasonal_ar     = 0,
#'     seasonal_differences = 1,
#'     seasonal_ma     = 1,
#'
#'     # XGBoost Args
#'     tree_depth = 6,
#'     learn_rate = 0.1
#' ) %>%
#'     set_engine(engine = "arima_xgboost")
#'
#' # FIT ----
#'
#' # Boosting - Happens by adding numeric date and month features
#' model_fit_boosted <- model_spec %>%
#'     fit(value ~ date + as.numeric(date) + month(date, label = TRUE),
#'         data = training(splits))
#' model_fit_boosted
#' }
#'
#'
#' @export
arima_boost <- function(mode = "regression", seasonal_period = NULL,
                        non_seasonal_ar = NULL, non_seasonal_differences = NULL, non_seasonal_ma = NULL,
                        seasonal_ar = NULL, seasonal_differences = NULL, seasonal_ma = NULL,
                        mtry = NULL, trees = NULL, min_n = NULL,
                        tree_depth = NULL, learn_rate = NULL,
                        loss_reduction = NULL,
                        sample_size = NULL, stop_iter = NULL
                        ) {

    args <- list(

        # ARIMA
        seasonal_period           = rlang::enquo(seasonal_period),
        non_seasonal_ar           = rlang::enquo(non_seasonal_ar),
        non_seasonal_differences  = rlang::enquo(non_seasonal_differences),
        non_seasonal_ma           = rlang::enquo(non_seasonal_ma),
        seasonal_ar               = rlang::enquo(seasonal_ar),
        seasonal_differences      = rlang::enquo(seasonal_differences),
        seasonal_ma               = rlang::enquo(seasonal_ma),

        # XGBoost
        mtry                      = rlang::enquo(mtry),
        trees                     = rlang::enquo(trees),
        min_n                     = rlang::enquo(min_n),
        tree_depth                = rlang::enquo(tree_depth),
        learn_rate                = rlang::enquo(learn_rate),
        loss_reduction            = rlang::enquo(loss_reduction),
        sample_size               = rlang::enquo(sample_size),
        stop_iter                 = rlang::enquo(stop_iter)
    )

    parsnip::new_model_spec(
        "arima_boost",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.arima_boost <- function(x, ...) {
    cat("Time Series Model w/ XGBoost Error Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.arima_boost <- function(object,
                               parameters = NULL, seasonal_period = NULL,
                               non_seasonal_ar = NULL, non_seasonal_differences = NULL, non_seasonal_ma = NULL,
                               seasonal_ar = NULL, seasonal_differences = NULL, seasonal_ma = NULL,
                               mtry = NULL, trees = NULL, min_n = NULL,
                               tree_depth = NULL, learn_rate = NULL,
                               loss_reduction = NULL,
                               sample_size = NULL, stop_iter = NULL,
                               fresh = FALSE, ...) {

    eng_args <- parsnip::update_engine_parameters(object$eng_args, fresh, ...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(

        # ARIMA
        seasonal_period           = rlang::enquo(seasonal_period),
        non_seasonal_ar           = rlang::enquo(non_seasonal_ar),
        non_seasonal_differences  = rlang::enquo(non_seasonal_differences),
        non_seasonal_ma           = rlang::enquo(non_seasonal_ma),
        seasonal_ar               = rlang::enquo(seasonal_ar),
        seasonal_differences      = rlang::enquo(seasonal_differences),
        seasonal_ma               = rlang::enquo(seasonal_ma),

        # XGBoost
        mtry                      = rlang::enquo(mtry),
        trees                     = rlang::enquo(trees),
        min_n                     = rlang::enquo(min_n),
        tree_depth                = rlang::enquo(tree_depth),
        learn_rate                = rlang::enquo(learn_rate),
        loss_reduction            = rlang::enquo(loss_reduction),
        sample_size               = rlang::enquo(sample_size),
        stop_iter                 = rlang::enquo(stop_iter)
    )

    args <- parsnip::update_main_parameters(args, parameters)

    if (fresh) {
        object$args <- args
        object$eng_args <- eng_args
    } else {
        null_args <- purrr::map_lgl(args, parsnip::null_value)
        if (any(null_args))
            args <- args[!null_args]
        if (length(args) > 0)
            object$args[names(args)] <- args
        if (length(eng_args) > 0)
            object$eng_args[names(eng_args)] <- eng_args
    }

    parsnip::new_model_spec(
        "arima_boost",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.arima_boost <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'auto_arima_xgboost'` for translation.")
        engine <- "auto_arima_xgboost"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}



# FIT BRIDGE - AUTO ARIMA ----

#' Bridge ARIMA-XGBoost Modeling function
#'
#' @inheritParams forecast::auto.arima
#' @inheritParams parsnip::xgb_train
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
#' @keywords internal
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
                                        max_depth = 6,
                                        nrounds = 15,
                                        eta  = 0.3,
                                        colsample_bytree = NULL,
                                        colsample_bynode = NULL,
                                        min_child_weight = 1,
                                        gamma = 0,
                                        subsample = 1,
                                        validation = 0,
                                        early_stop = NULL,
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
    xreg_recipe <- create_xreg_recipe(predictor, prepare = TRUE, one_hot = FALSE)
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
        fit_xgboost <- xgboost_impl(
            x = xreg_tbl,
            y = arima_residuals,
            max_depth = max_depth,
            nrounds = nrounds,
            eta  = eta,
            colsample_bytree = colsample_bytree,
            colsample_bynode = colsample_bynode,
            min_child_weight = min_child_weight,
            gamma = gamma,
            subsample = subsample,
            validation = validation,
            early_stop = early_stop,
            ...
        )
        xgboost_fitted    <- xgboost_predict(fit_xgboost, newdata = xreg_tbl)
    } else {
        fit_xgboost       <- NULL
        xgboost_fitted    <- rep(0, length(arima_residuals))
    }

    # RETURN A NEW MODELTIME BRIDGE

    # Class - Add a class for the model
    class <- "auto_arima_xgboost_fit_impl"

    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = fit_arima,
        model_2 = fit_xgboost
    )

    # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
    data <- index_tbl %>%
        dplyr::mutate(
            .actual    =  y,
            .fitted    =  arima_fitted + xgboost_fitted,
            .residuals = .actual - .fitted
        )

    # Extras - Pass on transformation recipe
    extras <- list(
        xreg_recipe = xreg_recipe
    )

    # Model Description - Gets printed to describe the high-level model structure
    desc <- paste0(get_arima_description(fit_arima),
                   ifelse(is.null(fit_xgboost), "", " w/ XGBoost Errors"))

    # Create new model
    new_modeltime_bridge(
        class  = class,
        models = models,
        data   = data,
        extras = extras,
        desc   = desc
    )
}

#' @export
print.auto_arima_xgboost_fit_impl <- function(x, ...) {

    if (!is.null(x$desc)) cat(paste0(x$desc,"\n"))
    cat("---\n")
    cat("Model 1: Auto ARIMA\n")
    print(x$models$model_1)
    cat("\n---\n")
    cat("Model 2: XGBoost Errors\n\n")
    print(x$models$model_2$call)
    invisible(x)
}


# FIT BRIDGE - STANDARD ARIMA ----

#' Bridge ARIMA-XGBoost Modeling function
#'
#' @inheritParams forecast::Arima
#' @inheritParams parsnip::xgb_train
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period A seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param p The order of the non-seasonal auto-regressive (AR) terms.
#' @param d The order of integration for non-seasonal differencing.
#' @param q The order of the non-seasonal moving average (MA) terms.
#' @param P The order of the seasonal auto-regressive (SAR) terms.
#' @param D The order of integration for seasonal differencing.
#' @param Q The order of the seasonal moving average (SMA) terms.
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
#' @param ... Additional arguments passed to `xgboost::xgb.train`
#'
#' @keywords internal
#' @export
#' @importFrom stats frequency
arima_xgboost_fit_impl <- function(x, y, period = "auto",
                                   p = 0, d = 0, q = 0,
                                   P = 0, D = 0, Q = 0,
                                   include.mean = TRUE,
                                   include.drift = FALSE,
                                   include.constant,
                                   lambda = model$lambda,
                                   biasadj = FALSE,
                                   method = c("CSS-ML", "ML", "CSS"),
                                   model = NULL,

                                   # xgboost params
                                   # xgboost params
                                   max_depth = 6,
                                   nrounds = 15,
                                   eta  = 0.3,
                                   colsample_bytree = NULL,
                                   colsample_bynode = NULL,
                                   min_child_weight = 1,
                                   gamma = 0,
                                   subsample = 1,
                                   validation = 0,
                                   early_stop = NULL,
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
    xreg_recipe <- create_xreg_recipe(predictor, prepare = TRUE)
    xreg_tbl    <- juice_xreg_recipe(xreg_recipe, format = "tbl")

    # FIT
    outcome <- stats::ts(outcome, frequency = period)

    # auto.arima
    fit_arima   <- forecast::Arima(outcome,
                                   order = c(p, d, q),
                                   seasonal = c(P, D, Q),
                                   include.mean = include.mean,
                                   include.drift = include.drift,
                                   include.constant = include.constant,
                                   lambda = model$lambda,
                                   biasadj = biasadj,
                                   method = method,
                                   model = model
    )

    arima_residuals <- as.numeric(fit_arima$residuals)
    arima_fitted    <- as.numeric(fit_arima$fitted)

    # xgboost
    if (!is.null(xreg_tbl)) {
        fit_xgboost <- xgboost_impl(
            x = xreg_tbl,
            y = arima_residuals,
            max_depth = max_depth,
            nrounds = nrounds,
            eta  = eta,
            colsample_bytree = colsample_bytree,
            colsample_bynode = colsample_bynode,
            min_child_weight = min_child_weight,
            gamma = gamma,
            subsample = subsample,
            validation = validation,
            early_stop = early_stop,
            ...
        )
        xgboost_fitted    <- xgboost_predict(fit_xgboost, newdata = xreg_tbl)
    } else {
        fit_xgboost       <- NULL
        xgboost_fitted    <- rep(0, length(arima_residuals))
    }

    # RETURN A NEW MODELTIME BRIDGE

    # Class - Add a class for the model
    class <- "arima_xgboost_fit_impl"

    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = fit_arima,
        model_2 = fit_xgboost
    )

    # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
    data <- index_tbl %>%
        dplyr::mutate(
            .actual    =  y,
            .fitted    =  arima_fitted + xgboost_fitted,
            .residuals = .actual - .fitted
        )

    # Extras - Pass on transformation recipe
    extras <- list(
        xreg_recipe = xreg_recipe
    )

    # Model Description - Gets printed to describe the high-level model structure
    desc <- paste0(get_arima_description(fit_arima),
                   ifelse(is.null(fit_xgboost), "", " w/ XGBoost Errors"))

    # Create new model
    new_modeltime_bridge(
        class  = class,
        models = models,
        data   = data,
        extras = extras,
        desc   = desc
    )

}

#' @export
print.arima_xgboost_fit_impl <- function(x, ...) {

    if (!is.null(x$desc)) cat(paste0(x$desc,"\n"))
    cat("---\n")
    cat("Model 1: Standard ARIMA\n")
    print(x$models$model_1)
    cat("\n---\n")
    cat("Model 2: XGBoost Errors\n\n")
    print(x$models$model_2$call)
    invisible(x)
}

# PREDICT BRIDGE ----

#' @export
predict.auto_arima_xgboost_fit_impl <- function(object, new_data, ...) {
    arima_xgboost_predict_impl(object, new_data, ...)
}

#' @export
predict.arima_xgboost_fit_impl <- function(object, new_data, ...) {
    arima_xgboost_predict_impl(object, new_data, ...)
}



#' Bridge prediction Function for ARIMA-XGBoost Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `predict.xgb.Booster()`
#'
#' @keywords internal
#' @export
arima_xgboost_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    arima_model   <- object$models$model_1
    xgboost_model <- object$models$model_2
    idx_train     <- object$data %>% timetk::tk_index()
    xreg_recipe   <- object$extras$xreg_recipe
    h_horizon     <- nrow(new_data)

    # XREG
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


