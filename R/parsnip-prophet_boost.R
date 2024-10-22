# PROPHET BOOST ----

#' General Interface for Boosted PROPHET Time Series Models
#'
#' `prophet_boost()` is a way to generate a _specification_ of a Boosted PROPHET model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `prophet`.
#'
#' @inheritParams arima_boost
#' @inheritParams prophet_reg
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `prophet_boost()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "prophet_xgboost" (default) - Connects to [prophet::prophet()] and [xgboost::xgb.train()]
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the __PROPHET__ model are:
#'
#' - `growth`: String 'linear' or 'logistic' to specify a linear or logistic trend.
#' - `changepoint_num`: Number of potential changepoints to include for modeling trend.
#' - `changepoint_range`: Range changepoints that adjusts how close to the end
#'    the last changepoint can be located.
#' - `season`: 'additive' (default) or 'multiplicative'.
#' - `prior_scale_changepoints`: Parameter modulating the flexibility of the
#'   automatic changepoint selection. Large values will allow many changepoints,
#'   small values will allow few changepoints.
#' - `prior_scale_seasonality`: Parameter modulating the strength of the
#'  seasonality model. Larger values allow the model to fit larger seasonal
#'  fluctuations, smaller values dampen the seasonality.
#' - `prior_scale_holidays`: Parameter modulating the strength of the holiday components model,
#'  unless overridden in the holidays input.
#' - `logistic_cap`: When growth is logistic, the upper-bound for "saturation".
#' - `logistic_floor`: When growth is logistic, the lower-bound for "saturation".
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
#' Model 1: PROPHET:
#'
#' ```{r echo = FALSE}
#' tibble::tribble(
#'     ~ "modeltime", ~ "prophet",
#'     "growth", "growth ('linear')",
#'     "changepoint_num", "n.changepoints (25)",
#'     "changepoint_range", "changepoints.range (0.8)",
#'     "seasonality_yearly", "yearly.seasonality ('auto')",
#'     "seasonality_weekly", "weekly.seasonality ('auto')",
#'     "seasonality_daily", "daily.seasonality ('auto')",
#'     "season", "seasonality.mode ('additive')",
#'     "prior_scale_changepoints", "changepoint.prior.scale (0.05)",
#'     "prior_scale_seasonality", "seasonality.prior.scale (10)",
#'     "prior_scale_holidays", "holidays.prior.scale (10)",
#'     "logistic_cap", "df$cap (NULL)",
#'     "logistic_floor", "df$floor (NULL)"
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
#'
#' __prophet_xgboost__
#'
#'
#' Model 1: PROPHET (`prophet::prophet`):
#' ```{r echo = FALSE}
#' str(prophet::prophet)
#' ```
#'
#' Parameter Notes:
#' - `df`: This is supplied via the parsnip / modeltime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
#' - `holidays`: A data.frame of holidays can be supplied via `set_engine()`
#' - `uncertainty.samples`: The default is set to 0 because the prophet
#'  uncertainty intervals are not used as part of the Modeltime Workflow.
#'  You can override this setting if you plan to use prophet's uncertainty tools.
#'
#' Logistic Growth and Saturation Levels:
#' - For `growth = "logistic"`, simply add numeric values for `logistic_cap` and / or
#'   `logistic_floor`. There is _no need_ to add additional columns
#'   for "cap" and "floor" to your data frame.
#'
#' Limitations:
#' - `prophet::add_seasonality()` is not currently implemented. It's used to
#'  specify non-standard seasonalities using fourier series. An alternative is to use
#'  `step_fourier()` and supply custom seasonalities as Extra Regressors.
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
#' @section Fit Details:
#'
#' __Date and Date-Time Variable__
#'
#' It's a requirement to have a date or date-time variable as a predictor.
#' The `fit()` interface accepts date and date-time features and handles them internally.
#'
#' - `fit(y ~ date)`
#'
#'
#' __Univariate (No Extra Regressors):__
#'
#' For univariate analysis, you must include a date or date-time feature. Simply use:
#'
#'  - Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.
#'  - XY Interface: `fit_xy(x = data[,"date"], y = data$y)` will ignore xreg's.
#'
#' __Multivariate (Extra Regressors)__
#'
#'  Extra Regressors parameter is populated using the `fit()` or `fit_xy()` function:
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
#'  The `month.lbl` is an exogenous regressor that can be passed to the `arima_reg()` using
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
#' m750
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.8)
#'
#' # ---- PROPHET ----
#'
#' # Model Spec
#' model_spec <- prophet_boost(
#'     learn_rate = 0.1
#' ) %>%
#'     set_engine("prophet_xgboost")
#'
#' # Fit Spec
#'
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date + as.numeric(date) + month(date, label = TRUE),
#'         data = training(splits))
#' model_fit
#' }
#'
#'
#'
#' @export
prophet_boost <- function(mode = "regression",
                          growth = NULL, changepoint_num = NULL, changepoint_range = NULL,
                          seasonality_yearly = NULL, seasonality_weekly = NULL, seasonality_daily = NULL,
                          season = NULL,
                          prior_scale_changepoints = NULL, prior_scale_seasonality = NULL,
                          prior_scale_holidays = NULL,
                          logistic_cap = NULL, logistic_floor = NULL,
                          mtry = NULL, trees = NULL, min_n = NULL,
                          tree_depth = NULL, learn_rate = NULL,
                          loss_reduction = NULL,
                          sample_size = NULL, stop_iter = NULL) {

    args <- list(

        # Prophet
        growth                    = rlang::enquo(growth),
        changepoint_num           = rlang::enquo(changepoint_num),
        changepoint_range         = rlang::enquo(changepoint_range),
        seasonality_yearly        = rlang::enquo(seasonality_yearly),
        seasonality_weekly        = rlang::enquo(seasonality_weekly),
        seasonality_daily         = rlang::enquo(seasonality_daily),
        season                    = rlang::enquo(season),
        prior_scale_changepoints  = rlang::enquo(prior_scale_changepoints),
        prior_scale_seasonality   = rlang::enquo(prior_scale_seasonality),
        prior_scale_holidays      = rlang::enquo(prior_scale_holidays),
        logistic_cap              = rlang::enquo(logistic_cap),
        logistic_floor            = rlang::enquo(logistic_floor),

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
        "prophet_boost",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.prophet_boost <- function(x, ...) {
    cat("PROPHET Regression Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.prophet_boost <- function(object, parameters = NULL,
                                 growth = NULL, changepoint_num = NULL, changepoint_range = NULL,
                                 seasonality_yearly = NULL, seasonality_weekly = NULL, seasonality_daily = NULL,
                                 season = NULL,
                                 prior_scale_changepoints = NULL, prior_scale_seasonality = NULL,
                                 prior_scale_holidays = NULL,
                                 logistic_cap = NULL, logistic_floor = NULL,
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

        # Prophet
        growth                    = rlang::enquo(growth),
        changepoint_num           = rlang::enquo(changepoint_num),
        changepoint_range         = rlang::enquo(changepoint_range),
        seasonality_yearly        = rlang::enquo(seasonality_yearly),
        seasonality_weekly        = rlang::enquo(seasonality_weekly),
        seasonality_daily         = rlang::enquo(seasonality_daily),
        season                    = rlang::enquo(season),
        prior_scale_changepoints  = rlang::enquo(prior_scale_changepoints),
        prior_scale_seasonality   = rlang::enquo(prior_scale_seasonality),
        prior_scale_holidays      = rlang::enquo(prior_scale_holidays),
        logistic_cap              = rlang::enquo(logistic_cap),
        logistic_floor            = rlang::enquo(logistic_floor),

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
        "prophet_boost",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.prophet_boost <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'prophet_xgboost'` for translation.")
        engine <- "prophet_xgboost"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}


# FIT - prophet -----

#' Low-Level PROPHET function for translating modeltime to Boosted PROPHET
#'
#' @inheritParams prophet::prophet
#' @inheritParams prophet_boost
#' @inheritParams parsnip::xgb_train
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
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
prophet_xgboost_fit_impl <- function(x, y,
                                     df = NULL,
                                     growth = "linear",
                                     changepoints = NULL,
                                     n.changepoints = 25,
                                     changepoint.range = 0.8,
                                     yearly.seasonality = "auto",
                                     weekly.seasonality = "auto",
                                     daily.seasonality = "auto",
                                     holidays = NULL,
                                     seasonality.mode = "additive",
                                     seasonality.prior.scale = 10,
                                     holidays.prior.scale = 10,
                                     changepoint.prior.scale = 0.05,
                                     logistic_cap = NULL,
                                     logistic_floor = NULL,
                                     mcmc.samples = 0,
                                     interval.width = 0.8,
                                     uncertainty.samples = 1000,
                                     fit = TRUE,

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

    growth <- tolower(growth)

    if (!growth[1] %in% c("linear", "logistic")) {
        message("growth must be 'linear' or 'logistic'. Defaulting to 'linear'.")
        growth <- 'linear'
    }

    if (!seasonality.mode[1] %in% c("additive", "multiplicative")) {
        message("seasonality.mode must be 'additive' or 'multiplicative'. Defaulting to 'additive'.")
        seasonality.mode <- 'additive'
    }

    if (growth == "logistic") {
        if (all(c(is.null(logistic_cap), is.null(logistic_floor)))) {
            cli::cli_abort("Capacities must be supplied for `growth = 'logistic'`. Try specifying at least one of 'logistic_cap' or 'logistic_floor'")
        }
    }

    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)
    # period    <- parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    # XREGS
    # Clean names, get xreg recipe, process predictors
    xreg_recipe <- create_xreg_recipe(predictor, prepare = TRUE)
    xreg_tbl    <- juice_xreg_recipe(xreg_recipe, format = "tbl")

    # FIT

    # Construct Data Frame
    df <- tibble::tibble(
        y  = outcome,
        ds = idx
    )

    # Add logistic cap / floor
    if (growth == "logistic") {
        df$cap   <- logistic_cap
        df$floor <- logistic_floor
    }

    # Construct model
    # Fit model
    fit_prophet <- prophet::prophet(
        df = df,
        growth = growth,
        changepoints = changepoints,
        n.changepoints = n.changepoints,
        changepoint.range = changepoint.range,
        yearly.seasonality = yearly.seasonality,
        weekly.seasonality = weekly.seasonality,
        daily.seasonality = daily.seasonality,
        holidays = holidays,
        seasonality.mode = seasonality.mode,
        seasonality.prior.scale = seasonality.prior.scale,
        holidays.prior.scale = holidays.prior.scale,
        changepoint.prior.scale = changepoint.prior.scale,
        mcmc.samples = mcmc.samples,
        interval.width = interval.width,
        uncertainty.samples = uncertainty.samples,
        fit = fit
    )


    # In-sample Predictions
    prophet_fitted    <- stats::predict(fit_prophet, df) %>% dplyr::pull(yhat)
    prophet_residuals <- outcome - prophet_fitted


    # Add regressors
    # xgboost
    if (!is.null(xreg_tbl)) {
        fit_xgboost <- xgboost_impl(
            x = xreg_tbl,
            y = prophet_residuals,
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
        xgboost_fitted    <- rep(0, length(prophet_residuals))
    }

    # RETURN A NEW MODELTIME BRIDGE

    # Class - Add a class for the model
    class <- "prophet_xgboost_fit_impl"

    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = fit_prophet,
        model_2 = fit_xgboost
    )

    # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
    data <- index_tbl %>%
        dplyr::mutate(
            .actual    =  y,
            .fitted    =  prophet_fitted + xgboost_fitted,
            .residuals = .actual - .fitted
        )

    # Extras - Pass on transformation recipe
    extras <- list(
        xreg_recipe = xreg_recipe,
        logistic_params = list(
            growth         = growth,
            logistic_cap   = logistic_cap,
            logistic_floor = logistic_floor
        )
    )

    # Model Description - Gets printed to describe the high-level model structure
    desc <- paste0("PROPHET",
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
print.prophet_xgboost_fit_impl <- function(x, ...) {

    prophet_model <- x$models$model_1

    logistic_params <- x$extras$logistic_params
    if (is.null(logistic_params$logistic_cap)) {
        cap <- "NULL"
    } else {
        cap <- logistic_params$logistic_cap
    }
    if (is.null(logistic_params$logistic_floor)) {
        floor <- "NULL"
    } else {
        floor <- logistic_params$logistic_floor
    }

    msg_1 <- stringr::str_glue(
         "
          - growth: '{prophet_model$growth}'
          - n.changepoints: {prophet_model$n.changepoints}
          - changepoint.range: {prophet_model$changepoint.range}
          - yearly.seasonality: '{prophet_model$yearly.seasonality}'
          - weekly.seasonality: '{prophet_model$weekly.seasonality}'
          - daily.seasonality: '{prophet_model$daily.seasonality}'
          - seasonality.mode: '{prophet_model$seasonality.mode}'
          - changepoint.prior.scale: {prophet_model$changepoint.prior.scale}
          - seasonality.prior.scale: {prophet_model$seasonality.prior.scale}
          - holidays.prior.scale: {prophet_model$holidays.prior.scale}
          - logistic_cap: {cap}
          - logistic_floor: {floor}
         ")

    if (!is.null(x$desc)) cat(paste0(x$desc,"\n"))
    cat("---\n")
    cat("Model 1: PROPHET\n")
    print(msg_1)
    cat("\n---\n")
    cat("Model 2: XGBoost Errors\n\n")
    print(x$models$model_2$call)
    invisible(x)

    invisible(x)
}


# PREDICT ----

#' @export
predict.prophet_xgboost_fit_impl <- function(object, new_data, ...) {
    prophet_xgboost_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for Boosted PROPHET models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `prophet::predict()`
#'
#' @keywords internal
#' @export
prophet_xgboost_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    prophet_model   <- object$models$model_1
    xgboost_model   <- object$models$model_2
    idx_future      <- new_data %>% timetk::tk_index()
    xreg_recipe     <- object$extras$xreg_recipe
    logistic_params <- object$extras$logistic_params

    # Construct Future Frame
    df <- tibble::tibble(
        ds = idx_future
    )

    # Logistic Growth
    if (logistic_params$growth == "logistic") {
        df$cap   <- logistic_params$logistic_cap
        df$floor <- logistic_params$logistic_floor
    }

    # PREDICTIONS
    preds_prophet_df <- stats::predict(prophet_model, df)

    # Return predictions as numeric vector
    preds_prophet <- preds_prophet_df %>% dplyr::pull(yhat)

    # XREG
    xreg_tbl <- bake_xreg_recipe(xreg_recipe, new_data, format = "tbl")

    # xgboost
    if (!is.null(xreg_tbl)) {
        preds_xgboost <- xgboost_predict(xgboost_model, newdata = xreg_tbl, ...)
    } else {
        preds_xgboost <- rep(0, nrow(df))
    }

    # Return predictions as numeric vector
    preds <- preds_prophet + preds_xgboost

    return(preds)

}
