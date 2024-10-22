#' General Interface for ADAM Regression Models
#'
#' `adam_reg()` is a way to generate a _specification_ of an ADAM model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `smooth`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
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
#' @param ets_model The type of ETS model. The first letter stands for the type of the error term ("A" or "M"),
#' the second (and sometimes the third as well) is for the trend ("N", "A", "Ad", "M" or "Md"), and the last one is
#' for the type of seasonality ("N", "A" or "M").
#' @param use_constant Logical, determining, whether the constant is needed in the model or not. This is mainly needed for
#' ARIMA part of the model, but can be used for ETS as well.
#' @param regressors_treatment 	The variable defines what to do with the provided explanatory variables: "use" means that all
#' of the data should be used, while "select" means that a selection using ic should be done, "adapt" will trigger the
#' mechanism of time varying parameters for the explanatory variables.
#' @param outliers_treatment Defines what to do with outliers: "ignore", so just returning the model, "detect" outliers based
#' on specified level and include dummies for them in the model, or detect and "select" those of them that reduce ic value.
#' @param outliers_ci What confidence level to use for detection of outliers. Default is 99%.
#' @param probability_model The type of model used in probability estimation. Can be "none" - none, "fixed" - constant
#' probability, "general" - the general Beta model with two parameters, "odds-ratio" - the Odds-ratio model with b=1 in
#' Beta distribution, "inverse-odds-ratio" - the model with a=1 in Beta distribution, "direct" - the TSB-like
#' (Teunter et al., 2011) probability update mechanism a+b=1, "auto" - the automatically selected type of occurrence model.
#' @param distribution what density function to assume for the error term. The full name of the distribution should be
#' provided, starting with the letter "d" - "density".
#' @param loss The type of Loss Function used in optimization.
#' @param information_criteria The information criterion to use in the model selection / combination procedure.
#' @param select_order If `TRUE`, then the function will select the most appropriate order. The values list(ar=...,i=...,ma=...)
#' specify the maximum orders to check in this case.
#'
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `adam_reg()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "auto_adam" (default) - Connects to [smooth::auto.adam()]
#'  - "adam" - Connects to [smooth::adam()]
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the model are:
#'
#'  - `seasonal_period`: The periodic nature of the seasonality. Uses "auto" by default.
#'  - `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `non_seasonal_differences`: The order of integration for non-seasonal differencing.
#'  - `non_seasonal_ma`: The order of the non-seasonal moving average (MA) terms.
#'  - `seasonal_ar`: The order of the seasonal auto-regressive (SAR) terms.
#'  - `seasonal_differences`: The order of integration for seasonal differencing.
#'  - `seasonal_ma`: The order of the seasonal moving average (SMA) terms.
#'  - `ets_model`: The type of ETS model.
#'  - `use_constant`: Logical, determining, whether the constant is needed in the model or not.
#'  - `regressors_treatment`: The variable defines what to do with the provided explanatory variables.
#'  - `outliers_treatment`: Defines what to do with outliers.
#'  - `probability_model`: The type of model used in probability estimation.
#'  - `distribution`: what density function to assume for the error term.
#'  - `loss`: The type of Loss Function used in optimization.
#'  - `information_criteria`: The information criterion to use in the model selection / combination procedure.
#'
#' These arguments are converted to their specific names at the
#'  time that the model is fit.
#'
#' Other options and argument can be
#'  set using `set_engine()` (See Engine Details below).
#'
#'  If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#'
#' __auto_adam (default engine)__
#'
#' The engine uses [smooth::auto.adam()].
#'
#' Function Parameters:
#' ```{r echo = FALSE, eval = rlang::is_installed("smooth")}
#' str(smooth::auto.adam)
#' ```
#' The _MAXIMUM_ nonseasonal ARIMA terms (`max.p`, `max.d`, `max.q`) and
#' seasonal ARIMA terms (`max.P`, `max.D`, `max.Q`) are provided to
#' [forecast::auto.arima()] via `arima_reg()` parameters.
#' Other options and argument can be set using `set_engine()`.
#'
#' Parameter Notes:
#' - All values of nonseasonal pdq and seasonal PDQ are maximums.
#'  The `smooth::auto.adam()` model will select a value using these as an upper limit.
#' - `xreg` - This is supplied via the parsnip / modeltime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
#'
#' __adam__
#'
#' The engine uses [smooth::adam()].
#'
#' Function Parameters:
#' ```{r echo = FALSE, eval = rlang::is_installed("smooth")}
#' str(smooth::adam)
#' ```
#'
#' The nonseasonal ARIMA terms (`orders`) and seasonal ARIMA terms (`orders`)
#' are provided to [smooth::adam()] via `adam_reg()` parameters.
#' Other options and argument can be set using `set_engine()`.
#'
#' Parameter Notes:
#' - `xreg` - This is supplied via the parsnip / modeltime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
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
#' The period can be non-seasonal (`seasonal_period = 1 or "none"`) or
#' yearly seasonal (e.g. For monthly time stamps, `seasonal_period = 12`, `seasonal_period = "12 months"`, or `seasonal_period = "yearly"`).
#' There are 3 ways to specify:
#'
#' 1. `seasonal_period = "auto"`: A seasonal period is selected based on the periodicity of the data (e.g. 12 if monthly)
#' 2. `seasonal_period = 12`: A numeric frequency. For example, 12 is common for monthly data
#' 3. `seasonal_period = "1 year"`: A time-based phrase. For example, "1 year" would convert to 12 for monthly data.
#'
#'
#' __Univariate (No xregs, Exogenous Regressors):__
#'
#' For univariate analysis, you must include a date or date-time feature. Simply use:
#'
#'  - Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.
#'
#' __Multivariate (xregs, Exogenous Regressors)__
#'
#'  The `xreg` parameter is populated using the `fit()` function:
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
#'
#'  Note that date or date-time class values are excluded from `xreg`.
#'
#' @seealso `fit.model_spec()`, `set_engine()`
#'
#' @examplesIf rlang::is_installed("smooth")
#'
#' \donttest{
#' library(dplyr)
#' library(parsnip)
#' library(rsample)
#' library(timetk)
#' library(smooth)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#' m750
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.8)
#'
#' # ---- AUTO ADAM ----
#'
#' # Model Spec
#' model_spec <- adam_reg() %>%
#'     set_engine("auto_adam")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#'
#'
#' # ---- STANDARD ADAM ----
#'
#' # Model Spec
#' model_spec <- adam_reg(
#'         seasonal_period          = 12,
#'         non_seasonal_ar          = 3,
#'         non_seasonal_differences = 1,
#'         non_seasonal_ma          = 3,
#'         seasonal_ar              = 1,
#'         seasonal_differences     = 0,
#'         seasonal_ma              = 1
#'     ) %>%
#'     set_engine("adam")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#' }
#'
#' @export
adam_reg <- function(mode = "regression", ets_model = NULL,
                     non_seasonal_ar = NULL, non_seasonal_differences = NULL, non_seasonal_ma = NULL,
                     seasonal_ar = NULL, seasonal_differences = NULL, seasonal_ma = NULL, use_constant = NULL,
                     regressors_treatment = NULL, outliers_treatment = NULL, outliers_ci = NULL,
                     probability_model = NULL, distribution = NULL, loss = NULL, information_criteria = NULL,
                     seasonal_period = NULL, select_order = NULL) {

    args <- list(
        ets_model                 = rlang::enquo(ets_model),
        non_seasonal_ar           = rlang::enquo(non_seasonal_ar),
        non_seasonal_differences  = rlang::enquo(non_seasonal_differences),
        non_seasonal_ma           = rlang::enquo(non_seasonal_ma),
        seasonal_ar               = rlang::enquo(seasonal_ar),
        seasonal_differences      = rlang::enquo(seasonal_differences),
        seasonal_ma               = rlang::enquo(seasonal_ma),
        use_constant              = rlang::enquo(use_constant),
        regressors_treatment      = rlang::enquo(regressors_treatment),
        outliers_treatment        = rlang::enquo(outliers_treatment),
        outliers_ci               = rlang::enquo(outliers_ci),
        probability_model         = rlang::enquo(probability_model),
        distribution              = rlang::enquo(distribution),
        loss                      = rlang::enquo(loss),
        information_criteria      = rlang::enquo(information_criteria),
        seasonal_period           = rlang::enquo(seasonal_period),
        select_order              = rlang::enquo(select_order)
    )

    parsnip::new_model_spec(
        "adam_reg",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.adam_reg <- function(x, ...) {
    cat("ADAM Regression Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.adam_reg <- function(object, parameters = NULL,
                             ets_model = NULL,
                             non_seasonal_ar = NULL, non_seasonal_differences = NULL, non_seasonal_ma = NULL,
                             seasonal_ar = NULL, seasonal_differences = NULL, seasonal_ma = NULL, use_constant = NULL,
                             regressors_treatment = NULL, outliers_treatment = NULL, outliers_ci = NULL,
                             probability_model = NULL, distribution = NULL, loss = NULL, information_criteria = NULL,
                             seasonal_period = NULL, select_order = NULL,
                             fresh = FALSE, ...) {

    eng_args <- parsnip::update_engine_parameters(object$eng_args, fresh, ...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        ets_model                 = rlang::enquo(ets_model),
        non_seasonal_ar           = rlang::enquo(non_seasonal_ar),
        non_seasonal_differences  = rlang::enquo(non_seasonal_differences),
        non_seasonal_ma           = rlang::enquo(non_seasonal_ma),
        seasonal_ar               = rlang::enquo(seasonal_ar),
        seasonal_differences      = rlang::enquo(seasonal_differences),
        seasonal_ma               = rlang::enquo(seasonal_ma),
        use_constant              = rlang::enquo(use_constant),
        regressors_treatment      = rlang::enquo(regressors_treatment),
        outliers_treatment        = rlang::enquo(outliers_treatment),
        outliers_ci               = rlang::enquo(outliers_ci),
        probability_model         = rlang::enquo(probability_model),
        distribution              = rlang::enquo(distribution),
        loss                      = rlang::enquo(loss),
        information_criteria      = rlang::enquo(information_criteria),
        seasonal_period           = rlang::enquo(seasonal_period),
        select_order              = rlang::enquo(select_order)
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
        "adam_reg",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.adam_reg <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'adam'` for translation.")
        engine <- "adam"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}


# FIT - ADAM -----

#' Low-Level ADAM function for translating modeltime to forecast
#'
#' @param x A data.frame of predictors
#' @param y A vector with outcome
#' @param period A seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param p The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.
#' @param d The order of integration for non-seasonal differencing. Often denoted "d" in pdq-notation.
#' @param q The order of the non-seasonal moving average (MA) terms. Often denoted "q" in pdq-notation.
#' @param P The order of the seasonal auto-regressive (SAR) terms. Often denoted "P" in PDQ-notation.
#' @param D The order of integration for seasonal differencing. Often denoted "D" in PDQ-notation.
#' @param Q The order of the seasonal moving average (SMA) terms. Often denoted "Q" in PDQ-notation.
#' @param model The type of ETS model.
#' @param constant 	Logical, determining, whether the constant is needed in the model or not.
#' @param regressors The variable defines what to do with the provided explanatory variables.
#' @param outliers Defines what to do with outliers.
#' @param level What confidence level to use for detection of outliers.
#' @param occurrence The type of model used in probability estimation.
#' @param distribution what density function to assume for the error term.
#' @param loss The type of Loss Function used in optimization.
#' @param ic The information criterion to use in the model selection / combination procedure.
#' @param select_order If TRUE, then the function will select the most appropriate order using a
#' mechanism similar to auto.msarima(), but implemented in auto.adam(). The values list(ar=...,i=...,ma=...)
#' specify the maximum orders to check in this case
#' @param ... Additional arguments passed to `smooth::adam`
#'
#' @keywords internal
#' @export
adam_fit_impl <- function(x, y, period = "auto", p = 0, d = 0, q = 0, P = 0, D = 0, Q = 0,
                          model = "ZXZ", constant = FALSE,
                          regressors =  c("use", "select", "adapt"),
                          outliers = c("ignore", "use", "select"), level = 0.99,
                          occurrence = c("none", "auto", "fixed", "general", "odds-ratio",
                                         "inverse-odds-ratio", "direct"),
                          distribution = c("default", "dnorm", "dlaplace", "ds", "dgnorm",
                                           "dlnorm", "dinvgauss", "dgamma"),
                          loss = c("likelihood", "MSE", "MAE", "HAM", "LASSO", "RIDGE", "MSEh",
                                   "TMSE", "GTMSE", "MSCE"),
                          ic   = c("AICc", "AIC", "BIC", "BICc"),
                          select_order = FALSE,
                           ...) {


    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x

    args <- list(...)

    if (!any(names(args) == "orders")){
        args[["orders"]] <- list(ar = c(p, P), i = c(d, D), ma = c(q, Q), select = select_order)
    }

    if (!any(names(args) == "model")){
        args[["model"]] <- model
    }

    if (!any(names(args) == "constant")){
        args[["constant"]] <- constant
    }

    if (!any(names(args) == "regressors")){
        args[["regressors"]] <- regressors[1]
    }

    if (!any(names(args) == "outliers")){
        args[["outliers"]] <- outliers[1]
    }

    if (!any(names(args) == "level")){
        args[["level"]] <- level
    }

    if (!any(names(args) == "occurrence")){
        args[["occurrence"]] <- occurrence[1]
    }

    if (!any(names(args) == "distribution")){
        args[["distribution"]] <- distribution[1]
    }

    if (!any(names(args) == "loss")){
        args[["loss"]] <- loss[1]
    }

    if (!any(names(args) == "ic")){
        args[["ic"]] <- ic[1]
    }


    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)
    period    <- parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    # # XREGS
    # # Clean names, get xreg recipe, process predictors

    xreg_recipe <- create_xreg_recipe(predictor, prepare = TRUE)
    xreg_tbl    <- juice_xreg_recipe(xreg_recipe, format = "tbl")

    # Combine Xregs and data
    args[["data"]] <- dplyr::bind_cols(
        tibble::tibble(..y = y),
        xreg_tbl
    ) %>%
        as.data.frame()

    fit_call <- parsnip::make_call(fun  = "adam",
                                   ns   = "smooth",
                                   args = args)

    fit_adam <- rlang::eval_tidy(fit_call)


    # RETURN
    new_modeltime_bridge(
        class = "Adam_fit_impl",

        # Models
        models = list(
            model_1 = fit_adam
        ),

        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  as.numeric(fit_adam$data[,1]),
            .fitted      =  as.numeric(fit_adam$fitted),
            .residuals   =  as.numeric(fit_adam$residuals)
        ),

        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            xreg_recipe = xreg_recipe
        ),

        # Description - Convert ADAM model parameters to short description
        desc = "ADAM Model"
    )

}

#' @export
print.Adam_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}


# PREDICT ----

#' @export
predict.Adam_fit_impl <- function(object, new_data, ...) {
    Adam_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for ADAM models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `smooth::adam()`
#'
#' @keywords internal
#' @export
Adam_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    model       <- object$models$model_1
    xreg_recipe <- object$extras$xreg_recipe
    h_horizon   <- nrow(new_data)

    # XREG
    xreg_tbl <- bake_xreg_recipe(xreg_recipe, new_data, format = "tbl")

    # PREDICTIONS
    if (!is.null(xreg_tbl)) {
        preds_forecast <- greybox::forecast(model, h = h_horizon, newdata = xreg_tbl, ...)$mean %>% as.vector()
    } else {
        preds_forecast <- greybox::forecast(model, h = h_horizon, ...)$mean %>% as.vector()
    }

    # Return predictions as numeric vector
    #preds <- tibble::as_tibble(preds_forecast)

    return(preds_forecast)

}





# FIT - AUTO ADAM -----

#' Low-Level ADAM function for translating modeltime to forecast
#'
#' @param x A data.frame of predictors
#' @param y A vector with outcome
#' @param period A seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param p The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.
#' @param d The order of integration for non-seasonal differencing. Often denoted "d" in pdq-notation.
#' @param q The order of the non-seasonal moving average (MA) terms. Often denoted "q" in pdq-notation.
#' @param P The order of the seasonal auto-regressive (SAR) terms. Often denoted "P" in PDQ-notation.
#' @param D The order of integration for seasonal differencing. Often denoted "D" in PDQ-notation.
#' @param Q The order of the seasonal moving average (SMA) terms. Often denoted "Q" in PDQ-notation.
#' @param model The type of ETS model.
#' @param constant 	Logical, determining, whether the constant is needed in the model or not.
#' @param regressors The variable defines what to do with the provided explanatory variables.
#' @param outliers Defines what to do with outliers.
#' @param level What confidence level to use for detection of outliers.
#' @param occurrence The type of model used in probability estimation.
#' @param distribution what density function to assume for the error term.
#' @param loss The type of Loss Function used in optimization.
#' @param ic The information criterion to use in the model selection / combination procedure.
#' @param select_order If TRUE, then the function will select the most appropriate order using
#' a mechanism similar to auto.msarima(), but implemented in auto.adam(). The values list(ar=...,i=...,ma=...)
#' specify the maximum orders to check in this case.
#' @param ... Additional arguments passed to `smooth::auto.adam`
#'
#' @keywords internal
#' @export
auto_adam_fit_impl <- function(
      x, y, period = "auto",
      p = 0, d = 0, q = 0, P = 0, D = 0, Q = 0,
      model = "ZXZ",
      constant = FALSE,
      regressors =  c("use", "select", "adapt"),
      outliers = c("ignore", "use", "select"), level = 0.99,
      occurrence = c("none", "auto", "fixed", "general", "odds-ratio",
                     "inverse-odds-ratio", "direct"),
      distribution = c("default", "dnorm", "dlaplace", "ds", "dgnorm",
                       "dlnorm", "dinvgauss", "dgamma"),
      loss = c("likelihood", "MSE", "MAE", "HAM", "LASSO", "RIDGE", "MSEh",
               "TMSE", "GTMSE", "MSCE"),
      ic   = c("AICc", "AIC", "BIC", "BICc"),
      select_order = FALSE,
      ...) {


    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x

    args <- list(...)

    if (!any(names(args) == "orders")){
        args[["orders"]] <- list(ar = c(p, P), i = c(d, D), ma = c(q, Q), select = select_order)
    }

    if (!any(names(args) == "model")){
        args[["model"]] <- model
    }

    if (!any(names(args) == "constant")){
        args[["constant"]] <- constant
    }

    if (!any(names(args) == "regressors")){
        args[["regressors"]] <- regressors[1]
    }

    if (!any(names(args) == "outliers")){
        args[["outliers"]] <- outliers[1]
    }

    if (!any(names(args) == "level")){
        args[["level"]] <- level
    }

    if (!any(names(args) == "occurrence")){
        args[["occurrence"]] <- occurrence[1]
    }

    if (!any(names(args) == "distribution")){
        args[["distribution"]] <- distribution[1]
    }

    if (!any(names(args) == "loss")){
        args[["loss"]] <- loss[1]
    }

    if (!any(names(args) == "ic")){
        args[["ic"]] <- ic[1]
    }


    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)
    period    <- parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    # # XREGS
    # # Clean names, get xreg recipe, process predictors
    xreg_recipe <- create_xreg_recipe(predictor, prepare = TRUE)
    xreg_tbl    <- juice_xreg_recipe(xreg_recipe, format = "tbl")

    args[["data"]] <- dplyr::bind_cols(
        tibble::tibble(..y = y),
        xreg_tbl
    ) %>%
        as.data.frame()

    fit_call <- parsnip::make_call(fun  = "auto.adam",
                                   ns   = "smooth",
                                   args = args)

    fit_adam <- rlang::eval_tidy(fit_call)

    # RETURN
    new_modeltime_bridge(
        class = "Auto_adam_fit_impl",

        # Models
        models = list(
            model_1 = fit_adam
        ),

        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = tibble::tibble(
            !! idx_col  := idx,
            .actual      =  as.numeric(fit_adam$data[,1]),
            .fitted      =  as.numeric(fit_adam$fitted),
            .residuals   =  as.numeric(fit_adam$residuals)
        ),

        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            xreg_recipe = xreg_recipe
        ),

        # Description - Convert ADAM model parameters to short description
        desc = "AUTO ADAM Model"
    )

}

#' @export
print.Auto_adam_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}


# PREDICT AUTO ADAM----

#' @export
predict.Auto_adam_fit_impl <- function(object, new_data, ...) {
    Auto_adam_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for AUTO ADAM models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `smooth::auto.adam()`
#'
#' @keywords internal
#' @export
Auto_adam_predict_impl <- function(object, new_data, ...) {

    # PREPARE INPUTS
    model       <- object$models$model_1
    xreg_recipe <- object$extras$xreg_recipe
    h_horizon   <- nrow(new_data)

    # XREG
    xreg_tbl <- bake_xreg_recipe(xreg_recipe, new_data, format = "tbl")

    # PREDICTIONS
    if (!is.null(xreg_tbl)) {
        preds_forecast <- greybox::forecast(model, h = h_horizon, newdata = xreg_tbl, ...)$mean %>% as.vector()
    } else {
        preds_forecast <- greybox::forecast(model, h = h_horizon, ...)$mean %>% as.vector()
    }

    # Return predictions as numeric vector
    #preds <- tibble::as_tibble(preds_forecast)

    return(preds_forecast)

}


