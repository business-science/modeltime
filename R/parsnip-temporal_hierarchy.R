# HIERARCHICAL REGRESSION ----

#' General Interface for Temporal Hierarchical Forecasting (THIEF) Models
#'
#' @description
#' `temporal_hierarchy()` is a way to generate a _specification_ of an Temporal Hierarchical Forecasting model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `thief`. Note this
#'  function requires the `thief` package to be installed.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#'
#' @param seasonal_period A seasonal frequency. Uses "auto" by default.
#'  A character phrase of "auto" or time-based phrase of "2 weeks"
#'  can be used if a date or date-time variable is provided.
#'  See Fit Details below.
#'
#' @param combination_method Combination method of temporal hierarchies, taking one of the following values:
#'
#'  * "struc" - Structural scaling: weights from temporal hierarchy
#'  * "mse" - Variance scaling: weights from in-sample MSE
#'  * "ols" - Unscaled OLS combination weights
#'  * "bu" - Bottom-up combination – i.e., all aggregate forecasts are ignored.
#'  * "shr" - GLS using a shrinkage (to block diagonal) estimate of residuals
#'  * "sam" - GLS using sample covariance matrix of residuals
#'
#' @param use_model Model used for forecasting each aggregation level:
#'
#'  * "ets" - exponential smoothing
#'  * "arima" - arima
#'  * "theta" - theta
#'  * "naive" - random walk forecasts
#'  * "snaive" - seasonal naive forecasts, based on the last year of observed data
#'
#' @references
#' \itemize{
#'   \item{For forecasting with temporal hierarchies see: Athanasopoulos G., Hyndman R.J., Kourentzes N., Petropoulos F. (2017) Forecasting with Temporal Hierarchies. \emph{European Journal of Operational research}, \bold{262}(\bold{1}), 60-74.}
#'   \item{For combination operators see: Kourentzes N., Barrow B.K., Crone S.F. (2014) Neural network ensemble operators for time series forecasting. \emph{Expert Systems with Applications}, \bold{41}(\bold{9}), 4235-4244.}
#' }
#'
#' @details
#'
#' Models can be created using the following _engines_:
#'
#'  - "thief" (default) - Connects to `thief::thief()`
#'
#' @section Engine Details:
#'
#' The standardized parameter names in `modeltime` can be mapped to their original
#' names in each engine:
#'
#' ```{r echo = FALSE, eval = rlang::is_installed("thief")}
#' # parsnip::convert_args("temporal_hierarchy")
#' tibble::tribble(
#'     ~ "modeltime", ~ "thief::thief()",
#'     "combination_method", "comb",
#'     "use_model", "usemodel") %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#' __thief (default engine)__
#'
#' The engine uses `thief::thief()`.
#'
#' Function Parameters:
#' ```{r echo = FALSE, eval = rlang::is_installed("thief")}
#' str(thief::thief)
#' ```
#' Other options and argument can be set using `set_engine()`.
#'
#' Parameter Notes:
#' - `xreg` - This model is not set up to use exogenous regressors. Only univariate
#'  models will be fit.
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
#' __Univariate:__
#'
#' For univariate analysis, you must include a date or date-time feature. Simply use:
#'
#'  - Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.
#'  - XY Interface: `fit_xy(x = data[,"date"], y = data$y)` will ignore xreg's.
#'
#' __Multivariate (xregs, Exogenous Regressors)__
#'
#' This model is not set up for use with exogenous regressors.
#'
#'
#' @seealso `fit.model_spec()`, `set_engine()`
#'
#' @examplesIf rlang::is_installed("thief")
#' library(dplyr)
#' library(parsnip)
#' library(rsample)
#' library(timetk)
#' library(thief)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#' m750
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.8)
#'
#' # ---- HIERARCHICAL ----
#'
#' # Model Spec - The default parameters are all set
#' # to "auto" if none are provided
#' model_spec <- temporal_hierarchy() %>%
#'     set_engine("thief")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date, data = training(splits))
#' model_fit
#'
#'
#'
#'
#' @export
temporal_hierarchy <- function(mode = "regression", seasonal_period = NULL,
                               combination_method = NULL, use_model = NULL) {

    args <- list(
        seasonal_period    = rlang::enquo(seasonal_period),
        combination_method = rlang::enquo(combination_method),
        use_model          = rlang::enquo(use_model)
    )

    parsnip::new_model_spec(
        "temporal_hierarchy",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.temporal_hierarchy <- function(x, ...) {
    cat("Temporal Hierarchical Forecasting Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.temporal_hierarchy <- function(object, parameters = NULL,
                                      seasonal_period = NULL,
                                      combination_method = NULL, use_model = NULL,
                                      fresh = FALSE, ...) {

    eng_args <- parsnip::update_engine_parameters(object$eng_args, fresh, ...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        seasonal_period    = rlang::enquo(seasonal_period),
        combination_method = rlang::enquo(combination_method),
        use_model          = rlang::enquo(use_model)
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
        "temporal_hierarchy",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.temporal_hierarchy <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'thief'` for translation.")
        engine <- "thief"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}


# temporal_hierarchy -----

#' Low-Level Temporaral Hierarchical function for translating modeltime to forecast
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period A seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param comb Combination method of temporal hierarchies
#' @param usemodel Model used for forecasting each aggregation level
#' @param ... Additional arguments passed to `forecast::ets`
#'
#' @keywords internal
#' @export
temporal_hier_fit_impl <- function(x, y,
                                   period = "auto",
                                   comb = c("struc", "mse", "ols", "bu", "shr", "sam"),
                                   usemodel = c("ets", "arima", "theta", "naive", "snaive"),
                                   ...) {

    others <- list(...)

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

    outcome <- stats::ts(y, frequency = period)

    fit_thief <- thief::thief(y = outcome, comb = comb, usemodel = usemodel, ...)

    if (length(idx) != length(as.numeric(fit_thief$fitted))){

        # SUPPRESS WARNING MESSAGE FROM tk_tbl()
        # Warning message:
        #     `type_convert()` only converts columns of type 'character'.
        # - `df` has no columns of type 'character'
        suppressWarnings({
            x         <- fit_thief$x %>% timetk::tk_tbl() %>% tibble::rowid_to_column()
            fitted    <- fit_thief$fitted %>% timetk::tk_tbl()
            residuals <- fit_thief$residuals %>% timetk::tk_tbl()
        })

        val <- x %>%
              dplyr::inner_join(fitted, by = "index") %>%
              dplyr::rename(x = value.x, fitted = value.y) %>%
              dplyr::inner_join(residuals, by = "index") %>%
              dplyr::rename(residuals = value)

        idx <- idx %>%
            timetk::tk_tbl(preserve_index = FALSE) %>%
            tibble::rowid_to_column() %>%
            dplyr::filter(rowid %in% val$rowid)

        data <- tibble::tibble(
            !! idx_col := idx$data,
            .actual    = val$x,
            .fitted    = val$fitted,
            .residuals = val$residuals
        )

    } else {

        data <- tibble::tibble(
            !! idx_col  := idx,
            .actual      =  as.numeric(fit_thief$x),
            .fitted      =  as.numeric(fit_thief$fitted),
            .residuals   =  as.numeric(fit_thief$residuals)
        )

    }

    # RETURN
    new_modeltime_bridge(
        class = "temporal_hier_fit_impl",

        # Models
        models = list(
            model_1 = fit_thief
        ),

        # Data - Date column (matches original), .actual, .fitted, and .residuals columns
        data = data,

        # Preprocessing Recipe (prepped) - Used in predict method
        extras = list(
            outcome  = outcome,
            comb     = comb,
            usemodel = usemodel,
            others   = others
        ),

        # Description
        desc = "Temporal Hierarchical Forecasting Model"
    )

}

#' @export
print.temporal_hier_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}



#' @export
predict.temporal_hier_fit_impl <- function(object, new_data, ...) {
    temporal_hier_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for TEMPORAL HIERARCHICAL models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `stats::predict()`
#'
#' @keywords internal
#' @export
temporal_hier_predict_impl <- function(object, new_data, ...) {
    # PREPARE INPUTS
    model         <- object$models$model_1
    outcome       <- object$extras$outcome
    comb          <- object$extras$comb
    usemodel      <- object$extras$usemodel
    others        <- object$extras$others

    h_horizon      <- nrow(new_data)

    preds <- tryCatch({

        pred_forecast <- forecast::forecast(model, h = h_horizon)

        as.numeric(pred_forecast$mean)

    }, error = function(e) {
        fit <- thief::thief(y = outcome, h = h_horizon, comb = comb, usemodel = usemodel)

        as.numeric(fit$mean)

    })

    return(preds)
}






