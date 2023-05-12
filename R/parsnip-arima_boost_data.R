# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_arima_boost <- function() {

    parsnip::set_new_model("arima_boost")
    parsnip::set_model_mode("arima_boost", "regression")

    # auto_arima_xgboost ----

    # * Model ----
    parsnip::set_model_engine("arima_boost", mode = "regression", eng = "auto_arima_xgboost")
    parsnip::set_dependency("arima_boost", "auto_arima_xgboost", "forecast")
    parsnip::set_dependency("arima_boost", "auto_arima_xgboost", "xgboost")
    parsnip::set_dependency("arima_boost", "auto_arima_xgboost", "modeltime")

    # * Args - ARIMA ----
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "non_seasonal_ar",
        original     = "max.p",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "non_seasonal_differences",
        original     = "max.d",
        func         = list(pkg = "modeltime", fun = "non_seasonal_differences"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "non_seasonal_ma",
        original     = "max.q",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ma"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "seasonal_ar",
        original     = "max.P",
        func         = list(pkg = "modeltime", fun = "seasonal_ar"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "seasonal_differences",
        original     = "max.D",
        func         = list(pkg = "modeltime", fun = "seasonal_differences"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "seasonal_ma",
        original     = "max.Q",
        func         = list(pkg = "modeltime", fun = "seasonal_ma"),
        has_submodel = FALSE
    )

    # * Args - Xgboost ----
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "tree_depth",
        original     = "max_depth",
        func         = list(pkg = "dials", fun = "tree_depth"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "trees",
        original     = "nrounds",
        func         = list(pkg = "dials", fun = "trees"),
        has_submodel = TRUE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "learn_rate",
        original     = "eta",
        func         = list(pkg = "dials", fun = "learn_rate"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "mtry",
        original     = "colsample_bynode",
        func         = list(pkg = "dials", fun = "mtry"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "min_n",
        original     = "min_child_weight",
        func         = list(pkg = "dials", fun = "min_n"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "loss_reduction",
        original     = "gamma",
        func         = list(pkg = "dials", fun = "loss_reduction"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "sample_size",
        original     = "subsample",
        func         = list(pkg = "dials", fun = "sample_size"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "auto_arima_xgboost",
        parsnip      = "stop_iter",
        original     = "early_stop",
        func         = list(pkg = "dials", fun = "stop_iter"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = "arima_boost",
        eng     = "auto_arima_xgboost",
        mode    = "regression",
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )

    # * Fit ----
    parsnip::set_fit(
        model         = "arima_boost",
        eng           = "auto_arima_xgboost",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "auto_arima_xgboost_fit_impl"),
            defaults  = list(objective = "reg:squarederror", nthread = 1, verbose = 0)
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "arima_boost",
        eng           = "auto_arima_xgboost",
        mode          = "regression",
        type          = "numeric",
        value         = list(
            pre       = NULL,
            post      = NULL,
            func      = c(fun = "predict"),
            args      =
                list(
                    object   = rlang::expr(object$fit),
                    new_data = rlang::expr(new_data)
                )
        )
    )


    # arima_xgboost ----


    # * Model ----
    parsnip::set_model_engine("arima_boost", mode = "regression", eng = "arima_xgboost")
    parsnip::set_dependency("arima_boost", "arima_xgboost", "forecast")
    parsnip::set_dependency("arima_boost", "arima_xgboost", "xgboost")
    parsnip::set_dependency("arima_boost", "arima_xgboost", "modeltime")

    # * Args - ARIMA ----
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "non_seasonal_ar",
        original     = "p",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "non_seasonal_differences",
        original     = "d",
        func         = list(pkg = "modeltime", fun = "non_seasonal_differences"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "non_seasonal_ma",
        original     = "q",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ma"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "seasonal_ar",
        original     = "P",
        func         = list(pkg = "modeltime", fun = "seasonal_ar"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "seasonal_differences",
        original     = "D",
        func         = list(pkg = "modeltime", fun = "seasonal_differences"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "seasonal_ma",
        original     = "Q",
        func         = list(pkg = "modeltime", fun = "seasonal_ma"),
        has_submodel = FALSE
    )

    # * Args - XGBoost ----
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "tree_depth",
        original     = "max_depth",
        func         = list(pkg = "dials", fun = "tree_depth"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "trees",
        original     = "nrounds",
        func         = list(pkg = "dials", fun = "trees"),
        has_submodel = TRUE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "learn_rate",
        original     = "eta",
        func         = list(pkg = "dials", fun = "learn_rate"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "mtry",
        original     = "colsample_bynode",
        func         = list(pkg = "dials", fun = "mtry"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "min_n",
        original     = "min_child_weight",
        func         = list(pkg = "dials", fun = "min_n"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "loss_reduction",
        original     = "gamma",
        func         = list(pkg = "dials", fun = "loss_reduction"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "sample_size",
        original     = "subsample",
        func         = list(pkg = "dials", fun = "sample_size"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "arima_boost",
        eng          = "arima_xgboost",
        parsnip      = "stop_iter",
        original     = "early_stop",
        func         = list(pkg = "dials", fun = "stop_iter"),
        has_submodel = FALSE
    )


    # * Encoding ----
    parsnip::set_encoding(
        model   = "arima_boost",
        eng     = "arima_xgboost",
        mode    = "regression",
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )

    # * Fit ----
    parsnip::set_fit(
        model         = "arima_boost",
        eng           = "arima_xgboost",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "arima_xgboost_fit_impl"),
            defaults  = list(objective = "reg:squarederror", nthread = 1, verbose = 0)
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "arima_boost",
        eng           = "arima_xgboost",
        mode          = "regression",
        type          = "numeric",
        value         = list(
            pre       = NULL,
            post      = NULL,
            func      = c(fun = "predict"),
            args      =
                list(
                    object   = rlang::expr(object$fit),
                    new_data = rlang::expr(new_data)
                )
        )
    )

}

# nocov end
