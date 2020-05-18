# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov


make_boost_time <- function() {

    parsnip::set_new_model("boost_time")
    parsnip::set_model_mode("boost_time", "regression")

    # auto.arima+xgboost ----

    # * Model ----
    parsnip::set_model_engine("boost_time", mode = "regression", eng = "auto.arima+xgboost")
    parsnip::set_dependency("boost_time", "auto.arima+xgboost", "forecast")
    parsnip::set_dependency("boost_time", "auto.arima+xgboost", "xgboost")

    # * Args ----
    parsnip::set_model_arg(
        model        = "boost_time",
        eng          = "auto.arima+xgboost",
        parsnip      = "period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "boost_time",
        eng          = "auto.arima+xgboost",
        parsnip      = "tree_depth",
        original     = "max_depth",
        func         = list(pkg = "dials", fun = "tree_depth"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_time",
        eng          = "auto.arima+xgboost",
        parsnip      = "trees",
        original     = "nrounds",
        func         = list(pkg = "dials", fun = "trees"),
        has_submodel = TRUE
    )
    parsnip::set_model_arg(
        model        = "boost_time",
        eng          = "auto.arima+xgboost",
        parsnip      = "learn_rate",
        original     = "eta",
        func         = list(pkg = "dials", fun = "learn_rate"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_time",
        eng          = "auto.arima+xgboost",
        parsnip      = "mtry",
        original     = "colsample_bytree",
        func         = list(pkg = "dials", fun = "mtry"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_time",
        eng          = "auto.arima+xgboost",
        parsnip      = "min_n",
        original     = "min_child_weight",
        func         = list(pkg = "dials", fun = "min_n"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_time",
        eng          = "auto.arima+xgboost",
        parsnip      = "loss_reduction",
        original     = "gamma",
        func         = list(pkg = "dials", fun = "loss_reduction"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_time",
        eng          = "auto.arima+xgboost",
        parsnip      = "sample_size",
        original     = "subsample",
        func         = list(pkg = "dials", fun = "sample_size"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_time",
        eng          = "auto.arima+xgboost",
        parsnip      = "stop_iter",
        original     = "early_stop",
        func         = list(pkg = "dials", fun = "stop_iter"),
        has_submodel = FALSE
    )


    # * Fit ----
    parsnip::set_fit(
        model         = "boost_time",
        eng           = "auto.arima+xgboost",
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
        model         = "boost_time",
        eng           = "auto.arima+xgboost",
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
