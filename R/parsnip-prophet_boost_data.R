# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_prophet_boost <- function() {

    parsnip::set_new_model("prophet_boost")
    parsnip::set_model_mode("prophet_boost", "regression")

    # prophet ----

    # * Model ----
    parsnip::set_model_engine("prophet_boost", mode = "regression", eng = "prophet_xgboost")
    parsnip::set_dependency("prophet_boost", eng = "prophet_xgboost", pkg = "prophet")
    parsnip::set_dependency("prophet_boost", eng = "prophet_xgboost", pkg = "xgboost")
    parsnip::set_dependency("prophet_boost", eng = "prophet_xgboost", pkg = "modeltime")

    # * Args - Prophet ----
    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "growth",
        original     = "growth",
        func         = list(pkg = "modeltime", fun = "growth"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "changepoint_num",
        original     = "n.changepoints",
        func         = list(pkg = "modeltime", fun = "changepoint_num"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "changepoint_range",
        original     = "changepoint.range",
        func         = list(pkg = "modeltime", fun = "changepoint_range"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "seasonality_yearly",
        original     = "yearly.seasonality",
        func         = list(pkg = "modeltime", fun = "seasonality_yearly"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "seasonality_weekly",
        original     = "weekly.seasonality",
        func         = list(pkg = "modeltime", fun = "seasonality_weekly"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "seasonality_daily",
        original     = "daily.seasonality",
        func         = list(pkg = "modeltime", fun = "seasonality_daily"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "season",
        original     = "seasonality.mode",
        func         = list(pkg = "modeltime", fun = "season"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "prior_scale_changepoints",
        original     = "changepoint.prior.scale",
        func         = list(pkg = "modeltime", fun = "prior_scale_changepoints"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "prior_scale_seasonality",
        original     = "seasonality.prior.scale",
        func         = list(pkg = "modeltime", fun = "prior_scale_seasonality"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "prior_scale_holidays",
        original     = "holidays.prior.scale",
        func         = list(pkg = "modeltime", fun = "prior_scale_holidays"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "logistic_cap",
        original     = "logistic_cap",
        func         = list(pkg = "modeltime", fun = "logistic_cap"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "logistic_floor",
        original     = "logistic_floor",
        func         = list(pkg = "modeltime", fun = "logistic_floor"),
        has_submodel = FALSE
    )

    # * Args - Xgboost ----

    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "tree_depth",
        original     = "max_depth",
        func         = list(pkg = "dials", fun = "tree_depth"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "trees",
        original     = "nrounds",
        func         = list(pkg = "dials", fun = "trees"),
        has_submodel = TRUE
    )
    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "learn_rate",
        original     = "eta",
        func         = list(pkg = "dials", fun = "learn_rate"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "mtry",
        original     = "colsample_bynode",
        func         = list(pkg = "dials", fun = "mtry"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "min_n",
        original     = "min_child_weight",
        func         = list(pkg = "dials", fun = "min_n"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "loss_reduction",
        original     = "gamma",
        func         = list(pkg = "dials", fun = "loss_reduction"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "sample_size",
        original     = "subsample",
        func         = list(pkg = "dials", fun = "sample_size"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "prophet_boost",
        eng          = "prophet_xgboost",
        parsnip      = "stop_iter",
        original     = "early_stop",
        func         = list(pkg = "dials", fun = "stop_iter"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = "prophet_boost",
        eng     = "prophet_xgboost",
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
        model         = "prophet_boost",
        eng           = "prophet_xgboost",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "prophet_xgboost_fit_impl"),
            defaults  = list(uncertainty.samples = 0,
                             objective = "reg:squarederror",
                             nthread = 1,
                             verbose = 0)
        )
    )


    # * Predict ----
    parsnip::set_pred(
        model         = "prophet_boost",
        eng           = "prophet_xgboost",
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
