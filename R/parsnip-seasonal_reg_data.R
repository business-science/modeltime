# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_seasonal_reg <- function() {

    parsnip::set_new_model("seasonal_reg")
    parsnip::set_model_mode("seasonal_reg", "regression")

    # TBATS ----

    # * Model ----
    parsnip::set_model_engine("seasonal_reg", mode = "regression", eng = "tbats")
    parsnip::set_dependency("seasonal_reg", "tbats", "forecast")
    parsnip::set_dependency("seasonal_reg", "tbats", "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = "seasonal_reg",
        eng          = "tbats",
        parsnip      = "seasonal_period_1",
        original     = "period_1",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "seasonal_reg",
        eng          = "tbats",
        parsnip      = "seasonal_period_2",
        original     = "period_2",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "seasonal_reg",
        eng          = "tbats",
        parsnip      = "seasonal_period_3",
        original     = "period_3",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = "seasonal_reg",
        eng     = "tbats",
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
        model         = "seasonal_reg",
        eng           = "tbats",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "tbats_fit_impl"),
            defaults  = list(use.parallel = FALSE)
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "seasonal_reg",
        eng           = "tbats",
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


    # STLM ETS ----

    # * Model ----
    parsnip::set_model_engine("seasonal_reg", mode = "regression", eng = "stlm_ets")
    parsnip::set_dependency("seasonal_reg", "stlm_ets", "forecast")
    parsnip::set_dependency("seasonal_reg", "stlm_ets", "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = "seasonal_reg",
        eng          = "stlm_ets",
        parsnip      = "seasonal_period_1",
        original     = "period_1",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "seasonal_reg",
        eng          = "stlm_ets",
        parsnip      = "seasonal_period_2",
        original     = "period_2",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "seasonal_reg",
        eng          = "stlm_ets",
        parsnip      = "seasonal_period_3",
        original     = "period_3",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = "seasonal_reg",
        eng     = "stlm_ets",
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
        model         = "seasonal_reg",
        eng           = "stlm_ets",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "stlm_ets_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "seasonal_reg",
        eng           = "stlm_ets",
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


    # STLM ARIMA ----

    # * Model ----
    parsnip::set_model_engine("seasonal_reg", mode = "regression", eng = "stlm_arima")
    parsnip::set_dependency("seasonal_reg", "stlm_arima", "forecast")
    parsnip::set_dependency("seasonal_reg", "stlm_arima", "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = "seasonal_reg",
        eng          = "stlm_arima",
        parsnip      = "seasonal_period_1",
        original     = "period_1",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "seasonal_reg",
        eng          = "stlm_arima",
        parsnip      = "seasonal_period_2",
        original     = "period_2",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "seasonal_reg",
        eng          = "stlm_arima",
        parsnip      = "seasonal_period_3",
        original     = "period_3",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = "seasonal_reg",
        eng     = "stlm_arima",
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
        model         = "seasonal_reg",
        eng           = "stlm_arima",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "stlm_arima_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "seasonal_reg",
        eng           = "stlm_arima",
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
