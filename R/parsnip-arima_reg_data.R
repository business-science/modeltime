# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_arima_reg <- function() {

    parsnip::set_new_model("arima_reg")
    parsnip::set_model_mode("arima_reg", "regression")

    # arima ----

    # * Model ----
    parsnip::set_model_engine("arima_reg", mode = "regression", eng = "arima")
    parsnip::set_dependency("arima_reg", "arima", "forecast")
    parsnip::set_dependency("arima_reg", "arima", "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "arima",
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "arima",
        parsnip      = "non_seasonal_ar",
        original     = "p",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "arima",
        parsnip      = "non_seasonal_differences",
        original     = "d",
        func         = list(pkg = "modeltime", fun = "non_seasonal_differences"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "arima",
        parsnip      = "non_seasonal_ma",
        original     = "q",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ma"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "arima",
        parsnip      = "seasonal_ar",
        original     = "P",
        func         = list(pkg = "modeltime", fun = "seasonal_ar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "arima",
        parsnip      = "seasonal_differences",
        original     = "D",
        func         = list(pkg = "modeltime", fun = "seasonal_differences"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "arima",
        parsnip      = "seasonal_ma",
        original     = "Q",
        func         = list(pkg = "modeltime", fun = "seasonal_ma"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = "arima_reg",
        eng     = "arima",
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
        model         = "arima_reg",
        eng           = "arima",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "Arima_fit_impl"),
            defaults  = list(method = "ML")
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "arima_reg",
        eng           = "arima",
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

    # auto_arima ----

    # * Model ----
    parsnip::set_model_engine("arima_reg", mode = "regression", eng = "auto_arima")
    parsnip::set_dependency("arima_reg", "auto_arima", "forecast")
    parsnip::set_dependency("arima_reg", "auto_arima", "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto_arima",
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto_arima",
        parsnip      = "non_seasonal_ar",
        original     = "max.p",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto_arima",
        parsnip      = "non_seasonal_differences",
        original     = "max.d",
        func         = list(pkg = "modeltime", fun = "non_seasonal_differences"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto_arima",
        parsnip      = "non_seasonal_ma",
        original     = "max.q",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ma"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto_arima",
        parsnip      = "seasonal_ar",
        original     = "max.P",
        func         = list(pkg = "modeltime", fun = "seasonal_ar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto_arima",
        parsnip      = "seasonal_differences",
        original     = "max.D",
        func         = list(pkg = "modeltime", fun = "seasonal_differences"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto_arima",
        parsnip      = "seasonal_ma",
        original     = "max.Q",
        func         = list(pkg = "modeltime", fun = "seasonal_ma"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = "arima_reg",
        eng     = "auto_arima",
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
        model         = "arima_reg",
        eng           = "auto_arima",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "auto_arima_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "arima_reg",
        eng           = "auto_arima",
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
