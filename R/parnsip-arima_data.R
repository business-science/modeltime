# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov


make_arima_reg <- function() {

    parsnip::set_new_model("arima_reg")
    parsnip::set_model_mode("arima_reg", "regression")

    # Arima ----

    # * Model ----
    parsnip::set_model_engine("arima_reg", mode = "regression", eng = "Arima")
    parsnip::set_dependency("arima_reg", "Arima", "forecast")

    # * Args ----
    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "Arima",
        parsnip      = "period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "Arima",
        parsnip      = "non_seasonal_ar",
        original     = "p",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "Arima",
        parsnip      = "non_seasonal_differences",
        original     = "d",
        func         = list(pkg = "modeltime", fun = "non_seasonal_differences"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "Arima",
        parsnip      = "non_seasonal_ma",
        original     = "q",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ma"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "Arima",
        parsnip      = "seasonal_ar",
        original     = "P",
        func         = list(pkg = "modeltime", fun = "seasonal_ar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "Arima",
        parsnip      = "seasonal_differences",
        original     = "D",
        func         = list(pkg = "modeltime", fun = "seasonal_differences"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "Arima",
        parsnip      = "seasonal_ma",
        original     = "Q",
        func         = list(pkg = "modeltime", fun = "seasonal_ma"),
        has_submodel = FALSE
    )

    # * Fit ----
    parsnip::set_fit(
        model         = "arima_reg",
        eng           = "Arima",
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
        eng           = "Arima",
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

    # auto.arima ----

    # * Model ----
    parsnip::set_model_engine("arima_reg", mode = "regression", eng = "auto.arima")
    parsnip::set_dependency("arima_reg", "auto.arima", "forecast")

    # * Args ----
    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto.arima",
        parsnip      = "period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto.arima",
        parsnip      = "non_seasonal_ar",
        original     = "max.p",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto.arima",
        parsnip      = "non_seasonal_differences",
        original     = "max.d",
        func         = list(pkg = "modeltime", fun = "non_seasonal_differences"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto.arima",
        parsnip      = "non_seasonal_ma",
        original     = "max.q",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ma"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto.arima",
        parsnip      = "seasonal_ar",
        original     = "max.P",
        func         = list(pkg = "modeltime", fun = "seasonal_ar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto.arima",
        parsnip      = "seasonal_differences",
        original     = "max.D",
        func         = list(pkg = "modeltime", fun = "seasonal_differences"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "auto.arima",
        parsnip      = "seasonal_ma",
        original     = "max.Q",
        func         = list(pkg = "modeltime", fun = "seasonal_ma"),
        has_submodel = FALSE
    )

    # * Fit ----
    parsnip::set_fit(
        model         = "arima_reg",
        eng           = "auto.arima",
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
        eng           = "auto.arima",
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
