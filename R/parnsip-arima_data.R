# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov


make_arima_reg <- function() {

    parsnip::set_new_model("arima_reg")
    parsnip::set_model_mode("arima_reg", "regression")

    # FORECAST ----

    # Model ----
    parsnip::set_model_engine("arima_reg", mode = "regression", eng = "forecast")

    # Args ----
    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "forecast",
        parsnip      = "period",
        original     = "period",
        func         = list(pkg = "foo", fun = "bar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "forecast",
        parsnip      = "p",
        original     = "p",
        func         = list(pkg = "foo", fun = "bar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "forecast",
        parsnip      = "d",
        original     = "d",
        func         = list(pkg = "foo", fun = "bar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "forecast",
        parsnip      = "q",
        original     = "q",
        func         = list(pkg = "foo", fun = "bar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "forecast",
        parsnip      = "P",
        original     = "P",
        func         = list(pkg = "foo", fun = "bar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "forecast",
        parsnip      = "D",
        original     = "D",
        func         = list(pkg = "foo", fun = "bar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "arima_reg",
        eng          = "forecast",
        parsnip      = "Q",
        original     = "Q",
        func         = list(pkg = "foo", fun = "bar"),
        has_submodel = FALSE
    )

    # Fit ----
    parsnip::set_fit(
        model         = "arima_reg",
        eng           = "forecast",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "Arima_fit_impl"),
            defaults  = list()
        )
    )

    # Predict ----
    parsnip::set_pred(
        model         = "arima_reg",
        eng           = "forecast",
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
