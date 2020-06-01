# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov


make_prophet_reg <- function() {

    parsnip::set_new_model("prophet_reg")
    parsnip::set_model_mode("prophet_reg", "regression")

    # prophet ----

    # * Model ----
    parsnip::set_model_engine("prophet_reg", mode = "regression", eng = "prophet")
    parsnip::set_dependency("prophet_reg", eng = "prophet", pkg = "prophet")

    # * Args ----
    parsnip::set_model_arg(
        model        = "prophet_reg",
        eng          = "prophet",
        parsnip      = "growth",
        original     = "growth",
        func         = list(pkg = "modeltime", fun = "growth"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_reg",
        eng          = "prophet",
        parsnip      = "num_changepoints",
        original     = "n.changepoints",
        func         = list(pkg = "modeltime", fun = "num_changepoints"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_reg",
        eng          = "prophet",
        parsnip      = "season",
        original     = "seasonality.mode",
        func         = list(pkg = "modeltime", fun = "season"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_reg",
        eng          = "prophet",
        parsnip      = "prior_scale_changepoints",
        original     = "changepoint.prior.scale",
        func         = list(pkg = "modeltime", fun = "prior_scale_changepoints"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_reg",
        eng          = "prophet",
        parsnip      = "prior_scale_seasonality",
        original     = "seasonality.prior.scale",
        func         = list(pkg = "modeltime", fun = "prior_scale_seasonality"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "prophet_reg",
        eng          = "prophet",
        parsnip      = "prior_scale_holidays",
        original     = "holidays.prior.scale",
        func         = list(pkg = "modeltime", fun = "prior_scale_holidays"),
        has_submodel = FALSE
    )

    # * Fit ----
    parsnip::set_fit(
        model         = "prophet_reg",
        eng           = "prophet",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "prophet_fit_impl"),
            defaults  = list(
                uncertainty.samples = 0
            )
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "prophet_reg",
        eng           = "prophet",
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
