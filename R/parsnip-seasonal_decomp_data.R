# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov


make_seasonal_decomp <- function() {

    parsnip::set_new_model("seasonal_decomp")
    parsnip::set_model_mode("seasonal_decomp", "regression")

    #  ----

    # * Model ----
    parsnip::set_model_engine("seasonal_decomp", mode = "regression", eng = "stlm_ets")
    parsnip::set_dependency("seasonal_decomp", "stlm_ets", "forecast")

    # * Args ----
    parsnip::set_model_arg(
        model        = "seasonal_decomp",
        eng          = "stlm_ets",
        parsnip      = "seasonal_period_1",
        original     = "period_1",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "seasonal_decomp",
        eng          = "stlm_ets",
        parsnip      = "seasonal_period_2",
        original     = "period_2",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "seasonal_decomp",
        eng          = "stlm_ets",
        parsnip      = "seasonal_period_3",
        original     = "period_3",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "seasonal_decomp",
        eng          = "stlm_ets",
        parsnip      = "seasonal_window",
        original     = "s.window",
        func         = list(pkg = "modeltime", fun = "seasonal_window"),
        has_submodel = FALSE
    )

    # * Fit ----
    parsnip::set_fit(
        model         = "seasonal_decomp",
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
        model         = "seasonal_decomp",
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

}

# nocov end
