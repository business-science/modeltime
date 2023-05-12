# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_temporal_hierarchy <- function() {

    parsnip::set_new_model("temporal_hierarchy")
    parsnip::set_model_mode("temporal_hierarchy", "regression")

    # temporal_hierarchy ----

    # * Model ----
    parsnip::set_model_engine("temporal_hierarchy", mode = "regression", eng = "thief")
    parsnip::set_dependency("temporal_hierarchy", "thief", "thief")
    parsnip::set_dependency("temporal_hierarchy", "thief", "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = "temporal_hierarchy",
        eng          = "thief",
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "temporal_hierarchy",
        eng          = "thief",
        parsnip      = "combination_method",
        original     = "comb",
        func         = list(pkg = "modeltime", fun = "combination_method"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "temporal_hierarchy",
        eng          = "thief",
        parsnip      = "use_model",
        original     = "usemodel",
        func         = list(pkg = "modeltime", fun = "use_model"),
        has_submodel = FALSE
    )


    # * Encoding ----
    parsnip::set_encoding(
        model   = "temporal_hierarchy",
        eng     = "thief",
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
        model         = "temporal_hierarchy",
        eng           = "thief",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "temporal_hier_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "temporal_hierarchy",
        eng           = "thief",
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
