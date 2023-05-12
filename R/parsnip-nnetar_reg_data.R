# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_nnetar_reg <- function() {

    # nnetar ----
    model  <- "nnetar_reg"
    mode   <- "regression"
    engine <- "nnetar"

    # * Model ----
    parsnip::set_new_model(model)
    parsnip::set_model_mode(model, mode)
    parsnip::set_model_engine(model, mode = mode, eng = engine)
    parsnip::set_dependency(model, engine, "forecast")
    parsnip::set_dependency(model, engine, "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "non_seasonal_ar",
        original     = "p",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "seasonal_ar",
        original     = "P",
        func         = list(pkg = "modeltime", fun = "seasonal_ar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "hidden_units",
        original     = "size",
        func         = list(pkg = "dials", fun = "hidden_units"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "num_networks",
        original     = "repeats",
        func         = list(pkg = "modeltime", fun = "num_networks"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "epochs",
        original     = "maxit",
        func         = list(pkg = "dials", fun = "epochs"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "penalty",
        original     = "decay",
        func         = list(pkg = "dials", fun = "penalty"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = model,
        eng     = engine,
        mode    = mode,
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )

    # * Fit ----
    parsnip::set_fit(
        model         = model,
        eng           = engine,
        mode          = mode,
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "nnetar_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = model,
        eng           = engine,
        mode          = mode,
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
