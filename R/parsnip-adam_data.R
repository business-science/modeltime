# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_adam_reg <- function(){

    model  <- "adam_reg"
    engine <- "adam"

    parsnip::set_new_model(model)
    parsnip::set_model_mode(model, "regression")

    # ADAM ----

    # * Model ----
    parsnip::set_model_engine(model, mode = "regression", eng = engine)
    parsnip::set_dependency(model, engine, "smooth")
    parsnip::set_dependency(model, engine, "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "ets_model",
        original     = "model",
        func         = list(pkg = "modeltime", fun = "ets_model"),
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
        parsnip      = "non_seasonal_differences",
        original     = "d",
        func         = list(pkg = "modeltime", fun = "non_seasonal_differences"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "non_seasonal_ma",
        original     = "q",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ma"),
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
        parsnip      = "seasonal_differences",
        original     = "D",
        func         = list(pkg = "modeltime", fun = "seasonal_differences"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "seasonal_ma",
        original     = "Q",
        func         = list(pkg = "modeltime", fun = "seasonal_ma"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "use_constant",
        original     = "constant",
        func         = list(pkg = "modeltime", fun = "use_constant"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "regressors_treatment",
        original     = "regressors",
        func         = list(pkg = "modeltime", fun = "regressors_treatment"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "outliers_treatment",
        original     = "outliers",
        func         = list(pkg = "modeltime", fun = "outliers_treatment"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "outliers_ci",
        original     = "level",
        func         = list(pkg = "modeltime", fun = "outliers_ci"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "probability_model",
        original     = "occurrence",
        func         = list(pkg = "modeltime", fun = "probability_model"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "distribution",
        original     = "distribution",
        func         = list(pkg = "modeltime", fun = "distribution"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "loss",
        original     = "loss",
        func         = list(pkg = "modeltime", fun = "loss"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine,
        parsnip      = "information_criteria",
        original     = "ic",
        func         = list(pkg = "modeltime", fun = "information_criteria"),
        has_submodel = FALSE
    )

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
        parsnip      = "select_order",
        original     = "select_order",
        func         = list(pkg = "modeltime", fun = "select_order"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = model,
        eng     = engine,
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
        model         = model,
        eng           = engine,
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "adam_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = model,
        eng           = engine,
        mode          = "regression",
        type          = "numeric",
        value         = list(
            pre       = NULL,
            post      = NULL, #function(results, object) { res <- tibble::as_tibble(results) %>% purrr::set_names(".pred")},
            func      = c(fun = "predict"),
            args      =
                list(
                    object   = rlang::expr(object$fit),
                    new_data = rlang::expr(new_data)
                )
        )
    )



    # AUTO ADAM ----

    engine_auto <- "auto_adam"

    # * Model ----
    parsnip::set_model_engine(model, mode = "regression", eng = engine_auto)
    parsnip::set_dependency(model, engine_auto, "smooth")
    parsnip::set_dependency(model, engine_auto, "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "ets_model",
        original     = "model",
        func         = list(pkg = "modeltime", fun = "ets_model"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "non_seasonal_ar",
        original     = "p",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "non_seasonal_differences",
        original     = "d",
        func         = list(pkg = "modeltime", fun = "non_seasonal_differences"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "non_seasonal_ma",
        original     = "q",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ma"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "seasonal_ar",
        original     = "P",
        func         = list(pkg = "modeltime", fun = "seasonal_ar"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "seasonal_differences",
        original     = "D",
        func         = list(pkg = "modeltime", fun = "seasonal_differences"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "seasonal_ma",
        original     = "Q",
        func         = list(pkg = "modeltime", fun = "seasonal_ma"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "use_constant",
        original     = "constant",
        func         = list(pkg = "modeltime", fun = "use_constant"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "regressors_treatment",
        original     = "regressors",
        func         = list(pkg = "modeltime", fun = "regressors_treatment"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "outliers_treatment",
        original     = "outliers",
        func         = list(pkg = "modeltime", fun = "outliers_treatment"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "outliers_ci",
        original     = "level",
        func         = list(pkg = "modeltime", fun = "outliers_ci"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "probability_model",
        original     = "occurrence",
        func         = list(pkg = "modeltime", fun = "probability_model"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "distribution",
        original     = "distribution",
        func         = list(pkg = "modeltime", fun = "distribution"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "loss",
        original     = "loss",
        func         = list(pkg = "modeltime", fun = "loss"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "information_criteria",
        original     = "ic",
        func         = list(pkg = "modeltime", fun = "information_criteria"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = engine_auto,
        parsnip      = "select_order",
        original     = "select_order",
        func         = list(pkg = "modeltime", fun = "select_order"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = model,
        eng     = engine_auto,
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
        model         = model,
        eng           = engine_auto,
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "auto_adam_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = model,
        eng           = engine_auto,
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

