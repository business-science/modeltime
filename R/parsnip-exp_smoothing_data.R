# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_exp_smoothing <- function() {

    parsnip::set_new_model("exp_smoothing")
    parsnip::set_model_mode("exp_smoothing", "regression")

    # ETS ----

    # * Model ----
    parsnip::set_model_engine("exp_smoothing", mode = "regression", eng = "ets")
    parsnip::set_dependency("exp_smoothing", "ets", "forecast")
    parsnip::set_dependency("exp_smoothing", "ets", "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "ets",
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "ets",
        parsnip      = "error",
        original     = "error",
        func         = list(pkg = "modeltime", fun = "error"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "ets",
        parsnip      = "trend",
        original     = "trend",
        func         = list(pkg = "modeltime", fun = "trend"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "ets",
        parsnip      = "season",
        original     = "season",
        func         = list(pkg = "modeltime", fun = "season"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "ets",
        parsnip      = "damping",
        original     = "damping",
        func         = list(pkg = "modeltime", fun = "damping"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "ets",
        parsnip      = "smooth_level",
        original     = "alpha",
        func         = list(pkg = "modeltime", fun = "smooth_level"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "ets",
        parsnip      = "smooth_trend",
        original     = "beta",
        func         = list(pkg = "modeltime", fun = "smooth_trend"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "ets",
        parsnip      = "smooth_seasonal",
        original     = "gamma",
        func         = list(pkg = "modeltime", fun = "smooth_seasonal"),
        has_submodel = FALSE
    )



    # * Encoding ----
    parsnip::set_encoding(
        model   = "exp_smoothing",
        eng     = "ets",
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
        model         = "exp_smoothing",
        eng           = "ets",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "ets_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "exp_smoothing",
        eng           = "ets",
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

    # CROSTON ----

    # * Model ----
    parsnip::set_model_engine("exp_smoothing", mode = "regression", eng = "croston")
    parsnip::set_dependency("exp_smoothing", "croston", "forecast")
    parsnip::set_dependency("exp_smoothing", "croston", "modeltime")

    # * Args ----

    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "croston",
        parsnip      = "smooth_level",
        original     = "alpha",
        func         = list(pkg = "modeltime", fun = "smooth_level"),
        has_submodel = FALSE
    )


    # * Encoding ----
    parsnip::set_encoding(
        model   = "exp_smoothing",
        eng     = "croston",
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
        model         = "exp_smoothing",
        eng           = "croston",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "croston_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "exp_smoothing",
        eng           = "croston",
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


    # THETA ----

    # * Model ----
    parsnip::set_model_engine("exp_smoothing", mode = "regression", eng = "theta")
    parsnip::set_dependency("exp_smoothing", "theta", "forecast")
    parsnip::set_dependency("exp_smoothing", "theta", "modeltime")


    # * Encoding ----
    parsnip::set_encoding(
        model   = "exp_smoothing",
        eng     = "theta",
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
        model         = "exp_smoothing",
        eng           = "theta",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "theta_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "exp_smoothing",
        eng           = "theta",
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

    # SMOOTH----

    # * Model ----
    parsnip::set_model_engine("exp_smoothing", mode = "regression", eng = "smooth_es")
    parsnip::set_dependency("exp_smoothing", "smooth_es", "smooth")
    parsnip::set_dependency("exp_smoothing", "smooth_es", "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "smooth_es",
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "smooth_es",
        parsnip      = "error",
        original     = "error",
        func         = list(pkg = "modeltime", fun = "error"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "smooth_es",
        parsnip      = "trend",
        original     = "trend",
        func         = list(pkg = "modeltime", fun = "trend_smooth"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "smooth_es",
        parsnip      = "season",
        original     = "season",
        func         = list(pkg = "modeltime", fun = "season"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "smooth_es",
        parsnip      = "damping",
        original     = "damping",
        func         = list(pkg = "modeltime", fun = "damping_smooth"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "smooth_es",
        parsnip      = "smooth_level",
        original     = "alpha",
        func         = list(pkg = "modeltime", fun = "smooth_level"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "smooth_es",
        parsnip      = "smooth_trend",
        original     = "beta",
        func         = list(pkg = "modeltime", fun = "smooth_trend"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "exp_smoothing",
        eng          = "smooth_es",
        parsnip      = "smooth_seasonal",
        original     = "gamma",
        func         = list(pkg = "modeltime", fun = "smooth_seasonal"),
        has_submodel = FALSE
    )



    # * Encoding ----
    parsnip::set_encoding(
        model   = "exp_smoothing",
        eng     = "smooth_es",
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
        model         = "exp_smoothing",
        eng           = "smooth_es",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "smooth_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "exp_smoothing",
        eng           = "smooth_es",
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
