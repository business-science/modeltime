# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov


make_naive_reg <- function() {

    parsnip::set_new_model("naive_reg")
    parsnip::set_model_mode("naive_reg", "regression")

    # naive ----

    # * Model ----
    parsnip::set_model_engine("naive_reg", mode = "regression", eng = "naive")
    parsnip::set_dependency("naive_reg", eng = "naive", pkg = "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = "naive_reg",
        eng          = "naive",
        parsnip      = "id",
        original     = "id",
        func         = list(pkg = "modeltime", fun = "id"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "naive_reg",
        eng          = "naive",
        parsnip      = "seasonal_period",
        original     = "seasonal_period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = "naive_reg",
        eng     = "naive",
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
        model         = "naive_reg",
        eng           = "naive",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "naive_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "naive_reg",
        eng           = "naive",
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

    # xnaive ----

    # * Model ----
    parsnip::set_model_engine("naive_reg", mode = "regression", eng = "snaive")
    parsnip::set_dependency("naive_reg", eng = "snaive", pkg = "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = "naive_reg",
        eng          = "snaive",
        parsnip      = "id",
        original     = "id",
        func         = list(pkg = "modeltime", fun = "id"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "naive_reg",
        eng          = "snaive",
        parsnip      = "seasonal_period",
        original     = "seasonal_period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = "naive_reg",
        eng     = "snaive",
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
        model         = "naive_reg",
        eng           = "snaive",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "snaive_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "naive_reg",
        eng           = "snaive",
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
