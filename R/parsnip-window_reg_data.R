# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov


make_window_reg <- function() {

    parsnip::set_new_model("window_reg")
    parsnip::set_model_mode("window_reg", "regression")

    # window_function ----

    # * Model ----
    parsnip::set_model_engine("window_reg", mode = "regression", eng = "window_function")
    parsnip::set_dependency("window_reg", eng = "window_function", pkg = "modeltime")

    # * Args ----
    parsnip::set_model_arg(
        model        = "window_reg",
        eng          = "window_function",
        parsnip      = "id",
        original     = "id",
        func         = list(pkg = "modeltime", fun = "id"),
        has_submodel = FALSE
    )

    # parsnip::set_model_arg(
    #     model        = "window_reg",
    #     eng          = "window_function",
    #     parsnip      = "window_function",
    #     original     = "window_function",
    #     func         = list(pkg = "modeltime", fun = "window_function"),
    #     has_submodel = FALSE
    # )

    parsnip::set_model_arg(
        model        = "window_reg",
        eng          = "window_function",
        parsnip      = "window_size",
        original     = "window_size",
        func         = list(pkg = "dials", fun = "window_size"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = "window_reg",
        eng     = "window_function",
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
        model         = "window_reg",
        eng           = "window_function",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "window_function_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = "window_reg",
        eng           = "window_function",
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

    # # window_lm ----
    #
    # # * Model ----
    # parsnip::set_model_engine("window_reg", mode = "regression", eng = "window_lm")
    # parsnip::set_dependency("window_reg", eng = "window_lm", pkg = "modeltime")
    #
    # # * Args ----
    # parsnip::set_model_arg(
    #     model        = "window_reg",
    #     eng          = "window_lm",
    #     parsnip      = "id",
    #     original     = "id",
    #     func         = list(pkg = "modeltime", fun = "id"),
    #     has_submodel = FALSE
    # )
    #
    # # parsnip::set_model_arg(
    # #     model        = "window_reg",
    # #     eng          = window_lm,
    # #     parsnip      = "window_function",
    # #     original     = "window_function",
    # #     func         = list(pkg = "modeltime", fun = "window_function"),
    # #     has_submodel = FALSE
    # # )
    #
    # parsnip::set_model_arg(
    #     model        = "window_reg",
    #     eng          = "window_lm",
    #     parsnip      = "window_size",
    #     original     = "window_size",
    #     func         = list(pkg = "dials", fun = "window_size"),
    #     has_submodel = FALSE
    # )
    #
    # # * Encoding ----
    # parsnip::set_encoding(
    #     model   = "window_reg",
    #     eng     = "window_lm",
    #     mode    = "regression",
    #     options = list(
    #         predictor_indicators = "none",
    #         compute_intercept    = FALSE,
    #         remove_intercept     = FALSE,
    #         allow_sparse_x       = FALSE
    #     )
    # )
    #
    # # * Fit ----
    # parsnip::set_fit(
    #     model         = "window_reg",
    #     eng           = "window_lm",
    #     mode          = "regression",
    #     value         = list(
    #         interface = "data.frame",
    #         protect   = c("x", "y"),
    #         func      = c(fun = "window_lm_fit_impl"),
    #         defaults  = list()
    #     )
    # )
    #
    # # * Predict ----
    # parsnip::set_pred(
    #     model         = "window_reg",
    #     eng           = "window_lm",
    #     mode          = "regression",
    #     type          = "numeric",
    #     value         = list(
    #         pre       = NULL,
    #         post      = NULL,
    #         func      = c(fun = "predict"),
    #         args      =
    #             list(
    #                 object   = rlang::expr(object$fit),
    #                 new_data = rlang::expr(new_data)
    #             )
    #     )
    # )

}

# nocov end
