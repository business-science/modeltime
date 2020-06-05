# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov


# make_seasonal_decomp <- function() {
#
#     parsnip::set_new_model("seasonal_decomp")
#     parsnip::set_model_mode("seasonal_decomp", "regression")
#
#     #  ----
#
#     # * Model ----
#     parsnip::set_model_engine("seasonal_decomp", mode = "regression", eng = "ets")
#     parsnip::set_dependency("seasonal_decomp", "ets", "forecast")
#
#     # * Args ----
#     parsnip::set_model_arg(
#         model        = "seasonal_decomp",
#         eng          = "ets",
#         parsnip      = "error",
#         original     = "error",
#         func         = list(pkg = "modeltime", fun = "error"),
#         has_submodel = FALSE
#     )
#     parsnip::set_model_arg(
#         model        = "seasonal_decomp",
#         eng          = "ets",
#         parsnip      = "trend",
#         original     = "trend",
#         func         = list(pkg = "modeltime", fun = "trend"),
#         has_submodel = FALSE
#     )
#     parsnip::set_model_arg(
#         model        = "seasonal_decomp",
#         eng          = "ets",
#         parsnip      = "season",
#         original     = "season",
#         func         = list(pkg = "modeltime", fun = "season"),
#         has_submodel = FALSE
#     )
#     parsnip::set_model_arg(
#         model        = "seasonal_decomp",
#         eng          = "ets",
#         parsnip      = "damping",
#         original     = "damping",
#         func         = list(pkg = "modeltime", fun = "damping"),
#         has_submodel = FALSE
#     )
#
#
#
#     # * Fit ----
#     parsnip::set_fit(
#         model         = "seasonal_decomp",
#         eng           = "ets",
#         mode          = "regression",
#         value         = list(
#             interface = "data.frame",
#             protect   = c("x", "y"),
#             func      = c(fun = "ets_fit_impl"),
#             defaults  = list()
#         )
#     )
#
#     # * Predict ----
#     parsnip::set_pred(
#         model         = "seasonal_decomp",
#         eng           = "ets",
#         mode          = "regression",
#         type          = "numeric",
#         value         = list(
#             pre       = NULL,
#             post      = NULL,
#             func      = c(fun = "predict"),
#             args      =
#                 list(
#                     object   = rlang::expr(object$fit),
#                     new_data = rlang::expr(new_data)
#                 )
#         )
#     )
#
# }

# nocov end
