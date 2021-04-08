make_croston_reg <- function() {
  parsnip::set_new_model("croston_reg")
}

make_croston_reg_croston <- function() {

  #### REGRESION
  model  = "croston_reg"
  mode   = "regression"
  engine = "croston"

  parsnip::set_model_engine(model = model, mode = mode, eng = engine)
  parsnip::set_dependency(model = model, eng = engine, pkg = "forecast")
  parsnip::set_dependency(model = model, eng = engine, pkg = "modeltime")

  # * Args ----
  parsnip::set_model_arg(
    model        = model,
    eng          = engine,
    parsnip      = "alpha",
    original     = "alpha",
    func         = list(pkg = "modeltime", fun = "alpha"),
    has_submodel = FALSE
  )

  parsnip::set_encoding(
    model = model,
    eng   = engine,
    mode  = mode,
    options = list(
      predictor_indicators = "none",
      compute_intercept    = FALSE,
      remove_intercept     = FALSE,
      allow_sparse_x       = FALSE
    )
  )

  parsnip::set_fit(
    model  = model,
    eng    = engine,
    mode   = mode,
    value  = list(
      interface = "data.frame",
      protect   = c("x", "y"),
      func      = c(fun = "croston_fit_impl"),
      defaults  = list()
    )
  )

  parsnip::set_pred(
    model  = model,
    eng    = engine,
    mode   = mode,
    type   = "numeric",
    value  = list(
      pre  = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data)
      )
    )
  )

}
