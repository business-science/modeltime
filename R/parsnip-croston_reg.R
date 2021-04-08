#' @export
croston_reg <- function(mode = "regression",
                      alpha = 0.1) {

  args <- list(
    alpha = rlang::enquo(alpha)
  )

  parsnip::new_model_spec(
    "croston_reg",
    args     = args,
    eng_args = NULL,
    mode     = mode,
    method   = NULL,
    engine   = NULL
  )

}

#' @export
print.croston_reg <- function(x, ...) {
  cat("CROSTON Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}

#' @export
#' @importFrom stats update
update.croston_reg <- function(object,
                             parameters = NULL,
                             alpha = NULL,
                             fresh = FALSE, ...) {

  parsnip::update_dot_check(...)

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }

  args <- list(
    alpha = rlang::enquo(alpha)
  )

  args <- parsnip::update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
  } else {
    null_args <- purrr::map_lgl(args, parsnip::null_value)
    if (any(null_args))
      args <- args[!null_args]
    if (length(args) > 0)
      object$args[names(args)] <- args
  }

  parsnip::new_model_spec(
    "theta_reg",
    args     = object$args,
    eng_args = object$eng_args,
    mode     = object$mode,
    method   = NULL,
    engine   = object$engine
  )
}


#' @export
#' @importFrom parsnip translate
translate.croston_reg <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'croston'` for translation.")
    engine <- "croston"
  }
  x <- parsnip::translate.default(x, engine, ...)

  x
}

#' @export
croston_fit_impl <- function(x, y, alpha = 0.1, ...){

  others <- list(...)

  outcome    <- y # Comes in as a vector
  predictors <- x # Comes in as a data.frame (dates and possible xregs)

  fit_thetaf <- forecast::croston(y = outcome, alpha = alpha, ...)

  # 2. Predictors - Handle Dates
  index_tbl <- modeltime::parse_index_from_data(predictors)
  idx_col   <- names(index_tbl)
  idx       <- timetk::tk_index(index_tbl)

  modeltime::new_modeltime_bridge(

    class  = "croston_fit_impl",

    models = list(model_1 = fit_thetaf),

    data   = tibble::tibble(
      idx_col   := idx,
      .actual    = as.numeric(fit_thetaf$x),
      .fitted    = as.numeric(fit_thetaf$fitted),
      .residuals = as.numeric(fit_thetaf$residuals)
    ),

    extras = list(
      outcome = outcome,
      others  = others
    ), # Can add xreg preprocessors here
    desc   = stringr::str_c("Croston Method: ", fit_thetaf$model$method)
  )
}

#' @export
predict.croston_fit_impl <- function(object, new_data, ...) {
  croston_predict_impl(object, new_data, ...)
}

#' Bridge prediction function for THETA models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `stats::predict()`
#'
#' @export
croston_predict_impl <- function(object, new_data, ...) {
  # PREPARE INPUTS
  model         <- object$models$model_1
  outcome       <- object$extras$outcome
  alpha         <- object$extras$alpha
  others        <- object$extras$others

  safely_forecast <- purrr::safely(forecast::forecast)

  h_horizon     <- nrow(new_data)

  args <- list(...)
  args[['h_horizon']] <- h_horizon
  args[['model']] <- model

  call <- purrr::partial(parsnip::make_call, fun = "forecast", ns = "forecast", args = args)

  safe_call <- purrr::safely(call)

  # PREDICTIONS
  preds_forecast <- safe_call %>% rlang::eval_tidy()

  if (!is.null(preds_forecast$error)){
    others[['h_horizon']]<-h_horizon
    model<- forecast::croston(y = outcome, h = h_horizon, alpha = alpha)
    #others[['model']] <- model
    #preds_forecast <- forecast::forecast(model, h = h_horizon)

    preds <- as.numeric(model$mean)

  } else {
    preds <- as.numeric(preds_forecast$result$mean)
  }

  # Return predictions as numeric vector

  return(preds)
}
