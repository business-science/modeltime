#' Fit a Model Specification to a Dataset
#'
#' `fit()` and `fit_xy()` take a model specification, translate the required
#'  code by substituting arguments, and execute the model fit
#'  routine.
#'
#' @inheritParams parsnip::fit.model_spec
#'
#' @details
#'
#' `fit()` and `fit_xy()` substitute the current arguments in the model
#'  specification into the computational engine's code, checks them
#'  for validity, then fits the model using the data and the
#'  engine-specific code. Different model functions have different
#'  interfaces (e.g. formula or `x`/`y`) and these functions translate
#'  between the interface used when `fit()` or `fit_xy()` were invoked and the one
#'  required by the underlying model.
#'
#' When possible, these functions attempt to avoid making copies of the
#'  data. For example, if the underlying model uses a formula and
#'  `fit()` is invoked, the original data are references
#'  when the model is fit. However, if the underlying model uses
#'  something else, such as `x`/`y`, the formula is evaluated and
#'  the data are converted to the required format. In this case, any
#'  calls in the resulting model objects reference the temporary
#'  objects used to fit the model.
#'
#' If the model engine has not been set, the model's default engine will be used
#'  (as discussed on each model page). If the `verbosity` option of
#'  [control_parsnip()] is greater than zero, a warning will be produced.
#'
#' @examples
#' # TODO
#'
#'
#' @return A `model_fit` object that contains several elements:
#' \itemize{
#'   \item \code{lvl}: If the outcome is a factor, this contains
#'    the factor levels at the time of model fitting.
#'   \item \code{spec}: The model specification object
#'    (\code{object} in the call to \code{fit})
#'   \item \code{fit}: when the model is executed without error,
#'    this is the model object. Otherwise, it is a \code{try-error}
#'    object with the error message.
#'   \item \code{preproc}: any objects needed to convert between
#'    a formula and non-formula interface (such as the \code{terms}
#'    object)
#' }
#'  The return value will also have a class related to the fitted model (e.g.
#'  `"_glm"`) before the base class of `"model_fit"`.
#'
#' @name fit.arima_reg
#' @export
NULL


#' @export
#' @rdname fit.arima_reg
#' @importFrom parsnip control_parsnip
fit.arima_reg <- function(object, formula, data, control = control_parsnip(), ...) {

    # Needed to preserve date and date time attributes
    # - Note this approach will not expand factors into dummy variables.
    parsnip::fit.model_spec(object, formula, data, control = control_parsnip(), ..., indicators = FALSE)

}

#' @export
#' @rdname fit.arima_reg
#' @importFrom parsnip control_parsnip
fit_xy.arima_reg <- function(object, x, y, control = control_parsnip(), ...) {

    # Needed to preserve date and date time attributes
    # - Note this approach will not expand factors into dummy variables.
    parsnip::fit_xy.model_spec(object, x, y, control = control_parsnip(), ..., indicators = FALSE)

}
