# IMPORTS ----
# StanHeaders - Used to prevent issues with Prophet dynload error

#' @import StanHeaders


# ON LOAD ----

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
    # This defines the model database

    # Prophet
    make_prophet_reg()
    make_prophet_boost()

    # ARIMA
    make_arima_reg()
    make_arima_boost()

    # Exponential Smoothing
    make_exp_smoothing()

    # Seasonal Decomposition
    make_seasonal_reg()
}
