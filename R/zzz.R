
# ON LOAD ----

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
    # This defines the model database
    make_arima_reg()
    make_arima_boost()
    make_exp_smoothing()
    make_prophet_reg()
    make_seasonal_decomp()
}
