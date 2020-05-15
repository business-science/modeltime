# FORECAST PLOTS -----
context("TEST MODELTIME PLOTS")

# SETUP ----

# Data
m750   <- m4_monthly %>% filter(id == "M750")
splits <- initial_time_split(m750, prop = 0.8)

# Model Spec
model_spec <- arima_reg(period = 12) %>%
    set_engine("forecast::auto.arima")

# PARSNIP INTERFACE ----

model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# * Forecast ----
forecast_tbl <- model_fit %>%
    modeltime_forecast(h = "3 years", actual_data = training(splits), conf_interval = 0.8)

# VISUALIZATIONS WITH CONF INTERVALS ----

# * ggplot2 visualization ----
g <- forecast_tbl %>%
    mutate_at(vars(.value:.conf_hi), exp) %>%
    plot_modeltime_forecast(.interactive = FALSE)

# * plotly visualization ----
p <- forecast_tbl %>%
    mutate_at(vars(.value:.conf_hi), exp) %>%
    plot_modeltime_forecast(.interactive = TRUE)


test_that("modeltime plot, Test Static ggplot", {

    # Structure
    testthat::expect_s3_class(g, "ggplot")
    testthat::expect_s3_class(g$layers[[1]]$geom, "GeomRibbon")


})

test_that("modeltime plot, Test Interactive plotly", {

    # Structure
    testthat::expect_s3_class(p, "plotly")

})

# PLOTS WITHOUT CONF INTERVALS -----

g <- forecast_tbl %>%
    mutate_at(vars(.value:.conf_hi), exp) %>%
    plot_modeltime_forecast(.interactive = FALSE, .include_conf_interval = FALSE)


p <- forecast_tbl %>%
    mutate_at(vars(.value:.conf_hi), exp) %>%
    plot_modeltime_forecast(.interactive = TRUE, .include_conf_interval = FALSE)


test_that("modeltime plot, Test Static ggplot", {

    # Structure
    testthat::expect_s3_class(g, "ggplot")
    testthat::expect_s3_class(g$layers[[1]]$geom, "GeomLine")


})

test_that("modeltime plot, Test Interactive plotly", {

    # Structure
    testthat::expect_s3_class(p, "plotly")

})
