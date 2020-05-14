# SETUP ----

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)




# ---- AUTO ARIMA ----

# * NO XREGS ----

# Model Spec
model_spec <- arima_reg(period = 12) %>%
    set_engine("forecast::auto.arima")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# Predictions
predictions_tbl <- model_fit %>%
    modeltime_forecast(new_data = testing(splits))


# TESTS
test_that("arima_reg: Auto Arima (No xregs), Test Model Fit Object", {

    testthat::expect_s3_class(model_fit$fit, "auto_arima_fit_impl")

    # $fit

    testthat::expect_s3_class(model_fit$fit$model, "Arima")

    testthat::expect_s3_class(model_fit$fit$index, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$index), "date")

    testthat::expect_true(is.null(model_fit$fit$xreg_terms))

    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")

})

test_that("arima_reg: Auto Arima (No xregs), Test Predictions", {

    # Structure
    testthat::expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 1500)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 700)

})

# * XREGS ----

# Model Spec
model_spec <- arima_reg(period = 12) %>%
    set_engine("forecast::auto.arima")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date + month(date, label = TRUE), data = training(splits))

# Predictions
predictions_tbl <- model_fit %>%
    modeltime_forecast(new_data = testing(splits))


# TESTS
test_that("arima_reg: Auto Arima (XREGS), Test Model Fit Object", {

    testthat::expect_s3_class(model_fit$fit, "auto_arima_fit_impl")

    # $fit

    testthat::expect_s3_class(model_fit$fit$model, "Arima")

    testthat::expect_s3_class(model_fit$fit$index, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$index), "date")

    testthat::expect_true(!is.null(model_fit$fit$xreg_terms))

    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")

})

test_that("arima_reg: Auto Arima (XREGS), Test Predictions", {

    # Structure
    testthat::expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 1200)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 500)

})





