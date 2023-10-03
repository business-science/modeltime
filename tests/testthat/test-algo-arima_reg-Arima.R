# ---- STANDARD ARIMA ----
context("TEST arima_reg: Arima")


# SETUP ----

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)

# Model Spec
model_spec <- arima_reg(
    seasonal_period          = 12,
    non_seasonal_ar          = 3,
    non_seasonal_differences = 1,
    non_seasonal_ma          = 3,
    seasonal_ar              = 1,
    seasonal_differences     = 0,
    seasonal_ma              = 1
) %>%
    set_engine("arima")


# PARSNIP ----

# * NO XREGS ----


# TESTS
test_that("arima_reg: Arima, (No xregs), Test Model Fit Object", {


    skip_on_cran()

    #


    # Fit Spec
    model_fit <- model_spec %>%
        fit(log(value) ~ date, data = training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(testing(splits)) %>%
        modeltime_forecast(new_data = testing(splits))

    expect_s3_class(model_fit$fit, "Arima_fit_impl")

    # $fit

    expect_s3_class(model_fit$fit$models$model_1, "Arima")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_true(is.null(model_fit$fit$extras$xreg_recipe))

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")


    # arima_reg: Arima, (No xregs), Test Predictions

    # Structure
    expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    expect_identical(testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 1500)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 700)

})

# * XREGS ----

# TESTS
test_that("arima_reg: Arima, (XREGS), Test Model Fit Object", {

    skip_on_cran()

    #

    # Fit Spec
    model_fit <- model_spec %>%
        fit(log(value) ~ date + month(date, label = TRUE), data = training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(testing(splits)) %>%
        modeltime_forecast(new_data = testing(splits))


    expect_s3_class(model_fit$fit, "Arima_fit_impl")

    # $fit

    expect_s3_class(model_fit$fit$models$model_1, "Arima")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_true(!is.null(model_fit$fit$extras$xreg_recipe))

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")


    # arima_reg: Arima (XREGS), Test Predictions

    # Structure
    expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    expect_identical(testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 1200)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 500)

})


# ---- WORKFLOWS ----

# TESTS
test_that("arima_reg: Arima (workflow), Test Model Fit Object", {


    skip_on_cran()


    # Model Spec
    model_spec <- arima_reg(
        seasonal_period          = 12,
        non_seasonal_ar          = 3,
        non_seasonal_differences = 1,
        non_seasonal_ma          = 3,
        seasonal_ar              = 1,
        seasonal_differences     = 0,
        seasonal_ma              = 1
    ) %>%
        set_engine("arima")

    # Recipe spec
    recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
        step_log(value, skip = FALSE)

    # Workflow
    wflw <- workflow() %>%
        add_recipe(recipe_spec) %>%
        add_model(model_spec)

    wflw_fit <- wflw %>%
        fit(training(splits))

    # Forecast
    predictions_tbl <- wflw_fit %>%
        modeltime_calibrate(testing(splits)) %>%
        modeltime_forecast(new_data = testing(splits), actual_data = training(splits)) %>%
        mutate_at(vars(.value), exp)


    expect_s3_class(wflw_fit$fit$fit$fit, "Arima_fit_impl")

    # $fit

    expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "Arima")

    expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    expect_true(is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    expect_equal(names(mld$outcomes), "value")


    # arima_reg: Arima (workflow), Test Predictions

    full_data <- bind_rows(training(splits), testing(splits))

    # Structure
    expect_identical(nrow(full_data), nrow(predictions_tbl))
    expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% filter(.key == "prediction")
    resid <- testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 1500)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 700)

})




