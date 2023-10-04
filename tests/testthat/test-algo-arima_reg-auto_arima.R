# ---- AUTO ARIMA ----
context("TEST arima_reg: auto.arima")

# SETUP ----

# Data
m750 <- timetk::m4_monthly %>% dplyr::filter(id == "M750")

# Split Data 80/20
splits <- rsample::initial_time_split(m750, prop = 0.8)


# ---- PARSNIP ----

# ** NO XREGS ----

# TESTS
test_that("arima_reg: auto.arima (No xregs), Test Model Fit Object", {

    skip_on_cran()

    # Model Spec
    model_spec <- arima_reg(seasonal_period = 12) %>%
        parsnip::set_engine("auto_arima")

    # Fit Spec
    model_fit <- model_spec %>%
        fit(log(value) ~ date, data = rsample::training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))

    expect_s3_class(model_fit$fit, "auto_arima_fit_impl")

    # $fit

    expect_s3_class(model_fit$fit$models$model_1, "Arima")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_null(model_fit$fit$extras$xreg_recipe)

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")



    # arima_reg: auto.arima (No xregs), Test Predictions

    # Structure
    expect_identical(nrow(rsample::testing(splits)), nrow(predictions_tbl))
    expect_identical(rsample::testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- rsample::testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 1500)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 700)

})

# ** XREGS ----

# TESTS
test_that("arima_reg: auto.arima (XREGS), Test Model Fit Object", {

    skip_on_cran()

    # build models

    # Model Spec
    model_spec <- arima_reg(seasonal_period = 12) %>%
        parsnip::set_engine("auto_arima")

    # Fit Spec
    model_fit <- model_spec %>%
        fit(log(value) ~ date + lubridate::month(date, label = TRUE), data = rsample::training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))

    # Test model fit

    expect_s3_class(model_fit$fit, "auto_arima_fit_impl")

    # $fit

    expect_s3_class(model_fit$fit$models$model_1, "Arima")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_true(!is.null(model_fit$fit$extras$xreg_recipe))

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")



    # arima_reg: auto.arima (XREGS), Test Predictions

    # Structure
    expect_identical(nrow(rsample::testing(splits)), nrow(predictions_tbl))
    expect_identical(rsample::testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- rsample::testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 1200)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 500)

})


# ---- WORKFLOWS ----

# TESTS
test_that("arima_reg: auto.arima (Workflow), Test Model Fit Object", {

    skip_on_cran()

    # Model Spec
    model_spec <- arima_reg(seasonal_period = 12) %>%
        parsnip::set_engine("auto_arima")

    # Recipe spec
    recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
        recipes::step_log(value, skip = FALSE)

    # Workflow
    wflw <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec)

    wflw_fit <- wflw %>%
        fit(rsample::training(splits))

    # Forecast
    predictions_tbl <- wflw_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits), actual_data = rsample::training(splits)) %>%
        dplyr::mutate(dplyr::across(.value, exp))

    # TEST ---

    expect_s3_class(wflw_fit$fit$fit$fit, "auto_arima_fit_impl")

    # $fit

    expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "Arima")

    expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    expect_null(wflw_fit$fit$fit$fit$extras$xreg_recipe)

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    expect_named(mld$outcomes, "value")


    # arima_reg: auto.arima (Workflow), Test Predictions

    full_data <- dplyr::bind_rows(rsample::training(splits), rsample::testing(splits))

    # Structure
    expect_identical(nrow(full_data), nrow(predictions_tbl))
    expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% dplyr::filter(.key == "prediction")
    resid <- rsample::testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 1500)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 700)

})

