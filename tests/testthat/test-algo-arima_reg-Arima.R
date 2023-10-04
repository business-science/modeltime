# ---- STANDARD ARIMA ----
context("TEST arima_reg: Arima")


# SETUP ----

# Data
m750 <- timetk::m4_monthly %>% dplyr::filter(id == "M750")

# Split Data 80/20
splits <- rsample::initial_time_split(m750, prop = 0.8)

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
    parsnip::set_engine("arima")


# PARSNIP ----

# * NO XREGS ----


# TESTS
test_that("arima_reg: Arima, (No xregs), Test Model Fit Object", {


    skip_on_cran()

    #


    # Fit Spec
    model_fit <- model_spec %>%
        fit(log(value) ~ date, data = rsample::training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))

    expect_s3_class(model_fit$fit, "Arima_fit_impl")

    # $fit

    expect_s3_class(model_fit$fit$models$model_1, "Arima")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_null(model_fit$fit$extras$xreg_recipe)

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")


    # arima_reg: Arima, (No xregs), Test Predictions

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

# * XREGS ----

# TESTS
test_that("arima_reg: Arima, (XREGS), Test Model Fit Object", {

    skip_on_cran()

    #

    # Fit Spec
    model_fit <- model_spec %>%
        fit(log(value) ~ date + lubridate::month(date, label = TRUE), data = rsample::training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))


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
        parsnip::set_engine("arima")

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


    expect_s3_class(wflw_fit$fit$fit$fit, "Arima_fit_impl")

    # $fit

    expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "Arima")

    expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    expect_null(wflw_fit$fit$fit$fit$extras$xreg_recipe)

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    expect_equal(names(mld$outcomes), "value")


    # arima_reg: Arima (workflow), Test Predictions

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




