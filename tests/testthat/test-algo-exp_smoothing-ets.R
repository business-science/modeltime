# ---- ETS, CROSTON ----
context("TEST exp_smoothing()")


# SETUP ----

# Data
m750 <- timetk::m4_monthly %>% dplyr::filter(id == "M750")

# Split Data 80/20
splits <- rsample::initial_time_split(m750, prop = 0.9)

# Model Spec
model_spec <- exp_smoothing() %>%
    parsnip::set_engine("ets")


# ETS PARSNIP ----

# * NO XREGS ----

# TESTS
test_that("exp_smoothing: ets, Test Model Fit Object", {

    skip_on_cran()

    #

    # Fit Spec
    model_fit <- model_spec %>%
        fit(log(value) ~ date, data = rsample::training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))

    expect_s3_class(model_fit$fit, "ets_fit_impl")

    # $fit

    expect_s3_class(model_fit$fit$models$model_1, "ets")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_null(model_fit$fit$extras$xreg_recipe)

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")


    # exp_smoothing: ets, Test Predictions

    # Structure
    expect_identical(nrow(rsample::testing(splits)), nrow(predictions_tbl))
    expect_identical(rsample::testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- rsample::testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 1500)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 800)

})




# ---- ETS WORKFLOWS ----


# TESTS
test_that("exp_smoothing: Arima (workflow), Test Model Fit Object", {

    skip_on_cran()

    #

    # Model Spec
    model_spec <- exp_smoothing(
        seasonal_period = 12,
        error = "multiplicative", trend = "additive", season = "multiplicative"
        ,
        smooth_level = 0.2, smooth_trend = 0.1, smooth_seasonal = 0.1
    ) %>%
        parsnip::set_engine("ets")

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
        modeltime_forecast(new_data = rsample::testing(splits),
                           actual_data = rsample::training(splits)) %>%
        dplyr::mutate(dplyr::across(.value, exp))



    expect_s3_class(wflw_fit$fit$fit$fit, "ets_fit_impl")

    # $fit

    expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "ets")

    expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    expect_null(wflw_fit$fit$fit$fit$extras$xreg_recipe)

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    expect_equal(names(mld$outcomes), "value")


    # exp_smoothing: ets (workflow), Test Predictions

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

# ---- CROSTON WORKFLOWS ----


# TESTS
test_that("exp_smoothing: CROSTON", {

    skip_on_cran()

    #


    # Model Spec
    model_spec <- exp_smoothing(
        smooth_level = 0.2
    ) %>%
        parsnip::set_engine("croston")

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
        modeltime_forecast(new_data = rsample::testing(splits),
                           actual_data = rsample::training(splits)) %>%
        dplyr::mutate(dplyr::across(.value, exp))


    expect_s3_class(wflw_fit$fit$fit$fit, "croston_fit_impl")

    # $fit

    expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "forecast")

    expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    expect_null(wflw_fit$fit$fit$fit$extras$xreg_recipe)

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    expect_equal(names(mld$outcomes), "value")



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
    expect_lte(mean(abs(resid)), 1000)

})


# ---- THETA WORKFLOWS ----


# TESTS
test_that("exp_smoothing: Theta", {

    skip_on_cran()

    #


    # Model Spec
    model_spec <- exp_smoothing() %>%
        parsnip::set_engine("theta")

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
        modeltime_forecast(new_data = rsample::testing(splits),
                           actual_data = rsample::training(splits)) %>%
        dplyr::mutate(dplyr::across(.value, exp))

    #

    expect_s3_class(wflw_fit$fit$fit$fit, "theta_fit_impl")

    # $fit

    expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "forecast")

    expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    expect_null(wflw_fit$fit$fit$fit$extras$xreg_recipe)

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    expect_equal(names(mld$outcomes), "value")



    full_data <- dplyr::bind_rows(rsample::training(splits), rsample::testing(splits))

    # Structure
    expect_identical(nrow(full_data), nrow(predictions_tbl))
    expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% dplyr::filter(.key == "prediction")
    resid <- rsample::testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 2408)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 805)

})



# SMOOTH PARSNIP ----

# * NO XREGS ----



# TESTS
test_that("exp_smoothing: smooth", {

    skip_on_cran()

    #

    model_spec <- exp_smoothing() %>%
        parsnip::set_engine("smooth_es")

    # Fit Spec
    model_fit <- model_spec %>%
        fit(log(value) ~ date, data = rsample::training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))

    #

    expect_s3_class(model_fit$fit, "smooth_fit_impl")

    # $fit

    expect_s3_class(model_fit$fit$models$model_1, "smooth")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_null(model_fit$fit$extras$xreg_recipe)

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")


    # Structure
    expect_identical(nrow(rsample::testing(splits)), nrow(predictions_tbl))
    expect_identical(rsample::testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- rsample::testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 1395)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 750)

})






# * WORKFLOWS XREGS ----

# TESTS
test_that("exp_smoothing: Arima (workflow), Test Model Fit Object", {

    skip_on_cran()

    #


    # Model Spec
    model_spec <- exp_smoothing(
        seasonal_period = 12,
        error = "multiplicative", trend = "additive", season = "multiplicative"
        ,
        smooth_level = 0.2, smooth_trend = 0.1, smooth_seasonal = 0.1
    ) %>%
        parsnip::set_engine("smooth_es")

    # Recipe spec
    recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
        recipes::step_log(value, skip = FALSE) %>%
        recipes::step_date(date, features = "month")

    # Workflow
    wflw <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec)

    # xreg did not contain values for the holdout, so we had to predict missing values.
    suppressWarnings({
        wflw_fit <- wflw %>%
            fit(rsample::training(splits))
    })

    # Forecast
    suppressWarnings({
        predictions_tbl <- wflw_fit %>%
            modeltime_calibrate(rsample::testing(splits)) %>%
            modeltime_forecast(new_data = rsample::testing(splits),
                               actual_data = rsample::training(splits)) %>%
            dplyr::mutate(dplyr::across(c(.value, .conf_lo, .conf_hi), exp))
    })

    #

    expect_s3_class(wflw_fit$fit$fit$fit, "smooth_fit_impl")

    # $fit

    expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "smooth")

    expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    expect_true(!is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    expect_equal(names(mld$outcomes), "value")


    # exp_smoothing: ets (workflow), Test Predictions

    full_data <- dplyr::bind_rows(rsample::training(splits), rsample::testing(splits))

    # Structure
    expect_identical(nrow(full_data), nrow(predictions_tbl))
    expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% dplyr::filter(.key == "prediction")
    resid <- rsample::testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    # expect_lte(max(abs(resid)), 1395)

    # - MAE less than 700
    # expect_lte(mean(abs(resid)), 750)

})

