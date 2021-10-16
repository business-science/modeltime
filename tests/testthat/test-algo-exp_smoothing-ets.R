# ---- ETS, CROSTON ----
context("TEST exp_smoothing: ets")


# SETUP ----

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)

# Model Spec
model_spec <- exp_smoothing() %>%
    set_engine("ets")


# ETS PARSNIP ----

# * NO XREGS ----

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# Predictions
predictions_tbl <- model_fit %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(new_data = testing(splits))


# TESTS
test_that("exp_smoothing: ets, Test Model Fit Object", {

    testthat::expect_s3_class(model_fit$fit, "ets_fit_impl")

    # $fit

    testthat::expect_s3_class(model_fit$fit$models$model_1, "ets")

    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$data)[1], "date")

    testthat::expect_true(is.null(model_fit$fit$extras$xreg_recipe))

    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")

})

test_that("exp_smoothing: ets, Test Predictions", {

    # Structure
    testthat::expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 1500)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 800)

})




# ---- ETS WORKFLOWS ----

# Model Spec
model_spec <- exp_smoothing(
    seasonal_period = 12,
    error = "multiplicative", trend = "additive", season = "multiplicative"
    ,
    smooth_level = 0.2, smooth_trend = 0.1, smooth_seasonal = 0.1
    ) %>%
    set_engine("ets")

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
    modeltime_forecast(new_data = testing(splits),
                       actual_data = training(splits)) %>%
    mutate_at(vars(.value), exp)



# TESTS
test_that("exp_smoothing: Arima (workflow), Test Model Fit Object", {

    testthat::expect_s3_class(wflw_fit$fit$fit$fit, "ets_fit_impl")

    # $fit

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "ets")

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    testthat::expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    testthat::expect_true(is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    testthat::expect_equal(names(mld$outcomes), "value")

})

test_that("exp_smoothing: ets (workflow), Test Predictions", {

    full_data <- bind_rows(training(splits), testing(splits))

    # Structure
    testthat::expect_identical(nrow(full_data), nrow(predictions_tbl))
    testthat::expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% filter(.key == "prediction")
    resid <- testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 1500)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 700)

})

# ---- CROSTON WORKFLOWS ----

# Model Spec
model_spec <- exp_smoothing(
    smooth_level = 0.2
) %>%
    set_engine("croston")

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
    modeltime_forecast(new_data = testing(splits),
                       actual_data = training(splits)) %>%
    mutate_at(vars(.value), exp)

# TESTS
test_that("exp_smoothing: Arima (workflow), Test Model Fit Object", {

    testthat::expect_s3_class(wflw_fit$fit$fit$fit, "croston_fit_impl")

    # $fit

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "forecast")

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    testthat::expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    testthat::expect_true(is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    testthat::expect_equal(names(mld$outcomes), "value")

})

test_that("exp_smoothing: ets (workflow), Test Predictions", {

    full_data <- bind_rows(training(splits), testing(splits))

    # Structure
    testthat::expect_identical(nrow(full_data), nrow(predictions_tbl))
    testthat::expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% filter(.key == "prediction")
    resid <- testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 1500)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 1000)

})


# ---- THETA WORKFLOWS ----

# Model Spec
model_spec <- exp_smoothing() %>%
    set_engine("theta")

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
    modeltime_forecast(new_data = testing(splits),
                       actual_data = training(splits)) %>%
    mutate_at(vars(.value), exp)

# TESTS
test_that("exp_smoothing: Theta (workflow), Test Model Fit Object", {

    testthat::expect_s3_class(wflw_fit$fit$fit$fit, "theta_fit_impl")

    # $fit

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "forecast")

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    testthat::expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    testthat::expect_true(is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    testthat::expect_equal(names(mld$outcomes), "value")

})

test_that("exp_smoothing: Theta (workflow), Test Predictions", {

    full_data <- bind_rows(training(splits), testing(splits))

    # Structure
    testthat::expect_identical(nrow(full_data), nrow(predictions_tbl))
    testthat::expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% filter(.key == "prediction")
    resid <- testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 2408)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 805)

})



# SMOOTH PARSNIP ----

# * NO XREGS ----

model_spec <- exp_smoothing() %>%
    set_engine("smooth_es")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# Predictions
predictions_tbl <- model_fit %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(new_data = testing(splits))


# TESTS
test_that("exp_smoothing: smooth, Test Model Fit Object", {

    testthat::expect_s3_class(model_fit$fit, "smooth_fit_impl")

    # $fit

    testthat::expect_s3_class(model_fit$fit$models$model_1, "smooth")

    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$data)[1], "date")

    testthat::expect_true(is.null(model_fit$fit$extras$xreg_recipe))

    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")

})

test_that("exp_smoothing: smooth, Test Predictions", {

    # Structure
    testthat::expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 1395)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 750)

})




# ---- ETS WORKFLOWS ----

# Model Spec
model_spec <- exp_smoothing(
    seasonal_period = 12,
    error = "multiplicative", trend = "additive", season = "multiplicative"
    ,
    smooth_level = 0.2, smooth_trend = 0.1, smooth_seasonal = 0.1
) %>%
    set_engine("smooth_es")

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
    modeltime_forecast(new_data = testing(splits),
                       actual_data = training(splits)) %>%
    mutate_at(vars(.value), exp)



# TESTS
test_that("exp_smoothing: Arima (workflow), Test Model Fit Object", {

    testthat::expect_s3_class(wflw_fit$fit$fit$fit, "smooth_fit_impl")

    # $fit

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "smooth")

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    testthat::expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    testthat::expect_true(is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    testthat::expect_equal(names(mld$outcomes), "value")

})

test_that("exp_smoothing: ets (workflow), Test Predictions", {

    full_data <- bind_rows(training(splits), testing(splits))

    # Structure
    testthat::expect_identical(nrow(full_data), nrow(predictions_tbl))
    testthat::expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% filter(.key == "prediction")
    resid <- testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 1395)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 750)

})

