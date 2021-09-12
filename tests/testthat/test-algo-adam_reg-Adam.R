# ---- STANDARD ADAM ----
context("TEST adam_reg: ADAM")


# SETUP ----

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)

# Model Spec
model_spec <- adam_reg(
    seasonal_period          = 12,
    non_seasonal_ar          = 3,
    non_seasonal_differences = 1,
    non_seasonal_ma          = 3,
    seasonal_ar              = 1,
    seasonal_differences     = 0,
    seasonal_ma              = 1
) %>%
    set_engine("adam")


# PARSNIP ----

# * NO XREGS ----

# Fit Spec
model_fit <- model_spec %>%
    fit(value ~ date, data = training(splits))

# Predictions
predictions_tbl <- model_fit %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(new_data = testing(splits))


# TESTS
test_that("adam_reg: Adam, (No xregs), Test Model Fit Object", {

    testthat::expect_s3_class(model_fit$fit, "Adam_fit_impl")

    # $fit

    testthat::expect_s3_class(model_fit$fit$models$model_1, "adam")

    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$data)[1], "date")

    testthat::expect_true(is.null(model_fit$fit$extras$xreg_recipe))

    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")

})

test_that("adam_reg: Adam, (No xregs), Test Predictions", {

    # Structure
    testthat::expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 1750)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 435)

})

# * XREGS ----

# Data
m750 <- m4_monthly %>% filter(id == "M750") %>% mutate(month = month(date, label = TRUE))

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)

# Fit Spec
model_fit <- model_spec %>%
    fit(value ~ date + month, data = training(splits))

# Predictions
predictions_tbl <- model_fit %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(new_data = testing(splits))


# TESTS
test_that("adam_reg: Adam, (XREGS), Test Model Fit Object", {

    testthat::expect_s3_class(model_fit$fit, "Adam_fit_impl")

    # $fit

    testthat::expect_s3_class(model_fit$fit$models$model_1, "adam")

    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$data)[1], "date")

    testthat::expect_true(!is.null(model_fit$fit$extras$xreg_recipe))

    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")

})

test_that("adam_reg: Adam (XREGS), Test Predictions", {

    # Structure
    testthat::expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 1614)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 674)

})


# ---- WORKFLOWS ----

# Model Spec
model_spec <- adam_reg(
    seasonal_period          = 12,
    non_seasonal_ar          = 3,
    non_seasonal_differences = 1,
    non_seasonal_ma          = 3,
    seasonal_ar              = 1,
    seasonal_differences     = 0,
    seasonal_ma              = 1
) %>%
    set_engine("adam")

# Recipe spec
recipe_spec <- recipe(value ~ date, data = training(splits))

# Workflow
wflw <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec)

wflw_fit <- wflw %>%
    fit(training(splits))

# Forecast
predictions_tbl <- wflw_fit %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(new_data = testing(splits), actual_data = training(splits))



# TESTS
test_that("adam_reg: Adam (workflow), Test Model Fit Object", {

    testthat::expect_s3_class(wflw_fit$fit$fit$fit, "Adam_fit_impl")

    # $fit

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "adam")

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    testthat::expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    testthat::expect_true(is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))

    # $preproc
    mld <- wflw_fit %>% workflows::pull_workflow_mold()
    testthat::expect_equal(names(mld$outcomes), "value")

})

test_that("adam_reg: Adam (workflow), Test Predictions", {

    full_data <- bind_rows(training(splits), testing(splits))

    # Structure
    testthat::expect_identical(nrow(full_data), nrow(predictions_tbl))
    testthat::expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% filter(.key == "prediction")
    resid <- testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 1750)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 435)

})




