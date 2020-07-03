# ---- STANDARD ARIMA ----
context("TEST seasonal_decomp() - stlm_ets")


# SETUP ----

# Split Data 80/20
splits <- initial_time_split(taylor_30_min, prop = 0.9)

# Model Spec
model_spec <- seasonal_decomp(seasonal_period_1 = "1 day", seasonal_period_2 = "week") %>%
    set_engine("stlm_ets")

# CHECKS ----
test_that("seasonal_decomp: checks", {

    # external regressors message
    expect_message({
        seasonal_decomp(seasonal_period_1 = 24*2) %>%
            set_engine("stlm_ets") %>%
            fit(value ~ date + month(date, label = TRUE), data = training(splits))
    })

    expect_error({
        seasonal_decomp(seasonal_period_1 = 1) %>%
            set_engine("stlm_ets") %>%
            fit(value ~ date, data = training(splits))
    })

})

# PARSNIP ----

# * NO XREGS ----

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# Predictions
predictions_tbl <- model_fit %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(new_data = testing(splits))


# TESTS
test_that("seasonal_decomp: parnip", {

    testthat::expect_s3_class(model_fit$fit, "stlm_ets_fit_impl")

    # $fit

    testthat::expect_s3_class(model_fit$fit$models$model_1, "stlm")

    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$data)[1], "date")

    testthat::expect_true(is.null(model_fit$fit$extras$xreg_recipe))

    # $fit xgboost

    testthat::expect_identical(model_fit$fit$models$model_2, NULL)

    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")


    # Structure
    testthat::expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 2500)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 700)

})



# ---- WORKFLOWS ----

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



# TESTS
test_that("seasonal_decomp: workflow", {

    testthat::expect_s3_class(wflw_fit$fit$fit$fit, "stlm_ets_fit_impl")

    # Structure

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    testthat::expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    testthat::expect_true(is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))

    # $fit arima
    testthat::expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "stlm")

    # $preproc
    mld <- wflw_fit %>% workflows::pull_workflow_mold()
    testthat::expect_equal(names(mld$outcomes), "value")


    full_data <- bind_rows(training(splits), testing(splits))

    # Structure
    testthat::expect_identical(nrow(full_data), nrow(predictions_tbl))
    testthat::expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% filter(.key == "prediction")
    resid <- testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 2500)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 700)

})




