# ---- STANDARD ARIMA ----
context("TEST arima_boost: auto_arima_xgboost")


# SETUP ----

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)

# Model Spec
model_spec <- arima_boost(
    period                   = 12,
    non_seasonal_ar          = 3,
    non_seasonal_differences = 1,
    non_seasonal_ma          = 3,
    seasonal_ar              = 2,
    seasonal_differences     = 1,
    seasonal_ma              = 2,
    mtry  = 25,
    trees = 250,
    min_n = 4,
    learn_rate = 0.1,
    tree_depth = 7,
    loss_reduction = 0.4,
    sample_size = 0.9
) %>%
    set_engine("auto_arima_xgboost")


# PARSNIP ----

# * NO XREGS ----

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# Predictions
predictions_tbl <- model_fit %>%
    modeltime_forecast(new_data = testing(splits))


# TESTS
test_that("arima_boost: Arima, (No xregs), Test Model Fit Object", {

    testthat::expect_s3_class(model_fit$fit, "auto_arima_xgboost_fit_impl")

    # $fit

    testthat::expect_s3_class(model_fit$fit$models$model_1, "Arima")

    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$data)[1], "date")

    testthat::expect_true(is.null(model_fit$fit$extras$xreg_recipe))

    # $fit xgboost

    testthat::expect_identical(model_fit$fit$models$model_2, NULL)

    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")



})

test_that("arima_boost: Arima, (No xregs), Test Predictions", {

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

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date + as.numeric(date) + month(date, label = TRUE), data = training(splits))

# Predictions
predictions_tbl <- model_fit %>%
    modeltime_forecast(new_data = testing(splits))


# TESTS
test_that("arima_boost: Arima, (XREGS), Test Model Fit Object", {

    testthat::expect_s3_class(model_fit$fit, "auto_arima_xgboost_fit_impl")

    # Structure

    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$data)[1], "date")

    testthat::expect_true(!is.null(model_fit$fit$extras$xreg_recipe))

    # $fit arima

    testthat::expect_s3_class(model_fit$fit$models$model_1, "Arima")

    # $fit xgboost

    testthat::expect_s3_class(model_fit$fit$models$model_2, "xgb.Booster")

    testthat::expect_identical(model_fit$fit$models$model_2$params$eta, 0.1)

    testthat::expect_identical(model_fit$fit$models$model_2$params$max_depth, 7)

    testthat::expect_identical(model_fit$fit$models$model_2$params$gamma, 0.4)

    testthat::expect_identical(model_fit$fit$models$model_2$params$colsample_bytree, 1)

    testthat::expect_identical(model_fit$fit$models$model_2$params$min_child_weight, 4)

    testthat::expect_identical(model_fit$fit$models$model_2$params$subsample, 0.9)

    testthat::expect_identical(model_fit$fit$models$model_2$params$objective, "reg:squarederror")

    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")

})

test_that("arima_boost: Arima (XREGS), Test Predictions", {

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


# ---- WORKFLOWS ----

# Model Spec
model_spec <- arima_boost(
    period                   = 12,
    non_seasonal_ar          = 3,
    non_seasonal_differences = 1,
    non_seasonal_ma          = 3,
    seasonal_ar              = 1,
    seasonal_differences     = 1,
    seasonal_ma              = 1,
    mtry  = 25,
    trees = 250,
    min_n = 4,
    learn_rate = 0.1,
    tree_depth = 7,
    loss_reduction = 0.4,
    sample_size = 0.9
) %>%
    set_engine("auto_arima_xgboost")

# Recipe spec
recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_log(value, skip = FALSE) %>%
    step_date(date, features = "month") %>%
    step_mutate(date_num = as.numeric(date))

# Workflow
wflw <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec)

wflw_fit <- wflw %>%
    fit(training(splits))

# Forecast
predictions_tbl <- wflw_fit %>%
    modeltime_forecast(new_data = testing(splits), actual_data = training(splits)) %>%
    mutate_at(vars(.value:.conf_hi), exp)



# TESTS
test_that("arima_boost: Arima (workflow), Test Model Fit Object", {

    testthat::expect_s3_class(wflw_fit$fit$fit$fit, "auto_arima_xgboost_fit_impl")

    # Structure

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    testthat::expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    testthat::expect_true(!is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))

    # $fit arima

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "Arima")

    # $fit xgboost

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$models$model_2, "xgb.Booster")

    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_2$params$eta, 0.1)

    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_2$params$max_depth, 7)

    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_2$params$gamma, 0.4)

    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_2$params$colsample_bytree, 1)

    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_2$params$min_child_weight, 4)

    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_2$params$subsample, 0.9)

    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_2$params$objective, "reg:squarederror")


    # $preproc
    mld <- wflw_fit %>% workflows::pull_workflow_mold()
    testthat::expect_equal(names(mld$outcomes), "value")

})

test_that("arima_boost: Arima (workflow), Test Predictions", {

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




