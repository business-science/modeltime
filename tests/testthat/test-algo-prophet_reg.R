# ---- STANDARD ARIMA ----
context("TEST prophet_reg: prophet")


# SETUP ----

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)

# Model Spec
model_spec <- prophet_reg(
    growth                   = 'linear',
    num_changepoints         = 10,
    season                   = 'multiplicative',
    prior_scale_changepoints = 20,
    prior_scale_seasonality  = 20,
    prior_scale_holidays     = 20
) %>%
    set_engine("prophet")


# PARSNIP ----

# * NO XREGS ----

test_that("prophet_reg: prophet, (XREGS), Test Model Fit Object", {


    # ** MODEL FIT ----

    # Model Fit
    model_fit <- model_spec %>%
        fit(log(value) ~ date, data = training(splits))

    # Structure

    testthat::expect_s3_class(model_fit$fit, "prophet_fit_impl")

    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$data)[1], "date")

    testthat::expect_true(is.null(model_fit$fit$extras$xreg_recipe))

    # $fit PROPHET

    testthat::expect_s3_class(model_fit$fit$models$model_1, "prophet")

    testthat::expect_identical(model_fit$fit$models$model_1$growth, "linear")
    testthat::expect_identical(model_fit$fit$models$model_1$n.changepoints, 10)
    testthat::expect_identical(model_fit$fit$models$model_1$seasonality.mode, 'multiplicative')
    testthat::expect_identical(model_fit$fit$models$model_1$seasonality.prior.scale, 20)
    testthat::expect_identical(model_fit$fit$models$model_1$changepoint.prior.scale, 20)
    testthat::expect_identical(model_fit$fit$models$model_1$holidays.prior.scale, 20)


    testthat::expect_identical(model_fit$fit$models$model_1$uncertainty.samples, 0)

    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")


    # ** PREDICTIONS ----

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(testing(splits)) %>%
        modeltime_forecast(new_data = testing(splits))

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

test_that("prophet_reg: prophet, (XREGS), Test Model Fit Object", {


    # ** MODEL FIT ----

    # Model Fit
    model_fit <- model_spec %>%
        fit(log(value) ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
            data = training(splits))

    # Structure

    testthat::expect_s3_class(model_fit$fit, "prophet_fit_impl")

    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$data)[1], "date")

    testthat::expect_true(!is.null(model_fit$fit$extras$xreg_recipe))

    # $fit PROPHET

    testthat::expect_s3_class(model_fit$fit$models$model_1, "prophet")

    testthat::expect_identical(model_fit$fit$models$model_1$growth, "linear")
    testthat::expect_identical(model_fit$fit$models$model_1$n.changepoints, 10)
    testthat::expect_identical(model_fit$fit$models$model_1$seasonality.mode, 'multiplicative')
    testthat::expect_identical(model_fit$fit$models$model_1$seasonality.prior.scale, 20)
    testthat::expect_identical(model_fit$fit$models$model_1$changepoint.prior.scale, 20)
    testthat::expect_identical(model_fit$fit$models$model_1$holidays.prior.scale, 20)


    testthat::expect_identical(model_fit$fit$models$model_1$uncertainty.samples, 0)

    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")


    # ** PREDICTIONS ----

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(testing(splits)) %>%
        modeltime_forecast(new_data = testing(splits))

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

# Recipe spec
recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_log(value, skip = FALSE) %>%
    step_date(date, features = "month") %>%
    step_mutate(date_num = as.numeric(date))

# Workflow
wflw <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec)





# TESTS
test_that("prophet_reg: prophet (workflow), Test Model Fit Object", {

    # Fitted Workflow
    wflw_fit <- wflw %>%
        fit(training(splits))


    # Structure

    testthat::expect_s3_class(wflw_fit$fit$fit$fit, "prophet_fit_impl")

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    testthat::expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    testthat::expect_true(!is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))

    # $fit prophet

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "prophet")

    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_1$growth, "linear")
    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_1$n.changepoints, 10)
    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_1$seasonality.mode, 'multiplicative')
    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_1$seasonality.prior.scale, 20)
    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_1$changepoint.prior.scale, 20)
    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_1$holidays.prior.scale, 20)


    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_1$uncertainty.samples, 0)

    # $preproc
    mld <- wflw_fit %>% workflows::pull_workflow_mold()
    testthat::expect_equal(names(mld$outcomes), "value")


    # ** PREDICTIONS ----

    # Forecast
    predictions_tbl <- wflw_fit %>%
        modeltime_calibrate(testing(splits)) %>%
        modeltime_forecast(new_data = testing(splits), actual_data = training(splits)) %>%
        mutate_at(vars(.value), exp)

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




