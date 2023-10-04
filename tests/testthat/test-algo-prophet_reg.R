# ---- STANDARD ARIMA ----
context("TEST prophet_reg: prophet")


# SETUP ----

# Data
m750 <- timetk::m4_monthly %>% dplyr::filter(id == "M750")

# Split Data 80/20
splits <- rsample::initial_time_split(m750, prop = 0.8)

# Model Spec
model_spec <- prophet_reg(
    growth                   = 'linear',
    changepoint_num          = 10,
    changepoint_range        = 0.75,
    seasonality_yearly       = TRUE,
    seasonality_weekly       = FALSE,
    seasonality_daily        = FALSE,
    season                   = 'multiplicative',
    prior_scale_changepoints = 20,
    prior_scale_seasonality  = 20,
    prior_scale_holidays     = 20
) %>%
    parsnip::set_engine("prophet")


# PARSNIP ----

# * NO XREGS ----

test_that("prophet_reg: prophet, (NO XREGS), Test Model Fit Object", {

    skip_on_cran()

    # ** MODEL FIT ----

    # Model Fit
    model_fit <- model_spec %>%
        fit(log(value) ~ date, data = rsample::training(splits))

    # Structure

    expect_s3_class(model_fit$fit, "prophet_fit_impl")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_null(model_fit$fit$extras$xreg_recipe)

    # $fit PROPHET

    expect_s3_class(model_fit$fit$models$model_1, "prophet")

    expect_identical(model_fit$fit$models$model_1$growth, "linear")
    expect_identical(model_fit$fit$models$model_1$n.changepoints, 10)

    expect_identical(model_fit$fit$models$model_1$changepoint.range, 0.75)
    expect_identical(model_fit$fit$models$model_1$yearly.seasonality, TRUE)
    expect_identical(model_fit$fit$models$model_1$weekly.seasonality, FALSE)
    expect_identical(model_fit$fit$models$model_1$daily.seasonality, FALSE)

    expect_identical(model_fit$fit$models$model_1$seasonality.mode, 'multiplicative')
    expect_identical(model_fit$fit$models$model_1$seasonality.prior.scale, 20)
    expect_identical(model_fit$fit$models$model_1$changepoint.prior.scale, 20)
    expect_identical(model_fit$fit$models$model_1$holidays.prior.scale, 20)


    expect_identical(model_fit$fit$models$model_1$uncertainty.samples, 0)

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")


    # ** PREDICTIONS ----

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))

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

test_that("prophet_reg: prophet, (XREGS), Test Model Fit Object", {

    skip_on_cran()

    # ** MODEL FIT ----

    # Model Fit
    model_fit <- model_spec %>%
        fit(log(value) ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
            data = rsample::training(splits))

    # Structure

    expect_s3_class(model_fit$fit, "prophet_fit_impl")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_true(!is.null(model_fit$fit$extras$xreg_recipe))

    # $fit PROPHET

    expect_s3_class(model_fit$fit$models$model_1, "prophet")

    expect_identical(model_fit$fit$models$model_1$growth, "linear")
    expect_identical(model_fit$fit$models$model_1$n.changepoints, 10)

    expect_identical(model_fit$fit$models$model_1$changepoint.range, 0.75)
    expect_identical(model_fit$fit$models$model_1$yearly.seasonality, TRUE)
    expect_identical(model_fit$fit$models$model_1$weekly.seasonality, FALSE)
    expect_identical(model_fit$fit$models$model_1$daily.seasonality, FALSE)

    expect_identical(model_fit$fit$models$model_1$seasonality.mode, 'multiplicative')
    expect_identical(model_fit$fit$models$model_1$seasonality.prior.scale, 20)
    expect_identical(model_fit$fit$models$model_1$changepoint.prior.scale, 20)
    expect_identical(model_fit$fit$models$model_1$holidays.prior.scale, 20)


    expect_identical(model_fit$fit$models$model_1$uncertainty.samples, 0)

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")


    # ** PREDICTIONS ----

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))

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


# ---- WORKFLOWS ----

# TESTS
test_that("prophet_reg: prophet (workflow), Test Model Fit Object", {

    skip_on_cran()

    #

    # Recipe spec
    recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
        recipes::step_log(value, skip = FALSE) %>%
        recipes::step_date(date, features = "month") %>%
        recipes::step_mutate(date_num = as.numeric(date))

    # Workflow
    wflw <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec)

    # Fitted Workflow
    wflw_fit <- wflw %>%
        fit(rsample::training(splits))


    # Structure

    expect_s3_class(wflw_fit$fit$fit$fit, "prophet_fit_impl")

    expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    expect_true(!is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))

    # $fit prophet

    expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "prophet")

    expect_identical(wflw_fit$fit$fit$fit$models$model_1$growth, "linear")
    expect_identical(wflw_fit$fit$fit$fit$models$model_1$n.changepoints, 10)

    expect_identical(wflw_fit$fit$fit$fit$models$model_1$changepoint.range, 0.75)
    expect_identical(wflw_fit$fit$fit$fit$models$model_1$yearly.seasonality, TRUE)
    expect_identical(wflw_fit$fit$fit$fit$models$model_1$weekly.seasonality, FALSE)
    expect_identical(wflw_fit$fit$fit$fit$models$model_1$daily.seasonality, FALSE)

    expect_identical(wflw_fit$fit$fit$fit$models$model_1$seasonality.mode, 'multiplicative')
    expect_identical(wflw_fit$fit$fit$fit$models$model_1$seasonality.prior.scale, 20)
    expect_identical(wflw_fit$fit$fit$fit$models$model_1$changepoint.prior.scale, 20)
    expect_identical(wflw_fit$fit$fit$fit$models$model_1$holidays.prior.scale, 20)


    expect_identical(wflw_fit$fit$fit$fit$models$model_1$uncertainty.samples, 0)

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    expect_equal(names(mld$outcomes), "value")


    # ** PREDICTIONS ----

    # Forecast
    predictions_tbl <- wflw_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits), actual_data = rsample::training(splits)) %>%
        dplyr::mutate(dplyr::across(.value, exp))

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


# LOGISTIC GROWTH ----

# * MODELS ----

test_that("prophet_reg: prophet, Logistic Growth", {

    skip_on_cran()

    # ** MODEL FIT ----

    # Model Fit
    model_fit <- prophet_reg(
        growth = "logistic",
        logistic_cap = 11000
    ) %>%
        parsnip::set_engine(engine = "prophet") %>%
        fit(value ~ date, m750)

    # Structure

    expect_s3_class(model_fit$fit, "prophet_fit_impl")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_false(is.null(model_fit$fit$extras$logistic_params$logistic_cap))

    # $fit PROPHET

    expect_s3_class(model_fit$fit$models$model_1, "prophet")

    expect_identical(model_fit$fit$models$model_1$growth, "logistic")

    expect_identical(model_fit$fit$extras$logistic_params$growth, "logistic")
    expect_identical(model_fit$fit$extras$logistic_params$logistic_cap, 11000)
    expect_null(model_fit$fit$extras$logistic_params$logistic_floor)

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")


    # ** PREDICTIONS ----
    forecast_prophet_logisitic <- modeltime_table(
        model_fit
    ) %>%
        modeltime_forecast(
            h = 12 * 10,
            actual_data = m750
        ) %>%
        filter(.model_desc != "ACTUAL")

    expect_lt(
        forecast_prophet_logisitic$.value %>% max(),
        11500
    )

    # ERROR IF CAP/FLOOR NOT SPECIFIED

    expect_error({
        prophet_reg(
            growth = "logistic"
        ) %>%
            parsnip::set_engine(engine = "prophet") %>%
            fit(value ~ date, m750)
    })

})




