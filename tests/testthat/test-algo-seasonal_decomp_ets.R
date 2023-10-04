# ---- STANDARD ARIMA ----
context("TEST seasonal_reg() - stlm_ets")




# CHECKS ----
test_that("seasonal_reg: stlm_ets", {

    skip_on_cran()

    # SETUP ----

    # Split Data 80/20
    splits <- rsample::initial_time_split(timetk::taylor_30_min, prop = 0.9)

    # Model Spec
    model_spec <- seasonal_reg(seasonal_period_1 = "1 day", seasonal_period_2 = "week") %>%
        parsnip::set_engine("stlm_ets")

    # external regressors message
    expect_message({
        seasonal_reg(seasonal_period_1 = 24*2) %>%
            parsnip::set_engine("stlm_ets") %>%
            fit(value ~ date + lubridate::month(date, label = TRUE), data = rsample::training(splits))
    })

    expect_error({
        seasonal_reg(seasonal_period_1 = 1) %>%
            parsnip::set_engine("stlm_ets") %>%
            fit(value ~ date, data = rsample::training(splits))
    })



    # PARSNIP ----

    # * NO XREGS ----

    # Fit Spec
    model_fit <- model_spec %>%
        fit(log(value) ~ date, data = rsample::training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))


    expect_s3_class(model_fit$fit, "stlm_ets_fit_impl")

    # $fit

    expect_s3_class(model_fit$fit$models$model_1, "stlm")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_null(model_fit$fit$extras$xreg_recipe)

    # $fit xgboost

    expect_identical(model_fit$fit$models$model_2, NULL)

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")


    # Structure
    expect_identical(nrow(rsample::testing(splits)), nrow(predictions_tbl))
    expect_identical(rsample::testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- rsample::testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 2500)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 700)



    # ---- WORKFLOWS ----

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


    expect_s3_class(wflw_fit$fit$fit$fit, "stlm_ets_fit_impl")

    # Structure

    expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    expect_null(wflw_fit$fit$fit$fit$extras$xreg_recipe)

    # $fit arima
    expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "stlm")

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
    expect_lte(max(abs(resid)), 2500)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 700)

})




