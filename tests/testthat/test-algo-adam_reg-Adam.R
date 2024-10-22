# ---- STANDARD ADAM ----
context("TEST adam_reg: ADAM")


# SETUP ----

# Data
m750 <- timetk::m4_monthly %>% dplyr::filter(id == "M750")

# Split Data 80/20
splits <- rsample::initial_time_split(m750, prop = 0.8)




# TESTS
test_that("adam_reg: Adam, (No xregs), Test Model Fit Object", {

    skip_on_cran()

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
        parsnip::set_engine("adam")


    # PARSNIP ----

    # * NO XREGS ----

    # Fit Spec
    model_fit <- model_spec %>%
        fit(value ~ date, data = rsample::training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))

    expect_s3_class(model_fit$fit, "Adam_fit_impl")

    # $fit

    expect_s3_class(model_fit$fit$models$model_1, "adam")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_null(model_fit$fit$extras$xreg_recipe)

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")

    # Structure
    expect_identical(nrow(rsample::testing(splits)), nrow(predictions_tbl))
    expect_identical(rsample::testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- rsample::testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 4000)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 2000)


    # * XREGS ----

    # Data
    m750 <- timetk::m4_monthly %>% dplyr::filter(id == "M750") %>%
        dplyr::mutate(month = lubridate::month(date, label = TRUE))

    # Split Data 80/20
    splits <- rsample::initial_time_split(m750, prop = 0.8)

    # Fit Spec
    model_fit <- model_spec %>%
        fit(value ~ date + month, data = rsample::training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))


    # Model Fit ----

    expect_s3_class(model_fit$fit, "Adam_fit_impl")

    # $fit

    expect_s3_class(model_fit$fit$models$model_1, "adam")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_true(!is.null(model_fit$fit$extras$xreg_recipe))

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")



    # Structure
    expect_identical(nrow(rsample::testing(splits)), nrow(predictions_tbl))
    expect_identical(rsample::testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- rsample::testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 4000)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 2000)

})


# ---- WORKFLOWS ----

test_that("adam_reg: Adam (workflow)", {

    skip_on_cran()

    # * Model Spec ====
    model_spec <- adam_reg(
        seasonal_period          = 12,
        non_seasonal_ar          = 3,
        non_seasonal_differences = 1,
        non_seasonal_ma          = 3,
        seasonal_ar              = 1,
        seasonal_differences     = 0,
        seasonal_ma              = 1
    ) %>%
        parsnip::set_engine("adam")

    # Recipe spec
    recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits))

    # Workflow
    wflw <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec)

    wflw_fit <- wflw %>%
        fit(rsample::training(splits))

    # Forecast
    predictions_tbl <- wflw_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits), actual_data = rsample::training(splits))

    expect_s3_class(wflw_fit$fit$fit$fit, "Adam_fit_impl")

    # * Structure ----

    expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "adam")

    expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    expect_null(wflw_fit$fit$fit$fit$extras$xreg_recipe)

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    expect_equal(names(mld$outcomes), "value")

    # * Test Predictions ----

    full_data <- dplyr::bind_rows(rsample::training(splits), rsample::testing(splits))

    # Structure
    expect_identical(nrow(full_data), nrow(predictions_tbl))
    expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% dplyr::filter(.key == "prediction")
    resid <- rsample::testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 4000)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 2000)

})




