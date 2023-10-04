# ---- NNETAR ----
context("TEST nnetar_reg")


# SETUP ----

# Data
m750 <- timetk::m4_monthly %>% dplyr::filter(id == "M750")

# Split Data 80/20
splits <- rsample::initial_time_split(m750, prop = 0.8)


# PARSNIP ----

# * NO XREGS ----

# TESTS
test_that("nnetar_reg: Parsnip", {

    skip_on_cran()

    # Model Spec
    model_spec <- nnetar_reg(
        seasonal_period          = 12,
        non_seasonal_ar          = 3,
        seasonal_ar              = 1,
        hidden_units             = 6,
        num_networks             = 15,
        penalty                  = 0.1,
        epochs                   = 50
    ) %>%
        parsnip::set_engine("nnetar")

    # Fit Spec
    set.seed(123)
    model_fit <- model_spec %>%
        fit(log(value) ~ date, data = rsample::training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))

    expect_s3_class(model_fit$fit, "nnetar_fit_impl")

    # $fit

    expect_s3_class(model_fit$fit$models$model_1, "nnetar")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_null(model_fit$fit$extras$xreg_recipe)


    expect_identical(model_fit$fit$models$model_1$p, 3)
    expect_identical(model_fit$fit$models$model_1$P, 1)
    expect_identical(model_fit$fit$models$model_1$size, 6)

    # nnets
    expect_identical(length(model_fit$fit$models$model_1$model), 15L)
    expect_identical(model_fit$fit$models$model_1$model[[1]]$decay, 0.1)


    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")


    # Structure
    expect_identical(nrow(rsample::testing(splits)), nrow(predictions_tbl))
    expect_identical(rsample::testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- rsample::testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    expect_lte(max(abs(resid)), 1600)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 700)


    # * XREGS ----

    # Fit
    set.seed(123)
    model_fit <- model_spec %>%
        fit(log(value) ~ date + lubridate::month(date, label = TRUE), data = rsample::training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits))


    expect_s3_class(model_fit$fit, "nnetar_fit_impl")

    # $fit

    expect_s3_class(model_fit$fit$models$model_1, "nnetar")

    expect_s3_class(model_fit$fit$data, "tbl_df")

    expect_equal(names(model_fit$fit$data)[1], "date")

    expect_true(!is.null(model_fit$fit$extras$xreg_recipe))

    expect_identical(model_fit$fit$models$model_1$p, 3)
    expect_identical(model_fit$fit$models$model_1$P, 1)
    expect_identical(model_fit$fit$models$model_1$size, 6)

    # nnets
    expect_identical(length(model_fit$fit$models$model_1$model), 15L)
    expect_identical(model_fit$fit$models$model_1$model[[1]]$decay, 0.1)

    # $preproc

    expect_equal(model_fit$preproc$y_var, "value")

    # Structure
    expect_identical(nrow(rsample::testing(splits)), nrow(predictions_tbl))
    expect_identical(rsample::testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- rsample::testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error 967.2171
    expect_lte(max(abs(resid)), 1250)

    # - MAE 407.0114
    expect_lte(mean(abs(resid)), 500)

})


# ---- WORKFLOWS ----

# TESTS
test_that("nnetar_reg: (workflow)", {

    skip_on_cran()

    # Model Spec
    model_spec <- nnetar_reg(
        seasonal_period          = 12,
        non_seasonal_ar          = 3,
        seasonal_ar              = 1,
        hidden_units             = 6,
        num_networks             = 15,
        penalty                  = 0.1,
        epochs                   = 50
    ) %>%
        parsnip::set_engine("nnetar")

    # Recipe spec
    recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
        recipes::step_log(value, skip = FALSE)

    # Workflow
    wflw <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec)

    set.seed(123)
    wflw_fit <- wflw %>%
        fit(rsample::training(splits))

    # Forecast
    predictions_tbl <- wflw_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(new_data = rsample::testing(splits), actual_data = rsample::training(splits)) %>%
        dplyr::mutate(dplyr::across(.value, exp))

    expect_s3_class(wflw_fit$fit$fit$fit, "nnetar_fit_impl")

    # $fit

    expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "nnetar")

    expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    expect_null(wflw_fit$fit$fit$fit$extras$xreg_recipe)

    expect_identical(wflw_fit$fit$fit$fit$models$model_1$p, 3)
    expect_identical(wflw_fit$fit$fit$fit$models$model_1$P, 1)
    expect_identical(wflw_fit$fit$fit$fit$models$model_1$size, 6)

    # nnets
    expect_identical(length(wflw_fit$fit$fit$fit$models$model_1$model), 15L)
    expect_identical(wflw_fit$fit$fit$fit$models$model_1$model[[1]]$decay, 0.1)

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    expect_equal(names(mld$outcomes), "value")

    # Predictions

    full_data <- dplyr::bind_rows(rsample::training(splits), rsample::testing(splits))

    # Structure
    expect_identical(nrow(full_data), nrow(predictions_tbl))
    expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% dplyr::filter(.key == "prediction")
    resid <- rsample::testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1501.464
    expect_lte(max(abs(resid)), 1600)

    # - MAE less than 700
    expect_lte(mean(abs(resid)), 700)

})




