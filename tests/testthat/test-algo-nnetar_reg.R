# ---- NNETAR ----
context("TEST nnetar_reg")


# SETUP ----

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)


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
        set_engine("nnetar")

    # Fit Spec
    set.seed(123)
    model_fit <- model_spec %>%
        fit(log(value) ~ date, data = training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(testing(splits)) %>%
        modeltime_forecast(new_data = testing(splits))

    testthat::expect_s3_class(model_fit$fit, "nnetar_fit_impl")

    # $fit

    testthat::expect_s3_class(model_fit$fit$models$model_1, "nnetar")

    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$data)[1], "date")

    testthat::expect_true(is.null(model_fit$fit$extras$xreg_recipe))


    testthat::expect_identical(model_fit$fit$models$model_1$p, 3)
    testthat::expect_identical(model_fit$fit$models$model_1$P, 1)
    testthat::expect_identical(model_fit$fit$models$model_1$size, 6)

    # nnets
    testthat::expect_identical(length(model_fit$fit$models$model_1$model), 15L)
    testthat::expect_identical(model_fit$fit$models$model_1$model[[1]]$decay, 0.1)


    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")


    # Structure
    testthat::expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 1600)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 700)


    # * XREGS ----

    # Fit
    set.seed(123)
    model_fit <- model_spec %>%
        fit(log(value) ~ date + month(date, label = TRUE), data = training(splits))

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(testing(splits)) %>%
        modeltime_forecast(new_data = testing(splits))


    testthat::expect_s3_class(model_fit$fit, "nnetar_fit_impl")

    # $fit

    testthat::expect_s3_class(model_fit$fit$models$model_1, "nnetar")

    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$data)[1], "date")

    testthat::expect_true(!is.null(model_fit$fit$extras$xreg_recipe))

    testthat::expect_identical(model_fit$fit$models$model_1$p, 3)
    testthat::expect_identical(model_fit$fit$models$model_1$P, 1)
    testthat::expect_identical(model_fit$fit$models$model_1$size, 6)

    # nnets
    testthat::expect_identical(length(model_fit$fit$models$model_1$model), 15L)
    testthat::expect_identical(model_fit$fit$models$model_1$model[[1]]$decay, 0.1)

    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")

    # Structure
    testthat::expect_identical(nrow(testing(splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(splits)$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests

    resid <- testing(splits)$value - exp(predictions_tbl$.value)

    # - Max Error 967.2171
    testthat::expect_lte(max(abs(resid)), 1250)

    # - MAE 407.0114
    testthat::expect_lte(mean(abs(resid)), 500)

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
        set_engine("nnetar")

    # Recipe spec
    recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
        step_log(value, skip = FALSE)

    # Workflow
    wflw <- workflow() %>%
        add_recipe(recipe_spec) %>%
        add_model(model_spec)

    set.seed(123)
    wflw_fit <- wflw %>%
        fit(training(splits))

    # Forecast
    predictions_tbl <- wflw_fit %>%
        modeltime_calibrate(testing(splits)) %>%
        modeltime_forecast(new_data = testing(splits), actual_data = training(splits)) %>%
        mutate_at(vars(.value), exp)

    testthat::expect_s3_class(wflw_fit$fit$fit$fit, "nnetar_fit_impl")

    # $fit

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$models$model_1, "nnetar")

    testthat::expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")

    testthat::expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")

    testthat::expect_true(is.null(wflw_fit$fit$fit$fit$extras$xreg_recipe))

    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_1$p, 3)
    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_1$P, 1)
    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_1$size, 6)

    # nnets
    testthat::expect_identical(length(wflw_fit$fit$fit$fit$models$model_1$model), 15L)
    testthat::expect_identical(wflw_fit$fit$fit$fit$models$model_1$model[[1]]$decay, 0.1)

    # $preproc
    mld <- wflw_fit %>% workflows::extract_mold()
    testthat::expect_equal(names(mld$outcomes), "value")

    # Predictions

    full_data <- bind_rows(training(splits), testing(splits))

    # Structure
    testthat::expect_identical(nrow(full_data), nrow(predictions_tbl))
    testthat::expect_identical(full_data$date, predictions_tbl$.index)

    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% filter(.key == "prediction")
    resid <- testing(splits)$value - predictions_tbl$.value

    # - Max Error less than 1501.464
    testthat::expect_lte(max(abs(resid)), 1600)

    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 700)

})




