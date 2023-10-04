context("TEST MODELTIME WORKFLOW VS MODELS")

# Objectives
# - Test Multiple Parsnip Objects
# - Test Multiple Modeltime Objects

# Data
m750 <- timetk::m4_monthly %>% dplyr::filter(id == "M750")

# Split Data 80/20
splits <- rsample::initial_time_split(m750, prop = 0.9)


# MODELTIME MODELS ----

# * Auto ARIMA (Parsnip) ----

test_that("Auto ARIMA (Parsnip)", {

    skip_on_cran()

    model_fit_no_boost <- arima_reg() %>%
        parsnip::set_engine(engine = "auto_arima") %>%
        fit(log(value) ~ date, data = rsample::training(splits))


    # ** Model Table ----
    model_table <- modeltime_table(model_fit_no_boost)

    expect_s3_class(model_table, "mdl_time_tbl")

    expect_true(all(c(".model_id", ".model", ".model_desc") %in% names(model_table)))

    # ** Calibrate ----

    calibrated_tbl <- model_table %>%
        modeltime_calibrate(rsample::testing(splits))

    expect_s3_class(calibrated_tbl, "mdl_time_tbl")

    expect_equal(nrow(calibrated_tbl), 1)

    expect_true(".calibration_data" %in% names(calibrated_tbl))

    # ** Forecast ----
    expect_message({
        # Using calibration data
        calibrated_tbl %>%
            modeltime_forecast()
    })

    forecast_tbl <- calibrated_tbl %>%
        modeltime_forecast(rsample::testing(splits))

    expect_equal(nrow(forecast_tbl), nrow(rsample::testing(splits)))

    # ** Accuracy ----
    accuracy_tbl <- calibrated_tbl %>%
        modeltime_accuracy(metric_set = yardstick::metric_set(rsq, yardstick::mae))

    expect_equal(nrow(accuracy_tbl), 1)

    expect_true(all(c("rsq", "mae") %in% names(accuracy_tbl)))

    expect_false(any(c("mape", "mase", "smape", "rmse") %in% names(accuracy_tbl)))



    # ** Refit ----
    future_forecast_tbl <- calibrated_tbl %>%
        modeltime_refit(data = m750) %>%
        modeltime_forecast(h = "3 years")

    expect_equal(future_forecast_tbl$.index[1], lubridate::ymd("2015-07-01"))
})



# * Auto ARIMA (Workflow) -----

test_that("Auto ARIMA (Workflow)", {


    skip_on_cran()

    #

    wflw_fit_arima <- workflows::workflow() %>%
        workflows::add_model(
            spec = arima_reg() %>%
                parsnip::set_engine("auto_arima")
        ) %>%
        workflows::add_recipe(
            recipe = recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
                recipes::step_date(date, features = "month") %>%
                recipes::step_log(value)
        ) %>%
        fit(rsample::training(splits))

    # ** Model Table ----
    model_table <- modeltime_table(wflw_fit_arima)

    expect_s3_class(model_table, "mdl_time_tbl")

    expect_true(all(c(".model_id", ".model", ".model_desc") %in% names(model_table)))

    # ** Calibrate ----

    calibrated_tbl <- model_table %>%
        modeltime_calibrate(rsample::testing(splits))

    expect_s3_class(calibrated_tbl, "mdl_time_tbl")

    expect_equal(nrow(calibrated_tbl), 1)

    expect_true(".calibration_data" %in% names(calibrated_tbl))

    # ** Forecast ----
    forecast_tbl <- calibrated_tbl %>%
        modeltime_forecast(rsample::testing(splits))

    expect_equal(nrow(forecast_tbl), nrow(rsample::testing(splits)))

    # ** Accuracy ----
    accuracy_tbl <- calibrated_tbl %>%
        modeltime_accuracy(metric_set = yardstick::metric_set(rsq, yardstick::mae))

    expect_equal(nrow(accuracy_tbl), 1)

    expect_true(all(c("rsq", "mae") %in% names(accuracy_tbl)))

    expect_false(any(c("mape", "mase", "smape", "rmse") %in% names(accuracy_tbl)))



    # ** Refit ----
    future_forecast_tbl <- calibrated_tbl %>%
        modeltime_refit(data = m750) %>%
        modeltime_forecast(h = "3 years")

    expect_equal(future_forecast_tbl$.index[1], lubridate::ymd("2015-07-01"))
})


# MORE MODELTIME MODELS -----
# - Use these to do a mega test below
test_that("Models for Mega Test", {

    skip_on_cran()

    # * Auto ARIMA (Parsnip) ----
    model_fit_no_boost <- arima_reg() %>%
        parsnip::set_engine(engine = "auto_arima") %>%
        fit(log(value) ~ date, data = rsample::training(splits))

    # * Auto ARIMA (Workflow) -----
    wflw_fit_arima <- workflows::workflow() %>%
        workflows::add_model(
            spec = arima_reg() %>%
                parsnip::set_engine("auto_arima")
        ) %>%
        workflows::add_recipe(
            recipe = recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
                recipes::step_date(date, features = "month") %>%
                recipes::step_log(value)
        ) %>%
        fit(rsample::training(splits))

    # * ARIMA Boosted (Parsnip) ----

    model_fit_boosted <- arima_boost(
        non_seasonal_ar = 0,
        non_seasonal_differences = 1,
        non_seasonal_ma = 1,
        seasonal_ar = 1,
        seasonal_differences = 1,
        seasonal_ma = 1
    ) %>%
        parsnip::set_engine(engine = "arima_xgboost") %>%
        fit(log(value) ~ date + as.numeric(date) + lubridate::month(date, label = TRUE),
            data = rsample::training(splits))


    # * ETS (Parsnip) ----

    model_fit_ets <- exp_smoothing() %>%
        parsnip::set_engine("ets") %>%
        fit(log(value) ~ date + as.numeric(date) + lubridate::month(date, label = TRUE),
            data = rsample::training(splits))




    # * ETS (Workflow) ----

    model_spec <- exp_smoothing(
        error  = "multiplicative",
        trend  = "additive",
        season = "multiplicative") %>%
        parsnip::set_engine("ets")

    recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
        recipes::step_log(value)

    wflw_fit_ets <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec) %>%
        fit(rsample::training(splits))



    # * PARSNIP MODELS ----
    # - Shouldn't need tests for these, just using to to create checkpoints
    # - Using these in the scale tests

    # * LM (Parsnip Model) ----

    model_fit_lm <- parsnip::linear_reg() %>%
        parsnip::set_engine("lm") %>%
        fit(log(value) ~ as.numeric(date) + lubridate::month(date, label = TRUE),
            data = rsample::training(splits))


    # * LM workflow -----

    model_spec <- parsnip::linear_reg() %>%
        parsnip::set_engine("lm")

    recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
        recipes::step_date(date, features = "month") %>%
        recipes::step_log(value)

    wflw_fit_lm <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec) %>%
        fit(rsample::training(splits))



    # * MARS (Parsnip Model) ----
    skip_if_not_installed("earth")
    model_fit_mars <- parsnip::mars(mode = "regression") %>%
        parsnip::set_engine("earth") %>%
        fit(log(value) ~ as.numeric(date) + lubridate::month(date, label = TRUE),
            data = rsample::training(splits))




    # * MARS (Workflow) -----

    model_spec <- parsnip::mars(mode = "regression") %>%
        parsnip::set_engine("earth")

    recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
        recipes::step_date(date, features = "month", ordinal = FALSE) %>%
        recipes::step_mutate(date_num = as.numeric(date)) %>%
        recipes::step_normalize(date_num) %>%
        recipes::step_rm(date) %>%
        recipes::step_log(value)

    wflw_fit_mars <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec) %>%
        fit(rsample::training(splits))




    # * SVM (Parsnip Model) ----
    skip_if_not_installed("kernlab")
    model_fit_svm <- parsnip::svm_rbf(mode = "regression") %>%
        parsnip::set_engine("kernlab") %>%
        fit(log(value) ~ as.numeric(date) + lubridate::month(date, label = TRUE),
            data = rsample::training(splits))




    # * SVM (Workflow) -----
    skip_if_not_installed("kernlab")

    model_spec <- parsnip::svm_rbf(mode = "regression") %>%
        parsnip::set_engine("kernlab")

    recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
        recipes::step_date(date, features = "month") %>%
        recipes::step_rm(date) %>%
        # SVM requires dummy variables
        recipes::step_dummy(recipes::all_nominal()) %>%
        recipes::step_log(value)

    wflw_fit_svm <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec) %>%
        fit(rsample::training(splits))




    # * GLMNET (parsnip) ----
    # - Not using GLMnet because of requirement for R3.6+

    # # Error if penalty value is not included
    # model_fit_glmnet <- parsnip::linear_reg(
    #     penalty = 0.000388
    #     ) %>%
    #     parsnip::set_engine("glmnet") %>%
    #     fit(log(value) ~ as.numeric(date) + lubridate::month(date, label = TRUE),
    #         data = rsample::training(splits))
    #
    # model_fit_glmnet %>%
    #     modeltime_calibrate(rsample::testing(splits)) %>%
    #     modeltime_accuracy()


    # * GLMNET (workflow) ----

    # model_spec <- linear_reg(penalty = 0.000388) %>%
    #     parsnip::set_engine("glmnet")
    #
    # recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
    #     recipes::step_date(date, features = "month") %>%
    #     recipes::step_mutate(date_num = as.numeric(date)) %>%
    #     recipes::step_rm(date) %>%
    #     recipes::step_dummy(recipes::all_nominal()) %>%
    #     recipes::step_log(value)
    #
    # wflw_fit_glmnet <- workflows::workflow() %>%
    #     workflows::add_recipe(recipe_spec) %>%
    #     workflows::add_model(model_spec) %>%
    #     fit(rsample::training(splits))
    #
    # wflw_fit_glmnet %>%
    #     modeltime_calibrate(rsample::testing(splits)) %>%
    #     modeltime_accuracy()

    # * randomForest (parsnip) ----

    model_fit_randomForest <- parsnip::rand_forest(mode = "regression") %>%
        parsnip::set_engine("randomForest") %>%
        fit(log(value) ~ as.numeric(date) + lubridate::month(date, label = TRUE),
            data = rsample::training(splits))




    # * randomForest (workflow) ----

    model_spec <- parsnip::rand_forest("regression") %>%
        parsnip::set_engine("randomForest")

    recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
        recipes::step_date(date, features = "month") %>%
        recipes::step_mutate(date_num = as.numeric(date)) %>%
        recipes::step_rm(date) %>%
        recipes::step_dummy(recipes::all_nominal()) %>%
        recipes::step_log(value)

    wflw_fit_randomForest <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec) %>%
        fit(rsample::training(splits))



    # * XGBoost (parsnip) ----

    model_fit_xgboost <- parsnip::boost_tree(mode = "regression") %>%
        parsnip::set_engine("xgboost", objective = "reg:squarederror") %>%
        fit(log(value) ~ as.numeric(date) + lubridate::month(date, label = TRUE),
            data = rsample::training(splits))



    # * XGBoost (workflow) ----

    model_spec <- parsnip::boost_tree("regression") %>%
        parsnip::set_engine("xgboost", objective = "reg:squarederror")

    recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
        recipes::step_date(date, features = "month") %>%
        recipes::step_mutate(date_num = as.numeric(date)) %>%
        recipes::step_rm(date) %>%
        recipes::step_dummy(recipes::all_nominal()) %>%
        recipes::step_log(value)

    wflw_fit_xgboost <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec) %>%
        fit(rsample::training(splits))




    # MODELTIME TABLE ----

    model_table <- modeltime_table(
        # Modeltime
        model_fit_no_boost,
        wflw_fit_arima,
        model_fit_boosted,
        model_fit_ets,
        wflw_fit_ets,

        # Parsnip
        model_fit_lm,
        wflw_fit_lm,
        model_fit_mars,
        wflw_fit_mars,
        model_fit_svm,
        wflw_fit_svm,
        # model_fit_glmnet,
        # wflw_fit_glmnet,
        model_fit_randomForest,
        wflw_fit_randomForest,
        model_fit_xgboost,
        wflw_fit_xgboost
    )

    # MODELTIME TABLE ----

    expect_error(modeltime_table("a"))

    expect_s3_class(model_table, "mdl_time_tbl")

    expect_equal(ncol(model_table), 3)



    # MODELTIME ACCURACY ----

    expect_error(modeltime_accuracy(1))

    accuracy_tbl <- model_table %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_accuracy()

    # Structure
    expect_s3_class(accuracy_tbl, "tbl_df")

    # No missing values
    expect_true(all(!is.na(accuracy_tbl$mae)))


    # FORECAST ----
    expect_error(modeltime_forecast(1))

    forecast_tbl <- model_table %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_forecast(
            new_data    = rsample::testing(splits),
            actual_data = dplyr::bind_rows(rsample::training(splits), rsample::testing(splits))
        )

    # forecast_tbl %>%
    #     plot_modeltime_forecast()

    # Structure
    expect_s3_class(forecast_tbl, "tbl_df")

    # Correct number of forecasts produced
    expect_equal(
        nrow(forecast_tbl),
        nrow(model_table) * nrow(rsample::testing(splits)) + nrow(dplyr::bind_rows(rsample::training(splits), rsample::testing(splits)))
    )


    # REFITTING ----

    model_table_refit <- model_table %>%
        # filter(.model_id %in% c(10)) %>%
        # filter(.model_id %in% c(1,3,4,6,8,10,12,14)) %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_refit(data = m750)

    # Refit Structure
    expect_s3_class(model_table_refit, "mdl_time_tbl")

    # Forecast
    forecast_tbl <- model_table_refit %>%

        # REMOVE MARS-PARSNIP MODEL
        # - Issue: https://github.com/tidymodels/parsnip/issues/341
        filter(!.model_id %in% c(8)) %>%

        modeltime_forecast(
            new_data    = future_frame(m750, .length_out = "3 years"),
            actual_data = m750
        )
    # forecast_tbl %>% plot_modeltime_forecast()

    # Forecast Structure
    expect_s3_class(forecast_tbl, "tbl_df")

    actual_tbl <- forecast_tbl %>% dplyr::filter(.model_desc == "ACTUAL")
    future_predictions_tbl <- forecast_tbl %>% dplyr::filter(.model_desc != "ACTUAL")

    expect_true(all(tail(actual_tbl$.index, 1) < future_predictions_tbl$.index))


})






