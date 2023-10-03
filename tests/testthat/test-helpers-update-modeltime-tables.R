context("TEST MODELTIME TABLE HELPERS")



# * Pluck Modeltime Table Works ----

test_that("TEST MODELTIME TABLE HELPERS", {

    skip_on_cran()

    # SETUP

    # Data
    m750   <- timetk::m4_monthly %>% dplyr::filter(id == "M750")
    splits <- rsample::initial_time_split(m750, prop = 0.8)

    # Model Specs

    # This model updates
    model_spec_arima_1 <- arima_reg(seasonal_period = 1) %>%
        parsnip::set_engine("auto_arima")

    # This model does not update
    model_spec_arima_12 <- arima_reg(seasonal_period = 12) %>%
        parsnip::set_engine("auto_arima")



    # PARSNIP INTERFACE ----

    model_fit_arima_1 <- model_spec_arima_1 %>%
        fit(log(value) ~ date, data = rsample::training(splits))

    model_fit_arima_12 <- model_spec_arima_12 %>%
        fit(log(value) ~ date, data = rsample::training(splits))

    # WORKFLOW INTERFACE ----
    recipe_spec <- recipes::recipe(value ~ date, rsample::training(splits)) %>%
        recipes::step_log(value)

    workflow_fit_arima_1 <- workflows::workflow() %>%
        workflows::add_model(model_spec_arima_1) %>%
        workflows::add_recipe(recipe_spec) %>%
        fit(rsample::training(splits))

    workflow_fit_arima_12 <- workflows::workflow() %>%
        workflows::add_model(model_spec_arima_12) %>%
        workflows::add_recipe(recipe_spec) %>%
        fit(rsample::training(splits))

    # TESTS ----

    model_tbl <- modeltime_table(
        model_fit_arima_1,
        model_fit_arima_12,
        workflow_fit_arima_1,
        workflow_fit_arima_12
    )

    # Pluck Model

    mdl_1 <- model_tbl %>%
        pluck_modeltime_model(1)

    expect_s3_class(mdl_1, "model_fit")

    expect_error({
        "Hi" %>%
            pluck_modeltime_model(1)
    })


    # 1 - Description Updates, Automatic

    expected <- c(
        "ARIMA(2,1,1) WITH DRIFT",
        "ARIMA(0,1,1)(1,1,1)[12]",
        "ARIMA(2,1,1) WITH DRIFT",
        "ARIMA(0,1,1)(1,1,1)[12]"
    )

    expect_equal(model_tbl$.model_desc, expected)


    # 2 - Description Updates, Post Refit

    updated_model_tbl <- model_tbl %>%
        update_model_description(1, "ARIMA - Parsnip") %>%
        update_model_description(2, "ARIMA - Workflow")

    expected <- c(
        "ARIMA - Parsnip",
        "ARIMA - Workflow",
        "ARIMA(2,1,1) WITH DRIFT",
        "ARIMA(0,1,1)(1,1,1)[12]"
    )

    expect_equal(updated_model_tbl$.model_desc, expected)



    # Description Updates, Post Refit

    refit_tbl <- updated_model_tbl %>%
        modeltime_calibrate(rsample::training(splits)) %>%
        modeltime_refit(m750)

    expected <- c(
        "UPDATE: ARIMA(2,1,3) WITH DRIFT",
        "ARIMA - Workflow",
        "UPDATE: ARIMA(2,1,3) WITH DRIFT",
        "ARIMA(0,1,1)(1,1,1)[12]"
    )

    expect_equal(refit_tbl$.model_desc, expected)

})



