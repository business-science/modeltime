context("TEST MODELTIME TABLE HELPERS")

# SETUP ----

# Data
m750   <- m4_monthly %>% filter(id == "M750")
splits <- initial_time_split(m750, prop = 0.8)

# Model Specs

# This model updates
model_spec_arima_1 <- arima_reg(seasonal_period = 1) %>%
    set_engine("auto_arima")

# This model does not update
model_spec_arima_12 <- arima_reg(seasonal_period = 12) %>%
    set_engine("auto_arima")



# PARSNIP INTERFACE ----

model_fit_arima_1 <- model_spec_arima_1 %>%
    fit(log(value) ~ date, data = training(splits))

model_fit_arima_12 <- model_spec_arima_12 %>%
    fit(log(value) ~ date, data = training(splits))

# WORKFLOW INTERFACE ----
recipe_spec <- recipe(value ~ date, training(splits)) %>%
    step_log(value)

workflow_fit_arima_1 <- workflow() %>%
    add_model(model_spec_arima_1) %>%
    add_recipe(recipe_spec) %>%
    fit(training(splits))

workflow_fit_arima_12 <- workflow() %>%
    add_model(model_spec_arima_12) %>%
    add_recipe(recipe_spec) %>%
    fit(training(splits))

# TESTS ----

model_tbl <- modeltime_table(
    model_fit_arima_1,
    model_fit_arima_12,
    workflow_fit_arima_1,
    workflow_fit_arima_12
)

# * Pluck Modeltime Table Works ----

test_that("Pluck Model", {

    mdl_1 <- model_tbl %>%
        pluck_modeltime_model(1)

    testthat::expect_s3_class(mdl_1, "model_fit")

    expect_error({
        "Hi" %>%
            pluck_modeltime_model(1)
    })

})


# * Test Automatic Descriptions ----

test_that("1 - Description Updates, Automatic", {

    expected <- c(
        "ARIMA(2,1,1) WITH DRIFT",
        "ARIMA(0,1,1)(1,1,1)[12]",
        "ARIMA(2,1,1) WITH DRIFT",
        "ARIMA(0,1,1)(1,1,1)[12]"
    )

    expect_equal(model_tbl$.model_desc, expected)

})


# * Test Description Updates - Before Refit ----
updated_model_tbl <- model_tbl %>%
    update_model_description(1, "ARIMA - Parsnip") %>%
    update_model_description(2, "ARIMA - Workflow")

test_that("2 - Description Updates, Post Refit", {

    expected <- c(
        "ARIMA - Parsnip",
        "ARIMA - Workflow",
        "ARIMA(2,1,1) WITH DRIFT",
        "ARIMA(0,1,1)(1,1,1)[12]"
    )

    expect_equal(updated_model_tbl$.model_desc, expected)

})


# * Test Description Updates - Post Refit ----

refit_tbl <- updated_model_tbl %>%
    modeltime_calibrate(training(splits)) %>%
    modeltime_refit(m750)

test_that("Description Updates, Post Refit", {

    expected <- c(
        "UPDATE: ARIMA(2,1,3) WITH DRIFT",
        "ARIMA - Workflow",
        "UPDATE: ARIMA(2,1,3) WITH DRIFT",
        "ARIMA(0,1,1)(1,1,1)[12]"
    )

    expect_equal(refit_tbl$.model_desc, expected)

})



