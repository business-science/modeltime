# FORECAST PLOTS -----
context("TEST MODELTIME PLOTS")

# SETUP ----

# Data
m750   <- m4_monthly %>% filter(id == "M750")
splits <- initial_time_split(m750, prop = 0.8)

# Model Spec
model_spec <- arima_reg(period = 12) %>%
    set_engine("auto_arima")

# PARSNIP INTERFACE ----

model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# * Accuracy ----
accuracy_tbl <- model_fit %>%
    modeltime_calibrate(new_data = testing(splits)) %>%
    modeltime_accuracy()

# TABLES ----

# * Reactable table ----
react <- accuracy_tbl %>%
    table_modeltime_accuracy()

# * GT table ----
gt_tab <- accuracy_tbl %>%
    table_modeltime_accuracy(.interactive = FALSE)


test_that("accuracy table, GT - Parsnip Interface", {

    # Structure
    testthat::expect_s3_class(gt_tab, "gt_tbl")
    testthat::expect_equal(gt_tab$`_heading`$title, "Accuracy Table")


})

test_that("accuracy table, Reactable - Parnsip Interface", {

    # Structure
    testthat::expect_s3_class(react, "reactable")

})


# WORKFLOW INTERFACE ----

# Model Spec
model_spec <- arima_reg(period = 12) %>%
    set_engine("auto_arima")

# Recipe spec
recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_log(value, skip = FALSE)

# Workflow
wflw <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec)

wflw_fit <- wflw %>%
    fit(training(splits))

accuracy_tbl <- wflw_fit %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_accuracy()

# * Reactable table ----
react <- accuracy_tbl %>%
    table_modeltime_accuracy()

# * GT table ----
gt_tab <- accuracy_tbl %>%
    table_modeltime_accuracy(.interactive = FALSE)


test_that("accuracy table, GT - Workflow Interface", {

    # Structure
    testthat::expect_s3_class(gt_tab, "gt_tbl")
    testthat::expect_equal(gt_tab$`_heading`$title, "Accuracy Table")


})

test_that("accuracy table, Reactable - Workflow Interface", {

    # Structure
    testthat::expect_s3_class(react, "reactable")

})
