# FORECAST PLOTS -----
context("TEST MODELTIME ACCURACY & TABLES")

# SETUP ----

# Data
m750   <- m4_monthly %>% filter(id == "M750")
splits <- initial_time_split(m750, prop = 0.8)

# Model Spec
model_fit_arima <- arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(value ~ date, training(splits))

model_fit_prophet <- prophet_reg() %>%
    set_engine("prophet") %>%
    fit(value ~ date, training(splits))

model_fit_lm <- linear_reg() %>%
    set_engine("lm") %>%
    fit(value ~ splines::ns(date, df = 5)
        + month(date, label = TRUE),
        training(splits))

# Model Table
model_tbl <- modeltime_table(
    model_fit_arima,
    model_fit_prophet,
    model_fit_lm
)

calibration_tbl <- model_tbl %>%
    modeltime_calibrate(testing(splits))



# ACCURACY ----

test_that("Test Modeltime Accuracy", {

    acc_tbl_1 <- calibration_tbl %>% modeltime_accuracy()

    acc_tbl_2 <- model_tbl %>% modeltime_accuracy(testing(splits))

    acc_tbl_3 <- calibration_tbl %>% modeltime_accuracy(training(splits))

    acc_tbl_4 <- model_tbl %>% modeltime_accuracy(training(splits))

    # Structure
    nms_expected <- c(".model_id", ".model_desc", ".type",
                      "mae", "mape", "mase", "smape", "rmse", "rsq")

    expect_true(all(nms_expected %in% names(acc_tbl_1)))

    # Equivalence
    expect_equal(acc_tbl_1, acc_tbl_2)

    expect_equal(acc_tbl_3, acc_tbl_4)

    # Metric set
    met_set <- yardstick::metric_set(yardstick::mae, yardstick::rmse)

    acc_tbl_5 <- calibration_tbl %>% modeltime_accuracy(metric_set = met_set)

    nms_expected <- c(".model_id", ".model_desc", ".type",
                      "mae", "rmse")

    expect_true(all(nms_expected %in% names(acc_tbl_5)))

    # Errors
    expect_error({
        # Missing new_data or calibration data
        model_tbl %>% modeltime_accuracy()
    })

})



# TABLES ----

accuracy_tbl <- calibration_tbl %>%
    modeltime_accuracy()

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
model_spec <- arima_reg(seasonal_period = 12) %>%
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
