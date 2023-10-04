# FORECAST PLOTS -----
context("TEST MODELTIME ACCURACY & TABLES")

# SETUP ----

# Data
m750   <- timetk::m4_monthly %>% dplyr::filter(id == "M750")
splits <- rsample::initial_time_split(m750, prop = 0.8)


# ACCURACY ----

test_that("Test Modeltime Accuracy", {

    skip_on_cran()

    #

    # Model Spec
    model_fit_arima <- arima_reg() %>%
        parsnip::set_engine("auto_arima") %>%
        fit(value ~ date, rsample::training(splits))

    model_fit_prophet <- prophet_reg() %>%
        parsnip::set_engine("prophet") %>%
        fit(value ~ date, rsample::training(splits))

    model_fit_lm <- linear_reg() %>%
        parsnip::set_engine("lm") %>%
        fit(value ~ splines::ns(date, df = 5)
            + lubridate::month(date, label = TRUE),
            rsample::training(splits))

    # Model Table
    model_tbl <- modeltime_table(
        model_fit_arima,
        model_fit_prophet,
        model_fit_lm
    )

    calibration_tbl <- model_tbl %>%
        modeltime_calibrate(rsample::testing(splits))

    # Test Modeltime Accuracy


    acc_tbl_1 <- calibration_tbl %>% modeltime_accuracy()

    acc_tbl_2 <- model_tbl %>% modeltime_accuracy(rsample::testing(splits))

    acc_tbl_3 <- calibration_tbl %>% modeltime_accuracy(rsample::training(splits))

    acc_tbl_4 <- model_tbl %>% modeltime_accuracy(rsample::training(splits))

    # Structure
    nms_expected <- c(".model_id", ".model_desc", ".type",
                      "mae", "mape", "mase", "smape", "rmse", "rsq")

    expect_true(all(nms_expected %in% names(acc_tbl_1)))

    # Results
    expect_false(any(is.na(acc_tbl_1$mae)))
    expect_equal(acc_tbl_1$.type, rep("Test", 3))

    expect_false(any(is.na(acc_tbl_3$mae)))
    expect_equal(acc_tbl_3$.type, c("Fitted", "Fitted", "Test"))

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



    # Modifying Default Forecast Accuracy Metric Set

    my_metric_set <- default_forecast_accuracy_metric_set(
        yardstick::metric_tweak("mase12", yardstick::mase, m = 12)
    )

    acc_tbl_6 <- calibration_tbl %>%
        modeltime_accuracy(
            metric_set = my_metric_set
        )

    nms_expected <- c(".model_id", ".model_desc", ".type",
                      "mae", "mape", "mase", "smape", "rmse", "rsq", "mase12")

    expect_true(all(nms_expected %in% names(acc_tbl_6)))



    # TABLES ----

    accuracy_tbl <- calibration_tbl %>%
        modeltime_accuracy()

    # * Reactable table ----
    react <- accuracy_tbl %>%
        table_modeltime_accuracy()

    # * GT table ----
    gt_tab <- accuracy_tbl %>%
        table_modeltime_accuracy(.interactive = FALSE)



    # accuracy table, GT - Parsnip Interface

    # Structure
    expect_s3_class(gt_tab, "gt_tbl")
    expect_equal(gt_tab$`_heading`$title, "Accuracy Table")



    # accuracy table, Reactable - Parnsip Interface

    # Structure
    expect_s3_class(react, "reactable")

})









test_that("accuracy table, GT - Workflow Interface", {

    skip_on_cran()

    #

    # WORKFLOW INTERFACE ----

    # Model Spec
    model_spec <- arima_reg(seasonal_period = 12) %>%
        parsnip::set_engine("auto_arima")

    # Recipe spec
    recipe_spec <- recipes::recipe(value ~ date, data = rsample::training(splits)) %>%
        recipes::step_log(value, skip = FALSE)

    # Workflow
    wflw <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec)

    wflw_fit <- wflw %>%
        fit(rsample::training(splits))

    accuracy_tbl <- wflw_fit %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_accuracy()

    # * Reactable table ----
    react <- accuracy_tbl %>%
        table_modeltime_accuracy()

    # * GT table ----
    gt_tab <- accuracy_tbl %>%
        table_modeltime_accuracy(.interactive = FALSE)

    # accuracy table, GT - Workflow Interface

    # Structure
    expect_s3_class(gt_tab, "gt_tbl")
    expect_equal(gt_tab$`_heading`$title, "Accuracy Table")



    # accuracy table, Reactable - Workflow Interface

    # Structure
    expect_s3_class(react, "reactable")

})

