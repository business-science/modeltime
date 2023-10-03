context("PANEL DATA")


# PANEL DATA - FORECAST JUMBLED ----

test_that("Panel Data - Forecast Jumbled", {

    skip_on_cran()

    #

    m4_monthly_jumbled <- m4_monthly %>%
        arrange(desc(date))

    data_set <- m4_monthly_jumbled

    recipe_spec <- recipe(value ~ date + id, data_set) %>%
        step_mutate(date_num = as.numeric(date)) %>%
        step_mutate(month_lbl = lubridate::month(date, label = TRUE)) %>%
        step_dummy(all_nominal(), one_hot = TRUE)

    set.seed(123)
    wflw_fit_prophet <- workflow() %>%
        add_model(
            prophet_boost(
                seasonality_yearly = F,
                seasonality_weekly = F,
                seasonality_daily  = F
            ) %>%
                set_engine(
                    "prophet_xgboost"
                    # ,
                    # colsample_bytree = 1
                )
        ) %>%
        add_recipe(recipe_spec) %>%
        fit(data_set)

    set.seed(123)
    wflw_fit_svm <- workflow() %>%
        add_model(svm_rbf(mode = "regression") %>% set_engine("kernlab")) %>%
        add_recipe(recipe_spec %>% step_rm(date)) %>%
        fit(data_set)

    # set.seed(123)
    # wflw_fit_xgb <- workflow() %>%
    #     add_model(boost_tree() %>% set_engine("xgboost")) %>%
    #     add_recipe(recipe_spec %>% step_rm(date)) %>%
    #     fit(data_set)

    # Panel Data - Forecast Jumbled

    model_tbl <- modeltime_table(
        wflw_fit_prophet,
        wflw_fit_svm
    )

    # Calibration
    calibration_tbl <- model_tbl %>%
        modeltime_calibrate(data_set)

    expect_equal(calibration_tbl$.type, c("Fitted", "Test"))
    expect_true(all(c(".type", ".calibration_data") %in% names(calibration_tbl)))
    expect_equal(nrow(data_set), calibration_tbl %>% pluck(".calibration_data", 1) %>% nrow())

    # Accuracy
    accuracy_tbl <- calibration_tbl %>%
        modeltime_accuracy()

    expect_true(all(!is.na(accuracy_tbl$mae)))
    expect_true(all(is.double(accuracy_tbl$mae)))

    # * Forecast ----
    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data       = data_set,
            actual_data    = data_set,
            keep_data      = TRUE,
            arrange_index  = FALSE
        )

    # forecast_tbl %>% group_by(id) %>% plot_modeltime_forecast()

    # * Test Actual ----
    actual_tbl <- forecast_tbl %>%
        filter(.key == "actual")

    expect_equal(nrow(actual_tbl), nrow(data_set))
    expect_equal(actual_tbl$.value, actual_tbl$value)

    # * Test Model ----
    svm_tbl <- forecast_tbl %>%
        filter(.model_id == 2)

    expect_equal(nrow(svm_tbl), nrow(data_set))
    expect_equal(svm_tbl$.index, svm_tbl$date)


    # Panel Data - Error Checks

    # Using h with overlapping actual data
    expect_error({
        modeltime_table(
            wflw_fit_prophet,
            wflw_fit_svm
        ) %>%
            modeltime_forecast(h = "2 years", actual_data = data_set)
    })

    # Using h with overlapping actual data
    expect_error({
        modeltime_table(
            wflw_fit_prophet,
            wflw_fit_svm
        ) %>%
            modeltime_calibrate(data_set, quiet = FALSE) %>%
            modeltime_forecast(h = "2 years")
    })

})

