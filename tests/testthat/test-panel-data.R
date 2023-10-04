context("PANEL DATA")


# PANEL DATA - FORECAST JUMBLED ----

test_that("Panel Data - Forecast Jumbled", {

    skip_on_cran()
    skip_if_not_installed("kernlab")
    #

    m4_monthly_jumbled <- timetk::m4_monthly %>%
        dplyr::arrange(dplyr::desc(date))

    data_set <- m4_monthly_jumbled

    recipe_spec <- recipes::recipe(value ~ date + id, data_set) %>%
        recipes::step_mutate(date_num = as.numeric(date)) %>%
        recipes::step_mutate(month_lbl = lubridate::month(date, label = TRUE)) %>%
        recipes::step_dummy(all_nominal(), one_hot = TRUE)

    set.seed(123)
    wflw_fit_prophet <- workflows::workflow() %>%
        workflows::add_model(
            prophet_boost(
                seasonality_yearly = FALSE,
                seasonality_weekly = FALSE,
                seasonality_daily  = FALSE
            ) %>%
                parsnip::set_engine(
                    "prophet_xgboost"
                    # ,
                    # colsample_bytree = 1
                )
        ) %>%
        workflows::add_recipe(recipe_spec) %>%
        fit(data_set)

    set.seed(123)
    wflw_fit_svm <- workflows::workflow() %>%
        workflows::add_model(svm_rbf(mode = "regression") %>% parsnip::set_engine("kernlab")) %>%
        workflows::add_recipe(recipe_spec %>% step_rm(date)) %>%
        fit(data_set)

    # set.seed(123)
    # wflw_fit_xgb <- workflows::workflow() %>%
    #     workflows::add_model(boost_tree() %>% parsnip::set_engine("xgboost")) %>%
    #     workflows::add_recipe(recipe_spec %>% step_rm(date)) %>%
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
        dplyr::filter(.model_id == 2)

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

