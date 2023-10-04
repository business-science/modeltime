context("ACCURACY / CONFIDENCE INTERVAL BY ID")


test_that("Confidence and Accuracy by ID", {

    skip_on_cran()

    library(tidymodels)
    library(timetk)
    library(dplyr)

    # Data
    data <- walmart_sales_weekly %>%
        dplyr::select(ID = id, date = Date, value = Weekly_Sales)

    splits <- data %>% time_series_split(assess = "3 months", cumulative = TRUE)

    rec_obj <- recipes::recipe(value ~ ., rsample::training(splits)) %>%
        step_timeseries_signature(date) %>%
        recipes::step_rm(date) %>%
        recipes::step_zv(recipes::all_predictors()) %>%
        recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE)


    wflw_xgb <- workflows::workflow() %>%
        workflows::add_model(
            boost_tree("regression") %>% parsnip::set_engine("xgboost")
        ) %>%
        workflows::add_recipe(rec_obj) %>%
        fit(rsample::training(splits))


    model_tbl <- modeltime_table(
        wflw_xgb,
        wflw_xgb
    )


    # CALIBRATION BY ID ----

    test_data <- rsample::testing(splits) %>% dplyr::arrange(ID, date)

    calib_tbl <- model_tbl %>%
        modeltime_calibrate(new_data = test_data, id = "ID")

    df <- calib_tbl$.calibration_data[[1]]

    expect_equal(ncol(df), 5)
    expect_equal(names(df)[5], "ID")

    expect_equal(
        df %>% dplyr::select(ID, date, value = .actual),
        test_data
    )

    # ACCURACY BY ID ----

    df_1 <- calib_tbl %>% modeltime_accuracy(acc_by_id = FALSE)

    df_1_ncol <- ncol(df_1)
    df_1_nrow <- nrow(df_1)

    expect_equal(df_1_nrow, 2)

    df_2 <- calib_tbl %>% modeltime_accuracy(acc_by_id = TRUE)

    df_2_ncol <- ncol(df_2)
    df_2_nrow <- nrow(df_2)

    expect_equal(df_2_ncol, df_1_ncol + 1)
    expect_equal(df_2_nrow, 7 * 2)


    # FORECAST CONF INTERVAL BY ID ----

    forecast_tbl <- calib_tbl %>%
        modeltime_forecast(
            new_data    = rsample::testing(splits),
            actual_data = NULL,
            conf_by_id  = TRUE
        )

    expect_equal(ncol(forecast_tbl), 8)
    expect_equal(names(forecast_tbl)[8], "ID")

})



