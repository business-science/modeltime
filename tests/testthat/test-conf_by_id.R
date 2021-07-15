context("ACCURACY / CONFIDENCE INTERVAL BY ID")


test_that("Confidence and Accuracy by ID", {

    skip_on_cran()

    library(tidymodels)
    library(timetk)
    library(modeltime)
    library(tidyverse)

    # Data
    data <- walmart_sales_weekly %>%
        select(id, Date, Weekly_Sales) %>%
        set_names(c("ID", "date", "value"))

    splits <- data %>% time_series_split(assess = "3 months", cumulative = TRUE)

    rec_obj <- recipe(value ~ ., training(splits)) %>%
        step_timeseries_signature(date) %>%
        step_rm(date) %>%
        step_zv(all_predictors()) %>%
        step_dummy(all_nominal_predictors(), one_hot = TRUE)


    wflw_xgb <- workflow() %>%
        add_model(
            boost_tree() %>% set_engine("xgboost")
        ) %>%
        add_recipe(rec_obj) %>%
        fit(training(splits))


    model_tbl <- modeltime_table(
        wflw_xgb,
        wflw_xgb
    )


    # CALIBRATION ID ----

    calib_tbl <- model_tbl %>%
        modeltime_calibrate(new_data = testing(splits), id = "ID")

    df <- calib_tbl$.calibration_data[[1]]

    expect_equal(ncol(df), 5)
    expect_equal(names(df)[5], "ID")

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
            new_data    = testing(splits),
            actual_data = NULL,
            conf_by_id  = TRUE
        )

    expect_equal(ncol(forecast_tbl), 8)
    expect_equal(names(forecast_tbl)[8], "ID")

})



