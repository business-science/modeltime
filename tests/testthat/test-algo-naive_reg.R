context("TEST naive_reg()")

# Data - Single Time Series
m750 <- m4_monthly %>% filter(id == "M750")

splits <- initial_time_split(m750, prop = 0.8)

# Data - Multiple Time Series (Panel)
full_data_tbl <- m4_monthly %>%
    group_by(id) %>%
    future_frame(date, .length_out = 60, .bind_data = TRUE) %>%
    ungroup()

future_tbl <- full_data_tbl %>% filter(is.na(value))

data_prepared_tbl <- full_data_tbl %>% filter(!is.na(value))

# 1.0 NAIVE ----

# * SINGLE TIME SERIES -----

testthat::test_that("NAIVE - Single Time Series (No ID)", {

    model_fit <- naive_reg() %>%
        set_engine("naive") %>%
        fit(value ~ date, data = training(splits))

    calibration_tbl <- modeltime_table(
        model_fit
    ) %>%
        modeltime_calibrate(testing(splits))

    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data    = testing(splits),
            actual_data = m750
        )

    forecast_vec <- forecast_tbl %>%
        filter(.model_id == 1) %>%
        pull(.value)

    future_forecast_tbl <- calibration_tbl %>%
        modeltime_refit(m750) %>%
        modeltime_forecast(
            h = nrow(testing(splits)),
            actual_data = m750
        )

    future_forecast_vec <- future_forecast_tbl %>%
        filter(.model_id == 1) %>%
        pull(.value)

    expect_equal(nrow(forecast_tbl), 368)
    expect_equal(forecast_vec, rep_len(10810, 62))

    expect_equal(nrow(future_forecast_tbl), 368)
    expect_equal(future_forecast_vec, rep_len(11000, 62))

})


# * PANEL DATA ----

testthat::test_that("NAIVE - Multiple Time Series (Panel uses ID)", {

    model_fit_panel <- naive_reg(id = "id") %>%
        set_engine("naive") %>%
        fit(value ~ date + id, data = data_prepared_tbl)


    future_forecast_panel_tbl <- modeltime_table(
        model_fit_panel
    ) %>%
        modeltime_forecast(
            new_data    = future_tbl,
            actual_data = data_prepared_tbl,
            keep_data   = TRUE
        )

    future_vec <- future_forecast_panel_tbl %>%
        filter(!is.na(.model_id)) %>%
        filter(id == "M1") %>%
        pull(.value)


    expect_equal(nrow(future_forecast_panel_tbl), 1814)
    expect_equal(future_vec, rep_len(6890, 60))

})

# * UNSEEN PANEL DATA ----

testthat::test_that("SNAIVE - Check New Factors", {

    wflw_fit_panel <- workflow() %>%
        add_model(naive_reg(id = "id") %>% set_engine("naive")) %>%
        add_recipe(recipe(value ~ date + id, data = data_prepared_tbl)) %>%
        fit(data_prepared_tbl)

    # FOR SOME REASON PARSNIP MODELS FAIL
    # Error: Problem occurred during prediction. Error in model.frame.default(mod_terms, new_data, na.action = na.action, : factor id has new levels UNSEEN
    # model_fit_panel <- naive_reg(id = "id") %>%
    #     set_engine("naive") %>%
    #     fit(value ~ date + id, data = data_prepared_tbl)

    future_forecast_panel_tbl <- modeltime_table(
        # model_fit_panel,
        wflw_fit_panel
    ) %>%
        modeltime_forecast(
            new_data    = bind_rows(
                future_tbl,
                future_tbl %>%
                    filter(id == "M1") %>%
                    mutate(id = fct_recode(id, UNSEEN = "M1"))
            ),
            actual_data = bind_rows(
                data_prepared_tbl,
                data_prepared_tbl %>%
                    filter(id == "M1") %>%
                    mutate(id = fct_recode(id, UNSEEN = "M1"))
            ),
            keep_data   = TRUE
        )

    future_forecast_vec <- future_forecast_panel_tbl %>%
        filter(!is.na(.model_id)) %>%
        filter(id == "UNSEEN") %>%
        pull(.value)


    expect_equal(future_forecast_vec, rep_len(NA_real_, 60))

})

# 2.0 SNAIVE -----

# * SINGLE TIME SERIES -----

testthat::test_that("SNAIVE - Single Time Series (No ID)", {

    model_fit <- naive_reg() %>%
        set_engine("snaive") %>%
        fit(value ~ date, data = training(splits))

    calibration_tbl <- modeltime_table(
        model_fit
    ) %>%
        modeltime_calibrate(testing(splits))

    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data    = testing(splits),
            actual_data = m750
        )

    forecast_vec <- forecast_tbl %>%
        filter(.model_id == 1) %>%
        pull(.value)

    last_series <- training(splits) %>%
        slice_tail(n = 12) %>%
        pull(value)

    future_forecast_tbl <- calibration_tbl %>%
        modeltime_refit(m750) %>%
        modeltime_forecast(
            h = nrow(testing(splits)),
            actual_data = m750
        )

    future_forecast_vec <- future_forecast_tbl %>%
        filter(.model_id == 1) %>%
        pull(.value)

    future_last_series <- m750 %>%
        slice_tail(n = 12) %>%
        pull(value)


    expect_equal(model_fit$fit$extras$period, 12)
    expect_equal(nrow(forecast_tbl), 368)
    expect_equal(forecast_vec, rep_len(last_series, 62))
    expect_equal(future_forecast_vec, rep_len(future_last_series, 62))

})


# * PANEL DATA ----

testthat::test_that("SNAIVE - Multiple Time Series (Panel ID)", {

    model_fit <- naive_reg() %>%
        set_engine("snaive") %>%
        fit(value ~ date, data = training(splits))

    calibration_tbl <- modeltime_table(
        model_fit
    ) %>%
        modeltime_calibrate(testing(splits))

    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data    = testing(splits),
            actual_data = m750
        )

    forecast_vec <- forecast_tbl %>%
        filter(.model_id == 1) %>%
        pull(.value)

    last_series <- training(splits) %>%
        slice_tail(n = 12) %>%
        pull(value)

    future_forecast_tbl <- calibration_tbl %>%
        modeltime_refit(m750) %>%
        modeltime_forecast(
            h = nrow(testing(splits)),
            actual_data = m750
        )

    future_forecast_vec <- future_forecast_tbl %>%
        filter(.model_id == 1) %>%
        pull(.value)

    future_last_series <- m750 %>%
        slice_tail(n = 12) %>%
        pull(value)


    expect_equal(model_fit$fit$extras$period, 12)
    expect_equal(nrow(forecast_tbl), 368)
    expect_equal(forecast_vec, rep_len(last_series, 62))
    expect_equal(future_forecast_vec, rep_len(future_last_series, 62))

})


# * UNSEEN PANEL DATA ----

testthat::test_that("SNAIVE - Check New Factors", {

    wflw_fit_panel <- workflow() %>%
        add_model(naive_reg(id = "id") %>% set_engine("snaive")) %>%
        add_recipe(recipe(value ~ date + id, data = data_prepared_tbl)) %>%
        fit(data_prepared_tbl)

    # FOR SOME REASON PARSNIP MODELS FAIL
    # Error: Problem occurred during prediction. Error in model.frame.default(mod_terms, new_data, na.action = na.action, : factor id has new levels UNSEEN
    # model_fit_panel <- naive_reg(id = "id") %>%
    #     set_engine("snaive") %>%
    #     fit(value ~ date + id, data = data_prepared_tbl)

    future_forecast_panel_tbl <- modeltime_table(
        # model_fit_panel,
        wflw_fit_panel
    ) %>%
        modeltime_forecast(
            new_data    = bind_rows(
                future_tbl,
                future_tbl %>%
                    filter(id == "M1") %>%
                    mutate(id = fct_recode(id, UNSEEN = "M1"))
            ),
            actual_data = bind_rows(
                data_prepared_tbl,
                data_prepared_tbl %>%
                    filter(id == "M1") %>%
                    mutate(id = fct_recode(id, UNSEEN = "M1"))
            ),
            keep_data   = TRUE
        )

    future_forecast_vec <- future_forecast_panel_tbl %>%
        filter(!is.na(.model_id)) %>%
        filter(id == "UNSEEN") %>%
        pull(.value)


    expect_equal(future_forecast_vec, rep_len(NA_real_, 60))

})

