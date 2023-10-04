context("TEST window_reg() and naive_reg()")

# Data - Single Time Series
m750 <- timetk::m4_monthly %>% dplyr::filter(id == "M750")

splits <- rsample::initial_time_split(m750, prop = 0.8)

# Data - Multiple Time Series (Panel)
full_data_tbl <- timetk::m4_monthly %>%
    dplyr::group_by(id) %>%
    timetk::future_frame(date, .length_out = 60, .bind_data = TRUE) %>%
    dplyr::ungroup()

future_tbl <- full_data_tbl %>% dplyr::filter(is.na(value))

data_prepared_tbl <- full_data_tbl %>% dplyr::filter(!is.na(value))

# 1.0 NAIVE ----

# * SINGLE TIME SERIES -----

test_that("NAIVE - Single Time Series (No ID)", {

    skip_on_cran()

    model_fit <- naive_reg() %>%
        parsnip::set_engine("naive") %>%
        fit(value ~ date, data = rsample::training(splits))

    calibration_tbl <- modeltime_table(
        model_fit
    ) %>%
        modeltime_calibrate(rsample::testing(splits))

    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data    = rsample::testing(splits),
            actual_data = m750
        )

    forecast_vec <- forecast_tbl %>%
        dplyr::filter(.model_id == 1) %>%
        dplyr::pull(.value)

    future_forecast_tbl <- calibration_tbl %>%
        modeltime_refit(m750) %>%
        modeltime_forecast(
            h = nrow(rsample::testing(splits)),
            actual_data = m750
        )

    future_forecast_vec <- future_forecast_tbl %>%
        dplyr::filter(.model_id == 1) %>%
        dplyr::pull(.value)

    expect_equal(nrow(forecast_tbl), 368)
    expect_equal(forecast_vec, rep_len(10810, 62))

    expect_equal(nrow(future_forecast_tbl), 368)
    expect_equal(future_forecast_vec, rep_len(11000, 62))

})


# * PANEL DATA ----

test_that("NAIVE - Multiple Time Series (Panel uses ID)", {

    skip_on_cran()

    model_fit_panel <- naive_reg(id = "id") %>%
        parsnip::set_engine("naive") %>%
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
        dplyr::filter(!is.na(.model_id)) %>%
        dplyr::filter(id == "M1") %>%
        dplyr::pull(.value)


    expect_equal(nrow(future_forecast_panel_tbl), 1814)
    expect_equal(future_vec, rep_len(6890, 60))

})

# * UNSEEN PANEL DATA ----

test_that("NAIVE - Check New Factors", {

    skip_on_cran()

    wflw_fit_panel <- workflows::workflow() %>%
        workflows::add_model(naive_reg(id = "id") %>% parsnip::set_engine("naive")) %>%
        workflows::add_recipe(recipes::recipe(value ~ date + id, data = data_prepared_tbl)) %>%
        fit(data_prepared_tbl)

    # FOR SOME REASON PARSNIP MODELS FAIL
    # Error: Problem occurred during prediction. Error in model.frame.default(mod_terms, new_data, na.action = na.action, : factor id has new levels UNSEEN
    # model_fit_panel <- naive_reg(id = "id") %>%
    #     parsnip::set_engine("naive") %>%
    #     fit(value ~ date + id, data = data_prepared_tbl)

    expect_warning({
        future_forecast_panel_tbl <- modeltime_table(
            # model_fit_panel,
            wflw_fit_panel
        ) %>%
            modeltime_forecast(
                new_data    = dplyr::bind_rows(
                    future_tbl,
                    future_tbl %>%
                        dplyr::filter(id == "M1") %>%
                        dplyr::mutate(id = forcats::fct_recode(id, UNSEEN = "M1"))
                ),
                actual_data = dplyr::bind_rows(
                    data_prepared_tbl,
                    data_prepared_tbl %>%
                        dplyr::filter(id == "M1") %>%
                        dplyr::mutate(id = forcats::fct_recode(id, UNSEEN = "M1"))
                ),
                keep_data   = TRUE
            )
    })



    future_forecast_vec <- future_forecast_panel_tbl %>%
        dplyr::filter(!is.na(.model_id)) %>%
        dplyr::filter(id == "UNSEEN") %>%
        dplyr::pull(.value)


    expect_equal(future_forecast_vec, rep_len(NA_real_, 60))

})

# 2.0 SNAIVE -----

# * SINGLE TIME SERIES -----

test_that("SNAIVE - Single Time Series (No ID)", {

    skip_on_cran()

    model_fit <- naive_reg() %>%
        parsnip::set_engine("snaive") %>%
        fit(value ~ date, data = rsample::training(splits))

    calibration_tbl <- modeltime_table(
        model_fit
    ) %>%
        modeltime_calibrate(rsample::testing(splits))

    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data    = rsample::testing(splits),
            actual_data = m750
        )

    forecast_vec <- forecast_tbl %>%
        dplyr::filter(.model_id == 1) %>%
        dplyr::pull(.value)

    last_series <- rsample::training(splits) %>%
        dplyr::slice_tail(n = 12) %>%
        dplyr::pull(value)

    future_forecast_tbl <- calibration_tbl %>%
        modeltime_refit(m750) %>%
        modeltime_forecast(
            h = nrow(rsample::testing(splits)),
            actual_data = m750
        )

    future_forecast_vec <- future_forecast_tbl %>%
        dplyr::filter(.model_id == 1) %>%
        dplyr::pull(.value)

    future_last_series <- m750 %>%
        dplyr::slice_tail(n = 12) %>%
        dplyr::pull(value)


    expect_equal(model_fit$fit$extras$period, 12)
    expect_equal(nrow(forecast_tbl), 368)
    expect_equal(forecast_vec, rep_len(last_series, 62))
    expect_equal(future_forecast_vec, rep_len(future_last_series, 62))

})


# * PANEL DATA ----

test_that("SNAIVE - Multiple Time Series (Panel ID)", {

    skip_on_cran()

    model_fit_panel <- naive_reg(id = "id") %>%
        parsnip::set_engine("snaive") %>%
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
        dplyr::filter(id == "M1", !is.na(.model_id)) %>%
        dplyr::pull(.value)

    last_series_vec <- future_forecast_panel_tbl %>%
        dplyr::filter(id == "M1", is.na(.model_id)) %>%
        dplyr::slice_tail(n = 12) %>%
        dplyr::pull(.value)


    expect_equal(nrow(future_forecast_panel_tbl), 1814)
    expect_equal(future_vec, rep_len(last_series_vec, 60))

})


# * UNSEEN PANEL DATA ----

test_that("SNAIVE - Check New Factors", {

    skip_on_cran()

    wflw_fit_panel <- workflows::workflow() %>%
        workflows::add_model(naive_reg(id = "id") %>% parsnip::set_engine("snaive")) %>%
        workflows::add_recipe(recipes::recipe(value ~ date + id, data = data_prepared_tbl)) %>%
        fit(data_prepared_tbl)

    # FOR SOME REASON PARSNIP MODELS FAIL
    # Error: Problem occurred during prediction. Error in model.frame.default(mod_terms, new_data, na.action = na.action, : factor id has new levels UNSEEN
    # model_fit_panel <- naive_reg(id = "id") %>%
    #     parsnip::set_engine("snaive") %>%
    #     fit(value ~ date + id, data = data_prepared_tbl)

    expect_warning({
        future_forecast_panel_tbl <- modeltime_table(
            # model_fit_panel,
            wflw_fit_panel
        ) %>%
            modeltime_forecast(
                new_data    = dplyr::bind_rows(
                    future_tbl,
                    future_tbl %>%
                        dplyr::filter(id == "M1") %>%
                        dplyr::mutate(id = forcats::fct_recode(id, UNSEEN = "M1"))
                ),
                actual_data = dplyr::bind_rows(
                    data_prepared_tbl,
                    data_prepared_tbl %>%
                        dplyr::filter(id == "M1") %>%
                        dplyr::mutate(id = forcats::fct_recode(id, UNSEEN = "M1"))
                ),
                keep_data   = TRUE
            )
    })


    future_forecast_vec <- future_forecast_panel_tbl %>%
        dplyr::filter(id == "UNSEEN", !is.na(.model_id)) %>%
        dplyr::pull(.value)


    expect_equal(future_forecast_vec, rep_len(NA_real_, 60))

})


# 3.0 WINDOW -----

# * SINGLE TIME SERIES -----

test_that("WINDOW - Single Time Series (No ID)", {

    skip_on_cran()

    model_fit_1 <- window_reg(
        window_size     = 24
    ) %>%
        parsnip::set_engine("window_function", window_function = ~ mean(.x, na.rm = TRUE),) %>%
        fit(value ~ date, data = rsample::training(splits))

    model_fit_2 <- window_reg(
        window_size     = 36
    ) %>%
        parsnip::set_engine("window_function", window_function = median, na.rm = TRUE) %>%
        fit(value ~ date, data = rsample::training(splits))

    model_fit_3 <- window_reg() %>%
        parsnip::set_engine("window_function",
                   window_function = ~ tail(.x, 12),
                   na.rm = TRUE) %>%
        fit(value ~ date, data = rsample::training(splits))

    calibration_tbl <- modeltime_table(
        model_fit_1,
        model_fit_2,
        model_fit_3
    ) %>%
        modeltime_calibrate(rsample::testing(splits))

    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data    = rsample::testing(splits),
            actual_data = m750
        )

    forecast_vec <- forecast_tbl %>%
        dplyr::filter(.model_id == 1) %>%
        dplyr::pull(.value)

    last_series <- rsample::training(splits) %>%
        dplyr::slice_tail(n = 12) %>%
        dplyr::pull(value)

    future_forecast_tbl <- calibration_tbl %>%
        modeltime_refit(m750) %>%
        modeltime_forecast(
            h = nrow(rsample::testing(splits)),
            actual_data = m750,
            keep_data = TRUE
        )

    future_forecast_vec <- future_forecast_tbl %>%
        dplyr::filter(.model_id == 1) %>%
        dplyr::pull(.value)

    future_last_series <- m750 %>%
        dplyr::slice_tail(n = 12) %>%
        dplyr::pull(value)


    expect_equal(model_fit_1$fit$extras$period, 24)
    expect_equal(nrow(forecast_tbl), 492)
    expect_true(all(forecast_vec < 10174))
    expect_true(all(forecast_vec > 10173))

})


# * PANEL DATA ----

test_that("WINDOW - Multiple Time Series (Panel ID)", {

    skip_on_cran()


    model_fit_panel <- window_reg(
            id = "id",
            window_size = 12
        ) %>%
        parsnip::set_engine("window_function", window_function = mean) %>%
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
        dplyr::filter(id == "M1", !is.na(.model_id)) %>%
        dplyr::pull(.value)

    last_series_vec <- future_forecast_panel_tbl %>%
        dplyr::filter(id == "M1", is.na(.model_id)) %>%
        dplyr::slice_tail(n = 12) %>%
        dplyr::pull(.value)


    expect_equal(nrow(future_forecast_panel_tbl), 1814)
    expect_equal(future_vec, rep_len(mean(last_series_vec), 60))

})


# * UNSEEN PANEL DATA ----

test_that("SNAIVE - Check New Factors", {

    skip_on_cran()


    wflw_fit_panel <- workflows::workflow() %>%
        workflows::add_model(window_reg(id = "id") %>% parsnip::set_engine("window_function")) %>%
        workflows::add_recipe(recipes::recipe(value ~ date + id, data = data_prepared_tbl)) %>%
        fit(data_prepared_tbl)

    # FOR SOME REASON PARSNIP MODELS FAIL
    # Error: Problem occurred during prediction. Error in model.frame.default(mod_terms, new_data, na.action = na.action, : factor id has new levels UNSEEN
    # model_fit_panel <- naive_reg(id = "id") %>%
    #     parsnip::set_engine("snaive") %>%
    #     fit(value ~ date + id, data = data_prepared_tbl)

    expect_warning({
        future_forecast_panel_tbl <- modeltime_table(
            # model_fit_panel,
            wflw_fit_panel
        ) %>%
            modeltime_forecast(
                new_data    = dplyr::bind_rows(
                    future_tbl,
                    future_tbl %>%
                        dplyr::filter(id == "M1") %>%
                        mutate(id = forcats::fct_recode(id, UNSEEN = "M1"))
                ),
                actual_data = dplyr::bind_rows(
                    data_prepared_tbl,
                    data_prepared_tbl %>%
                        dplyr::filter(id == "M1") %>%
                        dplyr::mutate(id = forcats::fct_recode(id, UNSEEN = "M1"))
                ),
                keep_data   = TRUE
            )
    })


    future_forecast_vec <- future_forecast_panel_tbl %>%
        dplyr::filter(id == "UNSEEN", !is.na(.model_id)) %>%
        dplyr::pull(.value)


    expect_equal(future_forecast_vec, rep_len(NA_real_, 60))

})

