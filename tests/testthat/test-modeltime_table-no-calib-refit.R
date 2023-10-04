context("TEST FORECASTING WITH NO CALIBRATION")


# SIMPLE PREDICTION ----

# library(tidymodels)
# library(dplyr)
# library(timetk)
# library(lubridate)


# TESTS ----

test_that("No Calibration", {

    skip_on_cran()

    # SETUP ----
    m750 <- timetk::m4_monthly %>%
        dplyr::filter(id == "M750")

    model_fit_arima <- arima_reg() %>%
        parsnip::set_engine("auto_arima") %>%
        fit(value ~ date, m750)

    model_fit_lm <- linear_reg() %>%
        parsnip::set_engine("lm") %>%
        fit(value ~ splines::ns(date, df = 5) + lubridate::month(date, label = TRUE), m750)

    model_fit_prophet <- prophet_reg() %>%
        parsnip::set_engine("prophet") %>%
        fit(value ~ date, m750)

    # Non-Calibration 1: h = 3 years, actual_data = m750 ----

    fcast <- modeltime_table(
        model_fit_arima,
        model_fit_lm,
        model_fit_prophet
    ) %>%
        modeltime_forecast(
            h = "3 years",
            actual_data = m750
        )

    expect_equal(nrow(fcast), 414)
    expect_equal(ncol(fcast), 5)

    # fcast %>% plot_modeltime_forecast(.conf_interval_show = F)

    # Non-Calibration 2: New Data = Actual Data ----

    fcast <- modeltime_table(
        model_fit_prophet,
        model_fit_lm
    ) %>%
        modeltime_forecast(
            new_data    = m750,
            actual_data = m750
        )

    expect_equal(nrow(fcast), 918)
    expect_equal(ncol(fcast), 5)

    # fcast %>% plot_modeltime_forecast(.conf_interval_show = F)

    # Non-Calibration 3: Actual Data Provided, New Data Missing ----

    expect_message({
        modeltime_table(
            model_fit_lm,
            model_fit_prophet
        ) %>%
            modeltime_forecast(
                actual_data = m750
            )
    })


    # Non-Calibration 4: New Data Only ----

    fcast <- modeltime_table(
        model_fit_prophet,
        model_fit_lm
    ) %>%
        modeltime_forecast(
            new_data    = m750 %>% tail(12*3)
        )

    expect_equal(nrow(fcast), 72)
    expect_equal(ncol(fcast), 5)

    # fcast %>% plot_modeltime_forecast(.conf_interval_show = F)


    # Non-Calibration 5: Errors ----

    # Error - Nothing provided - Needs new_data or h
    expect_error({
        modeltime_table(
            model_fit_lm
        ) %>%
            modeltime_forecast()
    })

    # Error - Only h provided - Needs calibration data or actual data
    expect_error({
        modeltime_table(
            model_fit_lm
        ) %>%
            modeltime_forecast(h = "3 years")
    })


})




