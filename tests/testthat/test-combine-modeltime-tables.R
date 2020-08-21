context("TEST COMBINING MULTIPLE MODELTIME TABLES")


# SETUP ----

# library(modeltime)
# library(tidymodels)
#
# library(tidyverse)
# library(timetk)
# library(lubridate)

m750 <- m4_monthly %>%
    filter(id == "M750")

splits <- time_series_split(m750, assess = "3 years", cumulative = TRUE)

model_fit_arima <- arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(value ~ date, training(splits))

model_fit_prophet <- prophet_reg() %>%
    set_engine("prophet") %>%
    fit(value ~ date, training(splits))

model_fit_ets <- exp_smoothing() %>%
    set_engine("ets") %>%
    fit(value ~ date, training(splits))

# Make 3 separate modeltime tables
model_tbl_1 <- modeltime_table(model_fit_arima)

model_tbl_2 <- modeltime_table(model_fit_prophet)

model_tbl_3 <- modeltime_table(model_fit_ets)

calib_tbl <- model_tbl_1 %>%
    modeltime_calibrate(testing(splits))

# Tests ----

test_that("combine_modeltime_table(): succeeds with mdl_time_tbl classes", {

    model_tbl_combo <- combine_modeltime_tables(model_tbl_1, model_tbl_2, model_tbl_3)

    expect_s3_class(model_tbl_combo, "mdl_time_tbl")

    expect_equal(model_tbl_combo$.model_id, 1:3)

})

test_that("combine_modeltime_table(): fails with non-mdl_time_tbl classes", {

    expect_error({
        combine_modeltime_tables(
            model_tbl_1, model_tbl_2, model_tbl_3,
            # Bad
            model_fit_arima
        )
    })

})

test_that("combine_modeltime_table(): Removes columns when combining calibration tbl", {

    expect_message({
        combine_modeltime_tables(
            model_tbl_1, model_tbl_2, model_tbl_3,
            # Calibrated
            calib_tbl, calib_tbl
        )
    })

    combo_tbl <- combine_modeltime_tables(
        model_tbl_1, model_tbl_2, model_tbl_3,
        # Calibrated
        calib_tbl
    )

    expect_equal(ncol(combo_tbl), 3)

    expect_equal(nrow(combo_tbl), 4)

})



