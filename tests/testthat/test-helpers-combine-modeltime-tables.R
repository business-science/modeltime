context("TEST COMBINING MULTIPLE MODELTIME TABLES")


# Tests ----

test_that("combine_modeltime_table(): succeeds with mdl_time_tbl classes", {

    skip_on_cran()

    #

    m750 <- timetk::m4_monthly %>%
        dplyr::filter(id == "M750")

    splits <- time_series_split(m750, assess = "3 years", cumulative = TRUE)

    model_fit_arima <- arima_reg() %>%
        parsnip::set_engine("auto_arima") %>%
        fit(value ~ date, rsample::training(splits))

    model_fit_prophet <- prophet_reg() %>%
        parsnip::set_engine("prophet") %>%
        fit(value ~ date, rsample::training(splits))

    model_fit_ets <- exp_smoothing() %>%
        parsnip::set_engine("ets") %>%
        fit(value ~ date, rsample::training(splits))

    # Make 3 separate modeltime tables
    model_tbl_1 <- modeltime_table(model_fit_arima)

    model_tbl_2 <- modeltime_table(model_fit_prophet)

    model_tbl_3 <- modeltime_table(model_fit_ets)

    calib_tbl <- model_tbl_1 %>%
        modeltime_calibrate(rsample::testing(splits))


    # combine_modeltime_table(): succeeds with mdl_time_tbl classes


    model_tbl_combo <- combine_modeltime_tables(model_tbl_1, model_tbl_2, model_tbl_3)

    expect_s3_class(model_tbl_combo, "mdl_time_tbl")

    expect_equal(model_tbl_combo$.model_id, 1:3)


    # combine_modeltime_table(): fails with non-mdl_time_tbl classes

    expect_error({
        combine_modeltime_tables(
            model_tbl_1, model_tbl_2, model_tbl_3,
            # Bad
            model_fit_arima
        )
    })


    # combine_modeltime_table(): Removes columns when combining calibration tbl

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



