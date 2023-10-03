# RESIDUALS TESTS-----
context("TEST MODELTIME RESIDUALS TESTS")


# RESIDUALS TESTS ----

test_that("Test Modeltime Residuals Tests", {

    skip_on_cran()

    # Data
    m750   <- timetk::m4_monthly %>% dplyr::filter(id == "M750")
    splits <- rsample::initial_time_split(m750, prop = 0.8)

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

    residuals_tbl <- model_tbl %>%
        modeltime_calibrate(rsample::testing(splits)) %>%
        modeltime_residuals()

    res_tbl_1 <- residuals_tbl %>% modeltime_residuals_test()

    res_tbl_2 <- model_tbl %>% modeltime_residuals_test(rsample::training(splits))

    res_tbl_3 <- model_tbl %>% modeltime_residuals_test(rsample::testing(splits))

    res_tbl_4 <- model_fit_arima %>%
                 modeltime_calibrate(new_data = rsample::training(splits)) %>%
                 modeltime_residuals_test(new_data = rsample::training(splits))

    res_tbl_5 <- model_fit_arima %>%
                 modeltime_calibrate(new_data = rsample::testing(splits)) %>%
                 modeltime_residuals_test(new_data = rsample::testing(splits))

    res_tbl_6 <- residuals_tbl %>% modeltime_residuals_test(rsample::training(splits))

    res_tbl_7 <- residuals_tbl %>% modeltime_residuals_test(rsample::testing(splits))

    # Structure
    nms_expected <- c(".model_id", ".model_desc", "shapiro_wilk", "box_pierce", "ljung_box", "durbin_watson")

    expect_true(all(nms_expected %in% names(res_tbl_1)))
    expect_true(all(nms_expected %in% names(res_tbl_2)))
    expect_true(all(nms_expected %in% names(res_tbl_3)))
    expect_true(all(nms_expected %in% names(res_tbl_4)))
    expect_true(all(nms_expected %in% names(res_tbl_5)))
    expect_true(all(nms_expected %in% names(res_tbl_6)))
    expect_true(all(nms_expected %in% names(res_tbl_7)))

    # Results
    expect_false(any(is.na(res_tbl_1$shapiro_wilk)))
    expect_false(any(is.na(res_tbl_2$ljung_box)))
    expect_false(any(is.na(res_tbl_3$box_pierce)))
    expect_false(any(is.na(res_tbl_1$durbin_watson)))


    # Equivalence
    expect_equal(res_tbl_6, res_tbl_7)
    expect_equal(res_tbl_6, res_tbl_1)
    expect_equal(res_tbl_4, res_tbl_2[1,])
    expect_equal(res_tbl_5, res_tbl_3[1,])

    # Errors
    expect_error({
        # Missing new_data or calibration data
        model_tbl %>% modeltime_residuals_test()
    })

    expect_error({
        # Missing new_data or calibration data
        model_fit_arima %>% modeltime_residuals_test(new_data = rsample::training(splits))
    })

    expect_error({
        # Missing new_data or calibration data
        model_fit_arima %>% modeltime_residuals_test()
    })

})



