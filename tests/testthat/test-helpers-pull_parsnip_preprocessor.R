
# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)


# * (MATRIX) ARIMA ----


test_that("arima - matrix interface", {

    testthat::skip_on_cran()

    form <- stats::formula("log(value) ~ date")

    model_fit_no_boost <- arima_reg() %>%
        set_engine(engine = "auto_arima") %>%
        fit(form, data = training(splits))

    form_extract <- model_fit_no_boost %>% pull_parsnip_preprocessor()

    expect_equal(form, form_extract)

})


# (FORMULA - S3) MARS ----

test_that("MARS - S3 FORMULA", {

    testthat::skip_on_cran()

    form <- stats::formula("log(value) ~ as.numeric(date) + month(date, label = TRUE)")

    model_fit_mars <- mars(mode = "regression") %>%
        set_engine("earth") %>%
        fit(form, data = training(splits))

    form_extract <- model_fit_mars %>% pull_parsnip_preprocessor()

    expect_equal(form, form_extract)

})

# (FORMULA - S4) SVM ----


test_that("SVM - S4 FORMULA", {

    testthat::skip_on_cran()

    form <- stats::formula("log(value) ~ as.numeric(date) + month(date, label = TRUE)")

    model_fit_svm <- svm_rbf(mode = "regression") %>%
        set_engine("kernlab") %>%
        fit(form, data = training(splits))

    form_extract <- model_fit_svm %>% pull_parsnip_preprocessor()

    expect_equal(form, form_extract)

})

# ERROR ----

test_that("pull_parsnip_preprocessor error", {

    testthat::skip_on_cran()

    expect_error(pull_parsnip_preprocessor(1))

})

