testthat::context("Modeltime Residuals")


testthat::test_that("modeltime_residuals(): Returns correct order", {

    testthat::skip_on_cran()

    library(tidymodels)
    library(modeltime)
    library(tidyverse)
    library(timetk)


    data <- walmart_sales_weekly %>%
        select(id, Date, Weekly_Sales) %>%
        set_names(c("ID", "date", "value"))


    splits <- data %>% time_series_split(assess = "3 months", cumulative = TRUE)


    rec_obj <- recipe(value ~ ., training(splits)) %>%
        step_mutate(ID = droplevels(ID)) %>%
        step_timeseries_signature(date) %>%
        step_rm(date) %>%
        step_zv(all_predictors()) %>%
        step_dummy(all_nominal_predictors(), one_hot = TRUE)



    # Workflow
    wflw_xgb <- workflow() %>%
        add_model(
            boost_tree() %>% set_engine("xgboost")
        ) %>%
        add_recipe(rec_obj) %>%
        fit(training(splits))


    wflw_glmnet <- workflow() %>%
        add_model(
            linear_reg(penalty = 1) %>%
                set_engine("glmnet")
        ) %>%
        add_recipe(rec_obj) %>%
        fit(training(splits))


    model_tbl <- modeltime_table(
        wflw_xgb,
        wflw_glmnet
    )


    i <- 1

    # Order is not changed for the training set
    calib_tbl_train <- model_tbl %>%
        modeltime_calibrate(
            training(splits)
        )

    testthat::expect_equal(training(splits)$value, calib_tbl_train$.calibration_data[[1]]$.actual)


    # Usage of the full data set
    # if i want to refit and than I use calibrate row order is changed
    refit_tbl <- model_tbl %>%
        modeltime_refit(data)

    calib_tbl <- refit_tbl %>%
        modeltime_calibrate(
            data
        )

    testthat::expect_equal(data$value, calib_tbl$.calibration_data[[1]]$.actual)

})
