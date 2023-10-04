context("Modeltime Residuals")


test_that("modeltime_residuals(): Returns correct order", {

    skip_on_cran()

    library(tidymodels)
    library(dplyr)
    library(timetk)


    data <- walmart_sales_weekly %>%
        dplyr::select(ID = id, date = Date, value = Weekly_Sales)


    splits <- data %>% time_series_split(assess = "3 months", cumulative = TRUE)


    rec_obj <- recipes::recipe(value ~ ., rsample::training(splits)) %>%
        recipes::step_mutate(ID = droplevels(ID)) %>%
        step_timeseries_signature(date) %>%
        recipes::step_rm(date) %>%
        recipes::step_zv(recipes::all_predictors()) %>%
        recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE)



    # Workflow
    wflw_xgb <- workflows::workflow() %>%
        workflows::add_model(
            boost_tree("regression") %>% parsnip::set_engine("xgboost")
        ) %>%
        workflows::add_recipe(rec_obj) %>%
        fit(rsample::training(splits))

    skip_if_not_installed("glmnet")
    wflw_glmnet <- workflows::workflow() %>%
        workflows::add_model(
            linear_reg(penalty = 1) %>%
                parsnip::set_engine("glmnet")
        ) %>%
        workflows::add_recipe(rec_obj) %>%
        fit(rsample::training(splits))


    model_tbl <- modeltime_table(
        wflw_xgb,
        wflw_glmnet
    )


    i <- 1

    # Order is not changed for the training set
    calib_tbl_train <- model_tbl %>%
        modeltime_calibrate(
            rsample::training(splits)
        )

    expect_equal(rsample::training(splits)$value, calib_tbl_train$.calibration_data[[1]]$.actual)


    # Usage of the full data set
    # if i want to refit and than I use calibrate row order is changed
    refit_tbl <- model_tbl %>%
        modeltime_refit(data)

    calib_tbl <- refit_tbl %>%
        modeltime_calibrate(
            data
        )

    expect_equal(data$value, calib_tbl$.calibration_data[[1]]$.actual)

})
