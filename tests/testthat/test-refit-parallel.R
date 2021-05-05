

testthat::test_that("refit works in parallel", {

    testthat::skip_on_cran()

    m4_monthly_jumbled <- m4_monthly %>%
        arrange(desc(date))

    data_set <- m4_monthly_jumbled

    recipe_spec <- recipe(value ~ date + id, data_set) %>%
        step_mutate(date_num = as.numeric(date)) %>%
        step_mutate(month_lbl = lubridate::month(date, label = TRUE)) %>%
        step_dummy(all_nominal(), one_hot = TRUE)

    set.seed(123)
    wflw_fit_prophet <- workflow() %>%
        add_model(
            prophet_boost(
                seasonality_yearly = F,
                seasonality_weekly = F,
                seasonality_daily  = F
            ) %>%
                set_engine("prophet_xgboost")) %>%
        add_recipe(recipe_spec) %>%
        fit(data_set)

    set.seed(123)
    wflw_fit_svm <- workflow() %>%
        add_model(svm_rbf() %>% set_engine("kernlab")) %>%
        add_recipe(recipe_spec %>% update_role(date, new_role = "ID")) %>%
        fit(data_set)


    model_tbl <- modeltime_table(
        wflw_fit_prophet,
        wflw_fit_svm
    )

    refit_tbl <- model_tbl %>%
        modeltime_refit(
            data = m4_monthly_jumbled,
            control = control_refit(
                verbose   = TRUE,
                allow_par = TRUE,
                cores     = 2 # R CMD CHECK ONLY ALLOWS 2
            )
        )

    testthat::expect_equal(nrow(refit_tbl), 2)

})
