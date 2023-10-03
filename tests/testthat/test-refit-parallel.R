

test_that("refit works in parallel", {

    skip_on_cran()
    m4_monthly_jumbled <- timetk::m4_monthly %>%
        arrange(desc(date))

    data_set <- m4_monthly_jumbled

    recipe_spec <- recipes::recipe(value ~ date + id, data_set) %>%
        recipes::step_mutate(date_num = as.numeric(date)) %>%
        recipes::step_mutate(month_lbl = lubridate::month(date, label = TRUE)) %>%
        recipes::step_dummy(all_nominal(), one_hot = TRUE)

    set.seed(123)
    wflw_fit_prophet <- workflows::workflow() %>%
        workflows::add_model(
            prophet_boost(
                seasonality_yearly = F,
                seasonality_weekly = F,
                seasonality_daily  = F
            ) %>%
                parsnip::set_engine("prophet_xgboost")) %>%
        workflows::add_recipe(recipe_spec) %>%
        fit(data_set)

    skip_if_not_installed("kernlab")
    set.seed(123)
    wflw_fit_svm <- workflows::workflow() %>%
        workflows::add_model(svm_rbf("regression") %>% parsnip::set_engine("kernlab")) %>%
        workflows::add_recipe(recipe_spec %>% update_role(date, new_role = "ID")) %>%
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
                cores     = 2 # R CMD CHECK ONLY ALLOWS 2 CORES MAX
            )
        )

    expect_equal(nrow(refit_tbl), 2)

})
