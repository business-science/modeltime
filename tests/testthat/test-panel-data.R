context("PANEL DATA")

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



test_that("Forecast Jumbled", {

    forecast_tbl <- modeltime_table(
        wflw_fit_prophet,
        wflw_fit_svm
    ) %>%
        modeltime_forecast(
            new_data       = data_set,
            actual_data    = m4_monthly_jumbled,
            keep_data      = TRUE,
            arrange_index  = FALSE
        )

    actual_tbl <- forecast_tbl %>%
        filter(.key == "actual")

    expect_equal(actual_tbl$.value, actual_tbl$value)

    svm_tbl <- forecast_tbl %>%
        filter(.model_id == 2)

    expect_equal(svm_tbl$.index, svm_tbl$date)

    # forecast_tbl %>%
    #     ggplot(aes(.index, .value, color = id)) +
    #     geom_line() +
    #     facet_wrap(~ id + .model_desc)

})


