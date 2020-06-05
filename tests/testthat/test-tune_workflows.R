context("TEST TUNE WORKFLOWS")

m750 <- m4_monthly %>% filter(id == "M750")

# RESAMPLE SPEC ----
resample_spec <- time_series_cv(data = m750,
                                initial     = "10 years",
                                assess      = "2 years",
                                skip        = "2 years",
                                cumulative  = FALSE,
                                slice_limit = 2)


# 1. ARIMA BOOST ----

# Recipe
recipe_spec <- recipe(value ~ date, data = m750) %>%
    step_mutate(as.numeric(date)) %>%
    step_date(date, features = "month")

# Model
model_spec <- arima_boost(

    # ARIMA(1,0,0)(0,1,0)[12]
    seasonal_period          = "none",
    non_seasonal_ar          = 1,
    non_seasonal_differences = 0,
    non_seasonal_ma          = 0,
    seasonal_ar              = 0,
    seasonal_differences     = 1,
    seasonal_ma              = 0,

    # XGBoost Tuning Params
    min_n      = tune(),
    learn_rate = tune()
) %>%
    set_engine("arima_xgboost")

# Grid
set.seed(3)
grid_spec <- grid_latin_hypercube(
    parameters(min_n(), learn_rate()),
    size = 3
)

# Tune
tune_results_boosted <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec) %>%
    tune_grid(
        resamples = resample_spec,
        grid      = grid_spec,
        metrics   = metric_set(mae, mape, smape, mase, rmse, rsq),
        control   = control_grid(verbose = FALSE)
    )

# Tests
test_that("Tuning, arima_boost", {

    # structure
    expect_equal(ncol(tune_results_boosted), 4)

    tune_results_boosted_metrics <- tune_results_boosted %>%
        select(.metrics) %>%
        unnest(.metrics)

    expect_equal(nrow(tune_results_boosted_metrics), 36)

})


