context("TEST TUNE WORKFLOWS")

# TESTS ----

# 1. ARIMA BOOST ----
test_that("Tuning, arima_boost", {

    skip_on_cran()

    #

    m750 <- timetk::m4_monthly %>% dplyr::filter(id == "M750")

    # RESAMPLE SPEC ----
    resample_spec <- time_series_cv(data = m750,
                                    initial     = "10 years",
                                    assess      = "2 years",
                                    skip        = "2 years",
                                    cumulative  = FALSE,
                                    slice_limit = 2)

    # Recipe
    recipe_spec <- recipes::recipe(value ~ date, data = m750) %>%
        recipes::step_mutate(as.numeric(date)) %>%
        recipes::step_date(date, features = "month")

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
        min_n      = tune::tune()
    ) %>%
        parsnip::set_engine("arima_xgboost")

    # Grid
    set.seed(3)
    grid_spec <- dials::grid_latin_hypercube(
        parameters(min_n()),
        size = 3
    )

    parallel_start(2)

    # Tune
    # This fails if no previous versions of modeltime exist.
    tune_results_boosted <- workflows::workflow() %>%
        workflows::add_recipe(recipe_spec) %>%
        workflows::add_model(model_spec) %>%
        tune::tune_grid(
            resamples = resample_spec,
            grid      = grid_spec,
            metrics   = default_forecast_accuracy_metric_set(),
            control   = tune::control_grid(
                verbose   = TRUE,
                allow_par = TRUE,
            )
        )

    parallel_stop()


    # structure
    expect_equal(ncol(tune_results_boosted), 4)

    tune_results_boosted_metrics <- tune_results_boosted %>%
        dplyr::select(.metrics) %>%
        tidyr::unnest(.metrics)

    expect_equal(nrow(tune_results_boosted_metrics), 36)

})


