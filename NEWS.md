
# modeltime 0.0.2.9000 (Development Version)

### New Features 

__Forecast without Claibration/Refitting__

Sometimes it's important to make fast forecasts without calculating out-of-sample accuracy and refitting (which requires 2 rounds of model training). You can now bypass the `modeltime_calibrate()` and `modeltime_refit()` steps and jump straight into forecasting the future. Here's an example with `h = "3 years"`. Note that you will not get confidence intervals with this approach because calibration data is needed for this. 

``` r
# Make forecasts without calibration/refitting
modeltime_table(
    model_fit_prophet,
    model_fit_lm
) %>%
    modeltime_forecast(
        h = "3 years",
        actual_data = m750
    ) %>%
    plot_modeltime_forecast(.conf_interval_show = F)
```

### New Models

__TBATS Model__

Use `seasonal_reg()` and set engine to "tbats".

``` r
seasonal_reg(
    seasonal_period_1 = "1 day",
    seasonal_period_2 = "1 week"
) %>% 
    set_engine("tbats")
```

### New Functions

- `combine_modeltime_tables()` - A helper function making it easy to combine multiple modeltime tables.
- `update_model_description()` - A helper function making it easier to update model descriptions. 

### Improvements

- `prophet_reg()` and `prophet_boost()`: New arguments making it easier to modify the `changepoint_num`, `changepoint_range`, `seasonality_yearly`, `seasonality_weekly`, and `seasonality_daily`

- `modeltime_refit()`: When modeltime model parameters update (e.g. when Auto ARIMA changes to a new model), the Model Description now alerts the user (e.g. "UPDATE: ARIMA(0,1,1)(1,1,1)[12]").

### Bug Fixes

- `modeltime_forecast()`: 
    - Implement `actual_data` reconciliation strategies when recipe removes rows. Strategy attempts to fill predictors using "downup" strategy to prevent `NA` values from removing rows. 
    - More descriptive errors when external regressors are required. 
    

### Breaking Changes

- `seasonal_decomp()` has changed to `seasonal_reg()` and now supports both TBATS and Seasonal Decomposition Models.
- `prophet_reg()` & `prophet_boost()`: Argument changes:
    - `num_changepoints` has become `changepoint_num`

# modeltime 0.0.2

### Confidence Interval Estimation

- `modeltime_forecast()`: Now estimates confidence intervals using centered standard deviation. The mean is assumed to be zero and residuals deviate from mean = 0. 

### Fixes

- Updates to work with `parsnip` 0.1.2.
- `prophet_boost()`: Set `nthreads = 1` (default) to ensure parallelization is thread safe. 

# modeltime 0.0.1

* Initial Release
