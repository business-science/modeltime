
# modeltime 0.0.2.9000 (Development Version)

### New Features 

__Forecast without Calibration/Refitting__

Sometimes it's important to make fast forecasts without calculating out-of-sample accuracy and refitting (which requires 2 rounds of model training). You can now bypass the `modeltime_calibrate()` and `modeltime_refit()` steps and jump straight into forecasting the future. Here's an example with `h = "3 years"`. Note that __you will not get confidence intervals__ with this approach because calibration data is needed for this. 

``` r
# Make forecasts without calibration/refitting (No Confidence Intervals)
# - This assumes the models have been trained on m750
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

__Residual Analysis & Diagonstics__

A common tool when forecasting and analyzing residuals, where residuals are `.resid = .actual - .prediction`. The residuals may have autocorrelation or nonzero mean, which can indicate model improvement opportunities. In addition, users may which to inspect in-sample and out-of-sample residuals, which can display different results. 

- `modeltime_residuals()` - A new function used to extract out residual information
- `plot_modeltime_residuals()` - Visualizes the output of `modeltime_residuals()`. Offers 3 plots:
    1. __Time Plot__ - Residuals over time
    2. __ACF Plot__ - Residual Autocorrelation vs Lags
    3. __Seasonality__ - Residual Seasonality Plot

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

__Prophet Model - Logistic Growth Support__

- `prophet_reg()` and `prophet_boost()`: 
    - Now supports logistic growth. Set `growth = 'logistic'` and one or more of `logistic_cap` and `logistic_floor` to valid saturation boundaries.
    - New arguments making it easier to modify the `changepoint_num`, `changepoint_range`, `seasonality_yearly`, `seasonality_weekly`, `seasonality_daily`, `logistic_cap`, `logistic_floor`

### New Workflow Helper Functions

- `combine_modeltime_tables()` - A helper function making it easy to combine multiple modeltime tables.
- `update_model_description()` - A helper function making it easier to update model descriptions. 

### Improvements

- `modeltime_refit()`: When modeltime model parameters update (e.g. when Auto ARIMA changes to a new model), the Model Description now alerts the user (e.g. "UPDATE: ARIMA(0,1,1)(1,1,1)[12]").

- `modeltime_calibrate()`: When training data is supplied in a time window that the model has previously been trained on (e.g. `training(splits)`), the calibration calculation first inspects whether the "Fitted" data exists. If it iexists, it returns the "Fitted" data. This helps prevent sequence-based (e.g. ARIMA, ETS, TBATS models) from displaying odd results because these algorithms can only predict sequences directly following the training window. If "Fitted" data is being used, the `.type` column will display "Fitted" instead of "Test".

### Bug Fixes

- `modeltime_forecast()`: 
    - Implement `actual_data` reconciliation strategies when recipe removes rows. Strategy attempts to fill predictors using "downup" strategy to prevent `NA` values from removing rows. 
    - More descriptive errors when external regressors are required. 
    
- `modeltime_accuracy()`: Fix issue with `new_data` not recalibrating. 

- `prophet_reg()` and `prophet_boost()` - Can now perform logistic growth `growth = 'logistic'`. The user can supply "saturation" bounds using `logistic_cap` and/or `logisitc_floor`. 
    

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
