# 0.4.2.9000

### Recursive Predictions

- `recursive_panel()` (#71) - Amazing NEW function that enables __recursive prediction on Panel Data!__
- New Vignette on Recursive Forecasting

### Breaking Changes

- Deprecating `modeltime::metric_tweak()` for `yardstick::metric_tweak()`. The `yardstick::metric_tweak()` has a required `.name` argument in addition to `.fn`, which is needed for tuning. 

# modeltime 0.4.2

### New Algorithms

Baseline algorithms (#5, #37) have been created for comparing high-performance methods with simple forecasting methods. 

- `window_reg`: Window-based methods such as mean, median, and even more complex seasonal models based on a forecasting window. The main tuning parameter is `window_size`.  
- `naive_reg`: NAIVE and Seasonal NAIVE (SNAIVE) Regression Models

### Yardstick Helpers

- `metric_tweak()` - Can modify `yardstick` metrics like `mase()`, which have seasonal parameters. 
- `default_forecast_accuracy_metric_set()` - Gets a `...` parameter that allows us to add more metrics beyond the defaults. 

### Modeltime Residual Tests

A new function is added `modeltime_residuals_test()` (#62, #68). Tests are implemented:

- Shapiro Test - Test for Normality of residuals
- Box-Pierce, Ljung-Box, and Durbin-Watson Tests - Test for Autocorrelation of residuals

### Fixes

- `plot_modeltime_forecast()` - When plotting a single point forecast, `plot_modeltime_forecast()` now uses `geom_point()` instead of `geom_line()`. Fixes #66. 

# modeltime 0.4.1 

__Fixes__

- `recursive()` & `modeltime_refit()`: Now able to refit a recursive workflow or recursive fitted parsnip object. 

# modeltime 0.4.0

__New Functions__

- `recursive()`: Turn a fitted model into a recursive predictor. (#49, #50)
- `update_modeltime_model()`: New function to update a modeltime model inside a Modeltime Table. 

__Breaking Changes__

- Removed `arima_workflow_tuned` dataset. 

# modeltime 0.3.1

`as_modeltime_table()`: New function to convert one or more fitted models stored in a `list` to a Modeltime Table. 

__Bug Fixes__

- Update `m750_models`: Fixes error "R parsnip Error: Internal error: Unknown `composition` type."



# modeltime 0.3.0

__Panel Data__

`modeltime_forecast()` upgrades: 

- `keep_data`: Gains a new argument `keep_data`. This is useful when the `new_data` and `actual_data` has important information needed in analyzing the forecast.
- `arrange_index`: Gains a new argument `arrange_index`. By default, the forecast keeps the rows in the same order as the incoming data. Prior versions arranged Model Predictions by `.index`, which impacts the users ability to match to Panel Data which is not likely to be arranged by date. Prediction best-practices are to keep the original order of the data, which 
will be preserved by default. To get the old behavior, simply toggle `arrange_index = TRUE`. 

`modeltime_calibrate()`: Can now handle panel data.

`modeltime_accuracy()`: Can now handle panel data. 

`plot_modeltime_forecast()`: Can handle panel data provided the data is grouped by an ID column prior to plotting. 

__Error Messaging__

- __Calibration:__ Improve error messaging during calibration. Provide warnings if models fail. Provide report with `modeltime_calibrate(quiet = FALSE)`.

__Compatibility__

- Compatibility with `parsnip >= 0.1.4`. Uses `set_encodings()` new parameter `allow_sparse_x`. 

# modeltime 0.2.1

__Ensembles__

- `modeltime_refit()` - Changes to improve fault tolerance and error handling / messaging when making ensembles. 

# modeltime 0.2.0

__Ensembles__

- Integrates `modeltime.ensemble`, a new R package designed for forecasting with ensemble models. 

___New Workflow Helper Functions___

- `add_modeltime_model()` - A helper function making it easy to add a fitted parsnip or workflow object to a modeltime table
- `pluck_modeltime_model()` & `pull_modeltime_model()` - A helper function making it easy to extract a model from a modeltime table 


__Improvements__

- Documentation - Algorithms now identify default parameter values in the  "Engine Details" Section in their respective documentation. E.g. `?prophet_boost`
- `prophet_reg()` can now have regressors controlled via `set_engine()` using the following parameters:
    - `regressors.mode` - Set to `seasonality.mode` by default.
    - `regressors.prior.scale` - Set to 10,000 by default.
    - `regressors.standardize` - Set to "auto" by default. 
    
__Data Sets__

Modeltime now includes 4 new data sets:

- `m750` - M750 Time Series Dataset
- `m750_models` - 3 Modeltime Models made on the M750 Dataset
- `m750_splits` - An `rsplit` object containing Train/test splits of the M750 data
- `m750_training_resamples` - A Time Series Cross Validation `time_series_cv` object made from the `training(m750_splits)`

__Bug Fix__

- `plot_modeltime_forecast()` fix issue with "ACTUAL" data being shown at bottom of legend list. Should be first item. 

# modeltime 0.1.0 

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

__NNETAR Model__

Use `nnetar_reg()` and set engine to "nnetar".

``` r
model_fit_nnetar <- nnetar_reg() %>%
    set_engine("nnetar") 
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
