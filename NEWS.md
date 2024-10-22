# modeltime 1.3.1.9000 (Development Version)

# modeltime 1.3.1

Parallel Computation:
- `parallel_start()`: New parameters `.export_vars` and `.packages` allows passing environment variables and packages to the parallel workers. 

Fixes:
- Adam (`adam_reg()`): Fixes #254 
- Adam: Add new dials parameters: `ets_model` and `loss`

# modeltime 1.3.0

#### Overview

This version and modeltime 1.2.8 (previous version) include changes to incorporate Conformal Prediction Intervals. There are a number of changes that include new "conformal" confidence methods and Tibble (Data Frame) table display improvements of forecasts aimed at helping the user understand what confidence method is being used and the confidence interval being used throughout the forecasting process in both Standard and Nested Modeltime Forecasting Workflows. 

#### Conformal Predictions:

- Integrate Conformal Predictions into Nested Forecast Workflow: `modeltime_nested_fit()` and `modeltime_nested_refit()`. #173
- Updated the `print` display for conformal prediction Conf Method, Conf Interval:
   - `modeltime_forecast()`
   - `extract_nested_test_forecast()`
   - `extract_nested_future_forecast()`
   - `modeltime_nested_forecast()`

#### Other Changes:

- Dials Parameters: Remove deprecated `default` inside `new_qual_param()`.
- Fix warning in dev-xregs: Use `all_of()` inside `prepare_xreg_recipe_from_predictors()`
- Fix broken test: `test-tune_workflows` Unused argument: `cores = 2`

# modeltime 1.2.8

- Integrate Conformal Predictions into Standard Modeltime Forecast Workflow: `modeltime_forecast()` #173
- New Vignette: Conformal Forecast Prediction Intervals in Modeltime


#### Other Changes:

- Reduced test times on CRAN
- CRAN Vignettes & Tests: Enforce no parallel cores `Sys.setenv("OMP_THREAD_LIMIT" = 1)`
- Change the default parallel processing to one (1) core from all available cores (-1):
    - `control_refit()`
    - `control_fit_workflowset()`
    - `control_nested_fit()`
    - `control_nested_refit()`
    - `control_nested_forecast()`

# modeltime 1.2.7

- Fixes for R4.3+ which returns `lm` models as `pred_res`. #228

# modeltim 1.2.6

- Fixes to get `modeltime` back on CRAN following inadvertent `timetk` archival. 

# modeltime 1.2.5

- Fixes for Smooth `es()` model #221

# modeltime 1.2.4

- Fix failing tests in test-developer-tools-xregs.R

# modeltime 1.2.3

- Recursive `chunk_size` (performance improvement) #197 #190
- Recursive model fixes #194, #188, #187, #174
- New function, `drop_modeltime_model` #160
- Updates for `workflows` mode = "regression"

# modeltime 1.2.2

### Fixes

- Updates for `hardhat 1.0.0` #182

# modeltime 1.2.1

### Trelliscope Plotting

- `plot_modeltime_forecast()`: Expose the `facet_trelliscope()` plotting parameters. 

### Fixes

- Use `step_rm()` to get rid of date rather than updating its role #181

# modeltime 1.2.0 

__New Features__

Many of the plotting functions have been upgraded for use with `trelliscopejs` for 
easier visualization of many time series. 

- `plot_modeltime_forecast()`: 
    - Gets a new argument `trelliscope`: Used for visualizing many time series.
    - Gets a new argument `.facet_strip_remove` to remove facet strips since trelliscope is automatically labeled.
    - Gets a new argument `.facet_nrow` to adjust grid with trelliscope.
    - The default argument for `facet_collapse = TRUE` was changed to `FALSE` for better compatibility with Trelliscope JS. This may cause some plots to have multiple groups take up extra space in the strip.


# modeltime 1.1.1

## Fixes

- Fixes issue of incorrect order of forecasts #142

# modeltime 1.1.0 

## Spark Backend

- Modeltime now has a Spark Backend 

- [NEW Vignette - Modeltime Spark Backend](https://business-science.github.io/modeltime/articles/modeltime-spark.html) describing how to set up Modeltime with the Spark Backend. 

## New Algorithms: Smooth Package Integration

If users install `smooth`, the following models become available:

- `adam_reg()`: Interfaces with the ADAM forecasting algorithm in `smooth`. 

- `exp_smoothing()`: A new engine "smooth_es" connects to the Exponential Smoothing algorithm in `smooth::es()`. This algorithm has several advantages, most importantly that it can use x-regs (unlike "ets" engine).

## Nested Modeltime Improvements

- New extractor: `extract_nested_modeltime_table()` - Extracts a nested modeltime table by row id. 

## (potentially) Breaking Changes

- `extract_nested_train_split` and `extract_nested_test_split`: Changed parameter from `.data` to `.object` for consistency with other "extract" functions

- Added a new logged feature to `modeltime_nested_fit()` to track the attribute "metric_set", which is needed for ensembles. Old nested modeltime objects will need to be re-run to get this new attribute. This will be used in ensembles. 

# modeltime 1.0.0

### New Feature: Nested (Iterative) Forecasting

__Nested (Iterative) Forecasting__ is aimed at making it easier to perform forecasting that is traditionally done in a _for-loop_ with models like ARIMA, Prophet, and Exponential Smoothing. Functionality has been added to:

#### Format data in a Nested Time Series structure

- __Data Preparation Utilities:__ `extend_timeseries()`, `nest_timeseries()`, and `split_nested_timeseris()`.

#### Nested Model Fitting (Train/Test)

- __`modeltime_nested_fit()`:__ Fits many models to nested time series data and organizes in a "Nested Modeltime Table". Logs Accuracy, Errors, and Test Forecasts. 

- __`control_nested_fit()`:__ Used to control the fitting process including verbosity and parallel processing. 

- __Logging Extractors:__ Functions that retrieve logged information from the initial fitting process. `extract_nested_test_accuracy()`, `extract_nested_error_report()`, and `extract_nested_test_forecast()`.

#### Nested Model Selection

- __`modeltime_nested_select_best()`__: Selects the best model for each time series ID. 

- __Logging Extractors:__ Functions that retrieve logged information from the model selection process. `extract_nested_best_model_report()`


#### Nested Model Refitting (Actual Data)

- __`modeltime_nested_refit()`:__ Refits to the `.future_data`. Logs Future Forecasts. 

- __`control_nested_refit()`:__ Used to control the re-fitting process including verbosity and parallel processing. 

- __Logging Extractors:__ Functions that retrieve logged information from the re-fitting process. `extract_nested_future_forecast()`.

### New Vignette

- [Nested Forecasting](https://business-science.github.io/modeltime/articles/nested-forecasting.html) 

### Vignette Improvements

- [Forecasting with Global Models](https://business-science.github.io/modeltime/articles/modeling-panel-data.html): Added more complete steps in the forecasting process so now user can see how to forecast each step from start to finish including future forecasting.  


### New Accuracy Metric Set and Yardstick Functions

- `extended_forecast_accuracy_metric_set()`: Adds the new MAAPE metric for handling intermittent data when MAPE returns Inf. 
- `maape()`: New yardstick metric that calculates "Mean Arctangent Absolute Percentage Error" (MAAPE). Used when MAPE returns Inf typically due to intermittent data. 

### Improvements

- `modeltime_fit_workflowset()`: Improved handling of Workflowset Descriptions, which now match the `wflow_id`. 

# modeltime 0.7.0 

### Group-Wise Accuracy and Confidence Interval by Time Series ID

We've expanded Panel Data functionality to produce model accuracy and confidence interval estimates by a Time Series ID (#114). This is useful when you have a Global Model that produces forecasts for more than one time series. You can more easily obtain grouped accuracy and confidence interval estimates. 

* `modeltime_calibrate()`: Gains an `id` argument that is a quoted column name. This identifies that the residuals should be tracked by an time series identifier feature that indicates the time series groups. 

* `modeltime_accuracy()`: Gains a `acc_by_id` argument that is `TRUE`/`FALSE`. If the data has been calibrated with `id`, then the user can return local model accuracy by the identifier column. The accuracy data frame will return a row for each combination of Model ID and Time Series ID. 

* `modeltime_forecast()`: Gains a `conf_by_id` argument that is `TRUE`/`FALSE`. If the data has been calibrated with `id`, then the user can return local model confidence by the identifier column. The forecast data frame will return an extra column indicating the identifier column. The confidence intervals will be adjusted based on the local time series ID variance instead of the global model variance. 

### New Vignette

[Forecasting Panel Data](https://business-science.github.io/modeltime/articles/modeling-panel-data.html) 

### New Algorithms

#### THIEF: Temporal Hierarchical Forecasting

- `temporal_hierarchy()`: Implements the `thief` package by Rob Hyndman and
Nikolaos Kourentzes for "Temporal HIErarchical Forecasting". #117

### Bug Fixes

- Issue #111: Fix bug with `modeltime_fit_workflowset()` where the workflowset (wflw_id) order was not maintained. 

# modeltime 0.6.1 

__Parallel Processing__

- New Vignette: [Parallel Processing](https://business-science.github.io/modeltime/articles/parallel-processing.html)

- `parallel_start()` and `parallel_stop()`: Helpers for setting up multicore processing. 

- `create_model_grid()`: Helper to generate model specifications with filled-in parameters from a parameter grid (e.g. `dials::grid_regular()`).

- `control_refit()` and `control_fit_workflowset()`: Better printing. 

__Bug Fixes__

- Issue #110: Fix bug with `cores > cores_available`.

# modeltime 0.6.0 

### Workflowset Integration

`modeltime_fit_workflowset()` (#85) makes it easy to convert `workflow_set` objects to Modeltime Tables (`mdl_time_tbl`). Requires a refitting process that can now be performed in parallel or in sequence. 


### New Algorithms

- CROSTON (#5, #98) - This is a new engine that has been added to `exp_smoothing()`. 
- THETA (#5, #93) - This is a new engine that has been added to `exp_smoothing()`.

### New Dials Parameters

`exp_smoothing()` gained 3 new tunable parameters:

- `smooth_level()`: This is often called the "alpha" parameter used as the base level smoothing factor for exponential smoothing models.
- `smooth_trend()`: This is often called the "beta" parameter used as the trend smoothing factor for exponential smoothing models.
- `smooth_seasonal()`: This is often called the "gamma" parameter used as the seasonal smoothing factor for exponential smoothing models.

### Parallel Processing

- `modeltime_refit()`: supports parallel processing. See `control_refit()` 
- `modeltime_fit_workflowset()`: supports parallel processing. See `control_workflowset()` 

### Updates for parsnip >= 0.1.6

- `boost_tree(mtry)`: Mapping switched from `colsample_bytree` to `colsample_bynode`. `prophet_boost()` and `arima_boost()` have been updated to reflect this change.  https://github.com/tidymodels/parsnip/pull/499

### General Improvements

- Improve Model Description of Recursive Models (#96)

### Potential Breaking Changes

- We've added new parameters to Exponential Smoothing Models. `exp_smoothing()` models produced in prior versions may require refitting with `modeltime_refit()` to upgrade their internals with the new parameters. 

# modeltime 0.5.1 

### Recursive Ensemble Predictions

- Add support for `recursive()` for ensembles. The new recursive ensemble functionality is in `modeltime.ensemble` >= 0.3.0.9000.

# modeltime 0.5.0

### Recursive Panel Predictions

- `recursive()` (#71) - Received a full upgrade to work with Panel Data. 
- __New Vignette__: ["Autoregressive Forecasting with Recursive"](https://business-science.github.io/modeltime/articles/recursive-forecasting.html)

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
