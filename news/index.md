# Changelog

## modeltime 1.3.3.9000

- Updated
  [`parallel_start()`](https://business-science.github.io/modeltime/reference/parallel_start.md)
  /
  [`parallel_stop()`](https://business-science.github.io/modeltime/reference/parallel_start.md)
  to track the PSOCK cluster created by `.method = "parallel"` and
  reliably close it in
  [`parallel_stop()`](https://business-science.github.io/modeltime/reference/parallel_start.md)
  (and before creating a new one), preventing the “closing unused
  connection …” warnings/leaks.

## modeltime 1.3.3

CRAN release: 2025-12-17

- Make package robust to `xgboost` version changes
  [\#263](https://github.com/business-science/modeltime/issues/263)
- Correct (reproducible parallel RNG): declare RNG properly

## modeltime 1.3.2

CRAN release: 2025-08-28

- Parallel Backends: Add `future` support; prepare for `tune` `foreach`
  deprecation; Prep
  [`parallel_start()`](https://business-science.github.io/modeltime/reference/parallel_start.md)
  and `parallel_stop`.
- `yardstick` deprecations: Add patches for
  [`metric_summarizer()`](https://yardstick.tidymodels.org/reference/metric_summarizer.html),
  [`metric_vec_template()`](https://yardstick.tidymodels.org/reference/metric_vec_template.html)
- `dials` deprecations: `grid_latin_hypercube()` -\>
  `grid_space_filling()`
- Fix Tests: adam_reg
- Make package work with all versions of xgboost.
  ([\#263](https://github.com/business-science/modeltime/issues/263))

## modeltime 1.3.1

CRAN release: 2024-10-22

Parallel Computation: -
[`parallel_start()`](https://business-science.github.io/modeltime/reference/parallel_start.md):
New parameters `.export_vars` and `.packages` allows passing environment
variables and packages to the parallel workers.

Fixes: - Adam
([`adam_reg()`](https://business-science.github.io/modeltime/reference/adam_reg.md)):
Fixes
[\#254](https://github.com/business-science/modeltime/issues/254) -
Adam: Add new dials parameters: `ets_model` and `loss`

## modeltime 1.3.0

CRAN release: 2024-07-29

##### Overview

This version and modeltime 1.2.8 (previous version) include changes to
incorporate Conformal Prediction Intervals. There are a number of
changes that include new “conformal” confidence methods and Tibble (Data
Frame) table display improvements of forecasts aimed at helping the user
understand what confidence method is being used and the confidence
interval being used throughout the forecasting process in both Standard
and Nested Modeltime Forecasting Workflows.

##### Conformal Predictions:

- Integrate Conformal Predictions into Nested Forecast Workflow:
  [`modeltime_nested_fit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_fit.md)
  and
  [`modeltime_nested_refit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_refit.md).
  [\#173](https://github.com/business-science/modeltime/issues/173)
- Updated the `print` display for conformal prediction Conf Method, Conf
  Interval:
  - [`modeltime_forecast()`](https://business-science.github.io/modeltime/reference/modeltime_forecast.md)
  - [`extract_nested_test_forecast()`](https://business-science.github.io/modeltime/reference/log_extractors.md)
  - [`extract_nested_future_forecast()`](https://business-science.github.io/modeltime/reference/log_extractors.md)
  - [`modeltime_nested_forecast()`](https://business-science.github.io/modeltime/reference/modeltime_nested_forecast.md)

##### Other Changes:

- Dials Parameters: Remove deprecated `default` inside
  `new_qual_param()`.
- Fix warning in dev-xregs: Use
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
  inside `prepare_xreg_recipe_from_predictors()`
- Fix broken test: `test-tune_workflows` Unused argument: `cores = 2`

## modeltime 1.2.8

CRAN release: 2023-09-02

- Integrate Conformal Predictions into Standard Modeltime Forecast
  Workflow:
  [`modeltime_forecast()`](https://business-science.github.io/modeltime/reference/modeltime_forecast.md)
  [\#173](https://github.com/business-science/modeltime/issues/173)
- New Vignette: Conformal Forecast Prediction Intervals in Modeltime

##### Other Changes:

- Reduced test times on CRAN
- CRAN Vignettes & Tests: Enforce no parallel cores
  `Sys.setenv("OMP_THREAD_LIMIT" = 1)`
- Change the default parallel processing to one (1) core from all
  available cores (-1):
  - [`control_refit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)
  - [`control_fit_workflowset()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)
  - [`control_nested_fit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)
  - [`control_nested_refit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)
  - [`control_nested_forecast()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)

## modeltime 1.2.7

CRAN release: 2023-07-03

- Fixes for R4.3+ which returns `lm` models as `pred_res`.
  [\#228](https://github.com/business-science/modeltime/issues/228)

## modeltime 1.2.5

CRAN release: 2023-02-07

- Fixes for Smooth [`es()`](https://rdrr.io/pkg/smooth/man/es.html)
  model
  [\#221](https://github.com/business-science/modeltime/issues/221)

## modeltime 1.2.4

CRAN release: 2022-11-16

- Fix failing tests in test-developer-tools-xregs.R

## modeltime 1.2.3

CRAN release: 2022-10-18

- Recursive `chunk_size` (performance improvement)
  [\#197](https://github.com/business-science/modeltime/issues/197)
  [\#190](https://github.com/business-science/modeltime/issues/190)
- Recursive model fixes
  [\#194](https://github.com/business-science/modeltime/issues/194),
  [\#188](https://github.com/business-science/modeltime/issues/188),
  [\#187](https://github.com/business-science/modeltime/issues/187),
  [\#174](https://github.com/business-science/modeltime/issues/174)
- New function, `drop_modeltime_model`
  [\#160](https://github.com/business-science/modeltime/issues/160)
- Updates for `workflows` mode = “regression”

## modeltime 1.2.2

CRAN release: 2022-06-07

#### Fixes

- Updates for `hardhat 1.0.0`
  [\#182](https://github.com/business-science/modeltime/issues/182)

## modeltime 1.2.1

CRAN release: 2022-06-01

#### Trelliscope Plotting

- [`plot_modeltime_forecast()`](https://business-science.github.io/modeltime/reference/plot_modeltime_forecast.md):
  Expose the `facet_trelliscope()` plotting parameters.

#### Fixes

- Use
  [`step_rm()`](https://recipes.tidymodels.org/reference/step_rm.html)
  to get rid of date rather than updating its role
  [\#181](https://github.com/business-science/modeltime/issues/181)

## modeltime 1.2.0

CRAN release: 2022-04-07

**New Features**

Many of the plotting functions have been upgraded for use with
`trelliscopejs` for easier visualization of many time series.

- [`plot_modeltime_forecast()`](https://business-science.github.io/modeltime/reference/plot_modeltime_forecast.md):
  - Gets a new argument `trelliscope`: Used for visualizing many time
    series.
  - Gets a new argument `.facet_strip_remove` to remove facet strips
    since trelliscope is automatically labeled.
  - Gets a new argument `.facet_nrow` to adjust grid with trelliscope.
  - The default argument for `facet_collapse = TRUE` was changed to
    `FALSE` for better compatibility with Trelliscope JS. This may cause
    some plots to have multiple groups take up extra space in the strip.

## modeltime 1.1.1

CRAN release: 2022-01-12

### Fixes

- Fixes issue of incorrect order of forecasts
  [\#142](https://github.com/business-science/modeltime/issues/142)

## modeltime 1.1.0

CRAN release: 2021-10-18

### Spark Backend

- Modeltime now has a Spark Backend

- [NEW Vignette - Modeltime Spark
  Backend](https://business-science.github.io/modeltime/articles/modeltime-spark.html)
  describing how to set up Modeltime with the Spark Backend.

### New Algorithms: Smooth Package Integration

If users install `smooth`, the following models become available:

- [`adam_reg()`](https://business-science.github.io/modeltime/reference/adam_reg.md):
  Interfaces with the ADAM forecasting algorithm in `smooth`.

- [`exp_smoothing()`](https://business-science.github.io/modeltime/reference/exp_smoothing.md):
  A new engine “smooth_es” connects to the Exponential Smoothing
  algorithm in [`smooth::es()`](https://rdrr.io/pkg/smooth/man/es.html).
  This algorithm has several advantages, most importantly that it can
  use x-regs (unlike “ets” engine).

### Nested Modeltime Improvements

- New extractor:
  [`extract_nested_modeltime_table()`](https://business-science.github.io/modeltime/reference/log_extractors.md) -
  Extracts a nested modeltime table by row id.

### (potentially) Breaking Changes

- `extract_nested_train_split` and `extract_nested_test_split`: Changed
  parameter from `.data` to `.object` for consistency with other
  “extract” functions

- Added a new logged feature to
  [`modeltime_nested_fit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_fit.md)
  to track the attribute “metric_set”, which is needed for ensembles.
  Old nested modeltime objects will need to be re-run to get this new
  attribute. This will be used in ensembles.

## modeltime 1.0.0

CRAN release: 2021-09-14

#### New Feature: Nested (Iterative) Forecasting

**Nested (Iterative) Forecasting** is aimed at making it easier to
perform forecasting that is traditionally done in a *for-loop* with
models like ARIMA, Prophet, and Exponential Smoothing. Functionality has
been added to:

##### Format data in a Nested Time Series structure

- **Data Preparation Utilities:**
  [`extend_timeseries()`](https://business-science.github.io/modeltime/reference/prep_nested.md),
  [`nest_timeseries()`](https://business-science.github.io/modeltime/reference/prep_nested.md),
  and `split_nested_timeseris()`.

##### Nested Model Fitting (Train/Test)

- **[`modeltime_nested_fit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_fit.md):**
  Fits many models to nested time series data and organizes in a “Nested
  Modeltime Table”. Logs Accuracy, Errors, and Test Forecasts.

- **[`control_nested_fit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md):**
  Used to control the fitting process including verbosity and parallel
  processing.

- **Logging Extractors:** Functions that retrieve logged information
  from the initial fitting process.
  [`extract_nested_test_accuracy()`](https://business-science.github.io/modeltime/reference/log_extractors.md),
  [`extract_nested_error_report()`](https://business-science.github.io/modeltime/reference/log_extractors.md),
  and
  [`extract_nested_test_forecast()`](https://business-science.github.io/modeltime/reference/log_extractors.md).

##### Nested Model Selection

- **[`modeltime_nested_select_best()`](https://business-science.github.io/modeltime/reference/modeltime_nested_select_best.md)**:
  Selects the best model for each time series ID.

- **Logging Extractors:** Functions that retrieve logged information
  from the model selection process.
  [`extract_nested_best_model_report()`](https://business-science.github.io/modeltime/reference/log_extractors.md)

##### Nested Model Refitting (Actual Data)

- **[`modeltime_nested_refit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_refit.md):**
  Refits to the `.future_data`. Logs Future Forecasts.

- **[`control_nested_refit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md):**
  Used to control the re-fitting process including verbosity and
  parallel processing.

- **Logging Extractors:** Functions that retrieve logged information
  from the re-fitting process.
  [`extract_nested_future_forecast()`](https://business-science.github.io/modeltime/reference/log_extractors.md).

#### New Vignette

- [Nested
  Forecasting](https://business-science.github.io/modeltime/articles/nested-forecasting.html)

#### Vignette Improvements

- [Forecasting with Global
  Models](https://business-science.github.io/modeltime/articles/modeling-panel-data.html):
  Added more complete steps in the forecasting process so now user can
  see how to forecast each step from start to finish including future
  forecasting.

#### New Accuracy Metric Set and Yardstick Functions

- [`extended_forecast_accuracy_metric_set()`](https://business-science.github.io/modeltime/reference/metric_sets.md):
  Adds the new MAAPE metric for handling intermittent data when MAPE
  returns Inf.
- [`maape()`](https://business-science.github.io/modeltime/reference/maape.md):
  New yardstick metric that calculates “Mean Arctangent Absolute
  Percentage Error” (MAAPE). Used when MAPE returns Inf typically due to
  intermittent data.

#### Improvements

- [`modeltime_fit_workflowset()`](https://business-science.github.io/modeltime/reference/modeltime_fit_workflowset.md):
  Improved handling of Workflowset Descriptions, which now match the
  `wflow_id`.

## modeltime 0.7.0

CRAN release: 2021-07-16

#### Group-Wise Accuracy and Confidence Interval by Time Series ID

We’ve expanded Panel Data functionality to produce model accuracy and
confidence interval estimates by a Time Series ID
([\#114](https://github.com/business-science/modeltime/issues/114)).
This is useful when you have a Global Model that produces forecasts for
more than one time series. You can more easily obtain grouped accuracy
and confidence interval estimates.

- [`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md):
  Gains an `id` argument that is a quoted column name. This identifies
  that the residuals should be tracked by an time series identifier
  feature that indicates the time series groups.

- [`modeltime_accuracy()`](https://business-science.github.io/modeltime/reference/modeltime_accuracy.md):
  Gains a `acc_by_id` argument that is `TRUE`/`FALSE`. If the data has
  been calibrated with `id`, then the user can return local model
  accuracy by the identifier column. The accuracy data frame will return
  a row for each combination of Model ID and Time Series ID.

- [`modeltime_forecast()`](https://business-science.github.io/modeltime/reference/modeltime_forecast.md):
  Gains a `conf_by_id` argument that is `TRUE`/`FALSE`. If the data has
  been calibrated with `id`, then the user can return local model
  confidence by the identifier column. The forecast data frame will
  return an extra column indicating the identifier column. The
  confidence intervals will be adjusted based on the local time series
  ID variance instead of the global model variance.

#### New Vignette

[Forecasting Panel
Data](https://business-science.github.io/modeltime/articles/modeling-panel-data.html)

#### New Algorithms

##### THIEF: Temporal Hierarchical Forecasting

- [`temporal_hierarchy()`](https://business-science.github.io/modeltime/reference/temporal_hierarchy.md):
  Implements the `thief` package by Rob Hyndman and Nikolaos Kourentzes
  for “Temporal HIErarchical Forecasting”.
  [\#117](https://github.com/business-science/modeltime/issues/117)

#### Bug Fixes

- Issue
  [\#111](https://github.com/business-science/modeltime/issues/111): Fix
  bug with
  [`modeltime_fit_workflowset()`](https://business-science.github.io/modeltime/reference/modeltime_fit_workflowset.md)
  where the workflowset (wflw_id) order was not maintained.

## modeltime 0.6.1

CRAN release: 2021-06-13

**Parallel Processing**

- New Vignette: [Parallel
  Processing](https://business-science.github.io/modeltime/articles/parallel-processing.html)

- [`parallel_start()`](https://business-science.github.io/modeltime/reference/parallel_start.md)
  and
  [`parallel_stop()`](https://business-science.github.io/modeltime/reference/parallel_start.md):
  Helpers for setting up multicore processing.

- [`create_model_grid()`](https://business-science.github.io/modeltime/reference/create_model_grid.md):
  Helper to generate model specifications with filled-in parameters from
  a parameter grid
  (e.g. [`dials::grid_regular()`](https://dials.tidymodels.org/reference/grid_regular.html)).

- [`control_refit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)
  and
  [`control_fit_workflowset()`](https://business-science.github.io/modeltime/reference/control_modeltime.md):
  Better printing.

**Bug Fixes**

- Issue
  [\#110](https://github.com/business-science/modeltime/issues/110): Fix
  bug with `cores > cores_available`.

## modeltime 0.6.0

CRAN release: 2021-05-30

#### Workflowset Integration

[`modeltime_fit_workflowset()`](https://business-science.github.io/modeltime/reference/modeltime_fit_workflowset.md)
([\#85](https://github.com/business-science/modeltime/issues/85)) makes
it easy to convert `workflow_set` objects to Modeltime Tables
(`mdl_time_tbl`). Requires a refitting process that can now be performed
in parallel or in sequence.

#### New Algorithms

- CROSTON
  ([\#5](https://github.com/business-science/modeltime/issues/5),
  [\#98](https://github.com/business-science/modeltime/issues/98)) -
  This is a new engine that has been added to
  [`exp_smoothing()`](https://business-science.github.io/modeltime/reference/exp_smoothing.md).
- THETA ([\#5](https://github.com/business-science/modeltime/issues/5),
  [\#93](https://github.com/business-science/modeltime/issues/93)) -
  This is a new engine that has been added to
  [`exp_smoothing()`](https://business-science.github.io/modeltime/reference/exp_smoothing.md).

#### New Dials Parameters

[`exp_smoothing()`](https://business-science.github.io/modeltime/reference/exp_smoothing.md)
gained 3 new tunable parameters:

- [`smooth_level()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md):
  This is often called the “alpha” parameter used as the base level
  smoothing factor for exponential smoothing models.
- [`smooth_trend()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md):
  This is often called the “beta” parameter used as the trend smoothing
  factor for exponential smoothing models.
- [`smooth_seasonal()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md):
  This is often called the “gamma” parameter used as the seasonal
  smoothing factor for exponential smoothing models.

#### Parallel Processing

- [`modeltime_refit()`](https://business-science.github.io/modeltime/reference/modeltime_refit.md):
  supports parallel processing. See
  [`control_refit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)
- [`modeltime_fit_workflowset()`](https://business-science.github.io/modeltime/reference/modeltime_fit_workflowset.md):
  supports parallel processing. See `control_workflowset()`

#### Updates for parsnip \>= 0.1.6

- `boost_tree(mtry)`: Mapping switched from `colsample_bytree` to
  `colsample_bynode`.
  [`prophet_boost()`](https://business-science.github.io/modeltime/reference/prophet_boost.md)
  and
  [`arima_boost()`](https://business-science.github.io/modeltime/reference/arima_boost.md)
  have been updated to reflect this change.
  <https://github.com/tidymodels/parsnip/pull/499>

#### General Improvements

- Improve Model Description of Recursive Models
  ([\#96](https://github.com/business-science/modeltime/issues/96))

#### Potential Breaking Changes

- We’ve added new parameters to Exponential Smoothing Models.
  [`exp_smoothing()`](https://business-science.github.io/modeltime/reference/exp_smoothing.md)
  models produced in prior versions may require refitting with
  [`modeltime_refit()`](https://business-science.github.io/modeltime/reference/modeltime_refit.md)
  to upgrade their internals with the new parameters.

## modeltime 0.5.1

CRAN release: 2021-04-03

#### Recursive Ensemble Predictions

- Add support for
  [`recursive()`](https://business-science.github.io/modeltime/reference/recursive.md)
  for ensembles. The new recursive ensemble functionality is in
  `modeltime.ensemble` \>= 0.3.0.9000.

## modeltime 0.5.0

CRAN release: 2021-03-29

#### Recursive Panel Predictions

- [`recursive()`](https://business-science.github.io/modeltime/reference/recursive.md)
  ([\#71](https://github.com/business-science/modeltime/issues/71)) -
  Received a full upgrade to work with Panel Data.
- **New Vignette**: [“Autoregressive Forecasting with
  Recursive”](https://business-science.github.io/modeltime/articles/recursive-forecasting.html)

#### Breaking Changes

- Deprecating
  [`modeltime::metric_tweak()`](https://yardstick.tidymodels.org/reference/metric_tweak.html)
  for
  [`yardstick::metric_tweak()`](https://yardstick.tidymodels.org/reference/metric_tweak.html).
  The
  [`yardstick::metric_tweak()`](https://yardstick.tidymodels.org/reference/metric_tweak.html)
  has a required `.name` argument in addition to `.fn`, which is needed
  for tuning.

## modeltime 0.4.2

CRAN release: 2021-03-19

#### New Algorithms

Baseline algorithms
([\#5](https://github.com/business-science/modeltime/issues/5),
[\#37](https://github.com/business-science/modeltime/issues/37)) have
been created for comparing high-performance methods with simple
forecasting methods.

- `window_reg`: Window-based methods such as mean, median, and even more
  complex seasonal models based on a forecasting window. The main tuning
  parameter is `window_size`.  
- `naive_reg`: NAIVE and Seasonal NAIVE (SNAIVE) Regression Models

#### Yardstick Helpers

- [`metric_tweak()`](https://yardstick.tidymodels.org/reference/metric_tweak.html) -
  Can modify `yardstick` metrics like
  [`mase()`](https://yardstick.tidymodels.org/reference/mase.html),
  which have seasonal parameters.
- [`default_forecast_accuracy_metric_set()`](https://business-science.github.io/modeltime/reference/metric_sets.md) -
  Gets a `...` parameter that allows us to add more metrics beyond the
  defaults.

#### Modeltime Residual Tests

A new function is added
[`modeltime_residuals_test()`](https://business-science.github.io/modeltime/reference/modeltime_residuals_test.md)
([\#62](https://github.com/business-science/modeltime/issues/62),
[\#68](https://github.com/business-science/modeltime/issues/68)). Tests
are implemented:

- Shapiro Test - Test for Normality of residuals
- Box-Pierce, Ljung-Box, and Durbin-Watson Tests - Test for
  Autocorrelation of residuals

#### Fixes

- [`plot_modeltime_forecast()`](https://business-science.github.io/modeltime/reference/plot_modeltime_forecast.md) -
  When plotting a single point forecast,
  [`plot_modeltime_forecast()`](https://business-science.github.io/modeltime/reference/plot_modeltime_forecast.md)
  now uses `geom_point()` instead of `geom_line()`. Fixes
  [\#66](https://github.com/business-science/modeltime/issues/66).

## modeltime 0.4.1

CRAN release: 2021-01-17

**Fixes**

- [`recursive()`](https://business-science.github.io/modeltime/reference/recursive.md)
  &
  [`modeltime_refit()`](https://business-science.github.io/modeltime/reference/modeltime_refit.md):
  Now able to refit a recursive workflow or recursive fitted parsnip
  object.

## modeltime 0.4.0

CRAN release: 2020-11-23

**New Functions**

- [`recursive()`](https://business-science.github.io/modeltime/reference/recursive.md):
  Turn a fitted model into a recursive predictor.
  ([\#49](https://github.com/business-science/modeltime/issues/49),
  [\#50](https://github.com/business-science/modeltime/issues/50))
- [`update_modeltime_model()`](https://business-science.github.io/modeltime/reference/update_modeltime_model.md):
  New function to update a modeltime model inside a Modeltime Table.

**Breaking Changes**

- Removed `arima_workflow_tuned` dataset.

## modeltime 0.3.1

CRAN release: 2020-11-09

[`as_modeltime_table()`](https://business-science.github.io/modeltime/reference/modeltime_table.md):
New function to convert one or more fitted models stored in a `list` to
a Modeltime Table.

**Bug Fixes**

- Update `m750_models`: Fixes error “R parsnip Error: Internal error:
  Unknown `composition` type.”

## modeltime 0.3.0

CRAN release: 2020-10-28

**Panel Data**

[`modeltime_forecast()`](https://business-science.github.io/modeltime/reference/modeltime_forecast.md)
upgrades:

- `keep_data`: Gains a new argument `keep_data`. This is useful when the
  `new_data` and `actual_data` has important information needed in
  analyzing the forecast.
- `arrange_index`: Gains a new argument `arrange_index`. By default, the
  forecast keeps the rows in the same order as the incoming data. Prior
  versions arranged Model Predictions by `.index`, which impacts the
  users ability to match to Panel Data which is not likely to be
  arranged by date. Prediction best-practices are to keep the original
  order of the data, which will be preserved by default. To get the old
  behavior, simply toggle `arrange_index = TRUE`.

[`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md):
Can now handle panel data.

[`modeltime_accuracy()`](https://business-science.github.io/modeltime/reference/modeltime_accuracy.md):
Can now handle panel data.

[`plot_modeltime_forecast()`](https://business-science.github.io/modeltime/reference/plot_modeltime_forecast.md):
Can handle panel data provided the data is grouped by an ID column prior
to plotting.

**Error Messaging**

- **Calibration:** Improve error messaging during calibration. Provide
  warnings if models fail. Provide report with
  `modeltime_calibrate(quiet = FALSE)`.

**Compatibility**

- Compatibility with `parsnip >= 0.1.4`. Uses `set_encodings()` new
  parameter `allow_sparse_x`.

## modeltime 0.2.1

CRAN release: 2020-10-08

**Ensembles**

- [`modeltime_refit()`](https://business-science.github.io/modeltime/reference/modeltime_refit.md) -
  Changes to improve fault tolerance and error handling / messaging when
  making ensembles.

## modeltime 0.2.0

CRAN release: 2020-09-28

**Ensembles**

- Integrates `modeltime.ensemble`, a new R package designed for
  forecasting with ensemble models.

***New Workflow Helper Functions***

- [`add_modeltime_model()`](https://business-science.github.io/modeltime/reference/add_modeltime_model.md) -
  A helper function making it easy to add a fitted parsnip or workflow
  object to a modeltime table
- [`pluck_modeltime_model()`](https://business-science.github.io/modeltime/reference/pluck_modeltime_model.md)
  &
  [`pull_modeltime_model()`](https://business-science.github.io/modeltime/reference/pluck_modeltime_model.md) -
  A helper function making it easy to extract a model from a modeltime
  table

**Improvements**

- Documentation - Algorithms now identify default parameter values in
  the “Engine Details” Section in their respective documentation. E.g.
  [`?prophet_boost`](https://business-science.github.io/modeltime/reference/prophet_boost.md)
- [`prophet_reg()`](https://business-science.github.io/modeltime/reference/prophet_reg.md)
  can now have regressors controlled via
  [`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)
  using the following parameters:
  - `regressors.mode` - Set to `seasonality.mode` by default.
  - `regressors.prior.scale` - Set to 10,000 by default.
  - `regressors.standardize` - Set to “auto” by default.

**Data Sets**

Modeltime now includes 4 new data sets:

- `m750` - M750 Time Series Dataset
- `m750_models` - 3 Modeltime Models made on the M750 Dataset
- `m750_splits` - An `rsplit` object containing Train/test splits of the
  M750 data
- `m750_training_resamples` - A Time Series Cross Validation
  `time_series_cv` object made from the `training(m750_splits)`

**Bug Fix**

- [`plot_modeltime_forecast()`](https://business-science.github.io/modeltime/reference/plot_modeltime_forecast.md)
  fix issue with “ACTUAL” data being shown at bottom of legend list.
  Should be first item.

## modeltime 0.1.0

CRAN release: 2020-09-02

#### New Features

**Forecast without Calibration/Refitting**

Sometimes it’s important to make fast forecasts without calculating
out-of-sample accuracy and refitting (which requires 2 rounds of model
training). You can now bypass the
[`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md)
and
[`modeltime_refit()`](https://business-science.github.io/modeltime/reference/modeltime_refit.md)
steps and jump straight into forecasting the future. Here’s an example
with `h = "3 years"`. Note that **you will not get confidence
intervals** with this approach because calibration data is needed for
this.

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

**Residual Analysis & Diagonstics**

A common tool when forecasting and analyzing residuals, where residuals
are `.resid = .actual - .prediction`. The residuals may have
autocorrelation or nonzero mean, which can indicate model improvement
opportunities. In addition, users may which to inspect in-sample and
out-of-sample residuals, which can display different results.

- [`modeltime_residuals()`](https://business-science.github.io/modeltime/reference/modeltime_residuals.md) -
  A new function used to extract out residual information
- [`plot_modeltime_residuals()`](https://business-science.github.io/modeltime/reference/plot_modeltime_residuals.md) -
  Visualizes the output of
  [`modeltime_residuals()`](https://business-science.github.io/modeltime/reference/modeltime_residuals.md).
  Offers 3 plots:
  1.  **Time Plot** - Residuals over time
  2.  **ACF Plot** - Residual Autocorrelation vs Lags
  3.  **Seasonality** - Residual Seasonality Plot

#### New Models

**TBATS Model**

Use
[`seasonal_reg()`](https://business-science.github.io/modeltime/reference/seasonal_reg.md)
and set engine to “tbats”.

``` r
seasonal_reg(
    seasonal_period_1 = "1 day",
    seasonal_period_2 = "1 week"
) %>% 
    set_engine("tbats")
```

**NNETAR Model**

Use
[`nnetar_reg()`](https://business-science.github.io/modeltime/reference/nnetar_reg.md)
and set engine to “nnetar”.

``` r
model_fit_nnetar <- nnetar_reg() %>%
    set_engine("nnetar") 
```

**Prophet Model - Logistic Growth Support**

- [`prophet_reg()`](https://business-science.github.io/modeltime/reference/prophet_reg.md)
  and
  [`prophet_boost()`](https://business-science.github.io/modeltime/reference/prophet_boost.md):
  - Now supports logistic growth. Set `growth = 'logistic'` and one or
    more of `logistic_cap` and `logistic_floor` to valid saturation
    boundaries.
  - New arguments making it easier to modify the `changepoint_num`,
    `changepoint_range`, `seasonality_yearly`, `seasonality_weekly`,
    `seasonality_daily`, `logistic_cap`, `logistic_floor`

#### New Workflow Helper Functions

- [`combine_modeltime_tables()`](https://business-science.github.io/modeltime/reference/combine_modeltime_tables.md) -
  A helper function making it easy to combine multiple modeltime tables.
- [`update_model_description()`](https://business-science.github.io/modeltime/reference/update_model_description.md) -
  A helper function making it easier to update model descriptions.

#### Improvements

- [`modeltime_refit()`](https://business-science.github.io/modeltime/reference/modeltime_refit.md):
  When modeltime model parameters update (e.g. when Auto ARIMA changes
  to a new model), the Model Description now alerts the user
  (e.g. “UPDATE: ARIMA(0,1,1)(1,1,1)\[12\]”).

- [`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md):
  When training data is supplied in a time window that the model has
  previously been trained on (e.g. `training(splits)`), the calibration
  calculation first inspects whether the “Fitted” data exists. If it
  iexists, it returns the “Fitted” data. This helps prevent
  sequence-based (e.g. ARIMA, ETS, TBATS models) from displaying odd
  results because these algorithms can only predict sequences directly
  following the training window. If “Fitted” data is being used, the
  `.type` column will display “Fitted” instead of “Test”.

#### Bug Fixes

- [`modeltime_forecast()`](https://business-science.github.io/modeltime/reference/modeltime_forecast.md):

  - Implement `actual_data` reconciliation strategies when recipe
    removes rows. Strategy attempts to fill predictors using “downup”
    strategy to prevent `NA` values from removing rows.
  - More descriptive errors when external regressors are required.

- [`modeltime_accuracy()`](https://business-science.github.io/modeltime/reference/modeltime_accuracy.md):
  Fix issue with `new_data` not recalibrating.

- [`prophet_reg()`](https://business-science.github.io/modeltime/reference/prophet_reg.md)
  and
  [`prophet_boost()`](https://business-science.github.io/modeltime/reference/prophet_boost.md) -
  Can now perform logistic growth `growth = 'logistic'`. The user can
  supply “saturation” bounds using `logistic_cap` and/or
  `logisitc_floor`.

#### Breaking Changes

- `seasonal_decomp()` has changed to
  [`seasonal_reg()`](https://business-science.github.io/modeltime/reference/seasonal_reg.md)
  and now supports both TBATS and Seasonal Decomposition Models.
- [`prophet_reg()`](https://business-science.github.io/modeltime/reference/prophet_reg.md)
  &
  [`prophet_boost()`](https://business-science.github.io/modeltime/reference/prophet_boost.md):
  Argument changes:
  - `num_changepoints` has become `changepoint_num`

## modeltime 0.0.2

CRAN release: 2020-07-03

#### Confidence Interval Estimation

- [`modeltime_forecast()`](https://business-science.github.io/modeltime/reference/modeltime_forecast.md):
  Now estimates confidence intervals using centered standard deviation.
  The mean is assumed to be zero and residuals deviate from mean = 0.

#### Fixes

- Updates to work with `parsnip` 0.1.2.
- [`prophet_boost()`](https://business-science.github.io/modeltime/reference/prophet_boost.md):
  Set `nthreads = 1` (default) to ensure parallelization is thread safe.

## modeltime 0.0.1

CRAN release: 2020-06-22

- Initial Release
