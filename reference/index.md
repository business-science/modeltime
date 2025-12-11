# Package index

## Modeltime Workflow

The main workflow functions for time series modeling.

### Core Functions

- [`modeltime_table()`](https://business-science.github.io/modeltime/reference/modeltime_table.md)
  [`as_modeltime_table()`](https://business-science.github.io/modeltime/reference/modeltime_table.md)
  : Scale forecast analysis with a Modeltime Table

- [`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md)
  : Preparation for forecasting

- [`modeltime_forecast()`](https://business-science.github.io/modeltime/reference/modeltime_forecast.md)
  : Forecast future data

- [`modeltime_accuracy()`](https://business-science.github.io/modeltime/reference/modeltime_accuracy.md)
  : Calculate Accuracy Metrics

- [`modeltime_refit()`](https://business-science.github.io/modeltime/reference/modeltime_refit.md)
  : Refit one or more trained models to new data

- [`modeltime_fit_workflowset()`](https://business-science.github.io/modeltime/reference/modeltime_fit_workflowset.md)
  :

  Fit a `workflowset` object to one or multiple time series

### Recursive Forecast Prediction

- [`recursive()`](https://business-science.github.io/modeltime/reference/recursive.md)
  : Create a Recursive Time Series Model from a Parsnip or Workflow
  Regression Model
- [`panel_tail()`](https://business-science.github.io/modeltime/reference/panel_tail.md)
  : Filter the last N rows (Tail) for multiple time series

### Plotting & Tables

- [`plot_modeltime_forecast()`](https://business-science.github.io/modeltime/reference/plot_modeltime_forecast.md)
  : Interactive Forecast Visualization
- [`plot_modeltime_residuals()`](https://business-science.github.io/modeltime/reference/plot_modeltime_residuals.md)
  : Interactive Residuals Visualization
- [`table_modeltime_accuracy()`](https://business-science.github.io/modeltime/reference/table_modeltime_accuracy.md)
  : Interactive Accuracy Tables

### Residual Analysis

- [`modeltime_residuals()`](https://business-science.github.io/modeltime/reference/modeltime_residuals.md)
  : Extract Residuals Information
- [`modeltime_residuals_test()`](https://business-science.github.io/modeltime/reference/modeltime_residuals_test.md)
  : Apply Statistical Tests to Residuals
- [`plot_modeltime_residuals()`](https://business-science.github.io/modeltime/reference/plot_modeltime_residuals.md)
  : Interactive Residuals Visualization

## Nested Forecasting

Forecast many time series iteratively using “nested modeltime tables”.
Used to apply models to each time series panel independently.

### Core functions

- [`modeltime_nested_fit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_fit.md)
  : Fit Tidymodels Workflows to Nested Time Series
- [`modeltime_nested_select_best()`](https://business-science.github.io/modeltime/reference/modeltime_nested_select_best.md)
  : Select the Best Models from Nested Modeltime Table
- [`modeltime_nested_refit()`](https://business-science.github.io/modeltime/reference/modeltime_nested_refit.md)
  : Refits a Nested Modeltime Table
- [`modeltime_nested_forecast()`](https://business-science.github.io/modeltime/reference/modeltime_nested_forecast.md)
  : Modeltime Nested Forecast

### Extractors

- [`extract_nested_test_accuracy()`](https://business-science.github.io/modeltime/reference/log_extractors.md)
  [`extract_nested_test_forecast()`](https://business-science.github.io/modeltime/reference/log_extractors.md)
  [`extract_nested_error_report()`](https://business-science.github.io/modeltime/reference/log_extractors.md)
  [`extract_nested_best_model_report()`](https://business-science.github.io/modeltime/reference/log_extractors.md)
  [`extract_nested_future_forecast()`](https://business-science.github.io/modeltime/reference/log_extractors.md)
  [`extract_nested_modeltime_table()`](https://business-science.github.io/modeltime/reference/log_extractors.md)
  [`extract_nested_train_split()`](https://business-science.github.io/modeltime/reference/log_extractors.md)
  [`extract_nested_test_split()`](https://business-science.github.io/modeltime/reference/log_extractors.md)
  : Log Extractor Functions for Modeltime Nested Tables

### Workflow

- [`extend_timeseries()`](https://business-science.github.io/modeltime/reference/prep_nested.md)
  [`nest_timeseries()`](https://business-science.github.io/modeltime/reference/prep_nested.md)
  [`split_nested_timeseries()`](https://business-science.github.io/modeltime/reference/prep_nested.md)
  : Prepared Nested Modeltime Data

## Algorithms

The `parsnip`-adjacent algorithms that implement time series models.

### Core Forecasting Methods

These models come with modeltime.

- [`prophet_reg()`](https://business-science.github.io/modeltime/reference/prophet_reg.md)
  : General Interface for PROPHET Time Series Models
- [`prophet_boost()`](https://business-science.github.io/modeltime/reference/prophet_boost.md)
  : General Interface for Boosted PROPHET Time Series Models
- [`arima_reg()`](https://business-science.github.io/modeltime/reference/arima_reg.md)
  : General Interface for ARIMA Regression Models
- [`arima_boost()`](https://business-science.github.io/modeltime/reference/arima_boost.md)
  : General Interface for "Boosted" ARIMA Regression Models
- [`exp_smoothing()`](https://business-science.github.io/modeltime/reference/exp_smoothing.md)
  : General Interface for Exponential Smoothing State Space Models
- [`seasonal_reg()`](https://business-science.github.io/modeltime/reference/seasonal_reg.md)
  : General Interface for Multiple Seasonality Regression Models (TBATS,
  STLM)
- [`nnetar_reg()`](https://business-science.github.io/modeltime/reference/nnetar_reg.md)
  : General Interface for NNETAR Regression Models

### Additional Algorithms

These algorithms have additional dependencies that can be installed with
`dependencies = TRUE`

- [`adam_reg()`](https://business-science.github.io/modeltime/reference/adam_reg.md)
  : General Interface for ADAM Regression Models
- [`temporal_hierarchy()`](https://business-science.github.io/modeltime/reference/temporal_hierarchy.md)
  : General Interface for Temporal Hierarchical Forecasting (THIEF)
  Models

### Baseline Algorithms (Simple Methods)

- [`window_reg()`](https://business-science.github.io/modeltime/reference/window_reg.md)
  : General Interface for Window Forecast Models
- [`naive_reg()`](https://business-science.github.io/modeltime/reference/naive_reg.md)
  : General Interface for NAIVE Forecast Models

## Parallel Processing

- [`parallel_start()`](https://business-science.github.io/modeltime/reference/parallel_start.md)
  [`parallel_stop()`](https://business-science.github.io/modeltime/reference/parallel_start.md)
  : Start parallel clusters / plans

- [`control_refit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)
  [`control_fit_workflowset()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)
  [`control_nested_fit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)
  [`control_nested_refit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)
  [`control_nested_forecast()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)
  : Control aspects of the training process

- [`create_model_grid()`](https://business-science.github.io/modeltime/reference/create_model_grid.md)
  :

  Helper to make `parsnip` model specs from a `dials` parameter grid

## Modeltime Workflow Helpers

- [`combine_modeltime_tables()`](https://business-science.github.io/modeltime/reference/combine_modeltime_tables.md)
  : Combine multiple Modeltime Tables into a single Modeltime Table
- [`add_modeltime_model()`](https://business-science.github.io/modeltime/reference/add_modeltime_model.md)
  : Add a Model into a Modeltime Table
- [`drop_modeltime_model()`](https://business-science.github.io/modeltime/reference/drop_modeltime_model.md)
  : Drop a Model from a Modeltime Table
- [`update_modeltime_model()`](https://business-science.github.io/modeltime/reference/update_modeltime_model.md)
  : Update the model by model id in a Modeltime Table
- [`update_model_description()`](https://business-science.github.io/modeltime/reference/update_model_description.md)
  [`update_modeltime_description()`](https://business-science.github.io/modeltime/reference/update_model_description.md)
  : Update the model description by model id in a Modeltime Table
- [`pluck_modeltime_model()`](https://business-science.github.io/modeltime/reference/pluck_modeltime_model.md)
  [`pull_modeltime_model()`](https://business-science.github.io/modeltime/reference/pluck_modeltime_model.md)
  : Extract model by model id in a Modeltime Table
- [`pull_modeltime_residuals()`](https://business-science.github.io/modeltime/reference/pull_modeltime_residuals.md)
  : Extracts modeltime residuals data from a Modeltime Model
- [`pull_parsnip_preprocessor()`](https://business-science.github.io/modeltime/reference/pull_parsnip_preprocessor.md)
  : Pulls the Formula from a Fitted Parsnip Model Object

## Accuracy Metrics (Yardstick)

### Metric Sets and Summarizers

- [`default_forecast_accuracy_metric_set()`](https://business-science.github.io/modeltime/reference/metric_sets.md)
  [`extended_forecast_accuracy_metric_set()`](https://business-science.github.io/modeltime/reference/metric_sets.md)
  : Forecast Accuracy Metrics Sets
- [`summarize_accuracy_metrics()`](https://business-science.github.io/modeltime/reference/summarize_accuracy_metrics.md)
  : Summarize Accuracy Metrics

### New Accuracy Metrics

- [`maape()`](https://business-science.github.io/modeltime/reference/maape.md)
  : Mean Arctangent Absolute Percentage Error
- [`maape_vec()`](https://business-science.github.io/modeltime/reference/maape_vec.md)
  : Mean Arctangent Absolute Percentage Error

## Parameters (Dials)

The `dials` parameter functions that support hyperparameter tuning with
`tune`.

### General Time Series

- [`seasonal_period()`](https://business-science.github.io/modeltime/reference/time_series_params.md)
  : Tuning Parameters for Time Series (ts-class) Models

### ARIMA

- [`non_seasonal_ar()`](https://business-science.github.io/modeltime/reference/arima_params.md)
  [`non_seasonal_differences()`](https://business-science.github.io/modeltime/reference/arima_params.md)
  [`non_seasonal_ma()`](https://business-science.github.io/modeltime/reference/arima_params.md)
  [`seasonal_ar()`](https://business-science.github.io/modeltime/reference/arima_params.md)
  [`seasonal_differences()`](https://business-science.github.io/modeltime/reference/arima_params.md)
  [`seasonal_ma()`](https://business-science.github.io/modeltime/reference/arima_params.md)
  : Tuning Parameters for ARIMA Models

### Exponential Smoothing

- [`error()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  [`trend()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  [`trend_smooth()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  [`season()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  [`damping()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  [`damping_smooth()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  [`smooth_level()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  [`smooth_trend()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  [`smooth_seasonal()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  : Tuning Parameters for Exponential Smoothing Models

### Prophet

- [`growth()`](https://business-science.github.io/modeltime/reference/prophet_params.md)
  [`changepoint_num()`](https://business-science.github.io/modeltime/reference/prophet_params.md)
  [`changepoint_range()`](https://business-science.github.io/modeltime/reference/prophet_params.md)
  [`seasonality_yearly()`](https://business-science.github.io/modeltime/reference/prophet_params.md)
  [`seasonality_weekly()`](https://business-science.github.io/modeltime/reference/prophet_params.md)
  [`seasonality_daily()`](https://business-science.github.io/modeltime/reference/prophet_params.md)
  [`prior_scale_changepoints()`](https://business-science.github.io/modeltime/reference/prophet_params.md)
  [`prior_scale_seasonality()`](https://business-science.github.io/modeltime/reference/prophet_params.md)
  [`prior_scale_holidays()`](https://business-science.github.io/modeltime/reference/prophet_params.md)
  : Tuning Parameters for Prophet Models

### NNETAR

- [`num_networks()`](https://business-science.github.io/modeltime/reference/nnetar_params.md)
  : Tuning Parameters for NNETAR Models

### ADAM

- [`ets_model()`](https://business-science.github.io/modeltime/reference/adam_params.md)
  [`loss()`](https://business-science.github.io/modeltime/reference/adam_params.md)
  [`use_constant()`](https://business-science.github.io/modeltime/reference/adam_params.md)
  [`regressors_treatment()`](https://business-science.github.io/modeltime/reference/adam_params.md)
  [`outliers_treatment()`](https://business-science.github.io/modeltime/reference/adam_params.md)
  [`probability_model()`](https://business-science.github.io/modeltime/reference/adam_params.md)
  [`distribution()`](https://business-science.github.io/modeltime/reference/adam_params.md)
  [`information_criteria()`](https://business-science.github.io/modeltime/reference/adam_params.md)
  [`select_order()`](https://business-science.github.io/modeltime/reference/adam_params.md)
  : Tuning Parameters for ADAM Models

### Temporal Hierachical Models

- [`combination_method()`](https://business-science.github.io/modeltime/reference/temporal_hierarchy_params.md)
  [`use_model()`](https://business-science.github.io/modeltime/reference/temporal_hierarchy_params.md)
  : Tuning Parameters for TEMPORAL HIERARCHICAL Models

## Developer Tools

Tools for extending `modeltime`.

- [`new_modeltime_bridge()`](https://business-science.github.io/modeltime/reference/new_modeltime_bridge.md)
  : Constructor for creating modeltime models
- [`create_xreg_recipe()`](https://business-science.github.io/modeltime/reference/create_xreg_recipe.md)
  : Developer Tools for preparing XREGS (Regressors)
- [`juice_xreg_recipe()`](https://business-science.github.io/modeltime/reference/recipe_helpers.md)
  [`bake_xreg_recipe()`](https://business-science.github.io/modeltime/reference/recipe_helpers.md)
  : Developer Tools for processing XREGS (Regressors)
- [`parse_index_from_data()`](https://business-science.github.io/modeltime/reference/parse_index.md)
  [`parse_period_from_index()`](https://business-science.github.io/modeltime/reference/parse_index.md)
  : Developer Tools for parsing date and date-time information
- [`get_model_description()`](https://business-science.github.io/modeltime/reference/get_model_description.md)
  : Get model descriptions for parsnip, workflows & modeltime objects
- [`get_arima_description()`](https://business-science.github.io/modeltime/reference/get_arima_description.md)
  : Get model descriptions for Arima objects
- [`get_tbats_description()`](https://business-science.github.io/modeltime/reference/get_tbats_description.md)
  : Get model descriptions for TBATS objects

## Data

- [`m750`](https://business-science.github.io/modeltime/reference/m750.md)
  : The 750th Monthly Time Series used in the M4 Competition
- [`m750_models`](https://business-science.github.io/modeltime/reference/m750_models.md)
  : Three (3) Models trained on the M750 Data (Training Set)
- [`m750_splits`](https://business-science.github.io/modeltime/reference/m750_splits.md)
  : The results of train/test splitting the M750 Data
- [`m750_training_resamples`](https://business-science.github.io/modeltime/reference/m750_training_resamples.md)
  : The Time Series Cross Validation Resamples the M750 Data (Training
  Set)
