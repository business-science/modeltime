# Forecast Accuracy Metrics Sets

This is a wrapper for `metric_set()` with several common forecast /
regression accuracy metrics included. These are the default time series
accuracy metrics used with
[`modeltime_accuracy()`](https://business-science.github.io/modeltime/reference/modeltime_accuracy.md).

## Usage

``` r
default_forecast_accuracy_metric_set(...)

extended_forecast_accuracy_metric_set(...)
```

## Arguments

- ...:

  Add additional `yardstick` metrics

## Default Forecast Accuracy Metric Set

The primary purpose is to use the default accuracy metrics to calculate
the following forecast accuracy metrics using
[`modeltime_accuracy()`](https://business-science.github.io/modeltime/reference/modeltime_accuracy.md):

- MAE - Mean absolute error,
  [`mae()`](https://yardstick.tidymodels.org/reference/mae.html)

- MAPE - Mean absolute percentage error,
  [`mape()`](https://yardstick.tidymodels.org/reference/mape.html)

- MASE - Mean absolute scaled error,
  [`mase()`](https://yardstick.tidymodels.org/reference/mase.html)

- SMAPE - Symmetric mean absolute percentage error,
  [`smape()`](https://yardstick.tidymodels.org/reference/smape.html)

- RMSE - Root mean squared error,
  [`rmse()`](https://yardstick.tidymodels.org/reference/rmse.html)

- RSQ - R-squared,
  [`rsq()`](https://yardstick.tidymodels.org/reference/rsq.html)

Adding additional metrics is possible via `...`.

## Extended Forecast Accuracy Metric Set

Extends the default metric set by adding:

- MAAPE - Mean Arctangent Absolute Percentage Error,
  [`maape()`](https://business-science.github.io/modeltime/reference/maape.md).
  MAAPE is designed for intermittent data where MAPE returns `Inf`.

## See also

- [`yardstick::metric_tweak()`](https://yardstick.tidymodels.org/reference/metric_tweak.html) -
  For modifying `yardstick` metrics

## Examples

``` r
library(tibble)
library(dplyr)
library(timetk)
library(yardstick)

fake_data <- tibble(
  y = c(1:12, 2*1:12),
  yhat = c(1 + 1:12, 2*1:12 - 1)
)

# ---- HOW IT WORKS ----

# Default Forecast Accuracy Metric Specification
default_forecast_accuracy_metric_set()
#> A metric set, consisting of:
#> - `mae()`, a numeric metric   | direction: minimize
#> - `mape()`, a numeric metric  | direction: minimize
#> - `mase()`, a numeric metric  | direction: minimize
#> - `smape()`, a numeric metric | direction: minimize
#> - `rmse()`, a numeric metric  | direction: minimize
#> - `rsq()`, a numeric metric   | direction: maximize

# Create a metric summarizer function from the metric set
calc_default_metrics <- default_forecast_accuracy_metric_set()

# Apply the metric summarizer to new data
calc_default_metrics(fake_data, y, yhat)
#> # A tibble: 6 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 mae     standard       1    
#> 2 mape    standard      19.4  
#> 3 mase    standard       0.535
#> 4 smape   standard      18.0  
#> 5 rmse    standard       1    
#> 6 rsq     standard       0.979

# ---- ADD MORE PARAMETERS ----

# Can create a version of mase() with seasonality = 12 (monthly)
mase12 <- metric_tweak(.name = "mase12", .fn = mase, m = 12)

# Add it to the default metric set
my_metric_set <- default_forecast_accuracy_metric_set(mase12)
my_metric_set
#> A metric set, consisting of:
#> - `mae()`, a numeric metric    | direction: minimize
#> - `mape()`, a numeric metric   | direction: minimize
#> - `mase()`, a numeric metric   | direction: minimize
#> - `smape()`, a numeric metric  | direction: minimize
#> - `rmse()`, a numeric metric   | direction: minimize
#> - `rsq()`, a numeric metric    | direction: maximize
#> - `mase12()`, a numeric metric | direction: minimize

# Apply the newly created metric set
my_metric_set(fake_data, y, yhat)
#> # A tibble: 7 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 mae     standard       1    
#> 2 mape    standard      19.4  
#> 3 mase    standard       0.535
#> 4 smape   standard      18.0  
#> 5 rmse    standard       1    
#> 6 rsq     standard       0.979
#> 7 mase12  standard       0.154
```
