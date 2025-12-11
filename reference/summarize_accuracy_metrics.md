# Summarize Accuracy Metrics

This is an internal function used by
[`modeltime_accuracy()`](https://business-science.github.io/modeltime/reference/modeltime_accuracy.md).

## Usage

``` r
summarize_accuracy_metrics(data, truth, estimate, metric_set)
```

## Arguments

- data:

  A `data.frame` containing the truth and estimate columns.

- truth:

  The column identifier for the true results (numeric).

- estimate:

  The column identifier for the predicted results (numeric).

- metric_set:

  A
  [`yardstick::metric_set`](https://yardstick.tidymodels.org/reference/metric_set.html)
  object specifying the metrics to compute.

## Value

A tibble with columns `.metric`, `.estimator`, and `.estimate`, pivoted
wider by metric and grouped by any grouping variables in the input data.

## Examples

``` r
library(dplyr)

predictions_tbl <- tibble(
  group = c("model 1", "model 1", "model 1",
            "model 2", "model 2", "model 2"),
  truth = c(1, 2, 3,
            1, 2, 3),
  estimate = c(1.2, 2.0, 2.5,
               0.9, 1.9, 3.3)
)

predictions_tbl %>%
  dplyr::group_by(group) %>%
  summarize_accuracy_metrics(
    truth, estimate,
    metric_set = default_forecast_accuracy_metric_set()
  )
#> # A tibble: 2 × 7
#>   group     mae  mape  mase smape  rmse   rsq
#>   <chr>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 model 1 0.233 12.2  0.233 12.1  0.311 0.983
#> 2 model 2 0.167  8.33 0.167  8.39 0.191 0.991
```
