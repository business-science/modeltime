# Refits a Nested Modeltime Table

Refits a Nested Modeltime Table to actual data using the following
process:

1.  Models are iteratively refit to .actual_data.

2.  Any model that returns an error is logged. Errors can be retrieved
    with
    [`extract_nested_error_report()`](https://business-science.github.io/modeltime/reference/log_extractors.md)

3.  Forecast is predicted on future_data and is logged. Forecast can be
    retrieved with
    [`extract_nested_future_forecast()`](https://business-science.github.io/modeltime/reference/log_extractors.md)

## Usage

``` r
modeltime_nested_refit(object, control = control_nested_refit())
```

## Arguments

- object:

  A Nested Modeltime Table

- control:

  Used to control verbosity and parallel processing. See
  [`control_nested_refit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md).
