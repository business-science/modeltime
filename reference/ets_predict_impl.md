# Bridge prediction function for Exponential Smoothing models

Bridge prediction function for Exponential Smoothing models

## Usage

``` r
ets_predict_impl(object, new_data, ...)
```

## Arguments

- object:

  A [model
  fit](https://parsnip.tidymodels.org/reference/model_fit.html).

- new_data:

  A rectangular data object, such as a data frame.

- ...:

  Additional arguments passed to
  [`forecast::ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.html)
