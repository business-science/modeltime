# Bridge prediction function for ARIMA models

Bridge prediction function for ARIMA models

## Usage

``` r
stlm_ets_predict_impl(object, new_data, ...)
```

## Arguments

- object:

  A [model
  fit](https://parsnip.tidymodels.org/reference/model_fit.html).

- new_data:

  A rectangular data object, such as a data frame.

- ...:

  Additional arguments passed to
  [`forecast::forecast()`](https://generics.r-lib.org/reference/forecast.html)
