# Bridge prediction function for Boosted PROPHET models

Bridge prediction function for Boosted PROPHET models

## Usage

``` r
prophet_xgboost_predict_impl(object, new_data, ...)
```

## Arguments

- object:

  A [model
  fit](https://parsnip.tidymodels.org/reference/model_fit.html).

- new_data:

  A rectangular data object, such as a data frame.

- ...:

  Additional arguments passed to `prophet::predict()`
