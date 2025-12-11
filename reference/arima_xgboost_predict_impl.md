# Bridge prediction Function for ARIMA-XGBoost Models

Bridge prediction Function for ARIMA-XGBoost Models

## Usage

``` r
arima_xgboost_predict_impl(object, new_data, ...)
```

## Arguments

- object:

  A [model
  fit](https://parsnip.tidymodels.org/reference/model_fit.html).

- new_data:

  A rectangular data object, such as a data frame.

- ...:

  Additional arguments passed to `predict.xgb.Booster()`
