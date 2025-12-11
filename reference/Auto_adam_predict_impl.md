# Bridge prediction function for AUTO ADAM models

Bridge prediction function for AUTO ADAM models

## Usage

``` r
Auto_adam_predict_impl(object, new_data, ...)
```

## Arguments

- object:

  A [model
  fit](https://parsnip.tidymodels.org/reference/model_fit.html).

- new_data:

  A rectangular data object, such as a data frame.

- ...:

  Additional arguments passed to
  [`smooth::auto.adam()`](https://rdrr.io/pkg/smooth/man/adam.html)
