# Tuning Parameters for NNETAR Models

Tuning Parameters for NNETAR Models

## Usage

``` r
num_networks(range = c(1L, 100L), trans = NULL)
```

## Arguments

- range:

  A two-element vector holding the *defaults* for the smallest and
  largest possible values, respectively. If a transformation is
  specified, these values should be in the *transformed units*.

- trans:

  A `trans` object from the `scales` package, such as
  [`scales::transform_log10()`](https://scales.r-lib.org/reference/transform_log.html)
  or
  [`scales::transform_reciprocal()`](https://scales.r-lib.org/reference/transform_reciprocal.html).
  If not provided, the default is used which matches the units used in
  `range`. If no transformation, `NULL`.

## Details

The main parameters for NNETAR models are:

- `non_seasonal_ar`: Number of non-seasonal auto-regressive (AR) lags.
  Often denoted "p" in pdq-notation.

- `seasonal_ar`: Number of seasonal auto-regressive (SAR) lags. Often
  denoted "P" in PDQ-notation.

- `hidden_units`: An integer for the number of units in the hidden
  model.

- `num_networks`: Number of networks to fit with different random
  starting weights. These are then averaged when producing forecasts.

- `penalty`: A non-negative numeric value for the amount of weight
  decay.

- `epochs`: An integer for the number of training iterations.

## See also

[`non_seasonal_ar()`](https://business-science.github.io/modeltime/reference/arima_params.md),
[`seasonal_ar()`](https://business-science.github.io/modeltime/reference/arima_params.md),
[`dials::hidden_units()`](https://dials.tidymodels.org/reference/dropout.html),
[`dials::penalty()`](https://dials.tidymodels.org/reference/penalty.html),
[`dials::epochs()`](https://dials.tidymodels.org/reference/dropout.html)

## Examples

``` r
num_networks()
#> Number of Neural Networks to Average (quantitative)
#> Range: [1, 100]
```
