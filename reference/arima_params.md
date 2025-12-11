# Tuning Parameters for ARIMA Models

Tuning Parameters for ARIMA Models

## Usage

``` r
non_seasonal_ar(range = c(0L, 5L), trans = NULL)

non_seasonal_differences(range = c(0L, 2L), trans = NULL)

non_seasonal_ma(range = c(0L, 5L), trans = NULL)

seasonal_ar(range = c(0L, 2L), trans = NULL)

seasonal_differences(range = c(0L, 1L), trans = NULL)

seasonal_ma(range = c(0L, 2L), trans = NULL)
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

The main parameters for ARIMA models are:

- `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR)
  terms.

- `non_seasonal_differences`: The order of integration for non-seasonal
  differencing.

- `non_seasonal_ma`: The order of the non-seasonal moving average (MA)
  terms.

- `seasonal_ar`: The order of the seasonal auto-regressive (SAR) terms.

- `seasonal_differences`: The order of integration for seasonal
  differencing.

- `seasonal_ma`: The order of the seasonal moving average (SMA) terms.

## Examples

``` r
ets_model()
#> ETS Model (qualitative)
#> 6 possible values include:
#> 'ZZZ', 'XXX', 'YYY', 'CCC', 'PPP', and 'FFF'

non_seasonal_ar()
#> Non-seasonal AR Term (quantitative)
#> Range: [0, 5]

non_seasonal_differences()
#> Non-seasonal Differencing Term (quantitative)
#> Range: [0, 2]

non_seasonal_ma()
#> Non-seasonal MA Term (quantitative)
#> Range: [0, 5]

```
