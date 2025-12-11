# Tuning Parameters for Exponential Smoothing Models

Tuning Parameters for Exponential Smoothing Models

## Usage

``` r
error(values = c("additive", "multiplicative"))

trend(values = c("additive", "multiplicative", "none"))

trend_smooth(
  values = c("additive", "multiplicative", "none", "additive_damped",
    "multiplicative_damped")
)

season(values = c("additive", "multiplicative", "none"))

damping(values = c("none", "damped"))

damping_smooth(range = c(0, 2), trans = NULL)

smooth_level(range = c(0, 1), trans = NULL)

smooth_trend(range = c(0, 1), trans = NULL)

smooth_seasonal(range = c(0, 1), trans = NULL)
```

## Arguments

- values:

  A character string of possible values.

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

The main parameters for Exponential Smoothing models are:

- `error`: The form of the error term: additive", or "multiplicative".
  If the error is multiplicative, the data must be non-negative.

- `trend`: The form of the trend term: "additive", "multiplicative" or
  "none".

- `season`: The form of the seasonal term: "additive", "multiplicative"
  or "none"..

- `damping`: Apply damping to a trend: "damped", or "none".

- `smooth_level`: This is often called the "alpha" parameter used as the
  base level smoothing factor for exponential smoothing models.

- `smooth_trend`: This is often called the "beta" parameter used as the
  trend smoothing factor for exponential smoothing models.

- `smooth_seasonal`: This is often called the "gamma" parameter used as
  the seasonal smoothing factor for exponential smoothing models.

## Examples

``` r
error()
#> Error Term (qualitative)
#> 2 possible values include:
#> 'additive' and 'multiplicative'

trend()
#> Trend Term (qualitative)
#> 3 possible values include:
#> 'additive', 'multiplicative', and 'none'

season()
#> Season Term (qualitative)
#> 3 possible values include:
#> 'additive', 'multiplicative', and 'none'
```
