# Tuning Parameters for Prophet Models

Tuning Parameters for Prophet Models

## Usage

``` r
growth(values = c("linear", "logistic"))

changepoint_num(range = c(0L, 50L), trans = NULL)

changepoint_range(range = c(0.6, 0.9), trans = NULL)

seasonality_yearly(values = c(TRUE, FALSE))

seasonality_weekly(values = c(TRUE, FALSE))

seasonality_daily(values = c(TRUE, FALSE))

prior_scale_changepoints(range = c(-3, 2), trans = log10_trans())

prior_scale_seasonality(range = c(-3, 2), trans = log10_trans())

prior_scale_holidays(range = c(-3, 2), trans = log10_trans())
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

The main parameters for Prophet models are:

- `growth`: The form of the trend: "linear", or "logistic".

- `changepoint_num`: The maximum number of trend changepoints allowed
  when modeling the trend

- `changepoint_range`: The range affects how close the changepoints can
  go to the end of the time series. The larger the value, the more
  flexible the trend.

- Yearly, Weekly, and Daily Seasonality:

  - *Yearly*: `seasonality_yearly` - Useful when seasonal patterns
    appear year-over-year

  - *Weekly*: `seasonality_weekly` - Useful when seasonal patterns
    appear week-over-week (e.g. daily data)

  - *Daily*: `seasonality_daily` - Useful when seasonal patterns appear
    day-over-day (e.g. hourly data)

- `season`:

  - The form of the seasonal term: "additive" or "multiplicative".

  - See
    [`season()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md).

- "Prior Scale": Controls flexibility of

  - *Changepoints:* `prior_scale_changepoints`

  - *Seasonality:* `prior_scale_seasonality`

  - *Holidays:* `prior_scale_holidays`

  - The `log10_trans()` converts priors to a scale from 0.001 to 100,
    which effectively weights lower values more heavily than larger
    values.

## Examples

``` r
growth()
#> Growth Trend (qualitative)
#> 2 possible values include:
#> 'linear' and 'logistic'

changepoint_num()
#> Number of Possible Trend Changepoints (quantitative)
#> Range: [0, 50]

season()
#> Season Term (qualitative)
#> 3 possible values include:
#> 'additive', 'multiplicative', and 'none'

prior_scale_changepoints()
#> Prior Scale Changepoints (quantitative)
#> Transformer: log-10 [1e-100, Inf]
#> Range (transformed scale): [-3, 2]
```
