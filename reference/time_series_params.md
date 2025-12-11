# Tuning Parameters for Time Series (ts-class) Models

Tuning Parameters for Time Series (ts-class) Models

## Usage

``` r
seasonal_period(values = c("none", "daily", "weekly", "yearly"))
```

## Arguments

- values:

  A time-based phrase

## Details

Time series models (e.g.
[`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.html)
and [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.html))
use [`stats::ts()`](https://rdrr.io/r/stats/ts.html) or
[`forecast::msts()`](https://pkg.robjhyndman.com/forecast/reference/msts.html)
to apply seasonality. We can do the same process using the following
general time series parameter:

- `period`: The periodic nature of the seasonality.

It's usually best practice to *not* tune this parameter, but rather set
to obvious values based on the seasonality of the data:

- **Daily Seasonality:** Often used with **hourly data** (e.g. 24 hourly
  timestamps per day)

- **Weekly Seasonality:** Often used with **daily data** (e.g. 7 daily
  timestamps per week)

- **Yearly Seasonalty:** Often used with **weekly, monthly, and
  quarterly data** (e.g. 12 monthly observations per year).

However, in the event that users want to experiment with period tuning,
you can do so with `seasonal_period()`.

## Examples

``` r
seasonal_period()
#> Period (Seasonal Frequency) (qualitative)
#> 4 possible values include:
#> 'none', 'daily', 'weekly', and 'yearly'


```
