# General Interface for Temporal Hierarchical Forecasting (THIEF) Models

`temporal_hierarchy()` is a way to generate a *specification* of an
Temporal Hierarchical Forecasting model before fitting and allows the
model to be created using different packages. Currently the only package
is `thief`. Note this function requires the `thief` package to be
installed.

## Usage

``` r
temporal_hierarchy(
  mode = "regression",
  seasonal_period = NULL,
  combination_method = NULL,
  use_model = NULL
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression".

- seasonal_period:

  A seasonal frequency. Uses "auto" by default. A character phrase of
  "auto" or time-based phrase of "2 weeks" can be used if a date or
  date-time variable is provided. See Fit Details below.

- combination_method:

  Combination method of temporal hierarchies, taking one of the
  following values:

  - "struc" - Structural scaling: weights from temporal hierarchy

  - "mse" - Variance scaling: weights from in-sample MSE

  - "ols" - Unscaled OLS combination weights

  - "bu" - Bottom-up combination – i.e., all aggregate forecasts are
    ignored.

  - "shr" - GLS using a shrinkage (to block diagonal) estimate of
    residuals

  - "sam" - GLS using sample covariance matrix of residuals

- use_model:

  Model used for forecasting each aggregation level:

  - "ets" - exponential smoothing

  - "arima" - arima

  - "theta" - theta

  - "naive" - random walk forecasts

  - "snaive" - seasonal naive forecasts, based on the last year of
    observed data

## Details

Models can be created using the following *engines*:

- "thief" (default) - Connects to
  [`thief::thief()`](http://robjhyndman.github.io/thief/reference/thief.md)

## Engine Details

The standardized parameter names in `modeltime` can be mapped to their
original names in each engine:

|                    |                |
|--------------------|----------------|
| modeltime          | thief::thief() |
| combination_method | comb           |
| use_model          | usemodel       |

Other options can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

**thief (default engine)**

The engine uses
[`thief::thief()`](http://robjhyndman.github.io/thief/reference/thief.md).

Function Parameters:

    #> function (y, m = frequency(y), h = m * 2, comb = c("struc", "mse", "ols",
    #>     "bu", "shr", "sam"), usemodel = c("ets", "arima", "theta", "naive",
    #>     "snaive"), forecastfunction = NULL, aggregatelist = NULL, ...)

Other options and argument can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

Parameter Notes:

- `xreg` - This model is not set up to use exogenous regressors. Only
  univariate models will be fit.

## Fit Details

**Date and Date-Time Variable**

It's a requirement to have a date or date-time variable as a predictor.
The [`fit()`](https://generics.r-lib.org/reference/fit.html) interface
accepts date and date-time features and handles them internally.

- `fit(y ~ date)`

**Univariate:**

For univariate analysis, you must include a date or date-time feature.
Simply use:

- Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.

- XY Interface: `fit_xy(x = data[,"date"], y = data$y)` will ignore
  xreg's.

**Multivariate (xregs, Exogenous Regressors)**

This model is not set up for use with exogenous regressors.

## References

- For forecasting with temporal hierarchies see: Athanasopoulos G.,
  Hyndman R.J., Kourentzes N., Petropoulos F. (2017) Forecasting with
  Temporal Hierarchies. *European Journal of Operational research*,
  **262**(**1**), 60-74.

- For combination operators see: Kourentzes N., Barrow B.K., Crone
  S.F. (2014) Neural network ensemble operators for time series
  forecasting. *Expert Systems with Applications*, **41**(**9**),
  4235-4244.

## See also

[`fit.model_spec()`](https://parsnip.tidymodels.org/reference/fit.html),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)

## Examples

``` r
library(dplyr)
library(parsnip)
library(rsample)
library(timetk)
library(thief)

# Data
m750 <- m4_monthly %>% filter(id == "M750")
m750
#> # A tibble: 306 × 3
#>    id    date       value
#>    <fct> <date>     <dbl>
#>  1 M750  1990-01-01  6370
#>  2 M750  1990-02-01  6430
#>  3 M750  1990-03-01  6520
#>  4 M750  1990-04-01  6580
#>  5 M750  1990-05-01  6620
#>  6 M750  1990-06-01  6690
#>  7 M750  1990-07-01  6000
#>  8 M750  1990-08-01  5450
#>  9 M750  1990-09-01  6480
#> 10 M750  1990-10-01  6820
#> # ℹ 296 more rows

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)

# ---- HIERARCHICAL ----

# Model Spec - The default parameters are all set
# to "auto" if none are provided
model_spec <- temporal_hierarchy() %>%
    set_engine("thief")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
#> frequency = 12 observations per 1 year
model_fit
#> parsnip model object
#> 
#>         Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
#> 21                                     9.292876 9.278808 9.177530 9.161701
#> 22 9.292288 9.290665 9.310183 9.314852 9.318892 9.304824 9.203573 9.187635
#> 23 9.318275 9.316547 9.336091 9.340759                                    
#>         Sep      Oct      Nov      Dec
#> 21 9.210732 9.279095 9.292842 9.296845
#> 22 9.236714 9.305078 9.318782 9.322784
#> 23                                    


```
