# General Interface for ARIMA Regression Models

`arima_reg()` is a way to generate a *specification* of an ARIMA model
before fitting and allows the model to be created using different
packages. Currently the only package is `forecast`.

## Usage

``` r
arima_reg(
  mode = "regression",
  seasonal_period = NULL,
  non_seasonal_ar = NULL,
  non_seasonal_differences = NULL,
  non_seasonal_ma = NULL,
  seasonal_ar = NULL,
  seasonal_differences = NULL,
  seasonal_ma = NULL
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

- non_seasonal_ar:

  The order of the non-seasonal auto-regressive (AR) terms. Often
  denoted "p" in pdq-notation.

- non_seasonal_differences:

  The order of integration for non-seasonal differencing. Often denoted
  "d" in pdq-notation.

- non_seasonal_ma:

  The order of the non-seasonal moving average (MA) terms. Often denoted
  "q" in pdq-notation.

- seasonal_ar:

  The order of the seasonal auto-regressive (SAR) terms. Often denoted
  "P" in PDQ-notation.

- seasonal_differences:

  The order of integration for seasonal differencing. Often denoted "D"
  in PDQ-notation.

- seasonal_ma:

  The order of the seasonal moving average (SMA) terms. Often denoted
  "Q" in PDQ-notation.

## Details

The data given to the function are not saved and are only used to
determine the *mode* of the model. For `arima_reg()`, the mode will
always be "regression".

The model can be created using the
[`fit()`](https://generics.r-lib.org/reference/fit.html) function using
the following *engines*:

- "auto_arima" (default) - Connects to
  [`forecast::auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html)

- "arima" - Connects to
  [`forecast::Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.html)

**Main Arguments**

The main arguments (tuning parameters) for the model are:

- `seasonal_period`: The periodic nature of the seasonality. Uses "auto"
  by default.

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

These arguments are converted to their specific names at the time that
the model is fit.

Other options and argument can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)
(See Engine Details below).

If parameters need to be modified,
[`update()`](https://rdrr.io/r/stats/update.html) can be used in lieu of
recreating the object from scratch.

## Engine Details

The standardized parameter names in `modeltime` can be mapped to their
original names in each engine:

|                                                            |                              |                                |
|------------------------------------------------------------|------------------------------|--------------------------------|
| modeltime                                                  | forecast::auto.arima         | forecast::Arima                |
| seasonal_period                                            | ts(frequency)                | ts(frequency)                  |
| non_seasonal_ar, non_seasonal_differences, non_seasonal_ma | max.p(5), max.d(2), max.q(5) | order = c(p(0), d(0), q(0))    |
| seasonal_ar, seasonal_differences, seasonal_ma             | max.P(2), max.D(1), max.Q(2) | seasonal = c(P(0), D(0), Q(0)) |

Other options can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

**auto_arima (default engine)**

The engine uses
[`forecast::auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html).

Function Parameters:

    #> function (y, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2, max.Q = 2,
    #>     max.order = 5, max.d = 2, max.D = 1, start.p = 2, start.q = 2, start.P = 1,
    #>     start.Q = 1, stationary = FALSE, seasonal = TRUE, ic = c("aicc", "aic",
    #>         "bic"), stepwise = TRUE, nmodels = 94, trace = FALSE, approximation = (length(x) >
    #>         150 | frequency(x) > 12), method = NULL, truncate = NULL, xreg = NULL,
    #>     test = c("kpss", "adf", "pp"), test.args = list(), seasonal.test = c("seas",
    #>         "ocsb", "hegy", "ch"), seasonal.test.args = list(), allowdrift = TRUE,
    #>     allowmean = TRUE, lambda = NULL, biasadj = FALSE, parallel = FALSE,
    #>     num.cores = 2, x = y, ...)

The *MAXIMUM* nonseasonal ARIMA terms (`max.p`, `max.d`, `max.q`) and
seasonal ARIMA terms (`max.P`, `max.D`, `max.Q`) are provided to
[`forecast::auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html)
via `arima_reg()` parameters. Other options and argument can be set
using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

Parameter Notes:

- All values of nonseasonal pdq and seasonal PDQ are maximums. The
  [`forecast::auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html)
  model will select a value using these as an upper limit.

- `xreg` - This is supplied via the parsnip / modeltime
  [`fit()`](https://generics.r-lib.org/reference/fit.html) interface (so
  don't provide this manually). See Fit Details (below).

**arima**

The engine uses
[`forecast::Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.html).

Function Parameters:

    #> function (y, order = c(0, 0, 0), seasonal = c(0, 0, 0), xreg = NULL, include.mean = TRUE,
    #>     include.drift = FALSE, include.constant, lambda = model$lambda, biasadj = FALSE,
    #>     method = c("CSS-ML", "ML", "CSS"), model = NULL, x = y, ...)

The nonseasonal ARIMA terms (`order`) and seasonal ARIMA terms
(`seasonal`) are provided to
[`forecast::Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.html)
via `arima_reg()` parameters. Other options and argument can be set
using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

Parameter Notes:

- `xreg` - This is supplied via the parsnip / modeltime
  [`fit()`](https://generics.r-lib.org/reference/fit.html) interface (so
  don't provide this manually). See Fit Details (below).

- `method` - The default is set to "ML" (Maximum Likelihood). This
  method is more robust at the expense of speed and possible selections
  may fail unit root inversion testing. Alternatively, you can add
  `method = "CSS-ML"` to evaluate Conditional Sum of Squares for
  starting values, then Maximium Likelihood.

## Fit Details

**Date and Date-Time Variable**

It's a requirement to have a date or date-time variable as a predictor.
The [`fit()`](https://generics.r-lib.org/reference/fit.html) interface
accepts date and date-time features and handles them internally.

- `fit(y ~ date)`

*Seasonal Period Specification*

The period can be non-seasonal (`seasonal_period = 1 or "none"`) or
yearly seasonal (e.g. For monthly time stamps, `seasonal_period = 12`,
`seasonal_period = "12 months"`, or `seasonal_period = "yearly"`). There
are 3 ways to specify:

1.  `seasonal_period = "auto"`: A seasonal period is selected based on
    the periodicity of the data (e.g. 12 if monthly)

2.  `seasonal_period = 12`: A numeric frequency. For example, 12 is
    common for monthly data

3.  `seasonal_period = "1 year"`: A time-based phrase. For example, "1
    year" would convert to 12 for monthly data.

**Univariate (No xregs, Exogenous Regressors):**

For univariate analysis, you must include a date or date-time feature.
Simply use:

- Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.

- XY Interface: `fit_xy(x = data[,"date"], y = data$y)` will ignore
  xreg's.

**Multivariate (xregs, Exogenous Regressors)**

The `xreg` parameter is populated using the
[`fit()`](https://generics.r-lib.org/reference/fit.html) or
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) function:

- Only `factor`, `ordered factor`, and `numeric` data will be used as
  xregs.

- Date and Date-time variables are not used as xregs

- `character` data should be converted to factor.

*Xreg Example:* Suppose you have 3 features:

1.  `y` (target)

2.  `date` (time stamp),

3.  `month.lbl` (labeled month as a ordered factor).

The `month.lbl` is an exogenous regressor that can be passed to the
`arima_reg()` using
[`fit()`](https://generics.r-lib.org/reference/fit.html):

- `fit(y ~ date + month.lbl)` will pass `month.lbl` on as an exogenous
  regressor.

- `fit_xy(data[,c("date", "month.lbl")], y = data$y)` will pass x, where
  x is a data frame containing `month.lbl` and the `date` feature. Only
  `month.lbl` will be used as an exogenous regressor.

Note that date or date-time class values are excluded from `xreg`.

## See also

[`fit.model_spec()`](https://parsnip.tidymodels.org/reference/fit.html),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)

## Examples

``` r
library(dplyr)
library(parsnip)
library(rsample)
library(timetk)

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

# ---- AUTO ARIMA ----

# Model Spec
model_spec <- arima_reg() %>%
    set_engine("auto_arima")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
#> frequency = 12 observations per 1 year
model_fit
#> parsnip model object
#> 
#> Series: outcome 
#> ARIMA(0,1,1)(1,1,1)[12] 
#> 
#> Coefficients:
#>           ma1    sar1     sma1
#>       -0.3591  0.2034  -0.7114
#> s.e.   0.0702  0.1166   0.0970
#> 
#> sigma^2 = 0.0003485:  log likelihood = 592.09
#> AIC=-1176.17   AICc=-1176   BIC=-1162.4


# ---- STANDARD ARIMA ----

# Model Spec
model_spec <- arima_reg(
        seasonal_period          = 12,
        non_seasonal_ar          = 3,
        non_seasonal_differences = 1,
        non_seasonal_ma          = 3,
        seasonal_ar              = 1,
        seasonal_differences     = 0,
        seasonal_ma              = 1
    ) %>%
    set_engine("arima")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
model_fit
#> parsnip model object
#> 
#> Series: outcome 
#> ARIMA(3,1,3)(1,0,1)[12] 
#> 
#> Coefficients:
#>          ar1     ar2      ar3      ma1      ma2     ma3    sar1     sma1
#>       0.2258  0.2542  -0.2801  -0.5205  -0.2663  0.2491  0.9846  -0.5381
#> s.e.  0.6883  0.4581   0.2378   0.6995   0.4192  0.3959  0.0077   0.0751
#> 
#> sigma^2 = 0.0003465:  log likelihood = 613.46
#> AIC=-1208.91   AICc=-1208.14   BIC=-1177.48
```
