# General Interface for NNETAR Regression Models

`nnetar_reg()` is a way to generate a *specification* of an NNETAR model
before fitting and allows the model to be created using different
packages. Currently the only package is `forecast`.

## Usage

``` r
nnetar_reg(
  mode = "regression",
  seasonal_period = NULL,
  non_seasonal_ar = NULL,
  seasonal_ar = NULL,
  hidden_units = NULL,
  num_networks = NULL,
  penalty = NULL,
  epochs = NULL
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

- seasonal_ar:

  The order of the seasonal auto-regressive (SAR) terms. Often denoted
  "P" in PDQ-notation.

- hidden_units:

  An integer for the number of units in the hidden model.

- num_networks:

  Number of networks to fit with different random starting weights.
  These are then averaged when producing forecasts.

- penalty:

  A non-negative numeric value for the amount of weight decay.

- epochs:

  An integer for the number of training iterations.

## Details

The data given to the function are not saved and are only used to
determine the *mode* of the model. For `nnetar_reg()`, the mode will
always be "regression".

The model can be created using the
[`fit()`](https://generics.r-lib.org/reference/fit.html) function using
the following *engines*:

- "nnetar" (default) - Connects to
  [`forecast::nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.html)

**Main Arguments**

The main arguments (tuning parameters) for the model are the parameters
in `nnetar_reg()` function. These arguments are converted to their
specific names at the time that the model is fit.

Other options and argument can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)
(See Engine Details below).

If parameters need to be modified,
[`update()`](https://rdrr.io/r/stats/update.html) can be used in lieu of
recreating the object from scratch.

## Engine Details

The standardized parameter names in `modeltime` can be mapped to their
original names in each engine:

|                 |                  |
|-----------------|------------------|
| modeltime       | forecast::nnetar |
| seasonal_period | ts(frequency)    |
| non_seasonal_ar | p (1)            |
| seasonal_ar     | P (1)            |
| hidden_units    | size (10)        |
| num_networks    | repeats (20)     |
| epochs          | maxit (100)      |
| penalty         | decay (0)        |

Other options can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

**nnetar**

The engine uses
[`forecast::nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.html).

Function Parameters:

    #> function (y, p, P = 1, size, repeats = 20, xreg = NULL, lambda = NULL,
    #>     model = NULL, subset = NULL, scale.inputs = TRUE, x = y, ...)

Parameter Notes:

- `xreg` - This is supplied via the parsnip / modeltime
  [`fit()`](https://generics.r-lib.org/reference/fit.html) interface (so
  don't provide this manually). See Fit Details (below).

- `size` - Is set to 10 by default. This differs from the `forecast`
  implementation

- `p` and `P` - Are set to 1 by default.

- `maxit` and `decay` are
  [`nnet::nnet`](https://rdrr.io/pkg/nnet/man/nnet.html) parameters that
  are exposed in the `nnetar_reg()` interface. These are key tuning
  parameters.

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
`nnetar_reg()` using
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

# ---- NNETAR ----

# Model Spec
model_spec <- nnetar_reg() %>%
    set_engine("nnetar")

# Fit Spec
set.seed(123)
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
#> frequency = 12 observations per 1 year
model_fit
#> parsnip model object
#> 
#> Series: outcome 
#> Model:  NNAR(1,1,10)[12] 
#> Call:   forecast::nnetar(y = outcome, p = p, P = P, size = size, repeats = repeats, 
#>     xreg = xreg_matrix, decay = decay, maxit = maxit)
#> 
#> Average of 20 networks, each of which is
#> a 2-10-1 network with 41 weights
#> options were - linear output units 
#> 
#> sigma^2 estimated as 0.0005869


```
