# General Interface for Exponential Smoothing State Space Models

`exp_smoothing()` is a way to generate a *specification* of an
Exponential Smoothing model before fitting and allows the model to be
created using different packages. Currently the only package is
`forecast`. Several algorithms are implemented:

- ETS - Automated Exponential Smoothing

- CROSTON - Croston's forecast is a special case of Exponential
  Smoothing for intermittent demand

- Theta - A special case of Exponential Smoothing with Drift that
  performed well in the M3 Competition

## Usage

``` r
exp_smoothing(
  mode = "regression",
  seasonal_period = NULL,
  error = NULL,
  trend = NULL,
  season = NULL,
  damping = NULL,
  smooth_level = NULL,
  smooth_trend = NULL,
  smooth_seasonal = NULL
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

- error:

  The form of the error term: "auto", "additive", or "multiplicative".
  If the error is multiplicative, the data must be non-negative.

- trend:

  The form of the trend term: "auto", "additive", "multiplicative" or
  "none".

- season:

  The form of the seasonal term: "auto", "additive", "multiplicative" or
  "none".

- damping:

  Apply damping to a trend: "auto", "damped", or "none".

- smooth_level:

  This is often called the "alpha" parameter used as the base level
  smoothing factor for exponential smoothing models.

- smooth_trend:

  This is often called the "beta" parameter used as the trend smoothing
  factor for exponential smoothing models.

- smooth_seasonal:

  This is often called the "gamma" parameter used as the seasonal
  smoothing factor for exponential smoothing models.

## Details

Models can be created using the following *engines*:

- "ets" (default) - Connects to
  [`forecast::ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.html)

- "croston" - Connects to
  [`forecast::croston()`](https://pkg.robjhyndman.com/forecast/reference/forecast.croston_model.html)

- "theta" - Connects to
  [`forecast::thetaf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.theta_model.html)

- "smooth_es" - Connects to
  [`smooth::es()`](https://rdrr.io/pkg/smooth/man/es.html)

## Engine Details

The standardized parameter names in `modeltime` can be mapped to their
original names in each engine:

|                            |               |                     |                    |                    |
|----------------------------|---------------|---------------------|--------------------|--------------------|
| modeltime                  | forecast::ets | forecast::croston() | forecast::thetaf() | smooth::es()       |
| seasonal_period()          | ts(frequency) | ts(frequency)       | ts(frequency)      | ts(frequency)      |
| error(), trend(), season() | model ('ZZZ') | NA                  | NA                 | model('ZZZ')       |
| damping()                  | damped (NULL) | NA                  | NA                 | phi                |
| smooth_level()             | alpha (NULL)  | alpha (0.1)         | NA                 | persistence(alpha) |
| smooth_trend()             | beta (NULL)   | NA                  | NA                 | persistence(beta)  |
| smooth_seasonal()          | gamma (NULL)  | NA                  | NA                 | persistence(gamma) |

Other options can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

**ets (default engine)**

The engine uses
[`forecast::ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.html).

Function Parameters:

    #> function (y, model = "ZZZ", damped = NULL, alpha = NULL, beta = NULL, gamma = NULL,
    #>     phi = NULL, additive.only = FALSE, lambda = NULL, biasadj = FALSE,
    #>     lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999, 3), 0.98), opt.crit = c("lik",
    #>         "amse", "mse", "sigma", "mae"), nmse = 3, bounds = c("both", "usual",
    #>         "admissible"), ic = c("aicc", "aic", "bic"), restrict = TRUE, allow.multiplicative.trend = FALSE,
    #>     use.initial.values = FALSE, na.action = c("na.contiguous", "na.interp",
    #>         "na.fail"), ...)

The main arguments are `model` and `damped` are defined using:

- [`error()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  = "auto", "additive", and "multiplicative" are converted to "Z", "A",
  and "M"

- [`trend()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  = "auto", "additive", "multiplicative", and "none" are converted to
  "Z","A","M" and "N"

- [`season()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  = "auto", "additive", "multiplicative", and "none" are converted to
  "Z","A","M" and "N"

- [`damping()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md) -
  "auto", "damped", "none" are converted to NULL, TRUE, FALSE

- [`smooth_level()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md),
  [`smooth_trend()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md),
  and
  [`smooth_seasonal()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  are automatically determined if not provided. They are mapped to
  "alpha", "beta" and "gamma", respectively.

By default, all arguments are set to "auto" to perform automated
Exponential Smoothing using *in-sample data* following the underlying
[`forecast::ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.html)
automation routine.

Other options and argument can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

Parameter Notes:

- `xreg` - This model is not set up to use exogenous regressors. Only
  univariate models will be fit.

**croston**

The engine uses
[`forecast::croston()`](https://pkg.robjhyndman.com/forecast/reference/forecast.croston_model.html).

Function Parameters:

    #> function (y, h = 10, alpha = 0.1, x = y)

The main arguments are defined using:

- [`smooth_level()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md):
  The "alpha" parameter

Parameter Notes:

- `xreg` - This model is not set up to use exogenous regressors. Only
  univariate models will be fit.

**theta**

The engine uses
[`forecast::thetaf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.theta_model.html)

Parameter Notes:

- `xreg` - This model is not set up to use exogenous regressors. Only
  univariate models will be fit.

**smooth_es**

The engine uses
[`smooth::es()`](https://rdrr.io/pkg/smooth/man/es.html).

Function Parameters:

    #> function (y, model = "ZXZ", lags = c(frequency(y)), persistence = NULL,
    #>     phi = NULL, initial = c("backcasting", "optimal", "two-stage", "complete"),
    #>     initialSeason = NULL, ic = c("AICc", "AIC", "BIC", "BICc"), loss = c("likelihood",
    #>         "MSE", "MAE", "HAM", "MSEh", "TMSE", "GTMSE", "MSCE"), h = 10,
    #>     holdout = FALSE, bounds = c("usual", "admissible", "none"), silent = TRUE,
    #>     xreg = NULL, regressors = c("use", "select"), initialX = NULL, ...)

The main arguments `model` and `phi` are defined using:

- [`error()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  = "auto", "additive" and "multiplicative" are converted to "Z", "A"
  and "M"

- [`trend()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  = "auto", "additive", "multiplicative", "additive_damped",
  "multiplicative_damped" and "none" are converted to "Z", "A", "M",
  "Ad", "Md" and "N".

- [`season()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  = "auto", "additive", "multiplicative", and "none" are converted "Z",
  "A","M" and "N"

- [`damping()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md) -
  Value of damping parameter. If NULL, then it is estimated.

- [`smooth_level()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md),
  [`smooth_trend()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md),
  and
  [`smooth_seasonal()`](https://business-science.github.io/modeltime/reference/exp_smoothing_params.md)
  are automatically determined if not provided. They are mapped to
  "persistence"("alpha", "beta" and "gamma", respectively).

By default, all arguments are set to "auto" to perform automated
Exponential Smoothing using *in-sample data* following the underlying
[`smooth::es()`](https://rdrr.io/pkg/smooth/man/es.html) automation
routine.

Other options and argument can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

Parameter Notes:

- `xreg` - This is supplied via the parsnip / modeltime
  [`fit()`](https://generics.r-lib.org/reference/fit.html) interface (so
  don't provide this manually). See Fit Details (below).

## Fit Details

**Date and Date-Time Variable**

It's a requirement to have a date or date-time variable as a predictor.
The [`fit()`](https://generics.r-lib.org/reference/fit.html) interface
accepts date and date-time features and handles them internally.

- `fit(y ~ date)`

*Seasonal Period Specification*

The period can be non-seasonal (`seasonal_period = 1` or `"none"`) or
seasonal (e.g. `seasonal_period = 12` or
`seasonal_period = "12 months"`). There are 3 ways to specify:

1.  `seasonal_period = "auto"`: A period is selected based on the
    periodicity of the data (e.g. 12 if monthly)

2.  `seasonal_period = 12`: A numeric frequency. For example, 12 is
    common for monthly data

3.  `seasonal_period = "1 year"`: A time-based phrase. For example, "1
    year" would convert to 12 for monthly data.

**Univariate:**

For univariate analysis, you must include a date or date-time feature.
Simply use:

- Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.

- XY Interface: `fit_xy(x = data[,"date"], y = data$y)` will ignore
  xreg's.

**Multivariate (xregs, Exogenous Regressors)**

Just for `smooth` engine.

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
[`arima_reg()`](https://business-science.github.io/modeltime/reference/arima_reg.md)
using [`fit()`](https://generics.r-lib.org/reference/fit.html):

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
library(smooth)

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

# ---- AUTO ETS ----

# Model Spec - The default parameters are all set
# to "auto" if none are provided
model_spec <- exp_smoothing() %>%
    set_engine("ets")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
#> frequency = 12 observations per 1 year
model_fit
#> parsnip model object
#> 
#> ETS(A,A,A) 
#> 
#> Call:
#> forecast::ets(y = outcome, model = model_ets, damped = damping_ets, 
#>     alpha = alpha, beta = beta, gamma = gamma)
#> 
#>   Smoothing parameters:
#>     alpha = 0.5893 
#>     beta  = 1e-04 
#>     gamma = 0.1771 
#> 
#>   Initial states:
#>     l = 8.7377 
#>     b = 0.002 
#>     s = 0.029 0.0259 0.0144 -0.0272 -0.1369 -0.0764
#>            0.0209 0.0358 0.036 0.035 0.0274 0.016
#> 
#>   sigma:  0.0186
#> 
#>       AIC      AICc       BIC 
#> -584.7384 -582.0304 -525.2865 


# ---- STANDARD ETS ----

# Model Spec
model_spec <- exp_smoothing(
        seasonal_period  = 12,
        error            = "multiplicative",
        trend            = "additive",
        season           = "multiplicative"
    ) %>%
    set_engine("ets")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
model_fit
#> parsnip model object
#> 
#> ETS(M,Ad,M) 
#> 
#> Call:
#> forecast::ets(y = outcome, model = model_ets, damped = damping_ets, 
#>     alpha = alpha, beta = beta, gamma = gamma)
#> 
#>   Smoothing parameters:
#>     alpha = 0.5889 
#>     beta  = 0.0065 
#>     gamma = 0.203 
#>     phi   = 0.98 
#> 
#>   Initial states:
#>     l = 8.7353 
#>     b = 0.0054 
#>     s = 1.0027 1.0025 1.0012 0.9972 0.9839 0.9921
#>            1.0024 1.0041 1.0045 1.0039 1.0033 1.0022
#> 
#>   sigma:  0.0021
#> 
#>       AIC      AICc       BIC 
#> -576.9488 -573.9088 -513.9998 


# ---- CROSTON ----
# \donttest{
# Model Spec
model_spec <- exp_smoothing(
        smooth_level = 0.2
    ) %>%
    set_engine("croston")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
model_fit
#> parsnip model object
#> 
#> Croston Method
#> ---
# }



# ---- THETA ----
# \donttest{
#' # Model Spec
model_spec <- exp_smoothing() %>%
    set_engine("theta")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
model_fit
#> parsnip model object
#> 
#> Theta Method
#> ---
# }




#' # ---- SMOOTH ----
# \donttest{
#' # Model Spec
model_spec <- exp_smoothing(
               seasonal_period  = 12,
               error            = "multiplicative",
               trend            = "additive_damped",
               season           = "additive"
         ) %>%
    set_engine("smooth_es")

# Fit Spec
model_fit <- model_spec %>%
    fit(value ~ date, data = training(splits))
model_fit
#> parsnip model object
#> 
#> Time elapsed: 0.05 seconds
#> Model estimated using es() function: ETS(MAdA)
#> With backcasting initialisation
#> Distribution assumed in the model: Normal
#> Loss function type: likelihood; Loss function value: 1570.127
#> Persistence vector g:
#>  alpha   beta  gamma 
#> 0.6272 0.0534 0.2153 
#> Damping parameter: 0.0124
#> Sample size: 244
#> Number of estimated parameters: 5
#> Number of degrees of freedom: 239
#> Information criteria:
#>      AIC     AICc      BIC     BICc 
#> 3150.254 3150.506 3167.740 3168.433 
# }
```
