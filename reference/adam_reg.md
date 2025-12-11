# General Interface for ADAM Regression Models

`adam_reg()` is a way to generate a *specification* of an ADAM model
before fitting and allows the model to be created using different
packages. Currently the only package is `smooth`.

## Usage

``` r
adam_reg(
  mode = "regression",
  ets_model = NULL,
  non_seasonal_ar = NULL,
  non_seasonal_differences = NULL,
  non_seasonal_ma = NULL,
  seasonal_ar = NULL,
  seasonal_differences = NULL,
  seasonal_ma = NULL,
  use_constant = NULL,
  regressors_treatment = NULL,
  outliers_treatment = NULL,
  outliers_ci = NULL,
  probability_model = NULL,
  distribution = NULL,
  loss = NULL,
  information_criteria = NULL,
  seasonal_period = NULL,
  select_order = NULL
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression".

- ets_model:

  The type of ETS model. The first letter stands for the type of the
  error term ("A" or "M"), the second (and sometimes the third as well)
  is for the trend ("N", "A", "Ad", "M" or "Md"), and the last one is
  for the type of seasonality ("N", "A" or "M").

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

- use_constant:

  Logical, determining, whether the constant is needed in the model or
  not. This is mainly needed for ARIMA part of the model, but can be
  used for ETS as well.

- regressors_treatment:

  The variable defines what to do with the provided explanatory
  variables: "use" means that all of the data should be used, while
  "select" means that a selection using ic should be done, "adapt" will
  trigger the mechanism of time varying parameters for the explanatory
  variables.

- outliers_treatment:

  Defines what to do with outliers: "ignore", so just returning the
  model, "detect" outliers based on specified level and include dummies
  for them in the model, or detect and "select" those of them that
  reduce ic value.

- outliers_ci:

  What confidence level to use for detection of outliers. Default is
  99%.

- probability_model:

  The type of model used in probability estimation. Can be "none" -
  none, "fixed" - constant probability, "general" - the general Beta
  model with two parameters, "odds-ratio" - the Odds-ratio model with
  b=1 in Beta distribution, "inverse-odds-ratio" - the model with a=1 in
  Beta distribution, "direct" - the TSB-like (Teunter et al., 2011)
  probability update mechanism a+b=1, "auto" - the automatically
  selected type of occurrence model.

- distribution:

  what density function to assume for the error term. The full name of
  the distribution should be provided, starting with the letter "d" -
  "density".

- loss:

  The type of Loss Function used in optimization.

- information_criteria:

  The information criterion to use in the model selection / combination
  procedure.

- seasonal_period:

  A seasonal frequency. Uses "auto" by default. A character phrase of
  "auto" or time-based phrase of "2 weeks" can be used if a date or
  date-time variable is provided. See Fit Details below.

- select_order:

  If `TRUE`, then the function will select the most appropriate order.
  The values list(ar=...,i=...,ma=...) specify the maximum orders to
  check in this case.

## Details

The data given to the function are not saved and are only used to
determine the *mode* of the model. For `adam_reg()`, the mode will
always be "regression".

The model can be created using the
[`fit()`](https://generics.r-lib.org/reference/fit.html) function using
the following *engines*:

- "auto_adam" (default) - Connects to
  [`smooth::auto.adam()`](https://rdrr.io/pkg/smooth/man/adam.html)

- "adam" - Connects to
  [`smooth::adam()`](https://rdrr.io/pkg/smooth/man/adam.html)

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

- `ets_model`: The type of ETS model.

- `use_constant`: Logical, determining, whether the constant is needed
  in the model or not.

- `regressors_treatment`: The variable defines what to do with the
  provided explanatory variables.

- `outliers_treatment`: Defines what to do with outliers.

- `probability_model`: The type of model used in probability estimation.

- `distribution`: what density function to assume for the error term.

- `loss`: The type of Loss Function used in optimization.

- `information_criteria`: The information criterion to use in the model
  selection / combination procedure.

These arguments are converted to their specific names at the time that
the model is fit.

Other options and argument can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)
(See Engine Details below).

If parameters need to be modified,
[`update()`](https://rdrr.io/r/stats/update.html) can be used in lieu of
recreating the object from scratch.

**auto_adam (default engine)**

The engine uses
[`smooth::auto.adam()`](https://rdrr.io/pkg/smooth/man/adam.html).

Function Parameters:

    #> function (data, model = "ZXZ", lags = c(frequency(data)), orders = list(ar = c(3,
    #>     3), i = c(2, 1), ma = c(3, 3), select = TRUE), formula = NULL, regressors = c("use",
    #>     "select", "adapt"), occurrence = c("none", "auto", "fixed", "general",
    #>     "odds-ratio", "inverse-odds-ratio", "direct"), distribution = c("dnorm",
    #>     "dlaplace", "ds", "dgnorm", "dlnorm", "dinvgauss", "dgamma"), outliers = c("ignore",
    #>     "use", "select"), level = 0.99, h = 0, holdout = FALSE, persistence = NULL,
    #>     phi = NULL, initial = c("optimal", "backcasting", "complete"), arma = NULL,
    #>     ic = c("AICc", "AIC", "BIC", "BICc"), bounds = c("usual", "admissible",
    #>         "none"), silent = TRUE, parallel = FALSE, ...)

The *MAXIMUM* nonseasonal ARIMA terms (`max.p`, `max.d`, `max.q`) and
seasonal ARIMA terms (`max.P`, `max.D`, `max.Q`) are provided to
[`forecast::auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html)
via
[`arima_reg()`](https://business-science.github.io/modeltime/reference/arima_reg.md)
parameters. Other options and argument can be set using
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

Parameter Notes:

- All values of nonseasonal pdq and seasonal PDQ are maximums. The
  [`smooth::auto.adam()`](https://rdrr.io/pkg/smooth/man/adam.html)
  model will select a value using these as an upper limit.

- `xreg` - This is supplied via the parsnip / modeltime
  [`fit()`](https://generics.r-lib.org/reference/fit.html) interface (so
  don't provide this manually). See Fit Details (below).

**adam**

The engine uses
[`smooth::adam()`](https://rdrr.io/pkg/smooth/man/adam.html).

Function Parameters:

    #> function (data, model = "ZXZ", lags = c(frequency(data)), orders = list(ar = c(0),
    #>     i = c(0), ma = c(0), select = FALSE), constant = FALSE, formula = NULL,
    #>     regressors = c("use", "select", "adapt"), occurrence = c("none", "auto",
    #>         "fixed", "general", "odds-ratio", "inverse-odds-ratio", "direct"),
    #>     distribution = c("default", "dnorm", "dlaplace", "ds", "dgnorm", "dlnorm",
    #>         "dinvgauss", "dgamma"), loss = c("likelihood", "MSE", "MAE", "HAM",
    #>         "LASSO", "RIDGE", "MSEh", "TMSE", "GTMSE", "MSCE"), outliers = c("ignore",
    #>         "use", "select"), level = 0.99, h = 0, holdout = FALSE, persistence = NULL,
    #>     phi = NULL, initial = c("optimal", "backcasting", "complete"), arma = NULL,
    #>     ic = c("AICc", "AIC", "BIC", "BICc"), bounds = c("usual", "admissible",
    #>         "none"), silent = TRUE, ...)

The nonseasonal ARIMA terms (`orders`) and seasonal ARIMA terms
(`orders`) are provided to
[`smooth::adam()`](https://rdrr.io/pkg/smooth/man/adam.html) via
`adam_reg()` parameters. Other options and argument can be set using
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

**Multivariate (xregs, Exogenous Regressors)**

The `xreg` parameter is populated using the
[`fit()`](https://generics.r-lib.org/reference/fit.html) function:

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

Note that date or date-time class values are excluded from `xreg`.

## See also

[`fit.model_spec()`](https://parsnip.tidymodels.org/reference/fit.html),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)

## Examples

``` r
# \donttest{
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(parsnip)
library(rsample)
library(timetk)
library(smooth)
#> Loading required package: greybox
#> Package "greybox", v2.0.6 loaded.
#> By the way, have you already tried temporaldummy() function from greybox?
#> This is package "smooth", v4.3.1
#> 
#> Attaching package: ‘smooth’
#> The following object is masked from ‘package:parsnip’:
#> 
#>     pls

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

# ---- AUTO ADAM ----

# Model Spec
model_spec <- adam_reg() %>%
    set_engine("auto_adam")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
#> frequency = 12 observations per 1 year
model_fit
#> parsnip model object
#> 
#> Time elapsed: 0.04 seconds
#> Model estimated using auto.adam() function: ETS(ANN)
#> With backcasting initialisation
#> Distribution assumed in the model: Normal
#> Loss function type: likelihood; Loss function value: -404.5356
#> Persistence vector g:
#> alpha 
#>     1 
#> 
#> Sample size: 244
#> Number of estimated parameters: 2
#> Number of degrees of freedom: 242
#> Information criteria:
#>       AIC      AICc       BIC      BICc 
#> -805.0713 -805.0215 -798.0769 -797.9401 


# ---- STANDARD ADAM ----

# Model Spec
model_spec <- adam_reg(
        seasonal_period          = 12,
        non_seasonal_ar          = 3,
        non_seasonal_differences = 1,
        non_seasonal_ma          = 3,
        seasonal_ar              = 1,
        seasonal_differences     = 0,
        seasonal_ma              = 1
    ) %>%
    set_engine("adam")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
model_fit
#> parsnip model object
#> 
#> Time elapsed: 0.85 seconds
#> Model estimated using adam() function: ETS(AAN)+ARIMA(3,1,3)
#> With backcasting initialisation
#> Distribution assumed in the model: Normal
#> Loss function type: likelihood; Loss function value: -443.5111
#> Persistence vector g:
#>  alpha   beta 
#> 0.0862 0.0250 
#> 
#> ARMA parameters of the model:
#>         Lag 1 Lag NA
#> AR(1)  0.2576     NA
#> AR(2)  0.0639     NA
#> AR(3) -0.0954     NA
#>         Lag 1 Lag NA
#> MA(1) -0.3945     NA
#> MA(2) -0.5379     NA
#> MA(3) -0.1305     NA
#> 
#> Sample size: 244
#> Number of estimated parameters: 9
#> Number of degrees of freedom: 235
#> Information criteria:
#>       AIC      AICc       BIC      BICc 
#> -869.0223 -868.2531 -837.5478 -835.4335 
# }
```
