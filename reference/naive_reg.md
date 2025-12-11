# General Interface for NAIVE Forecast Models

`naive_reg()` is a way to generate a *specification* of an NAIVE or
SNAIVE model before fitting and allows the model to be created using
different packages.

## Usage

``` r
naive_reg(mode = "regression", id = NULL, seasonal_period = NULL)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression".

- id:

  An optional quoted column name (e.g. "id") for identifying multiple
  time series (i.e. panel data).

- seasonal_period:

  SNAIVE only. A seasonal frequency. Uses "auto" by default. A character
  phrase of "auto" or time-based phrase of "2 weeks" can be used if a
  date or date-time variable is provided. See Fit Details below.

## Details

The data given to the function are not saved and are only used to
determine the *mode* of the model. For `naive_reg()`, the mode will
always be "regression".

The model can be created using the
[`fit()`](https://generics.r-lib.org/reference/fit.html) function using
the following *engines*:

- "naive" (default) - Performs a NAIVE forecast

- "snaive" - Performs a Seasonal NAIVE forecast

## Engine Details

**naive (default engine)**

- The engine uses
  [`naive_fit_impl()`](https://business-science.github.io/modeltime/reference/naive_fit_impl.md)

- The NAIVE implementation uses the last observation and forecasts this
  value forward.

- The `id` can be used to distinguish multiple time series contained in
  the data

- The `seasonal_period` is not used but provided for consistency with
  the SNAIVE implementation

**snaive (default engine)**

- The engine uses
  [`snaive_fit_impl()`](https://business-science.github.io/modeltime/reference/snaive_fit_impl.md)

- The SNAIVE implementation uses the last seasonal series in the data
  and forecasts this sequence of observations forward

- The `id` can be used to distinguish multiple time series contained in
  the data

- The `seasonal_period` is used to determine how far back to define the
  repeated series. This can be a numeric value (e.g. 28) or a period
  (e.g. "1 month")

## Fit Details

**Date and Date-Time Variable**

It's a requirement to have a date or date-time variable as a predictor.
The [`fit()`](https://generics.r-lib.org/reference/fit.html) interface
accepts date and date-time features and handles them internally.

- `fit(y ~ date)`

**ID features (Multiple Time Series, Panel Data)**

The `id` parameter is populated using the
[`fit()`](https://generics.r-lib.org/reference/fit.html) or
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) function:

*ID Example:* Suppose you have 3 features:

1.  `y` (target)

2.  `date` (time stamp),

3.  `series_id` (a unique identifer that identifies each time series in
    your data).

The `series_id` can be passed to the `naive_reg()` using
[`fit()`](https://generics.r-lib.org/reference/fit.html):

- `naive_reg(id = "series_id")` specifes that the `series_id` column
  should be used to identify each time series.

- `fit(y ~ date + series_id)` will pass `series_id` on to the underlying
  naive or snaive functions.

**Seasonal Period Specification (snaive)**

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

**External Regressors (Xregs)**

These models are univariate. No xregs are used in the modeling process.

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

# ---- NAIVE ----

# Model Spec
model_spec <- naive_reg() %>%
    set_engine("naive")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
model_fit
#> parsnip model object
#> 
#> NAIVE
#> --------
#> Model: 
#> # A tibble: 1 × 2
#>   date       value
#>   <date>     <dbl>
#> 1 2010-04-01  9.29


# ---- SEASONAL NAIVE ----

# Model Spec
model_spec <- naive_reg(
        id = "id",
        seasonal_period = 12
    ) %>%
    set_engine("snaive")

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date + id, data = training(splits))
model_fit
#> parsnip model object
#> 
#> SNAIVE [12]
#> --------
#> Model: 
#> # A tibble: 12 × 3
#>    id    date       value
#>    <fct> <date>     <dbl>
#>  1 M750  2009-05-01  9.27
#>  2 M750  2009-06-01  9.27
#>  3 M750  2009-07-01  9.15
#>  4 M750  2009-08-01  9.19
#>  5 M750  2009-09-01  9.18
#>  6 M750  2009-10-01  9.25
#>  7 M750  2009-11-01  9.26
#>  8 M750  2009-12-01  9.27
#>  9 M750  2010-01-01  9.26
#> 10 M750  2010-02-01  9.26
#> 11 M750  2010-03-01  9.29
#> 12 M750  2010-04-01  9.29
```
