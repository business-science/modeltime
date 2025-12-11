# General Interface for Window Forecast Models

`window_reg()` is a way to generate a *specification* of a window model
before fitting and allows the model to be created using different
backends.

## Usage

``` r
window_reg(mode = "regression", id = NULL, window_size = NULL)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression".

- id:

  An optional quoted column name (e.g. "id") for identifying multiple
  time series (i.e. panel data).

- window_size:

  A window to apply the window function. By default, the window uses the
  full data set, which is rarely the best choice.

## Details

A time series window regression is derived using `window_reg()`. The
model can be created using the
[`fit()`](https://generics.r-lib.org/reference/fit.html) function using
the following *engines*:

- **"window_function" (default)** - Performs a Window Forecast applying
  a `window_function` (engine parameter) to a window of size defined by
  `window_size`

## Engine Details

**function (default engine)**

The engine uses
[`window_function_fit_impl()`](https://business-science.github.io/modeltime/reference/window_function_fit_impl.md).
A time series window function applies a `window_function` to a window of
the data (last N observations).

- The function can return a scalar (single value) or multiple values
  that are repeated for each window

- Common use cases:

  - **Moving Average Forecasts:** Forecast forward a 20-day average

  - **Weighted Average Forecasts:** Exponentially weighting the most
    recent observations

  - **Median Forecasts:** Forecasting forward a 20-day median

  - **Repeating Forecasts:** Simulating a Seasonal Naive Forecast by
    broadcasting the last 12 observations of a monthly dataset into the
    future

The key engine parameter is the `window_function`. A function / formula:

- If a function, e.g. `mean`, the function is used with any additional
  arguments, `...` in
  [`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

- If a formula, e.g. `~ mean(., na.rm = TRUE)`, it is converted to a
  function.

This syntax allows you to create very compact anonymous functions.

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

The `series_id` can be passed to the `window_reg()` using
[`fit()`](https://generics.r-lib.org/reference/fit.html):

- `window_reg(id = "series_id")` specifes that the `series_id` column
  should be used to identify each time series.

- `fit(y ~ date + series_id)` will pass `series_id` on to the underlying
  functions.

**Window Function Specification (window_function)**

You can specify a function / formula using `purrr` syntax.

- If a function, e.g. `mean`, the function is used with any additional
  arguments, `...` in
  [`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

- If a formula, e.g. `~ mean(., na.rm = TRUE)`, it is converted to a
  function.

This syntax allows you to create very compact anonymous functions.

**Window Size Specification (window_size)**

The period can be non-seasonal (`window_size = 1 or "none"`) or yearly
seasonal (e.g. For monthly time stamps, `window_size = 12`,
`window_size = "12 months"`, or `window_size = "yearly"`). There are 3
ways to specify:

1.  `window_size = "all"`: A seasonal period is selected based on the
    periodicity of the data (e.g. 12 if monthly)

2.  `window_size = 12`: A numeric frequency. For example, 12 is common
    for monthly data

3.  `window_size = "1 year"`: A time-based phrase. For example, "1 year"
    would convert to 12 for monthly data.

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

# ---- WINDOW FUNCTION -----

# Used to make:
# - Mean/Median forecasts
# - Simple repeating forecasts

# Median Forecast ----

# Model Spec
model_spec <- window_reg(
        window_size     = 12
    ) %>%
    # Extra parameters passed as: set_engine(...)
    set_engine(
        engine          = "window_function",
        window_function = median,
        na.rm           = TRUE
    )

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))
model_fit
#> parsnip model object
#> 
#> WINDOW FUNC [12]
#> --------
#> Model: 
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1  9.26

# Predict
# - The 12-month median repeats going forward
predict(model_fit, testing(splits))
#> # A tibble: 62 × 1
#>    .pred
#>    <dbl>
#>  1  9.26
#>  2  9.26
#>  3  9.26
#>  4  9.26
#>  5  9.26
#>  6  9.26
#>  7  9.26
#>  8  9.26
#>  9  9.26
#> 10  9.26
#> # ℹ 52 more rows


# ---- PANEL FORECAST - WINDOW FUNCTION ----

# Weighted Average Forecast
model_spec <- window_reg(
        # Specify the ID column for Panel Data
        id          = "id",
        window_size = 12
    ) %>%
    set_engine(
        engine = "window_function",
        # Create a Weighted Average
        window_function = ~ sum(tail(.x, 3) * c(0.1, 0.3, 0.6)),
    )

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date + id, data = training(splits))
model_fit
#> parsnip model object
#> 
#> WINDOW FUNC [12]
#> --------
#> Model: 
#> # A tibble: 1 × 2
#>   id    value
#>   <fct> <dbl>
#> 1 M750   9.29

# Predict: The weighted average (scalar) repeats going forward
predict(model_fit, testing(splits))
#> # A tibble: 62 × 1
#>    .pred
#>    <dbl>
#>  1  9.29
#>  2  9.29
#>  3  9.29
#>  4  9.29
#>  5  9.29
#>  6  9.29
#>  7  9.29
#>  8  9.29
#>  9  9.29
#> 10  9.29
#> # ℹ 52 more rows

# ---- BROADCASTING PANELS (REPEATING) ----

# Simulating a Seasonal Naive Forecast by
# broadcasted model the last 12 observations into the future
model_spec <- window_reg(
        id          = "id",
        window_size = Inf
    ) %>%
    set_engine(
        engine          = "window_function",
        window_function = ~ tail(.x, 12),
    )

# Fit Spec
model_fit <- model_spec %>%
    fit(log(value) ~ date + id, data = training(splits))
model_fit
#> parsnip model object
#> 
#> WINDOW FUNC [Inf]
#> --------
#> Model: 
#> # A tibble: 12 × 2
#>    id    value
#>    <fct> <dbl>
#>  1 M750   9.27
#>  2 M750   9.27
#>  3 M750   9.15
#>  4 M750   9.19
#>  5 M750   9.18
#>  6 M750   9.25
#>  7 M750   9.26
#>  8 M750   9.27
#>  9 M750   9.26
#> 10 M750   9.26
#> 11 M750   9.29
#> 12 M750   9.29

# Predict: The sequence is broadcasted (repeated) during prediction
predict(model_fit, testing(splits))
#> # A tibble: 62 × 1
#>    .pred
#>    <dbl>
#>  1  9.27
#>  2  9.27
#>  3  9.15
#>  4  9.19
#>  5  9.18
#>  6  9.25
#>  7  9.26
#>  8  9.27
#>  9  9.26
#> 10  9.26
#> # ℹ 52 more rows
```
