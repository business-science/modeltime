# Scale forecast analysis with a Modeltime Table

Designed to perform forecasts at scale using models created with
`modeltime`, `parsnip`, `workflows`, and regression modeling extensions
in the `tidymodels` ecosystem.

## Usage

``` r
modeltime_table(...)

as_modeltime_table(.l)
```

## Arguments

- ...:

  Fitted `parsnip` model or `workflow` objects

- .l:

  A list containing fitted `parsnip` model or `workflow` objects

## Details

`modeltime_table()`:

1.  Creates a table of models

2.  Validates that all objects are models (parsnip or workflows objects)
    and all models have been fitted (trained)

3.  Provides an ID and Description of the models

`as_modeltime_table()`:

Converts a `list` of models to a modeltime table. Useful if
programatically creating Modeltime Tables from models stored in a
`list`.

## Examples

``` r
library(dplyr)
library(timetk)
library(parsnip)
library(rsample)

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)

# --- MODELS ---

# Model 1: prophet ----
model_fit_prophet <- prophet_reg() %>%
    set_engine(engine = "prophet") %>%
    fit(value ~ date, data = training(splits))
#> Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.
#> Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.


# ---- MODELTIME TABLE ----

# Make a Modeltime Table
models_tbl <- modeltime_table(
    model_fit_prophet
)

# Can also convert a list of models
list(model_fit_prophet) %>%
    as_modeltime_table()
#> # Modeltime Table
#> # A tibble: 1 × 3
#>   .model_id .model   .model_desc
#>       <int> <list>   <chr>      
#> 1         1 <fit[+]> PROPHET    

# ---- CALIBRATE ----

calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits))

# ---- ACCURACY ----

calibration_tbl %>%
    modeltime_accuracy()
#> # A tibble: 1 × 9
#>   .model_id .model_desc .type   mae  mape  mase smape  rmse   rsq
#>       <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1         1 PROPHET     Test   175.  1.67 0.596  1.67  232. 0.881

# ---- FORECAST ----

calibration_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = m750
    )
#> # Forecast Results
#>   
#> Conf Method: conformal_default | Conf Interval: 0.95 | Conf By ID: FALSE
#> (GLOBAL CONFIDENCE)
#> # A tibble: 337 × 7
#>    .model_id .model_desc .key   .index     .value .conf_lo .conf_hi
#>        <int> <chr>       <fct>  <date>      <dbl>    <dbl>    <dbl>
#>  1        NA ACTUAL      actual 1990-01-01   6370       NA       NA
#>  2        NA ACTUAL      actual 1990-02-01   6430       NA       NA
#>  3        NA ACTUAL      actual 1990-03-01   6520       NA       NA
#>  4        NA ACTUAL      actual 1990-04-01   6580       NA       NA
#>  5        NA ACTUAL      actual 1990-05-01   6620       NA       NA
#>  6        NA ACTUAL      actual 1990-06-01   6690       NA       NA
#>  7        NA ACTUAL      actual 1990-07-01   6000       NA       NA
#>  8        NA ACTUAL      actual 1990-08-01   5450       NA       NA
#>  9        NA ACTUAL      actual 1990-09-01   6480       NA       NA
#> 10        NA ACTUAL      actual 1990-10-01   6820       NA       NA
#> # ℹ 327 more rows
```
