# Extract Residuals Information

This is a convenience function to unnest model residuals

## Usage

``` r
modeltime_residuals(object, new_data = NULL, quiet = TRUE, ...)
```

## Arguments

- object:

  A Modeltime Table

- new_data:

  A `tibble` to predict and calculate residuals on. If provided,
  overrides any calibration data.

- quiet:

  Hide errors (`TRUE`, the default), or display them as they occur?

- ...:

  Not currently used.

## Value

A tibble with residuals.

## Examples

``` r
library(dplyr)
library(lubridate)
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

models_tbl <- modeltime_table(
    model_fit_prophet
)

# ---- RESIDUALS ----

# In-Sample
models_tbl %>%
    modeltime_calibrate(new_data = training(splits)) %>%
    modeltime_residuals() %>%
    plot_modeltime_residuals(.interactive = FALSE)


# Out-of-Sample
models_tbl %>%
    modeltime_calibrate(new_data = testing(splits)) %>%
    modeltime_residuals() %>%
    plot_modeltime_residuals(.interactive = FALSE)


```
