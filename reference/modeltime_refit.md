# Refit one or more trained models to new data

This is a wrapper for
[`fit()`](https://generics.r-lib.org/reference/fit.html) that takes a
Modeltime Table and retrains each model on *new data* re-using the
parameters and preprocessing steps used during the training process.

## Usage

``` r
modeltime_refit(object, data, ..., control = control_refit())
```

## Arguments

- object:

  A Modeltime Table

- data:

  A `tibble` that contains data to retrain the model(s) using.

- ...:

  Additional arguments to control refitting.

  **Ensemble Model Spec (`modeltime.ensemble`):**

  When making a meta-learner with
  `modeltime.ensemble::ensemble_model_spec()`, used to pass `resamples`
  argument containing results from
  `modeltime.resample::modeltime_fit_resamples()`.

- control:

  Used to control verbosity and parallel processing. See
  [`control_refit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md).

## Value

A Modeltime Table containing one or more re-trained models.

## Details

Refitting is an important step prior to forecasting time series models.
The `modeltime_refit()` function makes it easy to recycle models,
retraining on new data.

**Recycling Parameters**

Parameters are recycled during retraining using the following criteria:

- **Automated models** (e.g. "auto arima") will have parameters
  recalculated.

- **Non-automated models** (e.g. "arima") will have parameters
  preserved.

- All preprocessing steps will be reused on the data

**Refit**

The `modeltime_refit()` function is used to retrain models trained with
[`fit()`](https://generics.r-lib.org/reference/fit.html).

**Refit XY**

The XY format is not supported at this time.

## See also

[`control_refit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md)

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

model_fit_prophet <- prophet_reg() %>%
    set_engine(engine = "prophet") %>%
    fit(value ~ date, data = training(splits))
#> Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.
#> Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.


# ---- MODELTIME TABLE ----

models_tbl <- modeltime_table(
    model_fit_prophet
)

# ---- CALIBRATE ----
# - Calibrate on training data set

calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits))


# ---- REFIT ----
# - Refit on full data set

refit_tbl <- calibration_tbl %>%
    modeltime_refit(m750)
#> Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.
#> Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

```
