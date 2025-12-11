# Combine multiple Modeltime Tables into a single Modeltime Table

Combine multiple Modeltime Tables into a single Modeltime Table

## Usage

``` r
combine_modeltime_tables(...)
```

## Arguments

- ...:

  Multiple Modeltime Tables (class `mdl_time_tbl`)

## Details

This function combines multiple Modeltime Tables.

- The `.model_id` will automatically be renumbered to ensure each model
  has a unique ID.

- Only the `.model_id`, `.model`, and `.model_desc` columns will be
  returned.

**Re-Training Models on the Same Datasets**

One issue can arise if your models are trained on different datasets. If
your models have been trained on different datasets, you can run
[`modeltime_refit()`](https://business-science.github.io/modeltime/reference/modeltime_refit.md)
to train all models on the same data.

**Re-Calibrating Models**

If your data has been calibrated using
[`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md),
the `.test` and `.calibration_data` columns will be removed. To
re-calibrate, simply run
[`modeltime_calibrate()`](https://business-science.github.io/modeltime/reference/modeltime_calibrate.md)
on the newly combined Modeltime Table.

## See also

- `combine_modeltime_tables()`: Combine 2 or more Modeltime Tables
  together

- [`add_modeltime_model()`](https://business-science.github.io/modeltime/reference/add_modeltime_model.md):
  Adds a new row with a new model to a Modeltime Table

- [`drop_modeltime_model()`](https://business-science.github.io/modeltime/reference/drop_modeltime_model.md):
  Drop one or more models from a Modeltime Table

- [`update_modeltime_description()`](https://business-science.github.io/modeltime/reference/update_model_description.md):
  Updates a description for a model inside a Modeltime Table

- [`update_modeltime_model()`](https://business-science.github.io/modeltime/reference/update_modeltime_model.md):
  Updates a model inside a Modeltime Table

- [`pull_modeltime_model()`](https://business-science.github.io/modeltime/reference/pluck_modeltime_model.md):
  Extracts a model from a Modeltime Table

## Examples

``` r
library(tidymodels)
library(timetk)
library(dplyr)
library(lubridate)

# Setup
m750 <- m4_monthly %>% filter(id == "M750")

splits <- time_series_split(m750, assess = "3 years", cumulative = TRUE)
#> Using date_var: date

model_fit_arima <- arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(value ~ date, training(splits))
#> frequency = 12 observations per 1 year

model_fit_prophet <- prophet_reg() %>%
    set_engine("prophet") %>%
    fit(value ~ date, training(splits))
#> Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.
#> Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

# Multiple Modeltime Tables
model_tbl_1 <- modeltime_table(model_fit_arima)
model_tbl_2 <- modeltime_table(model_fit_prophet)

# Combine
combine_modeltime_tables(model_tbl_1, model_tbl_2)
#> # Modeltime Table
#> # A tibble: 2 × 3
#>   .model_id .model   .model_desc            
#>       <int> <list>   <chr>                  
#> 1         1 <fit[+]> ARIMA(0,1,1)(0,1,1)[12]
#> 2         2 <fit[+]> PROPHET                
```
