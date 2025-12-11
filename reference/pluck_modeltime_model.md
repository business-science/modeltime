# Extract model by model id in a Modeltime Table

The `pull_modeltime_model()` and `pluck_modeltime_model()` functions are
synonymns.

## Usage

``` r
pluck_modeltime_model(object, .model_id)

# S3 method for class 'mdl_time_tbl'
pluck_modeltime_model(object, .model_id)

pull_modeltime_model(object, .model_id)
```

## Arguments

- object:

  A Modeltime Table

- .model_id:

  A numeric value matching the .model_id that you want to update

## See also

- [`combine_modeltime_tables()`](https://business-science.github.io/modeltime/reference/combine_modeltime_tables.md):
  Combine 2 or more Modeltime Tables together

- [`add_modeltime_model()`](https://business-science.github.io/modeltime/reference/add_modeltime_model.md):
  Adds a new row with a new model to a Modeltime Table

- [`drop_modeltime_model()`](https://business-science.github.io/modeltime/reference/drop_modeltime_model.md):
  Drop one or more models from a Modeltime Table

- [`update_modeltime_description()`](https://business-science.github.io/modeltime/reference/update_model_description.md):
  Updates a description for a model inside a Modeltime Table

- [`update_modeltime_model()`](https://business-science.github.io/modeltime/reference/update_modeltime_model.md):
  Updates a model inside a Modeltime Table

- `pull_modeltime_model()`: Extracts a model from a Modeltime Table

## Examples

``` r
m750_models %>%
    pluck_modeltime_model(2)
#> ══ Workflow [trained] ══════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: prophet_reg()
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 6 Recipe Steps
#> 
#> • step_timeseries_signature()
#> • step_rm()
#> • step_normalize()
#> • step_dummy()
#> • step_fourier()
#> • step_rm()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> PROPHET Model
#> - growth: 'linear'
#> - n.changepoints: 25
#> - changepoint.range: 0.8
#> - yearly.seasonality: 'auto'
#> - weekly.seasonality: 'auto'
#> - daily.seasonality: 'auto'
#> - seasonality.mode: 'additive'
#> - changepoint.prior.scale: 0.05
#> - seasonality.prior.scale: 10
#> - holidays.prior.scale: 10
#> - logistic_cap: NULL
#> - logistic_floor: NULL
#> - extra_regressors: 0
```
