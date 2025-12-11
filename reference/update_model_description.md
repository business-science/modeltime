# Update the model description by model id in a Modeltime Table

The `update_model_description()` and `update_modeltime_description()`
functions are synonyms.

## Usage

``` r
update_model_description(object, .model_id, .new_model_desc)

update_modeltime_description(object, .model_id, .new_model_desc)
```

## Arguments

- object:

  A Modeltime Table

- .model_id:

  A numeric value matching the .model_id that you want to update

- .new_model_desc:

  Text describing the new model description

## See also

- [`combine_modeltime_tables()`](https://business-science.github.io/modeltime/reference/combine_modeltime_tables.md):
  Combine 2 or more Modeltime Tables together

- [`add_modeltime_model()`](https://business-science.github.io/modeltime/reference/add_modeltime_model.md):
  Adds a new row with a new model to a Modeltime Table

- [`drop_modeltime_model()`](https://business-science.github.io/modeltime/reference/drop_modeltime_model.md):
  Drop one or more models from a Modeltime Table

- `update_modeltime_description()`: Updates a description for a model
  inside a Modeltime Table

- [`update_modeltime_model()`](https://business-science.github.io/modeltime/reference/update_modeltime_model.md):
  Updates a model inside a Modeltime Table

- [`pull_modeltime_model()`](https://business-science.github.io/modeltime/reference/pluck_modeltime_model.md):
  Extracts a model from a Modeltime Table

## Examples

``` r
m750_models %>%
    update_modeltime_description(2, "PROPHET - No Regressors")
#> # Modeltime Table
#> # A tibble: 3 × 3
#>   .model_id .model     .model_desc            
#>       <int> <list>     <chr>                  
#> 1         1 <workflow> ARIMA(0,1,1)(0,1,1)[12]
#> 2         2 <workflow> PROPHET - No Regressors
#> 3         3 <workflow> GLMNET                 
```
