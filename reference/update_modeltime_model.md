# Update the model by model id in a Modeltime Table

Update the model by model id in a Modeltime Table

## Usage

``` r
update_modeltime_model(object, .model_id, .new_model)
```

## Arguments

- object:

  A Modeltime Table

- .model_id:

  A numeric value matching the .model_id that you want to update

- .new_model:

  A fitted workflow, model_fit, or mdl_time_ensmble object

## See also

- [`combine_modeltime_tables()`](https://business-science.github.io/modeltime/reference/combine_modeltime_tables.md):
  Combine 2 or more Modeltime Tables together

- [`add_modeltime_model()`](https://business-science.github.io/modeltime/reference/add_modeltime_model.md):
  Adds a new row with a new model to a Modeltime Table

- [`drop_modeltime_model()`](https://business-science.github.io/modeltime/reference/drop_modeltime_model.md):
  Drop one or more models from a Modeltime Table

- [`update_modeltime_description()`](https://business-science.github.io/modeltime/reference/update_model_description.md):
  Updates a description for a model inside a Modeltime Table

- `update_modeltime_model()`: Updates a model inside a Modeltime Table

- [`pull_modeltime_model()`](https://business-science.github.io/modeltime/reference/pluck_modeltime_model.md):
  Extracts a model from a Modeltime Table

## Examples

``` r
# \donttest{
library(tidymodels)

model_fit_ets <- exp_smoothing() %>%
    set_engine("ets") %>%
    fit(value ~ date, training(m750_splits))
#> frequency = 12 observations per 1 year

m750_models %>%
    update_modeltime_model(1, model_fit_ets)
#> # Modeltime Table
#> # A tibble: 3 × 3
#>   .model_id .model     .model_desc
#>       <int> <list>     <chr>      
#> 1         1 <fit[+]>   ETS(A,A,A) 
#> 2         2 <workflow> PROPHET    
#> 3         3 <workflow> GLMNET     
# }
```
