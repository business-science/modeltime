# Drop a Model from a Modeltime Table

Drop a Model from a Modeltime Table

## Usage

``` r
drop_modeltime_model(object, .model_id)
```

## Arguments

- object:

  A Modeltime Table (class `mdl_time_tbl`)

- .model_id:

  A numeric value matching the .model_id that you want to drop

## See also

- [`combine_modeltime_tables()`](https://business-science.github.io/modeltime/reference/combine_modeltime_tables.md):
  Combine 2 or more Modeltime Tables together

- [`add_modeltime_model()`](https://business-science.github.io/modeltime/reference/add_modeltime_model.md):
  Adds a new row with a new model to a Modeltime Table

- `drop_modeltime_model()`: Drop one or more models from a Modeltime
  Table

- [`update_modeltime_description()`](https://business-science.github.io/modeltime/reference/update_model_description.md):
  Updates a description for a model inside a Modeltime Table

- [`update_modeltime_model()`](https://business-science.github.io/modeltime/reference/update_modeltime_model.md):
  Updates a model inside a Modeltime Table

- [`pull_modeltime_model()`](https://business-science.github.io/modeltime/reference/pluck_modeltime_model.md):
  Extracts a model from a Modeltime Table

## Examples

``` r
# \donttest{
library(tidymodels)


m750_models %>%
    drop_modeltime_model(.model_id = c(2,3))
#> # Modeltime Table
#> # A tibble: 1 × 3
#>   .model_id .model     .model_desc            
#>       <int> <list>     <chr>                  
#> 1         1 <workflow> ARIMA(0,1,1)(0,1,1)[12]
# }
```
