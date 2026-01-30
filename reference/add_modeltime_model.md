# Add a Model into a Modeltime Table

Add a Model into a Modeltime Table

## Usage

``` r
add_modeltime_model(object, model, location = "bottom")
```

## Arguments

- object:

  Multiple Modeltime Tables (class `mdl_time_tbl`)

- model:

  A model of class `model_fit` or a fitted `workflow` object

- location:

  Where to add the model. Either "top" or "bottom". Default: "bottom".

## See also

- [`combine_modeltime_tables()`](https://business-science.github.io/modeltime/reference/combine_modeltime_tables.md):
  Combine 2 or more Modeltime Tables together

- `add_modeltime_model()`: Adds a new row with a new model to a
  Modeltime Table

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
# \donttest{
library(tidymodels)
#> ── Attaching packages ────────────────────────────────────── tidymodels 1.4.1 ──
#> ✔ broom        1.0.12     ✔ tailor       0.1.0 
#> ✔ dials        1.4.2      ✔ tidyr        1.3.2 
#> ✔ ggplot2      4.0.1      ✔ tune         2.0.1 
#> ✔ infer        1.1.0      ✔ workflows    1.3.0 
#> ✔ modeldata    1.5.1      ✔ workflowsets 1.1.1 
#> ✔ purrr        1.2.1      ✔ yardstick    1.3.2 
#> ✔ recipes      1.3.1      
#> ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
#> ✖ yardstick::accuracy() masks smooth::accuracy(), greybox::accuracy()
#> ✖ purrr::discard()      masks scales::discard()
#> ✖ dplyr::filter()       masks stats::filter()
#> ✖ dplyr::lag()          masks stats::lag()
#> ✖ smooth::pls()         masks parsnip::pls()
#> ✖ tidyr::spread()       masks greybox::spread()
#> ✖ recipes::step()       masks stats::step()

model_fit_ets <- exp_smoothing() %>%
    set_engine("ets") %>%
    fit(value ~ date, training(m750_splits))
#> frequency = 12 observations per 1 year

m750_models %>%
    add_modeltime_model(model_fit_ets)
#> # Modeltime Table
#> # A tibble: 4 × 3
#>   .model_id .model     .model_desc            
#>       <int> <list>     <chr>                  
#> 1         1 <workflow> ARIMA(0,1,1)(0,1,1)[12]
#> 2         2 <workflow> PROPHET                
#> 3         3 <workflow> GLMNET                 
#> 4         4 <fit[+]>   ETS(A,A,A)             
# }
```
