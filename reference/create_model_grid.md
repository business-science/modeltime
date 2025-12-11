# Helper to make `parsnip` model specs from a `dials` parameter grid

Helper to make `parsnip` model specs from a `dials` parameter grid

## Usage

``` r
create_model_grid(grid, f_model_spec, engine_name, ..., engine_params = list())
```

## Arguments

- grid:

  A tibble that forms a grid of parameters to adjust

- f_model_spec:

  A function name (quoted or unquoted) that specifies a `parsnip` model
  specification function

- engine_name:

  A name of an engine to use. Gets passed to
  [`parsnip::set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).

- ...:

  Static parameters that get passed to the f_model_spec

- engine_params:

  A `list` of additional parameters that can be passed to the engine via
  `parsnip::set_engine(...)`.

## Value

Tibble with a new colum named `.models`

## Details

This is a helper function that combines `dials` grids with `parsnip`
model specifications. The intent is to make it easier to generate
`workflowset` objects for forecast evaluations with
[`modeltime_fit_workflowset()`](https://business-science.github.io/modeltime/reference/modeltime_fit_workflowset.md).

The process follows:

1.  Generate a grid (hyperparemeter combination)

2.  Use `create_model_grid()` to apply the parameter combinations to a
    parsnip model spec and engine.

The output contains ".model" column that can be used as a list of models
inside the `workflow_set()` function.

## See also

- [`dials::grid_regular()`](https://dials.tidymodels.org/reference/grid_regular.html):
  For making parameter grids.

- [`workflowsets::workflow_set()`](https://workflowsets.tidymodels.org/reference/workflow_set.html):
  For creating a `workflowset` from the `.models` list stored in the
  ".models" column.

- [`modeltime_fit_workflowset()`](https://business-science.github.io/modeltime/reference/modeltime_fit_workflowset.md):
  For fitting a `workflowset` to forecast data.

## Examples

``` r
library(tidymodels)

# Parameters that get optimized
grid_tbl <- grid_regular(
    learn_rate(),
    levels = 3
)

# Generate model specs
grid_tbl %>%
    create_model_grid(
        f_model_spec = boost_tree,
        engine_name  = "xgboost",
        # Static boost_tree() args
        mode = "regression",
        # Static set_engine() args
        engine_params = list(
            max_depth = 5
        )
    )
#> # A tibble: 3 × 2
#>     learn_rate .models  
#>          <dbl> <list>   
#> 1 0.0000000001 <spec[+]>
#> 2 0.00000316   <spec[+]>
#> 3 0.1          <spec[+]>
```
