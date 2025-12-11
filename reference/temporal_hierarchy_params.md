# Tuning Parameters for TEMPORAL HIERARCHICAL Models

Tuning Parameters for TEMPORAL HIERARCHICAL Models

## Usage

``` r
combination_method(values = c("struc", "mse", "ols", "bu", "shr", "sam"))

use_model()
```

## Arguments

- values:

  A character string of possible values.

## Details

The main parameters for Temporal Hierarchical models are:

- `combination_method`: Combination method of temporal hierarchies.

- `use_model`: Model used for forecasting each aggregation level.

## Examples

``` r
combination_method()
#> Combination method of temporal hierarchies. (qualitative)
#> 6 possible values include:
#> 'struc', 'mse', 'ols', 'bu', 'shr', and 'sam'

use_model()
#> Model used for forecasting each aggregation level. (qualitative)
#> 5 possible values include:
#> 'ets', 'arima', 'theta', 'naive', and 'snaive'
```
