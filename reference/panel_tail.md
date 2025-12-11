# Filter the last N rows (Tail) for multiple time series

Filter the last N rows (Tail) for multiple time series

## Usage

``` r
panel_tail(data, id, n)
```

## Arguments

- data:

  A data frame

- id:

  An "id" feature indicating which column differentiates the time series
  panels

- n:

  The number of rows to filter

## Value

A data frame

## See also

- [`recursive()`](https://business-science.github.io/modeltime/reference/recursive.md) -
  used to generate recursive autoregressive models

## Examples

``` r
library(timetk)

# Get the last 6 observations from each group
m4_monthly %>%
    panel_tail(id = id, n = 6)
#> # A tibble: 24 × 3
#>    id    date       value
#>    <fct> <date>     <dbl>
#>  1 M1    2015-01-01  6040
#>  2 M1    2015-02-01  5130
#>  3 M1    2015-03-01  5090
#>  4 M1    2015-04-01  5210
#>  5 M1    2015-05-01  4910
#>  6 M1    2015-06-01  6890
#>  7 M2    2015-01-01  2210
#>  8 M2    2015-02-01  1930
#>  9 M2    2015-03-01  1960
#> 10 M2    2015-04-01  1590
#> # ℹ 14 more rows
```
