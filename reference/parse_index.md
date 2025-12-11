# Developer Tools for parsing date and date-time information

These functions are designed to assist developers in extending the
`modeltime` package.

## Usage

``` r
parse_index_from_data(data)

parse_period_from_index(data, period)
```

## Arguments

- data:

  A data frame

- period:

  A period to calculate from the time index. Numeric values are returned
  as-is. "auto" guesses a numeric value from the index. A time-based
  phrase (e.g. "7 days") calculates the number of timestamps that
  typically occur within the time-based phrase.

## Value

- parse_index_from_data(): Returns a tibble containing the date or
  date-time column.

- parse_period_from_index(): Returns the numeric period from a tibble
  containing the index.

## Examples

``` r
library(dplyr)
library(timetk)

predictors <- m4_monthly %>%
    filter(id == "M750") %>%
    select(-value)

index_tbl <- parse_index_from_data(predictors)
index_tbl
#> # A tibble: 306 × 1
#>    date      
#>    <date>    
#>  1 1990-01-01
#>  2 1990-02-01
#>  3 1990-03-01
#>  4 1990-04-01
#>  5 1990-05-01
#>  6 1990-06-01
#>  7 1990-07-01
#>  8 1990-08-01
#>  9 1990-09-01
#> 10 1990-10-01
#> # ℹ 296 more rows

period <- parse_period_from_index(index_tbl, period = "1 year")
#> frequency = 12 observations per 1 year
period
#> [1] 12
```
