# The results of train/test splitting the M750 Data

The results of train/test splitting the M750 Data

## Usage

``` r
m750_splits
```

## Format

An `rsplit` object split into approximately 23.5-years of training data
and 2-years of testing data

## Details

    library(timetk)
    m750_splits <- time_series_split(m750, assess = "2 years", cumulative = TRUE)

## Examples

``` r
library(rsample)

m750_splits
#> <Analysis/Assess/Total>
#> <282/24/306>

training(m750_splits)
#> # A tibble: 282 × 3
#>    id    date       value
#>    <fct> <date>     <dbl>
#>  1 M750  1990-01-01  6370
#>  2 M750  1990-02-01  6430
#>  3 M750  1990-03-01  6520
#>  4 M750  1990-04-01  6580
#>  5 M750  1990-05-01  6620
#>  6 M750  1990-06-01  6690
#>  7 M750  1990-07-01  6000
#>  8 M750  1990-08-01  5450
#>  9 M750  1990-09-01  6480
#> 10 M750  1990-10-01  6820
#> # ℹ 272 more rows
```
