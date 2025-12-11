# The Time Series Cross Validation Resamples the M750 Data (Training Set)

The Time Series Cross Validation Resamples the M750 Data (Training Set)

## Usage

``` r
m750_training_resamples
```

## Format

An `time_series_cv` object with 6 slices of Time Series Cross Validation
resamples made on the `training(m750_splits)`

## Details

    library(timetk)
    m750_training_resamples <- time_series_cv(
        data        = training(m750_splits),
        assess      = "2 years",
        skip        = "2 years",
        cumulative  = TRUE,
        slice_limit = 6
    )

## Examples

``` r
library(rsample)

m750_training_resamples
#> # Time Series Cross Validation Plan 
#> # A tibble: 6 × 2
#>   splits           id    
#>   <list>           <chr> 
#> 1 <split [258/24]> Slice1
#> 2 <split [234/24]> Slice2
#> 3 <split [210/24]> Slice3
#> 4 <split [186/24]> Slice4
#> 5 <split [162/24]> Slice5
#> 6 <split [138/24]> Slice6


```
