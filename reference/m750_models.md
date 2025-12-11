# Three (3) Models trained on the M750 Data (Training Set)

Three (3) Models trained on the M750 Data (Training Set)

## Usage

``` r
m750_models
```

## Format

An `time_series_cv` object with 6 slices of Time Series Cross Validation
resamples made on the `training(m750_splits)`

## Details

    m750_models <- modeltime_table(
        wflw_fit_arima,
        wflw_fit_prophet,
        wflw_fit_glmnet
    )

## Examples

``` r
m750_models
#> # Modeltime Table
#> # A tibble: 3 × 3
#>   .model_id .model     .model_desc            
#>       <int> <list>     <chr>                  
#> 1         1 <workflow> ARIMA(0,1,1)(0,1,1)[12]
#> 2         2 <workflow> PROPHET                
#> 3         3 <workflow> GLMNET                 
```
