
# modeltime 0.0.2.9000 (Development Version)

### New Models

__TBATS Model__

``` r
seasonal_reg() %>%
    set_engine("tbats")
```

### New Functions

- `update_model_description()` - A helper function making it easier to update model descriptions. 

### Bug Fixes

- `modeltime_forecast()`: 
    - Implement `actual_data` reconciliation strategies when recipe removes rows. Strategy attempts to fill predictors using "downup" strategy to prevent `NA` values from removing rows. 
    - More descriptive errors when external regressors are required. 
    
- `prophet_reg()` & `prophet_boost()`: 
    - `rstan` dependency for prophet failing to load on MacOS. 

### Breaking Changes

- `seasonal_decomp()` has changed to `seasonal_reg()`

# modeltime 0.0.2

### Confidence Interval Estimation

- `modeltime_forecast()`: Now estimates confidence intervals using centered standard deviation. The mean is assumed to be zero and residuals deviate from mean = 0. 

### Fixes

- Updates to work with `parsnip` 0.1.2.
- `prophet_boost()`: Set `nthreads = 1` (default) to ensure parallelization is thread safe. 

# modeltime 0.0.1

* Initial Release
