# Constructor for creating modeltime models

These functions are used to construct new `modeltime` bridge functions
that connect the `tidymodels` infrastructure to time-series models
containing date or date-time features.

## Usage

``` r
new_modeltime_bridge(class, models, data, extras = NULL, desc = NULL)
```

## Arguments

- class:

  A class name that is used for creating custom printing messages

- models:

  A list containing one or more models

- data:

  A data frame (or tibble) containing 4 columns: (date column with name
  that matches input data), .actual, .fitted, and .residuals.

- extras:

  An optional list that is typically used for transferring preprocessing
  recipes to the predict method.

- desc:

  An optional model description to appear when printing your modeltime
  objects

## Examples

``` r
library(dplyr)
library(lubridate)
library(timetk)

lm_model <- lm(value ~ as.numeric(date) + hour(date) + wday(date, label = TRUE),
               data = taylor_30_min)

data = tibble(
    date        = taylor_30_min$date, # Important - The column name must match the modeled data
    # These are standardized names: .actual, .fitted, .residuals
    .actual     = taylor_30_min$value,
    .fitted     = lm_model$fitted.values %>% as.numeric(),
    .residuals  = lm_model$residuals %>% as.numeric()
)

new_modeltime_bridge(
    class  = "lm_time_series_impl",
    models = list(model_1 = lm_model),
    data   = data,
    extras = NULL
)
#> $model_1
#> 
#> Call:
#> lm(formula = value ~ as.numeric(date) + hour(date) + wday(date, 
#>     label = TRUE), data = taylor_30_min)
#> 
#> Coefficients:
#>                (Intercept)            as.numeric(date)  
#>                  1.489e+05                  -1.284e-04  
#>                 hour(date)  wday(date, label = TRUE).L  
#>                  3.919e+02                   6.665e+02  
#> wday(date, label = TRUE).Q  wday(date, label = TRUE).C  
#>                 -5.952e+03                   5.479e+02  
#> wday(date, label = TRUE)^4  wday(date, label = TRUE)^5  
#>                 -1.955e+03                   2.482e+02  
#> wday(date, label = TRUE)^6  
#>                 -8.755e+01  
#> 
#> 

```
