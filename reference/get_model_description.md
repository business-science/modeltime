# Get model descriptions for parsnip, workflows & modeltime objects

Get model descriptions for parsnip, workflows & modeltime objects

## Usage

``` r
get_model_description(object, indicate_training = FALSE, upper_case = TRUE)
```

## Arguments

- object:

  Parsnip or workflow objects

- indicate_training:

  Whether or not to indicate if the model has been trained

- upper_case:

  Whether to return upper or lower case model descriptions

## Examples

``` r
library(dplyr)
library(timetk)
library(parsnip)

# Model Specification ----

arima_spec <- arima_reg() %>%
    set_engine("auto_arima")

get_model_description(arima_spec, indicate_training = TRUE)
#> [1] "AUTO_ARIMA (NOT TRAINED)"

# Fitted Model ----

m750 <- m4_monthly %>% filter(id == "M750")

arima_fit <- arima_spec %>%
    fit(value ~ date, data = m750)
#> frequency = 12 observations per 1 year

get_model_description(arima_fit, indicate_training = TRUE)
#> [1] "ARIMA(0,1,1)(0,1,1)[12] (TRAINED)"

```
