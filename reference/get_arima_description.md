# Get model descriptions for Arima objects

Get model descriptions for Arima objects

## Usage

``` r
get_arima_description(object, padding = FALSE)
```

## Source

- Forecast R Package, `forecast:::arima.string()`

## Arguments

- object:

  Objects of class `Arima`

- padding:

  Whether or not to include padding

## Examples

``` r
library(forecast)
#> 
#> Attaching package: ‘forecast’
#> The following object is masked from ‘package:yardstick’:
#> 
#>     accuracy

arima_fit <- forecast::Arima(1:10)

get_arima_description(arima_fit)
#> [1] "ARIMA(0,0,0) with non-zero mean"

```
