# Apply Statistical Tests to Residuals

This is a convenience function to calculate some statistical tests on
the residuals models. Currently, the following statistics are
calculated: the shapiro.test to check the normality of the residuals,
the box-pierce and ljung-box tests and the durbin watson test to check
the autocorrelation of the residuals. In all cases the p-values are
returned.

## Usage

``` r
modeltime_residuals_test(object, new_data = NULL, lag = 1, fitdf = 0, ...)
```

## Arguments

- object:

  A `tibble` extracted from modeltime::modeltime_residuals().

- new_data:

  A `tibble` to predict and calculate residuals on. If provided,
  overrides any calibration data.

- lag:

  The statistic will be based on lag autocorrelation coefficients.
  Default: 1 (Applies to Box-Pierce, Ljung-Box, and Durbin-Watson Tests)

- fitdf:

  Number of degrees of freedom to be subtracted. Default: 0 (Applies
  Box-Pierce and Ljung-Box Tests)

- ...:

  Not currently used

## Value

A tibble with with the p-values of the calculated statistical tests.

## Details

**Shapiro-Wilk Test**

The Shapiro-Wilk tests the Normality of the residuals. The Null
Hypothesis is that the residuals are normally distributed. A low P-Value
below a given significance level indicates the values are NOT Normally
Distributed.

If the **p-value \> 0.05 (good)**, this implies that the distribution of
the data are not significantly different from normal distribution. In
other words, we can assume the normality.

**Box-Pierce and Ljung-Box Tests Tests**

The Ljung-Box and Box-Pierce tests are methods that test for the absense
of autocorrelation in residuals. A low p-value below a given
significance level indicates the values are autocorrelated.

If the **p-value \> 0.05 (good)**, this implies that the residuals of
the data are are independent. In other words, we can assume the
residuals are not autocorrelated.

For more information about the parameters associated with the Box Pierce
and Ljung Box tests check ?Box.Test

**Durbin-Watson Test**

The Durbin-Watson test is a method that tests for the absense of
autocorrelation in residuals. The Durbin Watson test reports a test
statistic, with a value from 0 to 4, where:

- **2 is no autocorrelation (good)**

- From 0 to \<2 is positive autocorrelation (common in time series data)

- From \>2 to 4 is negative autocorrelation (less common in time series
  data)

## See also

[`stats::shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html),
[`stats::Box.test()`](https://rdrr.io/r/stats/box.test.html)

## Examples

``` r
library(dplyr)
library(timetk)
library(parsnip)
library(rsample)

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)

# --- MODELS ---

# Model 1: prophet ----
model_fit_prophet <- prophet_reg() %>%
    set_engine(engine = "prophet") %>%
    fit(value ~ date, data = training(splits))
#> Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.
#> Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.


# ---- MODELTIME TABLE ----

models_tbl <- modeltime_table(
    model_fit_prophet
)

# ---- RESIDUALS ----

# In-Sample
models_tbl %>%
    modeltime_calibrate(new_data = training(splits)) %>%
    modeltime_residuals() %>%
    modeltime_residuals_test()
#> # A tibble: 1 × 6
#>   .model_id .model_desc shapiro_wilk box_pierce ljung_box durbin_watson
#>       <int> <chr>              <dbl>      <dbl>     <dbl>         <dbl>
#> 1         1 PROPHET        0.0000495          0         0         0.922

# Out-of-Sample
models_tbl %>%
    modeltime_calibrate(new_data = testing(splits)) %>%
    modeltime_residuals() %>%
    modeltime_residuals_test()
#> # A tibble: 1 × 6
#>   .model_id .model_desc shapiro_wilk box_pierce ljung_box durbin_watson
#>       <int> <chr>              <dbl>      <dbl>     <dbl>         <dbl>
#> 1         1 PROPHET          0.00198      0.185     0.165          1.32

```
