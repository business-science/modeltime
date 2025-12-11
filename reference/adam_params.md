# Tuning Parameters for ADAM Models

Tuning Parameters for ADAM Models

## Usage

``` r
ets_model(values = c("ZZZ", "XXX", "YYY", "CCC", "PPP", "FFF"))

loss(
  values = c("likelihood", "MSE", "MAE", "HAM", "LASSO", "RIDGE", "TMSE", "GTMSE",
    "MSEh", "MSCE")
)

use_constant(values = c(FALSE, TRUE))

regressors_treatment(values = c("use", "select", "adapt"))

outliers_treatment(values = c("ignore", "use", "select"))

probability_model(
  values = c("none", "auto", "fixed", "general", "odds-ratio", "inverse-odds-ratio",
    "direct")
)

distribution(
  values = c("default", "dnorm", "dlaplace", "ds", "dgnorm", "dlnorm", "dinvgauss",
    "dgamma")
)

information_criteria(values = c("AICc", "AIC", "BICc", "BIC"))

select_order(values = c(FALSE, TRUE))
```

## Arguments

- values:

  A character string of possible values.

## Value

A `dials` parameter

A parameter

A parameter

A parameter

A parameter

A parameter

A parameter

A parameter

A parameter

A parameter

## Details

The main parameters for ADAM models are:

- `ets_model`:

  - model="ZZZ" means that the model will be selected based on the
    chosen information criteria type. The Branch and Bound is used in
    the process.

  - model="XXX" means that only additive components are tested, using
    Branch and Bound.

  - model="YYY" implies selecting between multiplicative components.

  - model="CCC" triggers the combination of forecasts of models using
    information criteria weights (Kolassa, 2011).

  - combinations between these four and the classical components are
    also accepted. For example, model="CAY" will combine models with
    additive trend and either none or multiplicative seasonality.

  - model="PPP" will produce the selection between pure additive and
    pure multiplicative models. "P" stands for "Pure". This cannot be
    mixed with other types of components.

  - model="FFF" will select between all the 30 types of models. "F"
    stands for "Full". This cannot be mixed with other types of
    components.

  - The parameter model can also be a vector of names of models for a
    finer tuning (pool of models). For example, model=c("ANN","AAA")
    will estimate only two models and select the best of them.

- `loss`:

  - likelihood - the model is estimated via the maximization of the
    likelihood of the function specified in distribution;

  - MSE (Mean Squared Error),

  - MAE (Mean Absolute Error),

  - HAM (Half Absolute Moment),

  - LASSO - use LASSO to shrink the parameters of the model;

  - RIDGE - use RIDGE to shrink the parameters of the model;

  - TMSE - Trace Mean Squared Error,

  - GTMSE - Geometric Trace Mean Squared Error,

  - MSEh - optimisation using only h-steps ahead error,

  - MSCE - Mean Squared Cumulative Error.

- `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR)
  terms.

- `non_seasonal_differences`: The order of integration for non-seasonal
  differencing.

- `non_seasonal_ma`: The order of the non-seasonal moving average (MA)
  terms.

- `seasonal_ar`: The order of the seasonal auto-regressive (SAR) terms.

- `seasonal_differences`: The order of integration for seasonal
  differencing.

- `seasonal_ma`: The order of the seasonal moving average (SMA) terms.

- `use_constant`: Logical, determining, whether the constant is needed
  in the model or not.

- `regressors_treatment`: The variable defines what to do with the
  provided explanatory variables.

- `outliers_treatment`: Defines what to do with outliers.

- `probability_model`: The type of model used in probability estimation.

- `distribution`: What density function to assume for the error term.

- `information_criteria`: The information criterion to use in the model
  selection / combination procedure.

- `select_order`: If TRUE, then the function will select the most
  appropriate order.

## Examples

``` r
use_constant()
#> Logical, determining, whether the constant is needed in the model or not
#> (qualitative)
#> 2 possible values include:
#> FALSE and TRUE

regressors_treatment()
#> The variable defines what to do with the provided explanatory variables.
#> (qualitative)
#> 3 possible values include:
#> 'use', 'select', and 'adapt'

distribution()
#> What density function to assume for the error term. (qualitative)
#> 8 possible values include:
#> 'default', 'dnorm', 'dlaplace', 'ds', 'dgnorm', 'dlnorm', 'dinvgauss', and
#> 'dgamma'

```
