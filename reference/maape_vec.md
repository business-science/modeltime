# Mean Arctangent Absolute Percentage Error

Wrapper around
[`TSrepr::maape()`](https://rdrr.io/pkg/TSrepr/man/maape.html) with
yardstick-compatible interface.

## Usage

``` r
maape_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
```

## Arguments

- truth:

  Numeric vector of ground-truth values.

- estimate:

  Numeric vector of predicted values.

- na_rm:

  Logical, whether to remove missing values before computation.

- case_weights:

  Optional numeric vector of non-negative case weights (not used by
  `maape_vec`).

- ...:

  Additional arguments (not used).

## Value

A numeric value representing the Mean Arctangent Absolute Percentage
Error.
