# Mean Arctangent Absolute Percentage Error

Useful when MAPE returns Inf (e.g., intermittent data with zeros).

## Usage

``` r
maape(data, ...)

# S3 method for class 'data.frame'
maape(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
```

## Arguments

- data:

  A data frame containing the truth and estimate columns.

- ...:

  Additional arguments (not used).

- truth:

  The column identifier for the true results (numeric).

- estimate:

  The column identifier for the predicted results (numeric).

- na_rm:

  Logical, whether to remove missing values before computation.

- case_weights:

  Optional column identifier for non-negative case weights (not used by
  `maape`).

## Value

A tibble with columns `.metric`, `.estimator`, and `.estimate`.
