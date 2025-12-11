# Wrapper for parsnip::xgb_train

Wrapper for parsnip::xgb_train

## Usage

``` r
xgboost_impl(
  x,
  y,
  max_depth = 6,
  nrounds = 15,
  eta = 0.3,
  colsample_bynode = NULL,
  colsample_bytree = NULL,
  min_child_weight = 1,
  gamma = 0,
  subsample = 1,
  validation = 0,
  early_stop = NULL,
  objective = NULL,
  counts = TRUE,
  event_level = c("first", "second"),
  ...
)
```

## Arguments

- x:

  A data frame or matrix of predictors

- y:

  A vector (factor or numeric) or matrix (numeric) of outcome data.

- max_depth:

  An integer for the maximum depth of the tree.

- nrounds:

  An integer for the number of boosting iterations.

- eta:

  A numeric value between zero and one to control the learning rate.

- colsample_bynode:

  Subsampling proportion of columns for each node within each tree. See
  the `counts` argument below. The default uses all columns.

- colsample_bytree:

  Subsampling proportion of columns for each tree. See the `counts`
  argument below. The default uses all columns.

- min_child_weight:

  A numeric value for the minimum sum of instance weights needed in a
  child to continue to split.

- gamma:

  A number for the minimum loss reduction required to make a further
  partition on a leaf node of the tree

- subsample:

  Subsampling proportion of rows. By default, all of the training data
  are used.

- validation:

  A positive number. If on `[0, 1)` the value, `validation` is a random
  proportion of data in `x` and `y` that are used for performance
  assessment and potential early stopping. If 1 or greater, it is the
  *number* of training set samples use for these purposes.

- early_stop:

  An integer or `NULL`. If not `NULL`, it is the number of training
  iterations without improvement before stopping. If `validation` is
  used, performance is base on the validation set; otherwise the
  training set is used.

- counts:

  A logical. If `FALSE`, `colsample_bynode` and `colsample_bytree` are
  both assumed to be *proportions* of the proportion of columns affects
  (instead of counts).

- event_level:

  For binary classification, this is a single string of either `"first"`
  or `"second"` to pass along describing which level of the outcome
  should be considered the "event".

- ...:

  Other options to pass to `xgb.train()` or xgboost's method for
  [`predict()`](https://rdrr.io/r/stats/predict.html).
