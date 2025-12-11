# Generate a Time Series Train/Test Split Indicies

Makes fast train/test split indicies for time series.

## Usage

``` r
make_ts_splits(.data, .length_test, .length_train = NULL)
```

## Arguments

- .data:

  A data frame containing ordered time seried data (ascending)

- .length_test:

  The number of rows to include in the test set

- .length_train:

  Optional. The number of rows to include in the training set. If NULL,
  returns all remaining row indicies.

## Value

A list containing train_idx and test_idx
