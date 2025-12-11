# Prepared Nested Modeltime Data

A set of functions to simplify preparation of nested data for iterative
(nested) forecasting with Nested Modeltime Tables.

## Usage

``` r
extend_timeseries(.data, .id_var, .date_var, .length_future, ...)

nest_timeseries(.data, .id_var, .length_future, .length_actual = NULL)

split_nested_timeseries(.data, .length_test, .length_train = NULL, ...)
```

## Arguments

- .data:

  A data frame or tibble containing time series data. The data should
  have:

  - identifier (.id_var): Identifying one or more time series groups

  - date variable (.date_var): A date or date time column

  - target variable (.value): A column containing numeric values that is
    to be forecasted

- .id_var:

  An id column

- .date_var:

  A date or datetime column

- .length_future:

  Varies based on the function:

  - `extend_timeseries()`: Defines how far into the future to extend the
    time series by each time series group.

  - `nest_timeseries()`: Defines which observations should be split into
    the `.future_data`.

- ...:

  Additional arguments passed to the helper function. See details.

- .length_actual:

  Can be used to slice the `.actual_data` to a most recent number of
  observations.

- .length_test:

  Defines the length of the test split for evaluation.

- .length_train:

  Defines the length of the training split for evaluation.

## Details

Preparation of nested time series follows a 3-Step Process:

### Step 1: Extend the Time Series

`extend_timeseries()`: A wrapper for
[`timetk::future_frame()`](https://business-science.github.io/timetk/reference/future_frame.html)
that extends a time series group-wise into the future.

- The group column is specified by `.id_var`.

- The date column is specified by `.date_var`.

- The length into the future is specified with `.length_future`.

- The `...` are additional parameters that can be passed to
  [`timetk::future_frame()`](https://business-science.github.io/timetk/reference/future_frame.html)

### Step 2: Nest the Time Series

`nest_timeseries()`: A helper for nesting your data into `.actual_data`
and `.future_data`.

- The group column is specified by `.id_var`

- The `.length_future` defines the length of the `.future_data`.

- The remaining data is converted to the `.actual_data`.

- The `.length_actual` can be used to slice the `.actual_data` to a most
  recent number of observations.

The result is a "nested data frame".

### Step 3: Split the Actual Data into Train/Test Splits

`split_nested_timeseries()`: A wrapper for
[`timetk::time_series_split()`](https://business-science.github.io/timetk/reference/time_series_split.html)
that generates training/testing splits from the `.actual_data` column.

- The `.length_test` is the primary argument that identifies the size of
  the testing sample. This is typically the same size as the
  `.future_data`.

- The `.length_train` is an optional size of the training data.

- The `...` (dots) are additional arguments that can be passed to
  [`timetk::time_series_split()`](https://business-science.github.io/timetk/reference/time_series_split.html).

### Helpers

[`extract_nested_train_split()`](https://business-science.github.io/modeltime/reference/log_extractors.md)
and
[`extract_nested_test_split()`](https://business-science.github.io/modeltime/reference/log_extractors.md)
are used to simplify extracting the training and testing data from the
actual data. This can be helpful when making preprocessing recipes using
the `recipes` package.

## Examples

``` r
library(dplyr)
library(timetk)


nested_data_tbl <- walmart_sales_weekly %>%
    select(id, date = Date, value = Weekly_Sales) %>%

    # Step 1: Extends the time series by id
    extend_timeseries(
        .id_var     = id,
        .date_var   = date,
        .length_future = 52
    ) %>%

    # Step 2: Nests the time series into .actual_data and .future_data
    nest_timeseries(
        .id_var     = id,
        .length_future = 52
    ) %>%

    # Step 3: Adds a column .splits that contains training/testing indices
    split_nested_timeseries(
        .length_test = 52
    )

nested_data_tbl
#> # A tibble: 7 × 4
#>   id    .actual_data       .future_data      .splits        
#>   <fct> <list>             <list>            <list>         
#> 1 1_1   <tibble [143 × 2]> <tibble [52 × 2]> <split [91|52]>
#> 2 1_3   <tibble [143 × 2]> <tibble [52 × 2]> <split [91|52]>
#> 3 1_8   <tibble [143 × 2]> <tibble [52 × 2]> <split [91|52]>
#> 4 1_13  <tibble [143 × 2]> <tibble [52 × 2]> <split [91|52]>
#> 5 1_38  <tibble [143 × 2]> <tibble [52 × 2]> <split [91|52]>
#> 6 1_93  <tibble [143 × 2]> <tibble [52 × 2]> <split [91|52]>
#> 7 1_95  <tibble [143 × 2]> <tibble [52 × 2]> <split [91|52]>

# Helpers: Getting the Train/Test Sets
extract_nested_train_split(nested_data_tbl, .row_id = 1)
#> # A tibble: 91 × 2
#>    date        value
#>    <date>      <dbl>
#>  1 2010-02-05 24924.
#>  2 2010-02-12 46039.
#>  3 2010-02-19 41596.
#>  4 2010-02-26 19404.
#>  5 2010-03-05 21828.
#>  6 2010-03-12 21043.
#>  7 2010-03-19 22137.
#>  8 2010-03-26 26229.
#>  9 2010-04-02 57258.
#> 10 2010-04-09 42961.
#> # ℹ 81 more rows
```
