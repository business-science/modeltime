# Developer Tools for preparing XREGS (Regressors)

These functions are designed to assist developers in extending the
`modeltime` package. `create_xregs_recipe()` makes it simple to automate
conversion of raw un-encoded features to machine-learning ready
features.

## Usage

``` r
create_xreg_recipe(
  data,
  prepare = TRUE,
  clean_names = TRUE,
  dummy_encode = TRUE,
  one_hot = FALSE
)
```

## Arguments

- data:

  A data frame

- prepare:

  Whether or not to run
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  on the final recipe. Default is to prepare. User can set this to FALSE
  to return an un prepared recipe.

- clean_names:

  Uses
  [`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)
  to process the names and improve robustness to failure during dummy
  (one-hot) encoding step.

- dummy_encode:

  Should `factors` (categorical data) be

- one_hot:

  If `dummy_encode = TRUE`, should the encoding return one column for
  each feature or one less column than each feature. Default is `FALSE`.

## Value

A `recipe` in either prepared or un-prepared format.

## Details

The default recipe contains steps to:

1.  Remove date features

2.  Clean the column names removing spaces and bad characters

3.  Convert ordered factors to regular factors

4.  Convert factors to dummy variables

5.  Remove any variables that have zero variance

## Examples

``` r
library(dplyr)
library(timetk)
library(recipes)
library(lubridate)

predictors <- m4_monthly %>%
    filter(id == "M750") %>%
    select(-value) %>%
    mutate(month = month(date, label = TRUE))
predictors
#> # A tibble: 306 × 3
#>    id    date       month
#>    <fct> <date>     <ord>
#>  1 M750  1990-01-01 Jan  
#>  2 M750  1990-02-01 Feb  
#>  3 M750  1990-03-01 Mar  
#>  4 M750  1990-04-01 Apr  
#>  5 M750  1990-05-01 May  
#>  6 M750  1990-06-01 Jun  
#>  7 M750  1990-07-01 Jul  
#>  8 M750  1990-08-01 Aug  
#>  9 M750  1990-09-01 Sep  
#> 10 M750  1990-10-01 Oct  
#> # ℹ 296 more rows

# Create default recipe
xreg_recipe_spec <- create_xreg_recipe(predictors, prepare = TRUE)

# Extracts the preprocessed training data from the recipe (used in your fit function)
juice_xreg_recipe(xreg_recipe_spec)
#> # A tibble: 306 × 11
#>    month_Feb month_Mar month_Apr month_May month_Jun month_Jul month_Aug
#>        <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#>  1         0         0         0         0         0         0         0
#>  2         1         0         0         0         0         0         0
#>  3         0         1         0         0         0         0         0
#>  4         0         0         1         0         0         0         0
#>  5         0         0         0         1         0         0         0
#>  6         0         0         0         0         1         0         0
#>  7         0         0         0         0         0         1         0
#>  8         0         0         0         0         0         0         1
#>  9         0         0         0         0         0         0         0
#> 10         0         0         0         0         0         0         0
#> # ℹ 296 more rows
#> # ℹ 4 more variables: month_Sep <dbl>, month_Oct <dbl>, month_Nov <dbl>,
#> #   month_Dec <dbl>

# Applies the prepared recipe to new data (used in your predict function)
bake_xreg_recipe(xreg_recipe_spec, new_data = predictors)
#> # A tibble: 306 × 11
#>    month_Feb month_Mar month_Apr month_May month_Jun month_Jul month_Aug
#>        <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#>  1         0         0         0         0         0         0         0
#>  2         1         0         0         0         0         0         0
#>  3         0         1         0         0         0         0         0
#>  4         0         0         1         0         0         0         0
#>  5         0         0         0         1         0         0         0
#>  6         0         0         0         0         1         0         0
#>  7         0         0         0         0         0         1         0
#>  8         0         0         0         0         0         0         1
#>  9         0         0         0         0         0         0         0
#> 10         0         0         0         0         0         0         0
#> # ℹ 296 more rows
#> # ℹ 4 more variables: month_Sep <dbl>, month_Oct <dbl>, month_Nov <dbl>,
#> #   month_Dec <dbl>
```
