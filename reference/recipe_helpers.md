# Developer Tools for processing XREGS (Regressors)

Wrappers for using
[`recipes::bake`](https://recipes.tidymodels.org/reference/bake.html)
and
[`recipes::juice`](https://recipes.tidymodels.org/reference/juice.html)
to process data returning data in either `data frame` or `matrix` format
(Common formats needed for machine learning algorithms).

## Usage

``` r
juice_xreg_recipe(recipe, format = c("tbl", "matrix"))

bake_xreg_recipe(recipe, new_data, format = c("tbl", "matrix"))
```

## Arguments

- recipe:

  A prepared recipe

- format:

  One of:

  - `tbl`: Returns a tibble (data.frame)

  - `matrix`: Returns a matrix

- new_data:

  Data to be processed by a recipe

## Value

Data in either the `tbl` (data.frame) or `matrix` formats

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
