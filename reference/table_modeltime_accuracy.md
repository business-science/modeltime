# Interactive Accuracy Tables

Converts results from
[`modeltime_accuracy()`](https://business-science.github.io/modeltime/reference/modeltime_accuracy.md)
into either interactive (`reactable`) or static (`gt`) tables.

## Usage

``` r
table_modeltime_accuracy(
  .data,
  .round_digits = 2,
  .sortable = TRUE,
  .show_sortable = TRUE,
  .searchable = TRUE,
  .filterable = FALSE,
  .expand_groups = TRUE,
  .title = "Accuracy Table",
  .interactive = TRUE,
  ...
)
```

## Arguments

- .data:

  A `tibble` that is the output of
  [`modeltime_accuracy()`](https://business-science.github.io/modeltime/reference/modeltime_accuracy.md)

- .round_digits:

  Rounds accuracy metrics to a specified number of digits. If `NULL`,
  rounding is not performed.

- .sortable:

  Allows sorting by columns. Only applied to `reactable` tables. Passed
  to `reactable(sortable)`.

- .show_sortable:

  Shows sorting. Only applied to `reactable` tables. Passed to
  `reactable(showSortable)`.

- .searchable:

  Adds search input. Only applied to `reactable` tables. Passed to
  `reactable(searchable)`.

- .filterable:

  Adds filters to table columns. Only applied to `reactable` tables.
  Passed to `reactable(filterable)`.

- .expand_groups:

  Expands groups dropdowns. Only applied to `reactable` tables. Passed
  to `reactable(defaultExpanded)`.

- .title:

  A title for static (`gt`) tables.

- .interactive:

  Return interactive or static tables. If `TRUE`, returns `reactable`
  table. If `FALSE`, returns static `gt` table.

- ...:

  Additional arguments passed to
  [`reactable::reactable()`](https://glin.github.io/reactable/reference/reactable.html)
  or [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) (depending
  on `.interactive` selection).

## Value

A static `gt` table or an interactive `reactable` table containing the
accuracy information.

## Details

**Groups**

The function respects
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
groups and thus scales with multiple groups.

**Reactable Output**

A `reactable()` table is an interactive format that enables live
searching and sorting. When `.interactive = TRUE`, a call is made to
[`reactable::reactable()`](https://glin.github.io/reactable/reference/reactable.html).

`table_modeltime_accuracy()` includes several common options like
toggles for sorting and searching. Additional arguments can be passed to
[`reactable::reactable()`](https://glin.github.io/reactable/reference/reactable.html)
via `...`.

**GT Output**

A `gt` table is an HTML-based table that is "static" (e.g.
non-searchable, non-sortable). It's commonly used in PDF and Word
documents that does not support interactive content.

When `.interactive = FALSE`, a call is made to
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html). Arguments can be
passed via `...`.

Table customization is implemented using a piping workflow (`%>%`). For
more information, refer to the [GT
Documentation](https://gt.rstudio.com/index.html).

## Examples

``` r
library(dplyr)
library(lubridate)
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

# ---- ACCURACY ----

models_tbl %>%
    modeltime_calibrate(new_data = testing(splits)) %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy()

{"x":{"tag":{"name":"Reactable","attribs":{"data":{".model_id":[1],".model_desc":["PROPHET"],".type":["Test"],"mae":[177.04],"mape":[1.69],"mase":[0.6],"smape":[1.69],"rmse":[234.22],"rsq":[0.88]},"columns":[{"id":".model_id","name":".model_id","type":"numeric"},{"id":".model_desc","name":".model_desc","type":"character"},{"id":".type","name":".type","type":"character"},{"id":"mae","name":"mae","type":"numeric"},{"id":"mape","name":"mape","type":"numeric"},{"id":"mase","name":"mase","type":"numeric"},{"id":"smape","name":"smape","type":"numeric"},{"id":"rmse","name":"rmse","type":"numeric"},{"id":"rsq","name":"rsq","type":"numeric"}],"searchable":true,"defaultExpanded":true,"showSortable":true,"dataKey":"0b1f2f90acd9169123928edcfe6e621c"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}
```
