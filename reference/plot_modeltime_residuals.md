# Interactive Residuals Visualization

This is a wrapper for examining residuals using:

- Time Plot:
  [`timetk::plot_time_series()`](https://business-science.github.io/timetk/reference/plot_time_series.html)

- ACF Plot:
  [`timetk::plot_acf_diagnostics()`](https://business-science.github.io/timetk/reference/plot_acf_diagnostics.html)

- Seasonality Plot:
  [`timetk::plot_seasonal_diagnostics()`](https://business-science.github.io/timetk/reference/plot_seasonal_diagnostics.html)

## Usage

``` r
plot_modeltime_residuals(
  .data,
  .type = c("timeplot", "acf", "seasonality"),
  .smooth = FALSE,
  .legend_show = TRUE,
  .legend_max_width = 40,
  .title = "Residuals Plot",
  .x_lab = "",
  .y_lab = "",
  .color_lab = "Legend",
  .interactive = TRUE,
  ...
)
```

## Arguments

- .data:

  A `tibble` that is the output of
  [`modeltime_residuals()`](https://business-science.github.io/modeltime/reference/modeltime_residuals.md)

- .type:

  One of "timeplot", "acf", or "seasonality". The default is "timeplot".

- .smooth:

  Logical - Whether or not to include a trendline smoother. Uses See
  [`smooth_vec()`](https://business-science.github.io/timetk/reference/smooth_vec.html)
  to apply a LOESS smoother.

- .legend_show:

  Logical. Whether or not to show the legend. Can save space with long
  model descriptions.

- .legend_max_width:

  Numeric. The width of truncation to apply to the legend text.

- .title:

  Title for the plot

- .x_lab:

  X-axis label for the plot

- .y_lab:

  Y-axis label for the plot

- .color_lab:

  Legend label if a `color_var` is used.

- .interactive:

  Returns either a static (`ggplot2`) visualization or an interactive
  (`plotly`) visualization

- ...:

  Additional arguments passed to:

  - Time Plot:
    [`timetk::plot_time_series()`](https://business-science.github.io/timetk/reference/plot_time_series.html)

  - ACF Plot:
    [`timetk::plot_acf_diagnostics()`](https://business-science.github.io/timetk/reference/plot_acf_diagnostics.html)

  - Seasonality Plot:
    [`timetk::plot_seasonal_diagnostics()`](https://business-science.github.io/timetk/reference/plot_seasonal_diagnostics.html)

## Value

A static `ggplot2` plot or an interactive `plotly` plot containing
residuals vs time

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

residuals_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits)) %>%
    modeltime_residuals()

residuals_tbl %>%
    plot_modeltime_residuals(
        .type = "timeplot",
        .interactive = FALSE
    )

```
