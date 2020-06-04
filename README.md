
<!-- README.md is generated from README.Rmd. Please edit that file -->

# modeltime

[![Travis build
status](https://travis-ci.org/business-science/modeltime.svg?branch=master)](https://travis-ci.org/business-science/modeltime)
[![codecov](https://codecov.io/gh/business-science/modeltime/branch/master/graph/badge.svg)](https://codecov.io/gh/business-science/modeltime)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/modeltime)](https://cran.r-project.org/package=modeltime)
![](http://cranlogs.r-pkg.org/badges/modeltime?color=brightgreen)
![](http://cranlogs.r-pkg.org/badges/grand-total/modeltime?color=brightgreen)

The time series forecasting package for the `tidymodels` ecosystem.

<img src="vignettes/forecast_plot.jpg" width="100%" style="display: block; margin: auto;" />

## Features & Benefits

#### Modeltime unlocks time series models and machine learning in 1 framework

No need to switch back and forth between various frameworks. `modeltime`
unlocks machine learning & classical time series analysis.

  - **forecast**: Use ARIMA, ETS, and more models coming.
  - **prophet**: Use Facebook’s `prophet()` algorithm
  - **tidymodels**: Use any `parsnip` model: `rand_forest()`, Boosted
    `boost_tree()`, `linear_reg()`, `mars()`, `svm_rbf()` to forecast

#### A streamlined workflow for forecasting

Modeltime incorporates a [simple workflow (see Getting Started with
Modeltime)](https://business-science.github.io/modeltime/articles/getting-started-with-modeltime.html)
for using best practices to forecast.

<hr>

<div class="figure" style="text-align: center">

<img src="vignettes/modeltime_workflow.jpg" alt="A streamlined workflow for forecasting" width="100%" />

<p class="caption">

A streamlined workflow for forecasting

</p>

</div>

<hr>

#### Interactive plotting by default

All plots incorporate both `plotly` (interactive) and `ggplot2` (static)
visualizations. This means you can quickly add forecasts to `shiny`
apps, `rmarkdown` documents, and more.

## Tutorials

  - [**Getting Started with
    Modeltime**](https://business-science.github.io/modeltime/articles/getting-started-with-modeltime.html):
    A walkthrough of the 6-Step Process for using `modeltime` to
    forecast

  - [**Modeltime
    Documentation**](https://business-science.github.io/modeltime/):
    Learn how to **use** `modeltime`, **find** *Modeltime Models*, and
    **extend** `modeltime` so you can use new algorithms inside the
    *Modeltime Workflow*.

## Installation

Install the development version from with:

``` r
# install.packages("devtools")
devtools::install_github("business-science/modeltime")
```

# Learning More

I teach `modeltime` in my **Time Series Analysis & Forecasting Course**.
If interested in learning Pro-Forecasting Strategies then [join my
waitlist](https://mailchi.mp/business-science/time-series-forecasting-course-coming-soon).
The course is coming soon.

<img src="vignettes/time_series_course.jpg" width="100%" />

You will learn:

  - Time Series Preprocessing, Noise Reduction, & Anomaly Detection
  - Feature engineering using lagged variables & external regressors
  - Hyperparameter Tuning
  - Time series cross-validation
  - Ensembling Multiple Machine Learning & Univariate Modeling
    Techniques (Competition Winner)
  - NEW - Deep Learning with RNNs (Competition Winner)
  - and more.

<p class="text-center" style="font-size:30px;">

<a href="https://mailchi.mp/business-science/time-series-forecasting-course-coming-soon">Signup
for the Time Series Course waitlist</a>

</p>
