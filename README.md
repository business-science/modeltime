
<!-- README.md is generated from README.Rmd. Please edit that file -->

# modeltime

<img src="vignettes/logo-modeltime.png" width="147" height="170" align="right" />

<!-- badges: start -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/modeltime)](https://cran.r-project.org/package=modeltime)
![](http://cranlogs.r-pkg.org/badges/modeltime?color=brightgreen)
![](http://cranlogs.r-pkg.org/badges/grand-total/modeltime?color=brightgreen)
[![R-CMD-check](https://github.com/business-science/modeltime/workflows/R-CMD-check/badge.svg)](https://github.com/business-science/modeltime/actions)
[![Codecov test
coverage](https://codecov.io/gh/business-science/modeltime/branch/master/graph/badge.svg)](https://codecov.io/gh/business-science/modeltime?branch=master)
<!-- badges: end -->

The time series forecasting package for the `tidymodels` ecosystem.

## Tutorials

-   [**Getting Started with
    Modeltime**](https://business-science.github.io/modeltime/articles/getting-started-with-modeltime.html):
    A walkthrough of the 6-Step Process for using `modeltime` to
    forecast

-   [**Modeltime
    Documentation**](https://business-science.github.io/modeltime/):
    Learn how to **use** `modeltime`, **find** *Modeltime Models*, and
    **extend** `modeltime` so you can use new algorithms inside the
    *Modeltime Workflow*.

## Installation

CRAN version:

``` r
install.packages("modeltime")
```

Development version:

``` r
remotes::install_github("business-science/modeltime")
```

## Why modeltime?

> Modeltime unlocks time series models and machine learning in one
> framework

<img src="vignettes/forecast_plot.jpg" width="100%" style="display: block; margin: auto;" />

No need to switch back and forth between various frameworks. `modeltime`
unlocks machine learning & classical time series analysis.

-   **forecast**: Use ARIMA, ETS, and more models coming (`arima_reg()`,
    `arima_boost()`, & `exp_smoothing()`).
-   **prophet**: Use Facebook’s Prophet algorithm (`prophet_reg()` &
    `prophet_boost()`)
-   **tidymodels**: Use any `parsnip` model: `rand_forest()`,
    `boost_tree()`, `linear_reg()`, `mars()`, `svm_rbf()` to forecast

## Forecast faster

> A streamlined workflow for forecasting

Modeltime incorporates a [streamlined workflow (see Getting Started with
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

## Meet the modeltime ecosystem

> Learn a growing ecosystem of forecasting packages

<div class="figure" style="text-align: center">

<img src="man/figures/modeltime_ecosystem.jpg" alt="The modeltime ecosystem is growing" width="100%" />
<p class="caption">
The modeltime ecosystem is growing
</p>

</div>

Modeltime is part of a **growing ecosystem** of Modeltime forecasting
packages.

-   [Learn Modeltime (Machine
    Learning)](https://business-science.github.io/modeltime/)

-   [Learn Timetk (Feature Engineering,
    Visualization)](https://business-science.github.io/timetk/)

-   [Learn Modeltime H2O
    (AutoML)](https://business-science.github.io/modeltime.h2o/)

-   [Learn Modeltime GluonTS (Deep
    Learning)](https://business-science.github.io/modeltime.gluonts/)

-   [Learn Modeltime Ensemble (Blending
    Forecasts)](https://business-science.github.io/modeltime.ensemble/)

-   [Learn Modeltime Resample
    (Backtesting)](https://business-science.github.io/modeltime.resample/)

## High-Performance Forecasting Course

<a href="https://www.youtube.com/embed/elQb4VzRINg" target="_blank"><img src="http://img.youtube.com/vi/elQb4VzRINg/0.jpg" alt="Anomalize" width="100%" height="450"/></a>

[*My Talk on High-Performance Time Series
Forecasting*](https://youtu.be/elQb4VzRINg)

### Time Series is Changing

Time series is changing. **Businesses now need 10,000+ time series
forecasts every day.** This is what I call a *High-Performance Time
Series Forecasting System (HPTSF)* - Accurate, Robust, and Scalable
Forecasting.

**High-Performance Forecasting Systems will save companies MILLIONS of
dollars.** Imagine what will happen to your career if you can provide
your organization a “High-Performance Time Series Forecasting System”
(HPTSF System).

### How to Learn High-Performance Time Series Forecasting

I teach how to build a HPTFS System in my **High-Performance Time Series
Forecasting Course**. If interested in learning Scalable
High-Performance Forecasting Strategies then [take my Time Series
Forecasting
Course](https://university.business-science.io/p/ds4b-203-r-high-performance-time-series-forecasting).
You will learn:

-   **Time Series Machine Learning** (cutting-edge) with `Modeltime` -
    30+ Models (Prophet, ARIMA, XGBoost, Random Forest, & many more)
-   **Deep Learning** with `GluonTS` (Competition Winners)
-   **Time Series Preprocessing**, Noise Reduction, & Anomaly Detection
-   **Feature engineering** using lagged variables & external regressors
-   **Hyperparameter Tuning**
-   **Time series cross-validation**
-   **Ensembling** Multiple Machine Learning & Univariate Modeling
    Techniques (Competition Winner)
-   **Scalable Forecasting** - Forecast 1000+ time series in parallel
-   and more.

<p class="text-center" style="font-size:30px;">
<a href="https://university.business-science.io/p/ds4b-203-r-high-performance-time-series-forecasting">Unlock
the High-Performance Time Series Forecasting Course</a>
</p>
