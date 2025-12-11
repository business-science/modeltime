# Modeltime Refit Helpers

Used for low-level refitting of modeltime, parnsip and workflow models
These functions are not intended for user use.

## Usage

``` r
mdl_time_refit(object, data, ..., control = NULL)
```

## Arguments

- object:

  A Modeltime Table

- data:

  A `tibble` that contains data to retrain the model(s) using.

- ...:

  Additional arguments to control refitting.

  **Ensemble Model Spec (`modeltime.ensemble`):**

  When making a meta-learner with
  `modeltime.ensemble::ensemble_model_spec()`, used to pass `resamples`
  argument containing results from
  `modeltime.resample::modeltime_fit_resamples()`.

- control:

  Used to control verbosity and parallel processing. See
  [`control_refit()`](https://business-science.github.io/modeltime/reference/control_modeltime.md).

## Value

A tibble with forecast features
