
# M750 ----

#' The 750th Monthly Time Series used in the M4 Competition
#'
#'
#' @format
#' A `tibble` with 306 rows and 3 variables:
#'
#'  - `id` Factor. Unique series identifier
#'  - `date` Date. Timestamp information. Monthly format.
#'  - `value` Numeric. Value at the corresponding timestamp.
#'
#' @examples
#' m750
#'
#' @source
#'  - [M4 Competition Website](https://mofc.unic.ac.cy/m4/)
#'
"m750"


# M750 SPLITS ----

#' The results of train/test splitting the M750 Data
#'
#'
#' @format
#' An `rsplit` object split into approximately 23.5-years of training data
#' and 2-years of testing data
#'
#' @details
#'
#' ``` {r eval = FALSE}
#' library(timetk)
#' m750_splits <- time_series_split(m750, assess = "2 years", cumulative = TRUE)
#' ```
#'
#' @examples
#' library(rsample)
#'
#' m750_splits
#'
#' training(m750_splits)
#'
"m750_splits"

# M750 TSCV RESAMPLES ----

#' The Time Series Cross Validation Resamples the M750 Data (Training Set)
#'
#'
#' @format
#' An `time_series_cv` object with 6 slices of Time Series Cross Validation
#' resamples made on the `training(m750_splits)`
#'
#' @details
#'
#' ``` {r eval = FALSE}
#' library(timetk)
#' m750_training_resamples <- time_series_cv(
#'     data        = training(m750_splits),
#'     assess      = "2 years",
#'     skip        = "2 years",
#'     cumulative  = TRUE,
#'     slice_limit = 6
#' )
#' ```
#'
#' @examples
#' library(rsample)
#'
#' m750_training_resamples
#'
#'
#'
"m750_training_resamples"


# M750 MODELS ----

#' Three (3) Models trained on the M750 Data (Training Set)
#'
#'
#' @format
#' An `time_series_cv` object with 6 slices of Time Series Cross Validation
#' resamples made on the `training(m750_splits)`
#'
#' @details
#'
#' ``` {r eval = FALSE}
#' library(modeltime)
#' m750_models <- modeltime_table(
#'     wflw_fit_arima,
#'     wflw_fit_prophet,
#'     wflw_fit_glmnet
#' )
#' ```
#'
#' @examples
#' library(modeltime)
#'
#' m750_models
#'
#'
#'
"m750_models"

# ARIMA TUNING RESULTS ----

#' Example ARIMA Tuning Results
#'
#' These objects are the results of an analysis of the
#' M750 data set, which came from the M4 Forecast Competition.
#'
#' @return
#' This is the output of `tune_grid()` for an ARIMA model created
#' with [arima_reg()].
#'
#'
#' @examples
#' arima_workflow_tuned
#'
"arima_workflow_tuned"
