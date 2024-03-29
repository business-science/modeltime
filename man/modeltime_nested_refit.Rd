% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nested-modeltime_refit.R
\name{modeltime_nested_refit}
\alias{modeltime_nested_refit}
\title{Refits a Nested Modeltime Table}
\usage{
modeltime_nested_refit(object, control = control_nested_refit())
}
\arguments{
\item{object}{A Nested Modeltime Table}

\item{control}{Used to control verbosity and parallel processing. See \code{\link[=control_nested_refit]{control_nested_refit()}}.}
}
\description{
Refits a Nested Modeltime Table to actual data using the following process:
\enumerate{
\item Models are iteratively refit to .actual_data.
\item Any model that returns an error is logged.
Errors can be retrieved with \code{\link[=extract_nested_error_report]{extract_nested_error_report()}}
\item Forecast is predicted on future_data and is logged.
Forecast can be retrieved with \code{\link[=extract_nested_future_forecast]{extract_nested_future_forecast()}}
}
}
