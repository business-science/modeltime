% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsnip-temporal_hierarchy.R
\name{temporal_hier_fit_impl}
\alias{temporal_hier_fit_impl}
\title{Low-Level Temporaral Hierarchical function for translating modeltime to forecast}
\usage{
temporal_hier_fit_impl(
  x,
  y,
  period = "auto",
  comb = c("struc", "mse", "ols", "bu", "shr", "sam"),
  usemodel = c("ets", "arima", "theta", "naive", "snaive"),
  ...
)
}
\arguments{
\item{x}{A dataframe of xreg (exogenous regressors)}

\item{y}{A numeric vector of values to fit}

\item{period}{A seasonal frequency. Uses "auto" by default. A character phrase
of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.}

\item{comb}{Combination method of temporal hierarchies}

\item{usemodel}{Model used for forecasting each aggregation level}

\item{...}{Additional arguments passed to \code{forecast::ets}}
}
\description{
Low-Level Temporaral Hierarchical function for translating modeltime to forecast
}
\keyword{internal}
