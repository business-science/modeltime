% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsnip-nnetar_reg.R
\name{nnetar_predict_impl}
\alias{nnetar_predict_impl}
\title{Bridge prediction function for ARIMA models}
\usage{
nnetar_predict_impl(object, new_data, ...)
}
\arguments{
\item{object}{An object of class \code{model_fit}.}

\item{new_data}{A rectangular data object, such as a data frame.}

\item{...}{Additional arguments passed to \code{forecast::forecast()}}
}
\description{
Bridge prediction function for ARIMA models
}
\keyword{internal}
