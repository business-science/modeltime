% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsnip-exp_smoothing.R
\name{exp_smoothing}
\alias{exp_smoothing}
\title{General Interface for Exponential Smoothing State Space Models}
\usage{
exp_smoothing(
  mode = "regression",
  seasonal_period = NULL,
  error = NULL,
  trend = NULL,
  season = NULL,
  damping = NULL,
  smooth_level = NULL,
  smooth_trend = NULL,
  smooth_seasonal = NULL
)
}
\arguments{
\item{mode}{A single character string for the type of model.
The only possible value for this model is "regression".}

\item{seasonal_period}{A seasonal frequency. Uses "auto" by default.
A character phrase of "auto" or time-based phrase of "2 weeks"
can be used if a date or date-time variable is provided.
See Fit Details below.}

\item{error}{The form of the error term: "auto", "additive", or "multiplicative".
If the error is multiplicative, the data must be non-negative.}

\item{trend}{The form of the trend term: "auto", "additive", "multiplicative" or "none".}

\item{season}{The form of the seasonal term: "auto", "additive", "multiplicative" or "none".}

\item{damping}{Apply damping to a trend: "auto", "damped", or "none".}

\item{smooth_level}{This is often called the "alpha" parameter used as the base level smoothing
factor for exponential smoothing models.}

\item{smooth_trend}{This is often called the "beta" parameter used as the trend smoothing
factor for exponential smoothing models.}

\item{smooth_seasonal}{This is often called the "gamma" parameter used as the seasonal smoothing
factor for exponential smoothing models.}
}
\description{
\code{exp_smoothing()} is a way to generate a \emph{specification} of an Exponential Smoothing model
before fitting and allows the model to be created using
different packages. Currently the only package is \code{forecast}. Several algorithms are implemented:
\itemize{
\item ETS - Automated Exponential Smoothing
\item CROSTON - Croston's forecast is a special case
of Exponential Smoothing for intermittent demand
\item Theta - A special case of Exponential Smoothing with Drift that
performed well in the M3 Competition
}
}
\details{
Models can be created using the following \emph{engines}:
\itemize{
\item "ets" (default) - Connects to \code{\link[forecast:ets]{forecast::ets()}}
\item "croston" - Connects to \code{\link[forecast:croston]{forecast::croston()}}
\item "theta" - Connects to \code{\link[forecast:thetaf]{forecast::thetaf()}}
}
}
\section{Engine Details}{


The standardized parameter names in \code{modeltime} can be mapped to their original
names in each engine:\tabular{lll}{
   modeltime \tab forecast::ets \tab forecast::croston() \cr
   seasonal_period() \tab ts(frequency) \tab ts(frequency) \cr
   error(), trend(), season() \tab model ('ZZZ') \tab NA \cr
   damping() \tab damped (NULL) \tab NA \cr
   smooth_level() \tab alpha (NULL) \tab alpha (0.1) \cr
   smooth_trend() \tab beta (NULL) \tab NA \cr
   smooth_seasonal() \tab gamma (NULL) \tab NA \cr
}


Other options can be set using \code{set_engine()}.

\strong{ets (default engine)}

The engine uses \code{\link[forecast:ets]{forecast::ets()}}.

Function Parameters:\preformatted{## function (y, model = "ZZZ", damped = NULL, alpha = NULL, beta = NULL, gamma = NULL, 
##     phi = NULL, additive.only = FALSE, lambda = NULL, biasadj = FALSE, 
##     lower = c(rep(1e-04, 3), 0.8), upper = c(rep(0.9999, 3), 0.98), opt.crit = c("lik", 
##         "amse", "mse", "sigma", "mae"), nmse = 3, bounds = c("both", "usual", 
##         "admissible"), ic = c("aicc", "aic", "bic"), restrict = TRUE, allow.multiplicative.trend = FALSE, 
##     use.initial.values = FALSE, na.action = c("na.contiguous", "na.interp", 
##         "na.fail"), ...)
}

The main arguments are \code{model} and \code{damped} are defined using:
\itemize{
\item \code{error()} = "auto", "additive", and "multiplicative" are converted to  "Z", "A", and "M"
\item \code{trend()} = "auto", "additive", "multiplicative", and "none" are converted to "Z","A","M" and "N"
\item \code{season()} = "auto", "additive", "multiplicative", and "none" are converted to "Z","A","M" and "N"
\item \code{damping()} - "auto", "damped", "none" are converted to NULL, TRUE, FALSE
\item \code{smooth_level()}, \code{smooth_trend()}, and \code{smooth_seasonal()} are
automatically determined if not provided. They are mapped to "alpha", "beta" and "gamma", respectively.
}

By default, all arguments are set to "auto" to perform automated Exponential Smoothing using
\emph{in-sample data} following the underlying \code{forecast::ets()} automation routine.

Other options and argument can be set using \code{set_engine()}.

Parameter Notes:
\itemize{
\item \code{xreg} - This model is not set up to use exogenous regressors. Only univariate
models will be fit.
}

\strong{croston}

The engine uses \code{\link[forecast:croston]{forecast::croston()}}.

Function Parameters:\preformatted{## function (y, h = 10, alpha = 0.1, x = y)
}

The main arguments are defined using:
\itemize{
\item \code{smooth_level()}: The "alpha" parameter
}

Parameter Notes:
\itemize{
\item \code{xreg} - This model is not set up to use exogenous regressors. Only univariate
models will be fit.
}

\strong{theta}

The engine uses \code{\link[forecast:thetaf]{forecast::thetaf()}}

Parameter Notes:
\itemize{
\item \code{xreg} - This model is not set up to use exogenous regressors. Only univariate
models will be fit.
}
}

\section{Fit Details}{


\strong{Date and Date-Time Variable}

It's a requirement to have a date or date-time variable as a predictor.
The \code{fit()} interface accepts date and date-time features and handles them internally.
\itemize{
\item \code{fit(y ~ date)}
}

\emph{Seasonal Period Specification}

The period can be non-seasonal (\code{seasonal_period = 1} or \code{"none"}) or seasonal (e.g. \code{seasonal_period = 12} or \code{seasonal_period = "12 months"}).
There are 3 ways to specify:
\enumerate{
\item \code{seasonal_period = "auto"}: A period is selected based on the periodicity of the data (e.g. 12 if monthly)
\item \code{seasonal_period = 12}: A numeric frequency. For example, 12 is common for monthly data
\item \code{seasonal_period = "1 year"}: A time-based phrase. For example, "1 year" would convert to 12 for monthly data.
}

\strong{Univariate:}

For univariate analysis, you must include a date or date-time feature. Simply use:
\itemize{
\item Formula Interface (recommended): \code{fit(y ~ date)} will ignore xreg's.
\item XY Interface: \code{fit_xy(x = data[,"date"], y = data$y)} will ignore xreg's.
}

\strong{Multivariate (xregs, Exogenous Regressors)}

This model is not set up for use with exogenous regressors.
}

\examples{
library(dplyr)
library(parsnip)
library(rsample)
library(timetk)
library(modeltime)

# Data
m750 <- m4_monthly \%>\% filter(id == "M750")
m750

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)

# ---- AUTO ETS ----

# Model Spec - The default parameters are all set
# to "auto" if none are provided
model_spec <- exp_smoothing() \%>\%
    set_engine("ets")

# Fit Spec
model_fit <- model_spec \%>\%
    fit(log(value) ~ date, data = training(splits))
model_fit


# ---- STANDARD ETS ----

# Model Spec
model_spec <- exp_smoothing(
        seasonal_period  = 12,
        error            = "multiplicative",
        trend            = "additive",
        season           = "multiplicative"
    ) \%>\%
    set_engine("ets")

# Fit Spec
model_fit <- model_spec \%>\%
    fit(log(value) ~ date, data = training(splits))
model_fit


# ---- CROSTON ----
\donttest{
# Model Spec
model_spec <- exp_smoothing(
        smooth_level = 0.2
    ) \%>\%
    set_engine("croston")

# Fit Spec
model_fit <- model_spec \%>\%
    fit(log(value) ~ date, data = training(splits))
model_fit
}



# ---- THETA ----
\donttest{
#' # Model Spec
model_spec <- exp_smoothing() \%>\%
    set_engine("theta")

# Fit Spec
model_fit <- model_spec \%>\%
    fit(log(value) ~ date, data = training(splits))
model_fit
}

}
\seealso{
\code{\link[=fit.model_spec]{fit.model_spec()}}, \code{\link[=set_engine]{set_engine()}}
}
