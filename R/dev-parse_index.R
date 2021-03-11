#' Developer Tools for parsing date and date-time information
#'
#' These functions are designed to assist developers in extending the `modeltime`
#' package.
#'
#' @param data A data frame
#' @param period A period to calculate from the time index. Numeric values are returned as-is.
#'  "auto" guesses a numeric value from the index. A time-based phrase (e.g. "7 days") calculates
#'  the number of timestamps that typically occur within the time-based phrase.
#'
#' @return
#' - parse_index_from_data(): Returns a tibble containing the date or date-time column.
#' - parse_period_from_index(): Returns the numeric period from a tibble containing the index.
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#' predictors <- m4_monthly %>%
#'     filter(id == "M750") %>%
#'     select(-value)
#'
#' index_tbl <- parse_index_from_data(predictors)
#' index_tbl
#'
#' period <- parse_period_from_index(index_tbl, period = "1 year")
#' period
#'
#' @name parse_index
NULL

#' @rdname parse_index
#' @export
parse_index_from_data <- function(data) {
    UseMethod("parse_index_from_data", data)
}

#' @export
parse_index_from_data.default <- function(data) {
    rlang::abort(paste0("No method for class ", class(data)[1]))
}

#' @export
parse_index_from_data.data.frame <- function(data) {

    idx <- NULL
    tryCatch({
        # Try to get a period from a user-provided index
        idx_col <- timetk::tk_get_timeseries_variables(data)[1]
        idx     <- data %>% timetk::tk_index() # Will generate an error if no time series index
    }, error = function(e) {
        rlang::abort("No date or date-time variable provided. Please supply a date or date-time variable as a predictor.")
    })

    return(tibble::tibble(!! idx_col := idx))
}


#' @rdname parse_index
#' @export
parse_period_from_index <- function(data, period) {
    UseMethod("parse_period_from_index", data)
}

#' @export
parse_period_from_index.default <- function(data, period) {
    rlang::abort(paste0("No method for class ", class(data)[1]))
}

#' @export
parse_period_from_index.data.frame <- function(data, period) {

    period <- switch_period(period)

    # If character, parse period / If numeric, pass over
    tryCatch({
        if (is.character(period)) {
            period <- tolower(period)
            if (period != "none") {
                idx     <- data %>% timetk::tk_index()
                period  <- unique(idx) %>% sort() %>% timetk::tk_get_frequency(period, message = TRUE)
            } else {
                # period = "none"
                message("Using period = 1 (no seasonal period).")
                period <- 1
            }
        }
    }, error = function(e) {
        rlang::abort("The `period` argument could not be parsed.")
    })

    return(period)

}

switch_period <- function(period) {
    if (tolower(period) == "daily") {
        period <- "1 day"
    }
    if (tolower(period) == "weekly") {
        period <- "1 week"
    }
    if (tolower(period) == "yearly") {
        period <- "1 year"
    }

    return(period)
}
