

parse_index_from_data <- function(data, error = TRUE) {

    idx <- NULL
    tryCatch({
        # Try to get a period from a user-provided index
        idx_col <- timetk::tk_get_timeseries_variables(data)[1]
        idx     <- data %>% timetk::tk_index() # Will generate an error if no time series index
    }, error = function(e) {
        if (error) rlang::abort("No date or date-time variable provided. Please supply a date or date-time variable as a predictor.")
    })

    return(tibble::tibble(!! idx_col := idx))
}

parse_period_from_index <- function(data, period, error = TRUE) {

    tryCatch({
        if (tolower(period) == "auto" | is.character(period)) {
            idx     <- data %>% timetk::tk_index()
            period  <- timetk::tk_get_frequency(idx, period, message = TRUE)
        }
    }, error = function(e) {
        if (error) {
            rlang::abort("The `period` argument could not be parsed.")
        }
    })

    return(period)

}
