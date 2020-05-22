
# ARIMA ----
# Source: forecast:::arima.string
get_arima_desc_from_arima_object <- function(object, padding=FALSE) {

    order <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
    m <- order[7]
    result <- paste("ARIMA(", order[1], ",", order[2], ",", order[3], ")", sep = "")

    if (m > 1 && sum(order[4:6]) > 0) {
        result <- paste(result, "(", order[4], ",", order[5], ",", order[6], ")[", m, "]", sep = "")
    }

    if (padding && m > 1 && sum(order[4:6]) == 0) {
        result <- paste(result, "         ", sep = "")
        if (m <= 9) {
            result <- paste(result, " ", sep = "")
        } else if (m <= 99) {
            result <- paste(result, "  ", sep = "")
        } else {
            result <- paste(result, "   ", sep = "")
        }
    }

    if (!is.null(object$xreg)) {
        if (NCOL(object$xreg) == 1 && is.element("drift", names(object$coef))) {
            result <- paste(result, "with drift        ")
        } else {
            result <- paste("Regression with", result, "errors")
        }
    } else {
        if (is.element("constant", names(object$coef)) || is.element("intercept", names(object$coef))) {
            result <- paste(result, "with non-zero mean")
        } else if (order[2] == 0 && order[5] == 0) {
            result <- paste(result, "with zero mean    ")
        } else {
            result <- paste(result, "                  ")
        }
    }

    if (!padding) {
        # Strip trailing spaces
        result <- gsub("[ ]*$", "", result)
    }

    return(result)
}
