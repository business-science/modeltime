


# CONFIDENCE INTERVAL ESTIMATION ----

add_conf_interval <- function(data, residuals, conf_interval, bootstrap = FALSE) {

    if (!is.null(conf_interval)) {

        if (conf_interval >= 1 | conf_interval <= 0.5) {
            rlang::abort("conf_interval must be between 0.5 and 0.95")
        }

        # Calculate limits
        if (bootstrap) {
            # Reference: https://blog.methodsconsultants.com/posts/understanding-bootstrap-confidence-interval-output-from-the-r-boot-package/
            # https://towardsdatascience.com/recreating-netflixs-quantile-bootstrapping-in-r-a4739a69adb6

            # TODO
        } else {
            # Assume normal
            limits_tbl <- normal_ci(residuals, conf_interval)
        }

        data <- data %>%
            dplyr::mutate(
                .conf_lo = ifelse(.key == "prediction", .value + limits_tbl$conf_lo, NA),
                .conf_hi = ifelse(.key == "prediction", .value + limits_tbl$conf_hi, NA)
            )
    }

    return(data)

}

normal_ci <- function(residuals, conf_interval = 0.8) {

    probs      <- 0.5 + c(-conf_interval/2, conf_interval/2)
    quantile_x <- stats::quantile(residuals, prob = probs, na.rm = TRUE)
    iq_range   <- quantile_x[[2]] - quantile_x[[1]]
    limits     <- quantile_x + iq_range * c(-1, 1)

    ret <- tibble::tibble(
        conf_lo = limits[1],
        conf_hi = limits[2]
    )

    return(ret)
}

# bootstrap_ci <- function(residuals, conf_interval = 0.8, times = 500) {
#
#     data  <- tibble(.resid = residuals)
#     probs <- 0.5 + c(-conf_interval/2, conf_interval/2)
#
#     ret <- replicate(times, data, simplify = FALSE) %>%
#         bind_rows(.key = ".key") %>%
#         mutate(.key = as_factor(.key)) %>%
#         group_by(.key) %>%
#         sample_n(size = times, replace = TRUE) %>%
#         ungroup() %>%
#         summarize(
#             conf_lo = quantile(.resid, prob = probs[1], na.rm = TRUE),
#             conf_hi = quantile(.resid, prob = probs[2], na.rm = TRUE)
#         )
#
#     return(ret)
#
# }

