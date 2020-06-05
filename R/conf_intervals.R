
# High Density Estimate
# hdi_mean_shifted <- function (x, conf_interval = 0.89) {
#
#     # Calculate HDI (High Density Interval)
#     # - https://easystats.github.io/bayestestR/articles/credible_interval.html
#     hdi_ci_estimates_df <- bayestestR::hdi(
#         x = x,
#         ci = conf_interval,
#         verbose = FALSE
#     )
#
#     ci_lo_vec = hdi_ci_estimates_df$CI_low
#     ci_hi_vec = hdi_ci_estimates_df$CI_high
#
#     # Apply mean shift
#     mu <- mean(x, na.rm = T)
#
#     ci_lo_vec_shifted <- min(ci_lo_vec, ci_lo_vec - mu)
#     ci_hi_vec_shifted <- max(ci_hi_vec, ci_hi_vec - mu)
#
#     # Tibble
#     ret <- tibble::tibble(
#         .conf_lo = ci_lo_vec_shifted,
#         .conf_hi = ci_hi_vec_shifted
#     )
#
#     return(ret)
#
#
# }



# https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals
# mean.pred.intervals <- function(x, y, pred.x) {
#
#     n <- length(y) # Find sample size
#
#     lm.model <- lm(y ~ x) # Fit linear model
#     y.fitted <- lm.model$fitted.values # Extract the fitted values of y
#
#     # Coefficients of the linear model, beta0 and beta1
#     b0 <- lm.model$coefficients[1]
#     b1 <- lm.model$coefficients[2]
#
#     pred.y <- b1 * pred.x + b0 # Predict y at the given value of x (argument pred.x)
#
#     # Find SSE and MSE
#     sse <- sum((y - y.fitted)^2)
#     mse <- sse / (n - 2)
#
#     t.val <- qt(0.975, n - 2) # Critical value of t
#
#     mean.se.fit <- (1 / n + (pred.x - mean(x))^2 / (sum((x - mean(x))^2))) # Standard error of the mean estimate
#     pred.se.fit <- (1 + (1 / n) + (pred.x - mean(x))^2 / (sum((x - mean(x))^2))) # Standard error of the prediction
#
#     # Mean Estimate Upper and Lower Confidence limits at 95% Confidence
#     mean.conf.upper <- pred.y + t.val * sqrt(mse * mean.se.fit)
#     mean.conf.lower <- pred.y - t.val * sqrt(mse * mean.se.fit)
#
#     # Prediction Upper and Lower Confidence limits at 95% Confidence
#     pred.conf.upper <- pred.y + t.val * sqrt(mse * pred.se.fit)
#     pred.conf.lower <- pred.y - t.val * sqrt(mse * pred.se.fit)
#
#     # Beta 1 Upper and Lower Confidence limits at 95% Confidence
#     b1.conf.upper <- b1 + t.val * sqrt(mse) / sqrt(sum((x - mean(x))^2))
#     b1.conf.lower <- b1 - t.val * sqrt(mse) / sqrt(sum((x - mean(x))^2))
#
#     # Build data.frame of upper and lower limits calculated above, as well as the predicted y and beta 1 values
#     upper <- data.frame(rbind(round(mean.conf.upper, 2), round(pred.conf.upper, 2), round(b1.conf.upper, 2)))
#     lower <- data.frame(rbind(round(mean.conf.lower, 2), round(pred.conf.lower, 2), round(b1.conf.lower, 2)))
#     fit <- data.frame(rbind(round(pred.y, 2), round(pred.y, 2), round(b1, 2)))
#
#     # Collect all into data.frame and rename columns
#     results <- data.frame(cbind(lower, upper, fit), row.names = c('Mean', 'Prediction', 'Coefficient'))
#     colnames(results) <- c('Lower', 'Upper', 'Fit')
#
#     return(results)
# }


# CONFIDENCE INTERVAL ESTIMATION ----

# add_conf_interval <- function(data, value, residuals, conf_interval, bootstrap = FALSE) {
#
#     if (!is.null(conf_interval)) {
#
#         if (conf_interval >= 1 | conf_interval <= 0.5) {
#             rlang::abort("conf_interval must be between 0.5 and 0.95")
#         }
#
#         value_expr     <- rlang::enquo(value)
#         residuals_expr <- rlang::enquo(residuals)
#
#         # Calculate limits
#         if (bootstrap) {
#             # Reference: https://blog.methodsconsultants.com/posts/understanding-bootstrap-confidence-interval-output-from-the-r-boot-package/
#             # https://towardsdatascience.com/recreating-netflixs-quantile-bootstrapping-in-r-a4739a69adb6
#
#             # TODO
#         } else {
#             # Assume normal
#             limits_tbl <- normal_ci(residuals_vec, conf_interval)
#
#             data <- data %>%
#                 dplyr::mutate(
#                     .conf_lo = ifelse(.key == "prediction", (!! value_expr) + normal_ci(!! residuals_expr, conf_interval)$conf_lo, NA),
#                     .conf_hi = ifelse(.key == "prediction", (!! value_expr) + normal_ci(!! residuals_expr, conf_interval)$conf_hi, NA)
#                 )
#         }
#
#
#     }
#
#     return(data)
#
# }



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

