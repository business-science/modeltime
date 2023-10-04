library(tidymodels)
library(dplyr)
library(timetk)

test_that("extended_forecast_accuracy_metric_set works", {

    skip_on_cran()

    set.seed(1)
    data <- dplyr::tibble(
        time  = timetk::tk_make_timeseries("2020", by = "sec", length_out = 10),
        y     = 1:10 + rnorm(10),
        y_hat = 1:10 + rnorm(10)
    )

    # Create a metric summarizer function from the metric set
    calc_default_metrics <- extended_forecast_accuracy_metric_set(yardstick::mae)

    # Apply the metric summarizer to new data
    skip_if_not_installed("TSrepr")
    ret <- calc_default_metrics(data, y, y_hat)

    expect_equal(nrow(ret), 8)
})

test_that("summarize_accuracy_metrics works", {
    skip_if_not_installed("TSrepr")
    skip_on_cran()

    predictions_tbl <- dplyr::tibble(
        group = c(rep("model_1", 4),
                  rep("model_2", 4)),
        truth = c(1, 2, 3, 4,
                  1, 2, 3, 4),
        estimate = c(1.2, 2.0, 2.5, 2.9,
                     0.9, 1.9, 3.3, 3.9)
    )

    accuracy_tbl <- predictions_tbl %>%
        dplyr::group_by(group) %>%
        summarize_accuracy_metrics(
            truth, estimate,
            metric_set = extended_forecast_accuracy_metric_set()
        )

    expect_equal(ncol(accuracy_tbl), 8)


    accuracy_tbl <- predictions_tbl %>%
        dplyr::group_by(group) %>%
        summarize_accuracy_metrics(
            truth, estimate,
            metric_set = extended_forecast_accuracy_metric_set(
                maape
            )
        )

    expect_equal(ncol(accuracy_tbl), 9)

})
