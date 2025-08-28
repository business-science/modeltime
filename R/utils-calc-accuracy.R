# UTILITIES ----

calc_accuracy_2 <- function(train_data = NULL, test_data = NULL, metric_set, by_id = FALSE, ...) {

    metrics <- metric_set

    # Training Metrics
    train_metrics_tbl <- tibble::tibble()

    # Testing Metrics
    test_metrics_tbl <- tibble::tibble()

    # Check by_id
    if (by_id) {
        if (length(names(test_data)) == 5) {
            id_col_text <- names(test_data)[5]
            test_data <- test_data %>%
                dplyr::group_by(!! rlang::ensym(id_col_text))
        } else {
            rlang::warn("The 'id' column in calibration data was not detected. Global accuracy is being returned.")
        }
    }

    if (!is.null(test_data)) {

        test_metrics_tbl <- test_data %>%
            summarize_accuracy_metrics(
                truth      = .actual,
                estimate   = .prediction,
                metric_set = metrics
            ) %>%
            dplyr::ungroup()
        metrics_tbl <- dplyr::bind_rows(train_metrics_tbl, test_metrics_tbl)
    }

    return(metrics_tbl)
}
