

make_grouped_predictions <- function(model, new_data, id_col, idx_col) {

    # print("Start")

    nested_tbl <- model %>%
        dplyr::group_by(!! rlang::sym(id_col)) %>%
        tidyr::nest(.pred_values = value) %>%
        dplyr::ungroup()

    # print("Nested")
    # print(nested_tbl)

    new_data_nested_tbl <- new_data %>%

        tibble::rowid_to_column(var = ".row_id") %>%
        dplyr::select(.row_id, !! rlang::sym(id_col), !! rlang::sym(idx_col)) %>%
        dplyr::group_by(!! rlang::sym(id_col)) %>%
        tidyr::nest(.idx_values = c(.row_id, !! rlang::sym(idx_col))) %>%
        dplyr::ungroup()

    # print("Data Nested")
    # print(new_data_nested_tbl)

    data_joined_tbl <- new_data_nested_tbl %>%
        dplyr::left_join(nested_tbl, by = id_col)

    # print("Data Joined")
    # print(data_joined_tbl)

    data_joined_tbl <- data_joined_tbl %>%
        dplyr::mutate(.final_values = purrr::map2(
            .x = .idx_values, .y = .pred_values, .f = function(x, y) {

                ret <- tryCatch({
                    tibble::tibble(value = rep_len(y$value, length.out = nrow(x)))
                }, error = function(e) {
                    tibble::tibble(value = rep_len(NA, length.out = nrow(x)))
                })

                return(ret)

            })) %>%
        dplyr::select(-.pred_values) %>%
        tidyr::unnest(cols = c(.idx_values, .final_values)) %>%

        dplyr::arrange(.row_id)

    # print(data_joined_tbl)

    preds <- data_joined_tbl$value

    return(preds)

}
