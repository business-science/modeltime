#' Succinct summary of Modeltime Tables
#'
#' `type_sum` controls how objects are shown when inside tibble
#'  columns.
#' @param x	A `mdl_time_tbl` object to summarise.
#' @return A character value.
#' @importFrom tibble type_sum
#' @export
type_sum.mdl_time_tbl <- function(x) {
    stringr::str_glue("model_time")
}

