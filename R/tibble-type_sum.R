#' Succinct summary of Modeltime Tables
#'
#' `type_sum` controls how objects are shown when inside tibble
#'  columns.
#' @param x	A `mdl_time_tbl` object to summarise.
#' @return A character value.
#' @importFrom tibble type_sum
#' @keywords internal
#' @export
type_sum.mdl_time_tbl <- function(x) {
    stringr::str_glue("model_time")
}

#' @importFrom tibble type_sum
#' @export
type_sum.recursive <- function(x) {
    class(x) <- class(x)[stringr::str_detect(class(x), "recursive", negate = TRUE)]
    type_sum(x)
}

#' @importFrom tibble type_sum
#' @export
type_sum.recursive_panel <- function(x) {
    class(x) <- class(x)[stringr::str_detect(class(x), "recursive", negate = TRUE)]
    type_sum(x)
}

#' @importFrom tibble type_sum
#' @export
type_sum.ts_split_indicies <- function(x) {

    stringr::str_glue("split [{length(x$idx_train)}|{length(x$idx_test)}]")
}
