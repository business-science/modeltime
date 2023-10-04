#' Interactive Accuracy Tables
#'
#' Converts results from [modeltime_accuracy()] into
#' either interactive (`reactable`) or static (`gt`) tables.
#'
#' @param .data A `tibble` that is the output of [modeltime_accuracy()]
#' @param .round_digits Rounds accuracy metrics to a specified number of digits.
#'  If `NULL`, rounding is not performed.
#' @param .sortable Allows sorting by columns.
#'  Only applied to `reactable` tables.
#'  Passed to `reactable(sortable)`.
#' @param .show_sortable Shows sorting.
#'  Only applied to `reactable` tables.
#'  Passed to `reactable(showSortable)`.
#' @param .searchable Adds search input.
#'  Only applied to `reactable` tables.
#'  Passed to `reactable(searchable)`.
#' @param .filterable Adds filters to table columns.
#'  Only applied to `reactable` tables.
#'  Passed to `reactable(filterable)`.
#' @param .expand_groups Expands groups dropdowns.
#' Only applied to `reactable` tables.
#'  Passed to `reactable(defaultExpanded)`.
#' @param .title Only applied to `gt` tables.
#'  Passed to `tab_header()` to modify the `gt()` table object.
#' @param .interactive Return interactive or static tables. If `TRUE`,
#' returns `reactable` table. If `FALSE`, returns static `gt` table.
#' @param .title A title for static (`gt`) tables.
#' @param ... Additional arguments passed to [reactable::reactable()]
#' or [gt::gt()] (depending on `.interactive` selection).
#'
#' @return A static `gt` table or an interactive `reactable` table containing
#'  the accuracy information.
#'
#'
#' @details
#'
#' __Groups__
#'
#' The function respects `dplyr::group_by()` groups and thus scales with multiple groups.
#'
#' __Reactable Output__
#'
#' A `reactable()` table is an interactive format that enables live searching and sorting.
#'  When `.interactive = TRUE`, a call is made to  [reactable::reactable()].
#'
#' `table_modeltime_accuracy()` includes several common options like toggles for sorting and searching.
#' Additional arguments can be passed to [reactable::reactable()] via `...`.
#'
#' __GT Output__
#'
#' A `gt` table is an HTML-based table that is "static" (e.g. non-searchable, non-sortable). It's
#' commonly used in PDF and Word documents that does not support interactive content.
#'
#' When `.interactive = FALSE`, a call is made to [gt::gt()]. Arguments can be passed via `...`.
#'
#' Table customization is implemented using a piping workflow (`%>%`).
#' For more information, refer to the [GT Documentation](https://gt.rstudio.com/index.html).
#'
#'
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#' library(timetk)
#' library(parsnip)
#' library(rsample)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.9)
#'
#' # --- MODELS ---
#'
#' # Model 1: prophet ----
#' model_fit_prophet <- prophet_reg() %>%
#'     set_engine(engine = "prophet") %>%
#'     fit(value ~ date, data = training(splits))
#'
#'
#' # ---- MODELTIME TABLE ----
#'
#' models_tbl <- modeltime_table(
#'     model_fit_prophet
#' )
#'
#' # ---- ACCURACY ----
#'
#' models_tbl %>%
#'     modeltime_calibrate(new_data = testing(splits)) %>%
#'     modeltime_accuracy() %>%
#'     table_modeltime_accuracy()
#'
#'
#' @export
table_modeltime_accuracy <- function(.data, .round_digits = 2,
                                     .sortable = TRUE, .show_sortable = TRUE, .searchable = TRUE,
                                     .filterable = FALSE, .expand_groups = TRUE,
                                     .title = "Accuracy Table",
                                     .interactive = TRUE, ...) {

    # Checks
    # If using an argument inside cli inline markup that starts with . like `.data`,
    # we must use {(.data)} for it. https://cli.r-lib.org/reference/inline-markup.html

    if (!inherits(.data, "data.frame")) {
        cli::cli_abort("No method for {.obj_type_friendly {(.data)}}. Expecting the output of 'modeltime_accuracy()'.")
    }

    if (!all(c(".model_id", ".model_desc") %in% names(.data))) {
        rlang::abort("Expecting the following names to be in the data frame: .model_id, .model_Desc. Try using 'modeltime_accuracy()' to return a data frame in the appropriate structure.")
    }

    # Data Preparation
    data_formatted <- .data

    if (!is.null(round)) {
            data_formatted <- data_formatted %>%
                dplyr::mutate(dplyr::across(dplyr::where(is.double), .fns = ~ round(.x, digits = .round_digits)))
    }

    # Output either reactable() or gt()
    if (.interactive) {
        # Reactable

        group_vars_text <- dplyr::group_vars(data_formatted)

        group_by_cols <- NULL
        if (length(group_vars_text) > 0) {
            group_by_cols <- group_vars_text
        }

        t <- data_formatted %>%
            reactable::reactable(
                groupBy         = group_by_cols,
                sortable        = .sortable,
                showSortable    = .show_sortable,
                searchable      = .searchable,
                filterable      = .filterable,
                defaultExpanded = .expand_groups,
                ...
            )
        # TODO gt now allows opt_interactive
       # gt %>%
       #      gt::opt_interactive(
       #          use_sorting = .show_sortable,
       #          use_filters = .filterable,
       #          use_search = .searchable,
       #      )

    } else {
        # gt()

        t <- data_formatted %>%
            gt::gt(...) %>%
            gt::tab_header(title = .title)
    }

    return(t)

}
