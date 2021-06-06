

control_modeltime_objects <- function(
    verbose = FALSE,
    allow_par = FALSE,
    cores = -1,
    packages = NULL,
    func = NULL
) {

    val_class_and_single(verbose, "logical", "control_refit()")
    val_class_and_single(allow_par, "logical", "control_refit()")
    val_class_and_single(cores, "numeric", "control_refit()")

    if (allow_par) {
        required_pkgs <- c("modeltime", "parsnip", "dplyr", "stats",
                           "lubridate", "tidymodels", "timetk")

        namespace_pkgs <- search() %>%
            stringr::str_subset(pattern = "^package") %>%
            stringr::str_remove("package:")

        packages <- c(required_pkgs, namespace_pkgs, packages) %>% unique()

        load_namespace(packages, full_load = packages)

    }

    if (!allow_par) {
        cores <- 1
    } else {
        cores_available <- parallel::detectCores(logical = FALSE) # Detect Physical Cores

        foreach_workers <- foreach::getDoParWorkers() # Detect how many workers currently set up

        if (foreach_workers > 1) {
            # WORKERS ALREADY SET UP
            cores_requested <- cores
            cores <- foreach_workers
            if ((cores_requested > 1) && (cores != cores_requested)) {
                rlang::warn(stringr::str_glue("Detected parallel backend with {cores} cores but user requested {cores_requested} cores. Using {cores} cores."))
            }
        } else {
            if (!is.na(cores_available)) {
                # NUMBER OF CORES DETERMINED
                if (cores < 1) cores <- cores_available
                # if (cores > cores_available) cores <- cores_available
            } else {
                # UNKNOWN NUMBER OF CORES
                if (cores < 1) {
                    rlang::warn(
                        stringr::str_glue("{if (!is.null(func)) paste0(func, ': ') }`allow_par` is TRUE but unknown number of `cores`. Setting `cores = 1`.")
                    )
                    cores <- 1
                }

            }
        }

    }

    class_cores <- check_class_integer(cores)
    if (class_cores == F) {
        rlang::abort(
            stringr::str_glue("{if (!is.null(func)) paste0(func, ': ') }Argument 'cores' should be a single integer value")
        )
    }

    list(
        allow_par = allow_par,
        cores     = cores,
        verbose   = verbose,
        packages  = packages
    )

}



pretty_print_list <- function(x, header=NULL, justify="left", sep=":") {

    if (!is.list(x) || is.null(names(x)))
        stop("x must be a list containing named objects")
    if (!is.null(header) && (!is.character(header) || length(header) > 1))
        stop("header must be a single character string")
    if (!is.character(justify) || length(justify) > 1)
        stop("justify must be a single character string")
    if (!is.character(sep) || length(sep) > 1)
        stop("sep must be a single character string")

    justify <- match.arg(justify, c("none","left","right","decimal"))

    if (!is.null(header))
        cat(header,"\n", rep("-",nchar(header)),"\n",sep="")

    # prune list of NULL values.
    # if x <- list("some really large name"=NULL, cat="dog")
    # the spearator will be spaced way too far to the right
    # due to the influence of the first entry name. thus,
    # we eliminate any such NULL entries altogether to
    # avoid this problem
    x <- x[!unlist(lapply(x, is.null))]

    if (!length(x))
        return(invisible(NULL))

    categories <- format(names(x), justify=justify)

    # Cat Print
    for (i in seq(along=categories)){
        if (!is.null(x[[i]]))
            cat(categories[i], sep, x[[i]], "\n", sep=" ")
    }

    invisible(NULL)
}
