
# PARALLEL START/STOP ----

.modeltime_parallel <- new.env(parent = emptyenv())
.modeltime_parallel$cluster <- NULL
.modeltime_parallel$created <- FALSE

#' Start parallel clusters / plans
#'
#' @param ... Parameters passed to underlying functions (See Details Section)
#' @param .method The method to create the parallel backend. Supports:
#'
#'  - "future"   - Uses the `future` package; foreach bridged via `doFuture`
#'  - "parallel" - Uses the `parallel` + `doParallel` packages
#'  - "spark"    - Uses the `sparklyr` package
#' @param .export_vars Environment variables that can be sent to the workers (not needed for "future")
#' @param .packages Packages that can be sent to the workers (auto-handled by "future")
#'
#' @details
#' ## future (`.method = "future"`)
#' Sets a `future::multisession` plan (portable across OSes) and registers a foreach
#' backend via `doFuture::registerDoFuture()`. This avoids the `tune` foreach/future warning.
#' - Pass the first unnamed `...` argument as worker count (numeric) or omit to
#'   default to `parallelly::availableCores(logical = FALSE)` or 2 if unknown.
#'
#' ## parallel (`.method = "parallel"`)
#' 1) `parallel::makeCluster(...)`  2) `doParallel::registerDoParallel(cl)`
#' 3) Set `.libPaths()` on workers; optional `clusterExport` and package loads.
#'
#' ## spark (`.method = "spark"`)
#' Requires `sparklyr::spark_connect()`; registers foreach via `sparklyr::registerDoSpark(...)`.
#'
#' @examples
#'
#' # Starts 2 clusters
#' parallel_start(2)
#'
#' # Returns to sequential processing
#' parallel_stop()
#'
#'
#'
#' @name parallel_start
#'


#' @export
#' @rdname parallel_start
parallel_start <- function(..., .method = c("parallel", "spark", "future"),
                           .export_vars = NULL, .packages = NULL) {

    meth <- tolower(.method[1])

    if (!meth %in% c("parallel","spark","future")) {
        rlang::abort("`.method` must be one of 'future', 'parallel', or 'spark'.")
    }

    if (meth == "future") {
        if (!requireNamespace("future", quietly = TRUE)) {
            rlang::abort("The 'future' package is required for `.method = 'future'`.")
        }
        # optional foreach bridge
        has_doFuture <- requireNamespace("doFuture", quietly = TRUE)
        # choose workers: first unnamed arg if numeric; else physical cores; else 2
        dots <- list(...)
        workers <- tryCatch(
            {
                if (length(dots) >= 1 && is.numeric(dots[[1]]) && length(dots[[1]]) == 1) {
                    as.integer(dots[[1]])
                } else if (requireNamespace("parallelly", quietly = TRUE)) {
                    parallelly::availableCores(logical = FALSE)
                } else {
                    2L
                }
            },
            error = function(e) 2L
        )
        # set plan
        future::plan(future::multisession, workers = workers)
        if (has_doFuture) {
            doFuture::registerDoFuture()
        }
        # 'future' auto-exports globals and attaches needed packages from the calling env.
        # We keep .export_vars / .packages for API compatibility but they are not required here.
        return(invisible(TRUE))
    }

    if (meth == "parallel") {
        if (!requireNamespace("parallel", quietly = TRUE) ||
            !requireNamespace("doParallel", quietly = TRUE)) {
            rlang::abort("The 'parallel' and 'doParallel' packages are required for `.method = 'parallel'`.")
        }
        # If we previously created a cluster, stop it before creating a new one
        if (isTRUE(.modeltime_parallel$created) && !is.null(.modeltime_parallel$cluster)) {
            try(parallel::stopCluster(.modeltime_parallel$cluster), silent = TRUE)
            .modeltime_parallel$cluster <- NULL
            .modeltime_parallel$created <- FALSE
        }
        # Step 1: Create the cluster
        cl <- parallel::makeCluster(...)
        # Step 2: Register the cluster
        doParallel::registerDoParallel(cl)
        # Step 3: Export variables (if provided)
        if (!is.null(.export_vars)) {
            parallel::clusterExport(cl, varlist = .export_vars)
        }
        # Step 4: Load .packages (if provided)
        if (!is.null(.packages)) {
            parallel::clusterCall(cl, function(pkgs) {
                lapply(pkgs, function(pkg) {
                    if (!requireNamespace(pkg, quietly = TRUE)) {
                        stop(paste("Package", pkg, "is not installed."))
                    }
                    library(pkg, character.only = TRUE)
                })
            }, .packages)
        }
        # Step 5: Set the library paths for each worker
        invisible(parallel::clusterCall(cl, function(x) .libPaths(x), .libPaths()))

        # Track the cluster so parallel_stop() can close it
        .modeltime_parallel$cluster <- cl
        .modeltime_parallel$created <- TRUE
        return(invisible(TRUE))
    }

    if (meth == "spark") {
        if (!requireNamespace("sparklyr", quietly = TRUE)) {
            rlang::abort("The 'sparklyr' package is required for `.method = 'spark'`.")
        }
        sparklyr::registerDoSpark(...)
        if (!is.null(.export_vars) || !is.null(.packages)) {
            # Best effort: context broadcast
            spark_apply_function <- function(partition, context) {
                if (!is.null(context$packages)) {
                    lapply(context$packages, function(pkg) {
                        if (!requireNamespace(pkg, quietly = TRUE)) {
                            stop(paste("Package", pkg, "is not installed."))
                        }
                        library(pkg, character.only = TRUE)
                    })
                }
                context$export_vars
                partition
            }
            context <- list(export_vars = .export_vars, packages = .packages)
            sparklyr::spark_apply(sparklyr::spark_session, spark_apply_function, context = context)
        }
        return(invisible(TRUE))
    }
}


#' @export
#' @rdname parallel_start
parallel_stop <- function() {
    # Stop any PSOCK cluster created by parallel_start(.method = "parallel")
    if (isTRUE(.modeltime_parallel$created) && !is.null(.modeltime_parallel$cluster)) {
        if (requireNamespace("parallel", quietly = TRUE)) {
            try(parallel::stopCluster(.modeltime_parallel$cluster), silent = TRUE)
        }
        .modeltime_parallel$cluster <- NULL
        .modeltime_parallel$created <- FALSE
    }
    if (requireNamespace("future", quietly = TRUE)) {
        # best-effort reset; ignore errors if no plan was set
        try(future::plan(future::sequential), silent = TRUE)
    }
    if (requireNamespace("foreach", quietly = TRUE)) {
        foreach::registerDoSEQ()
    }
    invisible(TRUE)
}


# USED TO SET UP THE PARALLEL BACKENDS IF NOT SET UP ALREADY
setup_parallel_processing <- function(control, is_par_setup, t1) {

    clusters_made <- FALSE
    cl            <- NULL

    # already set up externally?
    if ((control$cores > 1) && control$allow_par && (!is_par_setup)) {

        # Prefer future if present (no foreach-only warning in `tune`)
        if (requireNamespace("future", quietly = TRUE)) {

            if (control$verbose) {
                message(stringr::str_glue(" Starting future multisession backend with {control$cores} workers..."))
            }

            # set plan
            future::plan(future::multisession, workers = control$cores)
            # bridge foreach -> future if available
            if (requireNamespace("doFuture", quietly = TRUE)) {
                doFuture::registerDoFuture()
            }

            # No clusters to track/close
            clusters_made <- FALSE

            if (control$verbose) {
                t <- Sys.time()
                message(stringr::str_glue(" Parallel Backend Setup (future) | {round(t - t1, 3)} seconds"))
            }

        } else {
            # Fallback to legacy parallel backend
            if (control$verbose) {
                message(stringr::str_glue(" Starting parallel backend (doParallel) with {control$cores} clusters (cores)..."))
            }
            cl <- parallel::makeCluster(control$cores)
            doParallel::registerDoParallel(cl)
            parallel::clusterCall(cl, function(x) .libPaths(x), .libPaths())
            clusters_made <- TRUE

            if (control$verbose) {
                t <- Sys.time()
                message(stringr::str_glue(" Parallel Backend Setup | {round(t - t1, 3)} seconds"))
            }
        }

    } else if (!is_par_setup) {
        # sequential
        if (control$verbose) message("Running sequential backend. If parallel was intended, set `allow_par = TRUE` and `cores > 1`.")
        if (requireNamespace("foreach", quietly = TRUE)) foreach::registerDoSEQ()
    } else {
        if (control$verbose) message(stringr::str_glue("Using existing parallel backend with {foreach::getDoParWorkers()} workers..."))
    }

    list(
        clusters_made = clusters_made,
        cl            = cl
    )
}


# USED TO SHUT DOWN THE PARALLEL BACKENDS IF WE SET UP
finish_parallel_processing <- function(control, clusters_made, cl, t1) {

    t <- Sys.time()

    if (clusters_made) {
        # We set up parallel processing internally. We should close.
        doParallel::stopImplicitCluster()
        parallel::stopCluster(cl)
        foreach::registerDoSEQ()
        if (control$verbose) {
            message(stringr::str_glue(" Finishing parallel backend. Closing clusters. | {round(t-t1, 3)} seconds)"))
        }
    } else if ((control$cores > 1) && control$allow_par) {
        if (control$verbose) {
            message(stringr::str_glue(" Finishing parallel backend. Clusters are remaining open. | {round(t-t1, 3)} seconds"))
            message(" Close clusters by running: `parallel_stop()`.")
        }
    } else {
        if (control$verbose) {
            message(stringr::str_glue(" Finishing sequential backend. | {round(t-t1, 3)} seconds"))
        }
    }

}

# USED TO SELECT EITHER SEQ OR PAR FOREACH OPERATOR
get_operator <- function(allow_par = TRUE) {
    is_par <- foreach::getDoParWorkers() > 1

    cond <- allow_par && is_par
    if (cond) {
        if (requireNamespace("doRNG", quietly = TRUE)) {
            res <- doRNG::`%dorng%`
        } else {
            res <- foreach::`%dopar%`
        }
    } else {
        res <- foreach::`%do%`
    }
    return(res)
}


# CONTROL REFIT ----


#' Control aspects of the training process
#'
#' @description These functions are matched to the associated
#' training functions:
#'
#' - `control_refit()`: Used with [modeltime_refit()]
#' - `control_fit_workflowset()`: Used with [modeltime_fit_workflowset()]
#' - `control_nested_fit()`: Used with [modeltime_nested_fit()]
#' - `control_nested_refit()`: Used with [modeltime_nested_refit()]
#' - `control_nested_forecast()`: Used with [modeltime_nested_forecast()]
#'
#' @param allow_par Logical to allow parallel computation. Default: `FALSE` (single threaded).
#' @param cores Number of cores for computation. If -1, uses all available physical cores.
#'  Default: `1`.
#' @param packages An optional character string of additional R package names that should be loaded
#'  during parallel processing.
#'
#'  - Packages in your namespace are loaded by default
#'
#'  - Key Packages are loaded by default: `tidymodels`, `parsnip`, `modeltime`, `dplyr`, `stats`, `lubridate` and `timetk`.
#'
#' @param verbose Logical to control printing.
#'
#' @return
#' A List with the control settings.
#'
#'
#' @seealso
#' - Setting Up Parallel Processing: [parallel_start()], [parallel_stop())]
#' - Training Functions: [modeltime_refit()], [modeltime_fit_workflowset()], [modeltime_nested_fit()], [modeltime_nested_refit()]
#'
#' @examples
#'
#' # No parallel processing by default
#' control_refit()
#'
#' # Allow parallel processing and use all cores
#' control_refit(allow_par = TRUE, cores = -1)
#'
#' # Set verbosity to show additional training information
#' control_refit(verbose = TRUE)
#'
#' # Add additional packages used during modeling in parallel processing
#' # - This is useful if your namespace does not load all needed packages
#' #   to run models.
#' # - An example is if I use `temporal_hierarchy()`, which depends on the `thief` package
#' control_refit(allow_par = TRUE, packages = "thief")
#'
#' @name control_modeltime



#' @export
#' @rdname control_modeltime
control_refit <- function(verbose = FALSE,
                          allow_par = FALSE,
                          cores = 1,
                          packages = NULL) {

    ret <- control_modeltime_objects(
        verbose   = verbose,
        allow_par = allow_par,
        cores     = cores,
        packages  = packages,
        func      = "control_refit"
    )

    class(ret) <- c("control_refit")

    return(ret)
}

#' @export
print.control_refit <- function(x, ...) {
    pretty_print_list(x, header = "refit control object")
    invisible(x)
}

# CONTROL  WORKFLOWSET -----

#' @export
#' @rdname control_modeltime
control_fit_workflowset <- function(verbose = FALSE,
                                    allow_par = FALSE,
                                    cores = 1,
                                    packages = NULL) {

    ret <- control_modeltime_objects(
        verbose   = verbose,
        allow_par = allow_par,
        cores     = cores,
        packages  = packages,
        func      = "control_fit_workflowset"
    )

    class(ret) <- c("control_fit_workflowset")

    return(ret)

}

#' @export
print.control_fit_workflowset <- function(x, ...) {
    pretty_print_list(x, header = "workflowset control object")
    invisible(x)
}

# CONTROL NESTED FIT ----


#' @export
#' @rdname control_modeltime
control_nested_fit <- function(verbose = FALSE,
                               allow_par = FALSE,
                               cores = 1,
                               packages = NULL) {

    ret <- control_modeltime_objects(
        verbose   = verbose,
        allow_par = allow_par,
        cores     = cores,
        packages  = packages,
        func      = "control_nested_fit"
    )

    class(ret) <- c("control_nested_fit")

    return(ret)
}



#' @export
print.control_nested_fit <- function(x, ...) {
    pretty_print_list(x, header = "nested fit control object")
    invisible(x)
}

# CONTROL NESTED REFIT ----


#' @export
#' @rdname control_modeltime
control_nested_refit <- function(verbose = FALSE,
                                 allow_par = FALSE,
                                 cores = 1,
                                 packages = NULL) {

    ret <- control_modeltime_objects(
        verbose   = verbose,
        allow_par = allow_par,
        cores     = cores,
        packages  = packages,
        func      = "control_nested_refit"
    )

    class(ret) <- c("control_nested_refit")

    return(ret)
}



#' @export
print.control_nested_refit <- function(x, ...) {
    pretty_print_list(x, header = "nested refit control object")
    invisible(x)
}

# CONTROL NESTED FORECAST ----


#' @export
#' @rdname control_modeltime
control_nested_forecast <- function(verbose = FALSE,
                                 allow_par = FALSE,
                                 cores = 1,
                                 packages = NULL) {

    ret <- control_modeltime_objects(
        verbose   = verbose,
        allow_par = allow_par,
        cores     = cores,
        packages  = packages,
        func      = "control_nested_forecast"
    )

    class(ret) <- c("control_nested_forecast")

    return(ret)
}



#' @export
print.control_nested_refit <- function(x, ...) {
    pretty_print_list(x, header = "nested refit control object")
    invisible(x)
}

# CONTROL (generic) ----


control_modeltime_objects <- function(
    verbose = FALSE,
    allow_par = FALSE,
    cores = 1,
    packages = NULL,
    func = NULL
) {

    val_class_and_single(verbose, "logical", "control_refit()")
    val_class_and_single(allow_par, "logical", "control_refit()")
    val_class_and_single(cores, "numeric", "control_refit()")

    if (allow_par) {
        required_pkgs <- c("modeltime", "parsnip", "workflows", "dplyr", "stats",
                           "lubridate", "tidymodels", "timetk",
                           "rsample", "recipes", "yardstick", "dials", "tune")

        namespace_pkgs <- search() %>%
            stringr::str_subset(pattern = "^package") %>%
            stringr::str_remove("package:")

        packages <- c(required_pkgs, namespace_pkgs, packages) %>% unique()

        load_namespace(packages, full_load = packages)

    }

    if (!allow_par) {
        cores <- 1
    } else {
        cores_available <- parallelly::availableCores(logical = FALSE) # Detect Physical Cores

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
    if (!class_cores) {
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






# UTILITIES ----


val_class_and_single <- function (x, cls = "numeric", where = NULL) {
    cl <- match.call()
    fine <- check_class_and_single(x, cls)
    cls <- paste(cls, collapse = " or ")
    if (!fine) {
        msg <- glue::glue("Argument '{deparse(cl$x)}' should be a single {cls} value")
        if (!is.null(where)) {
            msg <- glue::glue(msg, " in `{where}`")
        }
        rlang::abort(msg)
    }
    invisible(NULL)
}


check_class_and_single <- function (x, cls = "numeric") {
    isTRUE(inherits(x, cls) & length(x) == 1)
}


check_class_integer <- function(x){
    if (x %% 1 == 0) TRUE else FALSE
}


#' These are not intended for use by the general public.
#'
#' @param x A vector
#' @param full_load A vector
#'
#' @return
#' Control information
#'
#' @keywords internal
#' @export
load_namespace <- function(x, full_load) {
    if (length(x) == 0) {
        return(invisible(TRUE))
    }

    x_full <- x[x %in% full_load]
    x <- x[!(x %in% full_load)]

    loaded <- purrr::map_lgl(x, isNamespaceLoaded)
    x <- x[!loaded]

    if (length(x) > 0) {
        did_load <- purrr::map_lgl(x, requireNamespace, quietly = TRUE)
        if (any(!did_load)) {
            bad <- x[!did_load]
            msg <- paste0("'", bad, "'", collapse = ", ")
            stop(paste("These packages could not be loaded:", msg), call. = FALSE)
        }
    }

    if (length(x_full) > 0) {
        purrr::map(x_full,
                   ~ try(suppressPackageStartupMessages(attachNamespace(.x)), silent = TRUE))
    }

    invisible(TRUE)
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
