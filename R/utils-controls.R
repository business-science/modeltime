

control_modeltime_objects <- function(
    verbose = FALSE,
    allow_par = FALSE,
    cores = -1,
    packages = NULL
) {

    required_pkgs <- c("modeltime", "parsnip", "dplyr", "stats",
                       "lubridate", "tidymodels", "timetk")

    namespace_pkgs <- search() %>%
        stringr::str_subset(pattern = "^package") %>%
        stringr::str_remove("package:")

    packages <- c(required_pkgs, namespace_pkgs, packages) %>% unique()

    load_namespace(packages, full_load = packages)


    val_class_and_single(verbose, "logical", "control_refit()")
    val_class_and_single(allow_par, "logical", "control_refit()")
    val_class_and_single(cores, "numeric", "control_refit()")

    if (!allow_par) cores <- 1
    class_cores <- check_class_integer(cores)

    cores_available <- parallel::detectCores(logical = FALSE) # Detect Physical Cores
    if (cores < 1) cores <- cores_available
    if (cores > cores_available) cores <- cores_available

    if (class_cores == F) {rlang::abort("Argument 'cores' should be a single integer value in `control_refit()`")}

    res <- list(allow_par = allow_par,
                cores = cores,
                verbose = verbose,
                packages = packages)

}
