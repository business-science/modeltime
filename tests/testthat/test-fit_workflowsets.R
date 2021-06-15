library(testthat)
library(tidymodels)
library(modeltime)
library(workflowsets)
library(tidyverse)
library(timetk)

context("TEST MODELTIME FIT WORKFLOWSETS")


data_set <- m4_monthly

# SETUP WORKFLOWSETS

rec1 <- recipe(value ~ date + id, data_set) %>%
    step_mutate(date_num = as.numeric(date)) %>%
    step_mutate(month_lbl = lubridate::month(date, label = TRUE)) %>%
    step_dummy(all_nominal(), one_hot = TRUE)

rec2 <- recipe(value ~ date + id, data_set) %>%
    step_mutate(date_num = as.numeric(date)) %>%
    step_mutate(month_lbl = lubridate::month(date, label = TRUE)) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_ts_clean(value)

mod_spec_prophet <- prophet_reg() %>%
    set_engine("prophet")

mod_spec_ets <- exp_smoothing() %>%
    set_engine("ets")

wfsets <- workflowsets::workflow_set(
    preproc = list(rec1 = rec1, rec2 = rec2),
    models  = list(
          prophet = mod_spec_prophet,
          ets = mod_spec_ets
        ),
    cross   = TRUE
) %>%
    mutate(.model_id = row_number()) # Generate ID for linking to the fitted modeltime tabe



# SEQUENTIAL ----
test_that("Sequential - workflowset is correct order", {

    model_tbl <- wfsets %>% modeltime_fit_workflowset(
        data_set,
        control = control_fit_workflowset(allow_par = FALSE)
    )

    # MODEL 1
    model_id <- 1

    # Check preprocessor
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_preprocessor(),
            model_tbl$.model[[model_id]] %>% pull_workflow_preprocessor()
        )
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_spec(),
            model_tbl$.model[[model_id]] %>% pull_workflow_spec()
        )
    )

    # MODEL 1
    model_id <- 2

    # Check preprocessor
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_preprocessor(),
            model_tbl$.model[[model_id]] %>% pull_workflow_preprocessor()
        )
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_spec(),
            model_tbl$.model[[model_id]] %>% pull_workflow_spec()
        )
    )

    # MODEL 1
    model_id <- 3

    # Check preprocessor
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_preprocessor(),
            model_tbl$.model[[model_id]] %>% pull_workflow_preprocessor()
        )
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_spec(),
            model_tbl$.model[[model_id]] %>% pull_workflow_spec()
        )
    )

    # MODEL 1
    model_id <- 4

    # Check preprocessor
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_preprocessor(),
            model_tbl$.model[[model_id]] %>% pull_workflow_preprocessor()
        )
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_spec(),
            model_tbl$.model[[model_id]] %>% pull_workflow_spec()
        )
    )

})

# PARALLEL ----
test_that("Parallel - workflowset is correct order", {

    skip_on_cran()

    model_par_tbl <- wfsets %>% modeltime_fit_workflowset(
        data_set,
        control = control_fit_workflowset(allow_par = TRUE, cores = 2)
    )

    # Note that environments will differ
    # waldo::compare(
    #     wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_preprocessor(),
    #     model_par_tbl$.model[[model_id]] %>% pull_workflow_preprocessor()
    # )

    # MODEL 1
    model_id <- 1

    # Check preprocessor
    expect_equal(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_preprocessor(),
            model_par_tbl$.model[[model_id]] %>% pull_workflow_preprocessor()
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_spec(),
            model_par_tbl$.model[[model_id]] %>% pull_workflow_spec()
        )
    )

    # MODEL 1
    model_id <- 2

    # Check preprocessor
    expect_equal(
        wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_preprocessor(),
        model_par_tbl$.model[[model_id]] %>% pull_workflow_preprocessor()
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_spec(),
            model_par_tbl$.model[[model_id]] %>% pull_workflow_spec()
        )
    )

    # MODEL 1
    model_id <- 3

    # Check preprocessor
    expect_equal(
        wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_preprocessor(),
        model_par_tbl$.model[[model_id]] %>% pull_workflow_preprocessor()
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_spec(),
            model_par_tbl$.model[[model_id]] %>% pull_workflow_spec()
        )
    )

    # MODEL 1
    model_id <- 4

    # Check preprocessor
    expect_equal(
        wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_preprocessor(),
        model_par_tbl$.model[[model_id]] %>% pull_workflow_preprocessor()
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% pull_workflow_spec(),
            model_par_tbl$.model[[model_id]] %>% pull_workflow_spec()
        )
    )

})





