context("WORKFLOWSETS")

library(tidymodels)
library(workflowsets)
library(dplyr)
library(timetk)




test_that("Workflowsets Tests", {

    skip_on_cran()

    # Sequential - workflowset is correct order ----

    data_set <- timetk::m4_monthly

    # SETUP WORKFLOWSETS

    rec1 <- recipes::recipe(value ~ date + id, data_set) %>%
        recipes::step_mutate(date_num = as.numeric(date)) %>%
        recipes::step_mutate(month_lbl = lubridate::month(date, label = TRUE)) %>%
        recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)

    rec2 <- recipes::recipe(value ~ date + id, data_set) %>%
        recipes::step_mutate(date_num = as.numeric(date)) %>%
        recipes::step_mutate(month_lbl = lubridate::month(date, label = TRUE)) %>%
        recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE) %>%
        step_ts_clean(value)

    mod_spec_prophet <- prophet_reg() %>%
        parsnip::set_engine("prophet")

    mod_spec_ets <- exp_smoothing() %>%
        parsnip::set_engine("ets")

    wfsets <- workflowsets::workflow_set(
        preproc = list(rec1 = rec1, rec2 = rec2),
        models  = list(
            prophet = mod_spec_prophet,
            ets = mod_spec_ets
        ),
        cross   = TRUE
    ) %>%
        mutate(.model_id = dplyr::row_number()) # Generate ID for linking to the fitted modeltime tabe



    model_tbl <- wfsets %>% modeltime_fit_workflowset(
        data_set,
        control = control_fit_workflowset(allow_par = FALSE)
    )

    # MODEL 1
    model_id <- 1

    # Check preprocessor
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_preprocessor(),
            model_tbl$.model[[model_id]] %>% extract_preprocessor()
        )
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_spec_parsnip(),
            model_tbl$.model[[model_id]] %>% extract_spec_parsnip()
        )
    )

    # MODEL 1
    model_id <- 2

    # Check preprocessor
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_preprocessor(),
            model_tbl$.model[[model_id]] %>% extract_preprocessor()
        )
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_spec_parsnip(),
            model_tbl$.model[[model_id]] %>% extract_spec_parsnip()
        )
    )

    # MODEL 1
    model_id <- 3

    # Check preprocessor
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_preprocessor(),
            model_tbl$.model[[model_id]] %>% extract_preprocessor()
        )
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_spec_parsnip(),
            model_tbl$.model[[model_id]] %>% extract_spec_parsnip()
        )
    )

    # MODEL 1
    model_id <- 4

    # Check preprocessor
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_preprocessor(),
            model_tbl$.model[[model_id]] %>% extract_preprocessor()
        )
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_spec_parsnip(),
            model_tbl$.model[[model_id]] %>% extract_spec_parsnip()
        )
    )


    # "Parallel - workflowset is correct order" ----
    # Note: this call fails if no previous versions of modeltime exist.
    model_par_tbl <- wfsets %>% modeltime_fit_workflowset(
        data_set,
        control = control_fit_workflowset(allow_par = TRUE, cores = 2)
    )

    # Note that environments will differ
    # waldo::compare(
    #     wfsets$info[[model_id]]$workflow[[1]] %>% extract_preprocessor(),
    #     model_par_tbl$.model[[model_id]] %>% extract_preprocessor()
    # )

    # MODEL 1
    model_id <- 1

    # Check preprocessor
    expect_equal(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_preprocessor(),
            model_par_tbl$.model[[model_id]] %>% extract_preprocessor()
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_spec_parsnip(),
            model_par_tbl$.model[[model_id]] %>% extract_spec_parsnip()
        )
    )

    # MODEL 1
    model_id <- 2

    # Check preprocessor
    expect_equal(
        wfsets$info[[model_id]]$workflow[[1]] %>% extract_preprocessor(),
        model_par_tbl$.model[[model_id]] %>% extract_preprocessor()
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_spec_parsnip(),
            model_par_tbl$.model[[model_id]] %>% extract_spec_parsnip()
        )
    )

    # MODEL 1
    model_id <- 3

    # Check preprocessor
    expect_equal(
        wfsets$info[[model_id]]$workflow[[1]] %>% extract_preprocessor(),
        model_par_tbl$.model[[model_id]] %>% extract_preprocessor()
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_spec_parsnip(),
            model_par_tbl$.model[[model_id]] %>% extract_spec_parsnip()
        )
    )

    # MODEL 1
    model_id <- 4

    # Check preprocessor
    expect_equal(
        wfsets$info[[model_id]]$workflow[[1]] %>% extract_preprocessor(),
        model_par_tbl$.model[[model_id]] %>% extract_preprocessor()
    )

    # Check model spec
    expect_true(
        identical(
            wfsets$info[[model_id]]$workflow[[1]] %>% extract_spec_parsnip(),
            model_par_tbl$.model[[model_id]] %>% extract_spec_parsnip()
        )
    )

})





