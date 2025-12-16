
test_that("parallel_start()/parallel_stop() clean up clusters", {

    skip_if_not_installed("doParallel")
    skip_if_not_installed("foreach")

    # ensure clean start
    parallel_stop()

    parallel_start(2, .method = "parallel")
    expect_true(foreach::getDoParWorkers() >= 2)
    expect_true(isTRUE(modeltime:::.modeltime_parallel$created))
    expect_false(is.null(modeltime:::.modeltime_parallel$cluster))

    parallel_stop()
    expect_equal(foreach::getDoParWorkers(), 1)
    expect_false(isTRUE(modeltime:::.modeltime_parallel$created))
    expect_true(is.null(modeltime:::.modeltime_parallel$cluster))

    # idempotent
    expect_silent(parallel_stop())
})

