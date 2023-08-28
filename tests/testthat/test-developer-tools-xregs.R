context("TEST DEVELOPER TOOLS - XREG TOOLS")

# CREATE XREG RECIPE ----

# test_that("create_xreg_recipe: recipe with no compliant features returns a warning and NULL value", {
#
#     # Recipe returns no features
#     # - id is removed because of zero variance
#     # - date is removed
#     expect_warning({
#         null_recipe <- m4_monthly %>%
#             filter(id == "M750") %>%
#             select(-value) %>%
#             create_xreg_recipe(prepare = TRUE, one_hot = TRUE)
#
#     })
#
#     # Return is NULL
#     expect_null(null_recipe)
#
#
# })

test_that("create_xreg_recipe: dummy_encode = FALSE returns factors", {

    testthat::skip_on_cran()

    # Month
    predictors <- m4_monthly %>%
        filter(id == "M750") %>%
        select(-value) %>%
        mutate(`month lbl` = month(date, label = TRUE))

    xreg_recipe_spec <- create_xreg_recipe(predictors, prepare = TRUE, dummy_encode = FALSE)

    juiced <- juice_xreg_recipe(xreg_recipe_spec)

    expect_equal(ncol(juiced), 1)

    expect_s3_class(pull(juiced), "factor")


})

test_that("create_xreg_recipe: dummy_encode = TRUE returns dummies", {

    testthat::skip_on_cran()

    # Month
    predictors <- m4_monthly %>%
        filter(id == "M750") %>%
        select(-value) %>%
        mutate(`month lbl` = month(date, label = TRUE))

    xreg_recipe_spec <- create_xreg_recipe(predictors, prepare = TRUE, dummy_encode = TRUE)

    juiced <- juice_xreg_recipe(xreg_recipe_spec)

    expect_equal(ncol(juiced), 11)

    expect_true(all(juiced %>% map(is.numeric) %>% unlist()))
})
