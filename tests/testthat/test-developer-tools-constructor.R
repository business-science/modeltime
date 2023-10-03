context("TEST DEVELOPER TOOLS - CONSTRUCTORS")


# NEW MODELTIME BRIDGE ----

# Tests ----

test_that("modeltime bridge: Good Structure", {

    skip_on_cran()

    #

    lm_model <- lm(value ~ as.numeric(date) + hour(date) + wday(date, label = TRUE),
                   data = timetk::taylor_30_min)

    data = tibble(
        date        = timetk::taylor_30_min$date, # Important - The column name must match the modeled data
        # These are standardized names: .value, .fitted, .resid
        .actual     = timetk::taylor_30_min$value,
        .fitted     = lm_model$fitted.values %>% as.numeric(),
        .residuals  = lm_model$residuals %>% as.numeric()
    )

    bridge <- new_modeltime_bridge(
        class  = "lm_time_series_impl",
        models = list(model_1 = lm_model),
        data   = data,
        extras = NULL,
        desc   = NULL
    )


    # modeltime bridge: Good Structure

    expect_s3_class(bridge, "lm_time_series_impl")

    expect_s3_class(bridge$models$model_1, "lm")


    # modeltime bridge: Bad Structures

    # Class missing
    expect_error({
        new_modeltime_bridge(

            models = list(model_1 = lm_model),
            data   = data,
            extras = NULL
        )
    })

    # Class not a number
    expect_error({
        new_modeltime_bridge(
            class  = 12,
            models = list(model_1 = lm_model),
            data   = data,
            extras = NULL
        )
    })

    # models missing
    expect_error({
        new_modeltime_bridge(
            class  = "lm_time_series_impl",

            data   = data,
            extras = NULL
        )
    })

    # Bad model names
    expect_error({
        new_modeltime_bridge(
            class  = "lm_time_series_impl",
            models = list("lm" = lm_model),
            data   = data,
            extras = NULL
        )
    })

    # Missing data
    expect_error({
        new_modeltime_bridge(
            class  = "lm_time_series_impl",
            models = list("model_1" = lm_model),

            extras = NULL
        )
    })

    # data not formatted correctly
    expect_error({
        new_modeltime_bridge(
            class  = "lm_time_series_impl",
            models = list("model_1" = lm_model),
            data   = tibble(target = "black"),
            extras = NULL
        )
    })

    # data: 4 columns but bad column name detected
    expect_error({
        new_modeltime_bridge(
            class  = "lm_time_series_impl",
            models = list("model_1" = lm_model),
            data   = tibble(
                date = 1,
                .value = 2,
                .resid = 3,
                bad_column_name = 4
            ),
            extras = NULL
        )
    })

    # extras not a list
    expect_error({
        new_modeltime_bridge(
            class  = "lm_time_series_impl",
            models = list("model_1" = lm_model),
            data   = data,
            extras = 1
        )
    })

    # desc not a character
    expect_error({
        new_modeltime_bridge(
            class  = "lm_time_series_impl",
            models = list("model_1" = lm_model),
            data   = data,
            extras = NULL,
            desc   = 1
        )
    })

    # desc not length 1
    expect_error({
        new_modeltime_bridge(
            class  = "lm_time_series_impl",
            models = list("model_1" = lm_model),
            data   = data,
            extras = NULL,
            desc   = c("a", "b")
        )
    })


})


