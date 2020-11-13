test_that("Test recursive model with recipe", ~{

    library(dplyr)
    library(parsnip)
    library(recipes)

    dax_stock <-
       as_tibble(EuStockMarkets) %>%
       select(DAX) %>%
       bind_rows(tibble(DAX = rep(NA, 30))) # Adding new data

    recipe_dax_stock <-
       recipe(DAX ~ ., data = dax_stock) %>%
       step_lag(all_outcomes(), lag = 1:10) %>%
       prep()

    dax_stock_m <-
       juice(recipe_dax_stock)

    train_data <-
       dax_stock_m %>%
       filter(!is.na(DAX)) %>%
       na.omit()

    new_data <-
       dax_stock_m %>%
       filter(is.na(DAX))

    model_linear <-
       linear_reg() %>%
       set_engine("lm") %>%
       fit(DAX ~ ., data = train_data)

    recursive_linear <-
       model_linear %>%
       recursive(recipe_dax_stock,
                 train_tail = tail(train_data, 10))
    pred <-
       recursive_linear %>%
       predict(new_data)

    expect_true(nrow(pred) == nrow(new_data))
})

test_that("Test recursive model with function", ~{

    library(dplyr)
    library(parsnip)

    dax_stock <-
      as_tibble(EuStockMarkets) %>%
      select(DAX) %>%
      bind_rows(tibble(DAX = rep(NA, 30))) # Adding new data

    transform_fun <- function(data, slice_idx){
       data %>%
       mutate(moving_sum = lag(slider::slide_dbl(
           DAX, .f = mean, .before = 4L
        ), 1))
     }

    dax_stock_m <-
      dax_stock %>%
      mutate(moving_sum = lag(slider::slide_dbl(
           DAX, .f = mean, .before = 4L
       ), 1))

    train_data <-
      dax_stock_m %>%
      filter(!is.na(DAX)) %>%
      na.omit()

    new_data <-
      dax_stock_m %>%
      filter(is.na(DAX))

    model_linear <-
       linear_reg() %>%
       set_engine("lm") %>%
       fit(DAX ~ ., data = train_data)

    recursive_linear <-
       model_linear %>%
       recursive(transform_fun,
                 train_tail = tail(train_data, 10))

    pred <-
       recursive_linear %>%
       predict(new_data)

    expect_true(nrow(pred) == nrow(new_data))
})

