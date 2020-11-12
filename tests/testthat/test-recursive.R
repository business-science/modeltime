test_that("Test recursive model", ~{

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
