context("RECURSIVE MODELS")

test_that("Test recursive model with recipe", {

    dax_stock <- as_tibble(EuStockMarkets) %>%
       select(DAX) %>%
       bind_rows(tibble(DAX = rep(NA, 30))) # Adding new data

    recipe_dax_stock <- recipe(DAX ~ ., data = dax_stock) %>%
       step_lag(all_outcomes(), lag = 1:10)

    dax_stock_m <- juice(prep(recipe_dax_stock))

    train_data <- dax_stock_m %>%
       filter(!is.na(DAX)) %>%
       na.omit()

    new_data <- dax_stock_m %>%
       filter(is.na(DAX))

    # model_fit

    model_linear <- linear_reg() %>%
       set_engine("lm") %>%
       fit(DAX ~ ., data = train_data)

    recursive_linear <- model_linear %>%
       recursive(recipe_dax_stock,
                 train_tail = tail(train_data, 10))

    pred <- recursive_linear %>%
       predict(new_data)

    expect_true(nrow(pred) == nrow(new_data))
    expect_true(all(is.double(pred$.pred)))

    # Workflow

    wflw_fit_linear_recursive <- workflow() %>%
      add_model(linear_reg() %>% set_engine("lm")) %>%
      add_recipe(recipe(DAX ~ ., data = train_data)) %>%
      fit(train_data) %>%
      recursive(recipe_dax_stock,
                train_tail = tail(train_data, 10))

    pred <- wflw_fit_linear_recursive %>% predict(new_data)

    expect_true(nrow(pred) == nrow(new_data))
    expect_true(all(is.double(pred$.pred)))
})

test_that("Test recursive model with function", {


    dax_stock <- as_tibble(EuStockMarkets) %>%
      select(DAX) %>%
      bind_rows(tibble(DAX = rep(NA, 30))) # Adding new data

    transform_fun <- function(data) {
       data %>%
         mutate(moving_sum = lag(slider::slide_dbl(
             DAX, .f = mean, .before = 4L
          ), 1))
     }

    dax_stock_m <- dax_stock %>%
      transform_fun()

    train_data <- dax_stock_m %>%
      filter(!is.na(DAX)) %>%
      na.omit()

    new_data <- dax_stock_m %>%
      filter(is.na(DAX))

    # model_fit

    model_linear <- linear_reg() %>%
      set_engine("lm") %>%
      fit(DAX ~ ., data = train_data)

    recursive_linear <- model_linear %>%
      recursive(transform_fun,
                train_tail = tail(train_data, 10))

    pred <- recursive_linear %>%
      predict(new_data)

    expect_true(nrow(pred) == nrow(new_data))
    expect_true(all(is.double(pred$.pred)))

    # workflow

    wflw_fit_linear <- workflow() %>%
      add_model(linear_reg() %>% set_engine("lm")) %>%
      add_recipe(recipe(DAX ~ ., data = train_data)) %>%
      fit(train_data)

    recursive_wflw_fit_linear <- wflw_fit_linear %>%
       recursive(transform_fun,
                 train_tail = tail(train_data, 10))

    pred <- recursive_wflw_fit_linear %>%
      predict(new_data)

    expect_true(nrow(pred) == nrow(new_data))
    expect_true(all(is.double(pred$.pred)))
})

