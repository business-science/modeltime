context("RECURSIVE MODELS")

FORECAST_HORIZON <- 24

m750_extended <- m750 %>%
  group_by(id) %>%
  future_frame(
    .length_out = FORECAST_HORIZON,
    .bind_data  = TRUE
  ) %>%
  ungroup()

# SINGLE / RECIPE / PARSNIP ----

test_that("recursive  - single / recipe / parsnip", {

  recipe_spec <- recipe(value ~ ., data = training(m750_splits)) %>%
    step_lag(all_outcomes(), lag = 1:12)

  # Lag Recipe
  recipe_lag <- recipe(value ~ date, m750_extended) %>%
    step_lag(value, lag = 1:FORECAST_HORIZON)

  # Data Preparation
  m750_lagged <- recipe_lag %>% prep() %>% juice()

  train_data <- m750_lagged %>%
    filter(!is.na(value)) %>%
    drop_na()

  future_data <- m750_lagged %>%
    filter(is.na(value))

  # * Recursive Modeling ----
  model_fit_lm <- linear_reg() %>%
    set_engine("lm") %>%
    fit(value ~ date, data = train_data)

  model_fit_lm_recursive <- linear_reg() %>%
    set_engine("lm") %>%
    fit(value ~ ., data = train_data) %>%
    recursive(
      transform  = recipe_lag,
      train_tail = tail(train_data, FORECAST_HORIZON)
    )

  expect_s3_class(model_fit_lm_recursive, "recursive")

  # * Modeltime Forecast  ----
  forecast_tbl <- modeltime_table(
    model_fit_lm,
    model_fit_lm_recursive
  ) %>%
    modeltime_forecast(
      new_data    = future_data,
      actual_data = m750,
      keep_data   = TRUE
    )

  # Visualize
  # forecast_tbl %>% plot_modeltime_forecast()

  preds <- forecast_tbl %>% filter(.model_id == 2) %>% pull(.value)
  expect_equal(
    length(future_data$value),
    length(preds)
  )

  expect_lt(max(preds), 11500)
  expect_gt(min(preds), 9650)

  # * Modeltime Refit ----

  retrain_tbl <- m750_lagged %>% slice(1:200)
  future_tbl  <- m750_lagged %>% slice(201:224)

  refit_tbl <- modeltime_table(
    model_fit_lm_recursive
  ) %>%
    modeltime_refit(
      data = retrain_tbl
    )

  forecast_refit_tbl <- refit_tbl %>%
    modeltime_forecast(
      new_data    = future_tbl,
      actual_data = retrain_tbl
    )

  # forecast_refit_tbl %>% plot_modeltime_forecast()

  preds <- forecast_refit_tbl %>% filter(.model_id == 1) %>% pull(.value)
  expect_equal(
    length(future_tbl$value),
    length(preds)
  )

  expect_lt(max(preds), 10600)
  expect_gt(min(preds), 8800)

})

# SINGLE / TRANSFORM FUNCTION / WORKFLOW ----

# Function run recursively that updates the forcasted dataset
lag_roll_transformer <- function(data){
  data %>%
    # Lags
    tk_augment_lags(value, .lags = 1:12) %>%
    # Rolling Features
    mutate(rolling_mean_12 = lag(slide_dbl(
      value, .f = mean, .before = 12, .complete = FALSE
    ), 1))
}

test_that("recursive - single / transform func / workflow", {

  # Data Preparation
  m750_rolling <- m750_extended %>%
    lag_roll_transformer() %>%
    select(-id)

  train_data <- m750_rolling %>%
    filter(!is.na(value)) %>%
    drop_na()

  future_data <- m750_rolling %>%
    filter(is.na(value))

  # * Recursive Modeling ----
  wflw_fit_lm <- workflow() %>%
    add_recipe(recipe(value ~ date, train_data)) %>%
    add_model(linear_reg() %>% set_engine("lm")) %>%
    fit(train_data)

  wflw_fit_lm_recursive <- workflow() %>%
    add_recipe(recipe(value ~ ., train_data)) %>%
    add_model(linear_reg() %>% set_engine("lm")) %>%
    fit(train_data) %>%
    recursive(
      transform  = lag_roll_transformer,
      train_tail = tail(train_data, FORECAST_HORIZON)
    )

  expect_s3_class(wflw_fit_lm_recursive, "recursive")

  # * Forecasting ----
  forecast_tbl <- modeltime_table(
    wflw_fit_lm,
    wflw_fit_lm_recursive
  ) %>%
    update_model_description(2, "LM - Lag Roll") %>%
    modeltime_forecast(
      new_data    = future_data,
      actual_data = m750
    )

  # forecast_tbl %>% plot_modeltime_forecast()

  preds <- forecast_tbl %>% filter(.model_id == 2) %>% pull(.value)
  expect_equal(
    length(future_data$value),
    length(preds)
  )

  expect_lt(max(preds), 11700)
  expect_gt(min(preds), 9650)

  # * Modeltime Refit ----

  retrain_tbl <- train_data %>% slice(1:200)
  future_tbl  <- train_data %>% slice(201:224)

  # wflw_fit_lm_recursive %>% mdl_time_refit(retrain_tbl)

  refit_tbl <- modeltime_table(
    wflw_fit_lm_recursive
  ) %>%
    modeltime_refit(
      data = retrain_tbl
    )

  forecast_refit_tbl <- refit_tbl %>%
    modeltime_forecast(
      new_data    = future_tbl,
      actual_data = retrain_tbl
    )

  # forecast_refit_tbl %>% plot_modeltime_forecast()

  preds <- forecast_refit_tbl %>% filter(.model_id == 1) %>% pull(.value)
  expect_equal(
    length(future_tbl$value),
    length(preds)
  )

  expect_lt(max(preds), 10000)
  expect_gt(min(preds), 8300)

})

# PANEL / FUNCTION / PARSNIP ----

test_that("recursive  - single / function / parsnip", {

  # Jumble the data to make sure it forecasts properly
  m4_monthly_updated <- m4_monthly %>%
    arrange(desc(id), date) %>%
    mutate(id = as_factor(as.character(id)))

  m4_extended <- m4_monthly_updated %>%
    group_by(id) %>%
    future_frame(
      .length_out = FORECAST_HORIZON,
      .bind_data  = TRUE
    ) %>%
    ungroup()

  lag_transformer <- function(data){
    data %>%
      group_by(id) %>%
      # Lags
      tk_augment_lags(value, .lags = 1:24) %>%
      ungroup()
  }

  m4_lags <- m4_extended %>%
    lag_transformer()

  train_data <- m4_lags %>%
    filter(!is.na(value)) %>%
    drop_na()

  future_data <- m4_lags %>%
    filter(is.na(value))

  # * Recursive Modeling ----

  model_fit_lm_recursive <- linear_reg() %>%
    set_engine("lm") %>%
    fit(value ~ ., data = train_data) %>%
    recursive(
      id         = "id",
      transform  = lag_transformer,
      train_tail = panel_tail(train_data, id, FORECAST_HORIZON)
    )

  wflw_fit_lm_recursive <- workflow() %>%
    add_recipe(recipe(value ~ ., train_data)) %>%
    add_model(linear_reg() %>% set_engine("lm")) %>%
    fit(train_data) %>%
    recursive(
      id         = "id",
      transform  = lag_roll_transformer,
      train_tail = panel_tail(train_data, id, FORECAST_HORIZON)
    )

  # wflw_fit_lm_recursive %>% class()
  expect_s3_class(model_fit_lm_recursive, "recursive_panel")
  expect_s3_class(wflw_fit_lm_recursive, "recursive_panel")

  # * Forecasting ----

  forecast_tbl <- modeltime_table(
    model_fit_lm_recursive
    ,
    wflw_fit_lm_recursive
  ) %>%
    modeltime_forecast(
      new_data    = future_data,
      actual_data = m4_monthly,
      keep_data   = TRUE
    )

  forecast_tbl %>%
    group_by(id) %>%
    plot_modeltime_forecast(
      .interactive = FALSE,
      .conf_interval_show = FALSE
    )

})




