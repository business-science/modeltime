context("TEST MODELTIME TABLE")

# Data
m750 <- m4_monthly %>% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)


# auto_arima ----
model_fit_no_boost <- arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(log(value) ~ date, data = training(splits))

# arima_boost ----
model_fit_boosted <- arima_boost(
    min_n = 2,
    learn_rate = 0.015
) %>%
    set_engine(engine = "auto_arima_xgboost") %>%
    fit(log(value) ~ date + as.numeric(date) + month(date, label = TRUE),
        data = training(splits))

# Workflow -----
model_spec <- arima_reg() %>%
    set_engine("auto_arima")

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_date(date, features = "month") %>%
    step_log(value)

wflw_fit_arima <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec) %>%
    fit(training(splits))


# LM (Parsnip Model) ----

model_spec <- linear_reg() %>%
    set_engine("lm")

model_fit_lm <- model_spec %>%
    fit(log(value) ~ as.numeric(date) + month(date, label = TRUE),
        data = training(splits))


# LM workflow -----

model_spec <- linear_reg() %>%
    set_engine("lm")

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_date(date, features = "month") %>%
    step_log(value)

wflw_fit_lm <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec) %>%
    fit(training(splits))


# Compare ----
model_table <- modeltime_table(
    model_fit_no_boost,
    model_fit_boosted,
    wflw_fit_arima,
    model_fit_lm,
    wflw_fit_lm
)

model_table

model_table %>%
    modeltime_accuracy(new_data = testing(splits))

model_fit_lm %>%
    modeltime_accuracy()

model_fit_lm %>%
    modeltime_accuracy(new_data = testing(splits))

model_fit_lm %>% modeltime_forecast(new_data = testing(splits), actual_data = testing(splits))
