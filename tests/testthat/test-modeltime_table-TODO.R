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
    non_seasonal_ar = 0,
    non_seasonal_differences = 1,
    non_seasonal_ma = 1,
    seasonal_ar = 1,
    seasonal_differences = 1,
    seasonal_ma = 1,
    learn_rate = 0.01
) %>%
    set_engine(engine = "arima_xgboost") %>%
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

model_fit_lm <- linear_reg() %>%
    set_engine("lm") %>%
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

# MARS (Parsnip Model) ----

model_fit_mars <- mars(mode = "regression") %>%
    set_engine("earth") %>%
    fit(log(value) ~ as.numeric(date) + month(date, label = TRUE),
        data = training(splits))

model_fit_mars %>%
    predict(new_data = testing(splits))

model_fit_mars %>%
    modeltime_accuracy(new_data = testing(splits))

# MARS workflow -----
library(earth)
model_spec <- mars(mode = "regression") %>%
    set_engine("earth")

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_date(date, features = "month") %>%
    step_log(value)

wflw_fit_mars <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec) %>%
    fit(training(splits))

wflw_fit_mars %>% modeltime_accuracy(testing(splits))


# Compare ----
model_table <- modeltime_table(
    model_fit_no_boost,
    model_fit_boosted,
    wflw_fit_arima,
    model_fit_lm,
    wflw_fit_lm,
    model_fit_mars,
    wflw_fit_mars
)

model_table

model_table %>%
    modeltime_accuracy(new_data = testing(splits))

model_forecast <- model_table %>%
    modeltime_forecast(new_data = testing(splits),
                       actual_data = bind_rows(training(splits), testing(splits)))

# g <- model_forecast %>%
#     mutate(.model_desc = ifelse(!is.na(.model_id), str_c(.model_id, "_", .model_desc), .model_desc)) %>%
#     mutate(.model_desc = as_factor(.model_desc)) %>%
#     ggplot(aes(.index, .value, color = .model_desc)) +
#     geom_line() +
#     tidyquant::scale_color_tq() +
#     tidyquant::theme_tq()
#
# plotly::ggplotly(g)
