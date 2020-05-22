# FORECAST PLOTS -----
context("TEST MODELTIME ACCURACY")

# SETUP ----

# Data
m750   <- m4_monthly %>% filter(id == "M750")
splits <- initial_time_split(m750, prop = 0.8)

# Model Spec
model_spec <- arima_reg(period = 12) %>%
    set_engine("auto_arima")


# PARSNIP INTERFACE ----

model_fit <- model_spec %>%
    fit(log(value) ~ date, data = training(splits))

accuracy_tbl <- model_fit %>%
    modeltime_accuracy(new_data = testing(splits))



# WORKFLOW INTERFACE ----

# Model Spec
model_spec <- arima_reg(period = 12) %>%
    set_engine("auto_arima")

# Recipe spec
recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_log(value, skip = FALSE)

# Workflow
wflw <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec)

wflw_fit <- wflw %>%
    fit(training(splits))

