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
    seasonal_ma = 1
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

wflw_fit_mars %>% modeltime_forecast(testing(splits))


# SVM (Parsnip Model) ----

model_fit_svm <- svm_rbf(mode = "regression") %>%
    set_engine("kernlab") %>%
    fit(log(value) ~ as.numeric(date) + month(date, label = TRUE),
        data = training(splits))

model_fit_svm %>%
    predict(new_data = testing(splits))

model_fit_svm %>%
    modeltime_accuracy(new_data = testing(splits))

model_fit_svm %>%
    modeltime_forecast(new_data = testing(splits))

# SVM workflow -----
model_spec <- svm_rbf(mode = "regression") %>%
    set_engine("kernlab")

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_date(date, features = "month") %>%
    step_rm(date) %>%
    # SVM requires dummy variables
    step_dummy(all_nominal()) %>%
    step_log(value)

wflw_fit_svm <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec) %>%
    fit(training(splits))

wflw_fit_svm %>% modeltime_accuracy(testing(splits))


# GLMNET (parsnip) ----

# Error if penalty value is not included
model_fit_glmnet <- linear_reg(
    penalty = 0.000388
    ) %>%
    set_engine("glmnet") %>%
    fit(log(value) ~ as.numeric(date) + month(date, label = TRUE),
        data = training(splits))

model_fit_glmnet %>%
    modeltime_accuracy(new_data = testing(splits))


# GLMNET (workflow) ----

model_spec <- linear_reg(penalty = 0.000388) %>%
    set_engine("glmnet")

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_date(date, features = "month") %>%
    step_mutate(date_num = as.numeric(date)) %>%
    step_rm(date) %>%
    step_dummy(all_nominal()) %>%
    step_log(value)

wflw_fit_glmnet <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec) %>%
    fit(training(splits))

wflw_fit_glmnet %>% modeltime_accuracy(testing(splits))

# randomForest (parsnip) ----

# Error if penalty value is not included
model_fit_randomForest <- rand_forest(mode = "regression") %>%
    set_engine("randomForest") %>%
    fit(log(value) ~ as.numeric(date) + month(date, label = TRUE),
        data = training(splits))

model_fit_randomForest %>%
    modeltime_accuracy(new_data = testing(splits))


# randomForest (workflow) ----

model_spec <- rand_forest() %>%
    set_engine("randomForest")

recipe_spec <- recipe(value ~ date, data = training(splits)) %>%
    step_date(date, features = "month") %>%
    step_mutate(date_num = as.numeric(date)) %>%
    step_rm(date) %>%
    step_dummy(all_nominal()) %>%
    step_log(value)

wflw_fit_randomForest <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec) %>%
    fit(training(splits))

wflw_fit_randomForest %>% modeltime_accuracy(testing(splits))

# Compare ----
model_table <- modeltime_table(
    model_fit_no_boost,
    model_fit_boosted,
    wflw_fit_arima,
    model_fit_lm,
    wflw_fit_lm,
    model_fit_mars,
    wflw_fit_mars,
    model_fit_svm,
    wflw_fit_svm,
    model_fit_randomForest,
    wflw_fit_randomForest,
    model_fit_glmnet,
    wflw_fit_glmnet
)

model_table

model_table %>%
    modeltime_accuracy(new_data = testing(splits))

model_forecast <- model_table %>%
    modeltime_forecast(new_data = testing(splits),
                       actual_data = bind_rows(training(splits), testing(splits)))

model_forecast %>%
    group_by(.model_id, .model_desc) %>%
    mutate(.value = exp(.value)) %>%
    plot_modeltime_forecast(.include_legend = TRUE, .interactive = FALSE)
