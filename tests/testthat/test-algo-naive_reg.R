library(dplyr)
library(parsnip)
library(rsample)
library(timetk)
library(modeltime)

# Data
m750 <- m4_monthly %>% filter(id == "M750")
m750

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.8)

# 1.0 NAIVE ----

# * SINGLE TIME SERIES -----

# Model Spec
model_fit <- naive_reg() %>%
    set_engine("naive") %>%
    fit(value ~ date, data = training(splits))
model_fit

modeltime_table(
    model_fit
) %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = m750
    ) %>%
    plot_modeltime_forecast()



modeltime_table(
    model_fit
) %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_refit(m750) %>%
    modeltime_forecast(
        h = nrow(testing(splits)),
        actual_data = m750
    ) %>%
    plot_modeltime_forecast()

# * PANEL DATA ----

full_data_tbl <- m4_monthly %>%
    group_by(id) %>%
    future_frame(date, .length_out = 60, .bind_data = TRUE) %>%
    ungroup()

future_tbl <- full_data_tbl %>% filter(is.na(value))

data_prepared_tbl <- full_data_tbl %>% filter(!is.na(value))

model_fit_panel <- naive_reg(id = "id") %>%
    set_engine("naive") %>%
    fit(value ~ date + id, data = data_prepared_tbl)
model_fit_panel

modeltime_table(
    model_fit_panel
) %>%
    modeltime_forecast(
        new_data    = future_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = TRUE
    ) %>%
    group_by(id) %>%
    plot_modeltime_forecast()

# 2.0 SNAIVE -----

# * SINGLE TIME SERIES -----

# Model Spec
model_fit <- naive_reg(seasonal_period = 24) %>%
    set_engine("snaive") %>%
    fit(value ~ date, data = training(splits))
model_fit

modeltime_table(
    model_fit
) %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = m750
    ) %>%
    plot_modeltime_forecast()


modeltime_table(
    model_fit
) %>%
    modeltime_calibrate(testing(splits)) %>%
    modeltime_refit(m750) %>%
    modeltime_forecast(
        h = nrow(testing(splits)),
        actual_data = m750
    ) %>%
    plot_modeltime_forecast()

# * PANEL DATA ----

full_data_tbl <- m4_monthly %>%
    group_by(id) %>%
    future_frame(date, .length_out = 60, .bind_data = TRUE) %>%
    ungroup()

future_tbl <- full_data_tbl %>% filter(is.na(value))

data_prepared_tbl <- full_data_tbl %>% filter(!is.na(value))

model_fit_panel <- naive_reg(id = "id") %>%
    set_engine("snaive") %>%
    fit(value ~ date + id, data = data_prepared_tbl)
model_fit_panel

modeltime_table(
    model_fit_panel
) %>%
    modeltime_forecast(
        new_data    = future_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = TRUE
    ) %>%
    group_by(id) %>%
    plot_modeltime_forecast()
