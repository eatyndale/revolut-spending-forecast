# Load processed and differenced data
load("data/processed_and_differenced.RData")

# Split data into train and test sets
train_size <- floor(0.8 * length(spending_diff_1))
train_end <- time(spending_diff_1)[train_size]
test_start <- time(spending_diff_1)[train_size + 1]

train_ts <- window(spending_diff_1, end = train_end)
test_ts <- window(spending_diff_1, start = test_start)

# SARIMA Model
sarima_model <- auto.arima(
  train_ts,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

# View model summary
summary(sarima_model)

# Plot residual diagnostics
checkresiduals(sarima_model)

# Forecast
sarima_forecast <- forecast(sarima_model, h = length(test_ts))
autoplot(sarima_forecast)

# ETS Model
ets_model <- ets(train_ts)
summary(ets_model)

ets_forecast <- forecast(ets_model, h = length(test_ts))
autoplot(ets_forecast)

# Holt-Winters Models
# Additive
hw_model_add <- HoltWinters(train_ts, seasonal = "additive")
summary(hw_model_add)
hw_forecast_add <- forecast(hw_model_add, h = length(test_ts))
autoplot(hw_forecast_add)

# Multiplicative
hw_model_multiple <- HoltWinters(train_ts, seasonal = "multiplicative")
summary(hw_model_multiple)
hw_forecast_multiple <- forecast(hw_model_multiple, h = length(test_ts))
autoplot(hw_forecast_multiple)

# Save models and forecasts
save(sarima_model, ets_model, hw_model_add, hw_model_multiple,
     sarima_forecast, ets_forecast, hw_forecast_add, hw_forecast_multiple,
     file = "data/univariate_models.RData") 