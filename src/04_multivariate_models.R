# Load processed and differenced data
load("data/processed_and_differenced.RData")

# Split data into train and test sets
train_size <- floor(0.8 * length(spending_diff_1))
train_end <- time(spending_diff_1)[train_size]
test_start <- time(spending_diff_1)[train_size + 1]

train_ts <- window(spending_diff_1, end = train_end)
test_ts <- window(spending_diff_1, start = test_start)

train_cpi <- window(cpi_diff_1, end = train_end)
test_cpi <- window(cpi_diff_1, start = test_start)

train_unemp <- window(unemp_diff_1, end = train_end)
test_unemp <- window(unemp_diff_1, start = test_start)

# SARIMAX Model
sarimax_model <- auto.arima(
  train_ts,
  xreg = as.numeric(train_cpi),
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(sarimax_model)
checkresiduals(sarimax_model)

# Forecast SARIMAX
forecast_sarimax <- forecast(sarimax_model, xreg = as.numeric(test_cpi), h = length(test_ts))
autoplot(forecast_sarimax)

# VAR Model
var_data <- cbind(spending_var = train_ts, cpi_var = train_cpi)
lag_selection <- VARselect(var_data, lag.max = 10, type = "const")
best_lag <- lag_selection$selection["AIC(n)"]

var_model <- VAR(var_data, p = best_lag, type = "const")
summary(var_model)

# VAR Forecast
var_forecast <- predict(var_model, n.ahead = length(test_ts))
spending_forecast_var <- var_forecast$fcst$spending_var[, "fcst"]
spending_forecast_ts <- ts(spending_forecast_var, start = start(test_ts), frequency = frequency(test_ts))

# VECM Model
# Combine level (non-stationary) series
vecm_data <- cbind(spending_vecm = train_ts, cpi_vecm = train_cpi)

# Johansen cointegration test
johansen_test <- ca.jo(vecm_data, type = "trace", ecdet = "const", K = 2)
summary(johansen_test)

# Convert to VECM
vecm_model <- cajorls(johansen_test, r = 1)
summary(vecm_model)

# Convert to VAR for forecasting
vecm_as_var <- vec2var(johansen_test, r = 1)
vecm_forecast <- predict(vecm_as_var, n.ahead = length(test_ts))
vecm_spending_forecast <- vecm_forecast$fcst$spending_vecm[, "fcst"]
vecm_forecast_ts <- ts(vecm_spending_forecast, start = start(test_ts), frequency = frequency(test_ts))

# Save models and forecasts
save(sarimax_model, var_model, vecm_model,
     forecast_sarimax, var_forecast, vecm_forecast,
     file = "data/multivariate_models.RData") 