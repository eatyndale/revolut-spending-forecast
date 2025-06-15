# Load required data and models
load("data/processed_data.RData")
load("data/multivariate_models.RData")

# Simulate future CPI values (12 months ahead starting from Feb 2025)
cpi_tail <- tail(cpi_ts, 3)
cpi_slope <- as.numeric(cpi_tail[3] - cpi_tail[2])

cpi_future <- numeric(12)
cpi_future[1] <- as.numeric(tail(cpi_ts, 1)) + cpi_slope

for (i in 2:12) {
  cpi_future[i] <- cpi_future[i - 1] + cpi_slope
}

# Fit final SARIMAX model using only CPI as exogenous regressor
xreg_cpi <- as.numeric(cpi_ts)
final_sarimax_model <- auto.arima(spending_ts, xreg = xreg_cpi, seasonal = TRUE)

# Forecast spending for Feb 2025 – Jan 2026 using future CPI values
final_forecast <- forecast(final_sarimax_model, xreg = cpi_future, h = 12)
final_forecast_ts <- ts(final_forecast$mean, start = c(2025, 2), frequency = 12)

# Plot original time series and forecast
library(ggplot2)
autoplot(spending_ts, series = "Historical Spending") +
  autolayer(final_forecast_ts, series = "SARIMAX Forecast (Feb 2025 – Jan 2026)", 
            linetype = "dashed", color = "blue") +
  ggtitle("SARIMAX Spending Forecast (Feb 2025 – Jan 2026)") +
  xlab("Time") + ylab("Spending") +
  theme_minimal()

# Create forecast data frame
future_dates <- seq(as.Date("2025-02-01"), by = "month", length.out = 12)
forecast_df <- data.frame(
  Month = format(future_dates, "%b %Y"),
  Predicted_Spending = round(final_forecast$mean, 2),
  CPI = round(cpi_future, 2)
)

# Print forecast results
print(forecast_df)

# Save final forecast results
save(final_sarimax_model, final_forecast, forecast_df,
     file = "data/final_forecast.RData") 