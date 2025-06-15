# Load all model results
load("data/univariate_models.RData")
load("data/multivariate_models.RData")
load("data/ml_models.RData")
load("data/processed_and_differenced.RData")

# Function to calculate RMSE and MAE
calculate_metrics <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  return(list(rmse = rmse, mae = mae))
}

# Evaluate SARIMA
sarima_metrics <- calculate_metrics(test_ts, sarima_forecast$mean)
cat("SARIMA RMSE:", sarima_metrics$rmse, "\n")
cat("SARIMA MAE:", sarima_metrics$mae, "\n")

# Evaluate ETS
ets_metrics <- calculate_metrics(test_ts, ets_forecast$mean)
cat("ETS RMSE:", ets_metrics$rmse, "\n")
cat("ETS MAE:", ets_metrics$mae, "\n")

# Evaluate Holt-Winters (Additive)
hw_add_metrics <- calculate_metrics(test_ts, hw_forecast_add$mean)
cat("HW (Additive) RMSE:", hw_add_metrics$rmse, "\n")
cat("HW (Additive) MAE:", hw_add_metrics$mae, "\n")

# Evaluate Holt-Winters (Multiplicative)
hw_mult_metrics <- calculate_metrics(test_ts, hw_forecast_multiple$mean)
cat("HW (Multiplicative) RMSE:", hw_mult_metrics$rmse, "\n")
cat("HW (Multiplicative) MAE:", hw_mult_metrics$mae, "\n")

# Evaluate SARIMAX
sarimax_metrics <- calculate_metrics(test_ts, forecast_sarimax$mean)
cat("SARIMAX RMSE:", sarimax_metrics$rmse, "\n")
cat("SARIMAX MAE:", sarimax_metrics$mae, "\n")

# Evaluate VAR
var_metrics <- calculate_metrics(test_ts, spending_forecast_ts)
cat("VAR RMSE:", var_metrics$rmse, "\n")
cat("VAR MAE:", var_metrics$mae, "\n")

# Evaluate VECM
vecm_metrics <- calculate_metrics(test_ts, vecm_forecast_ts)
cat("VECM RMSE:", vecm_metrics$rmse, "\n")
cat("VECM MAE:", vecm_metrics$mae, "\n")

# Evaluate Random Forest
rf_metrics <- calculate_metrics(test_ml$y, rf_preds)
cat("Random Forest RMSE:", rf_metrics$rmse, "\n")
cat("Random Forest MAE:", rf_metrics$mae, "\n")

# Evaluate SVR
svr_metrics <- calculate_metrics(test_ml$y, svr_preds)
cat("SVR RMSE:", svr_metrics$rmse, "\n")
cat("SVR MAE:", svr_metrics$mae, "\n")

# Evaluate LSTM
lstm_metrics <- calculate_metrics(test_ml$y, lstm_preds)
cat("LSTM RMSE:", lstm_metrics$rmse, "\n")
cat("LSTM MAE:", lstm_metrics$mae, "\n")

# Create comparison table
mae_results <- data.frame(
  Model = c(
    "SARIMA", "ETS", "HW (Multiplicative)", "HW (Additive)",
    "SARIMAX", "VAR", "VECM",
    "Random Forest", "SVR", "LSTM"
  ),
  MAE = c(
    sarima_metrics$mae, ets_metrics$mae, hw_mult_metrics$mae, hw_add_metrics$mae,
    sarimax_metrics$mae, var_metrics$mae, vecm_metrics$mae,
    rf_metrics$mae, svr_metrics$mae, lstm_metrics$mae
  )
)

# Plot comparison
library(ggplot2)
ggplot(mae_results, aes(x = reorder(Model, MAE), y = MAE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = round(MAE, 2)), hjust = -0.1, size = 3.5) +
  labs(
    title = "MAE Comparison of Forecasting Models",
    x = "Model",
    y = "Mean Absolute Error (MAE)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Save evaluation results
save(mae_results, file = "data/model_evaluation.RData") 