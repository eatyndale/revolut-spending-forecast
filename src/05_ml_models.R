# Load processed data
load("data/processed_data.RData")

# Create lagged features function
create_lagged_df <- function(y, x1, x2, lags = 1:3) {
  df <- data.frame(y = y)
  
  for (l in lags) {
    df[[paste0("lag_y_", l)]] <- stats::lag(y, -l)
    df[[paste0("lag_cpi_", l)]] <- stats::lag(x1, -l)
    df[[paste0("lag_unemp_", l)]] <- stats::lag(x2, -l)
  }
  
  return(na.omit(df))
}

# Create feature matrix
ml_df <- create_lagged_df(spending_ts, cpi_ts, unemployment_ts, lags = 1:3)

# Split data
n <- nrow(ml_df)
train_rows <- floor(0.8 * n)
train_ml <- ml_df[1:train_rows, ]
test_ml <- ml_df[(train_rows + 1):n, ]

train_start <- start(spending_ts)
train_freq <- frequency(spending_ts)
train_spending_ts <- ts(train_ml$y, start = train_start, frequency = train_freq)

# Random Forest Model
rf_model <- randomForest(
  y ~ .,
  data = train_ml,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

rf_preds <- predict(rf_model, newdata = test_ml)

# SVR Model
X_train <- as.matrix(train_ml[, -1])
y_train <- train_ml$y
X_test <- as.matrix(test_ml[, -1])
y_test <- test_ml$y

svr_model <- svm(
  x = X_train,
  y = y_train,
  type = "eps-regression",
  kernel = "radial",
  cost = 1,
  epsilon = 0.1
)

svr_preds <- predict(svr_model, X_test)

# LSTM Model
# Scale the data
scale_data <- function(df) {
  scale(df)
}

train_scaled <- scale_data(train_ml)
test_scaled <- scale_data(test_ml)

# Extract dimensions
timesteps <- 1
n_features <- ncol(train_scaled) - 1

# Reshape data for LSTM
X_train <- array(as.matrix(train_scaled[, -1]), dim = c(nrow(train_scaled), timesteps, n_features))
y_train <- train_scaled[, 1]
X_test <- array(as.matrix(test_scaled[, -1]), dim = c(nrow(test_scaled), timesteps, n_features))
y_test <- test_scaled[, 1]

# Build and train LSTM model
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(timesteps, n_features)) %>%
  layer_dense(units = 1)

model %>% compile(
  loss = "mean_squared_error",
  optimizer = "adam"
)

model %>% fit(
  x = X_train,
  y = y_train,
  epochs = 50,
  batch_size = 8,
  verbose = 0
)

# LSTM predictions
lstm_preds_scaled <- model %>% predict(X_test)

# Inverse scale predictions
y_train_scaled <- scale(train_ml$y)
mean_y <- attr(y_train_scaled, "scaled:center")
sd_y <- attr(y_train_scaled, "scaled:scale")
lstm_preds <- lstm_preds_scaled * sd_y + mean_y

# Save models and predictions
save(rf_model, svr_model, model,
     rf_preds, svr_preds, lstm_preds,
     file = "data/ml_models.RData") 