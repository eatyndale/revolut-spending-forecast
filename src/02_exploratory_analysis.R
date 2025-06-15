# Load processed data
load("data/processed_data.RData")

# Basic data exploration
summary(spending_ts)
summary(cpi_ts)
summary(unemployment_ts)

# Time series plots
par(mfrow = c(3, 1))
plot(spending_ts, main = "Spending Time Series", ylab = "Total Spending", xlab = "Time")
plot(cpi_ts, main = "CPI Time Series", ylab = "CPI Index", xlab = "Time")
plot(unemployment_ts, main = "Unemployment Rate", ylab = "Rate", xlab = "Time")
par(mfrow = c(1, 1))

# Check for transformation using Box-Cox
lambda_spending <- BoxCox.lambda(spending_ts)
lambda_cpi <- BoxCox.lambda(cpi_ts)
lambda_unemployment <- BoxCox.lambda(unemployment_ts)

print(paste0("lambda_spending is ", lambda_spending))
print(paste0("lambda_cpi is ", lambda_cpi))
print(paste0("lambda_unemployment is ", lambda_unemployment))

# Apply Box-Cox transformation to unemployment
unemployment_ts_transformed <- BoxCox(unemployment_ts, lambda = -1)

# Time series decomposition
decomp_spending <- stl(spending_ts, s.window = "periodic")
decomp_cpi <- stl(cpi_ts, s.window = "periodic")
decomp_unemp <- stl(unemployment_ts_transformed, s.window = "periodic")

# Plot decompositions
plot(decomp_spending, main = "Spending Decomposition")
plot(decomp_cpi, main = "CPI Index Decomposition")
plot(decomp_unemp, main = "Unemployment Rate Decomposition")

# Stationarity testing
cat("ADF Test for Spending:\n")
print(adf.test(spending_ts))

cat("\nADF Test for CPI:\n")
print(adf.test(cpi_ts))

cat("\nADF Test for Unemployment (Transformed):\n")
print(adf.test(unemployment_ts_transformed))

# Differencing
spending_diff_1 <- diff(diff(spending_ts), lag = 12)
cpi_diff_1 <- diff(diff(cpi_ts), lag = 12)
unemp_diff_1 <- diff(diff(unemployment_ts_transformed), lag = 12)

# ADF Test for differenced series
cat("ADF Test - Spending (combined diff):\n")
print(adf.test(spending_diff_1))

cat("\nADF Test - CPI (combined diff):\n")
print(adf.test(cpi_diff_1))

cat("\nADF Test - Unemployment (transformed + combined diff):\n")
print(adf.test(unemp_diff_1))

# ACF and PACF plots
par(mfrow = c(2, 3))
acf(spending_diff_1, main = "ACF: Spending")
acf(cpi_diff_1, main = "ACF: CPI Index")
acf(unemp_diff_1, main = "ACF: Transformed Unemployment")
pacf(spending_diff_1, main = "PACF: Spending")
pacf(cpi_diff_1, main = "PACF: CPI Index")
pacf(unemp_diff_1, main = "PACF: Transformed Unemployment")
par(mfrow = c(1, 1))

# Save processed and differenced data
save(spending_diff_1, cpi_diff_1, unemp_diff_1, 
     unemployment_ts_transformed,
     file = "data/processed_and_differenced.RData") 