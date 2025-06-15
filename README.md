# Revolut Spending Time Series Analysis

This project analyzes and forecasts Revolut spending data using various time series and machine learning models.

## Project Structure

```
├── README.md
├── data/
│   ├── spending_dataset.csv
│   ├── Monthly CPI.csv
│   └── unemployment_rate.csv
├── src/
│   ├── 01_data_preparation.R
│   ├── 02_exploratory_analysis.R
│   ├── 03_univariate_models.R
│   ├── 04_multivariate_models.R
│   ├── 05_ml_models.R
│   ├── 06_model_evaluation.R
│   └── 07_final_forecasting.R
└── requirements.txt
```

## Setup Instructions

1. Install R and RStudio
2. Install required R packages:
   ```R
   install.packages(c(
     "tidyverse", "lubridate", "zoo", "forecast", "tseries",
     "TSstudio", "lmtest", "urca", "vars", "tsDyn",
     "randomForest", "Metrics", "e1071", "dplyr", "skimr",
     "keras"
   ))
   ```
3. Place your data files in the `data/` directory:
   - spending_dataset.csv
   - Monthly CPI.csv
   - unemployment_rate.csv

## Running the Analysis

The scripts are numbered in order of execution. Run them sequentially:

1. `01_data_preparation.R`: Data loading and preprocessing
2. `02_exploratory_analysis.R`: Exploratory data analysis and visualization
3. `03_univariate_models.R`: Univariate time series models (SARIMA, ETS, Holt-Winters)
4. `04_multivariate_models.R`: Multivariate models (SARIMAX, VAR, VECM)
5. `05_ml_models.R`: Machine learning models (Random Forest, SVR, LSTM)
6. `06_model_evaluation.R`: Model comparison and evaluation
7. `07_final_forecasting.R`: Final forecasting using the best model

## Results

The analysis includes:
- Time series decomposition
- Stationarity testing
- Model fitting and evaluation
- Forecasting for future periods

The best performing model was SARIMAX, which was used for the final forecasting. 