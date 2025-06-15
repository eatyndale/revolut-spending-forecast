# Load required packages
required_packages <- c(
  "tidyverse", "lubridate", "zoo", "forecast", "tseries",
  "TSstudio", "lmtest", "urca", "vars", "tsDyn",
  "randomForest", "Metrics", "e1071", "dplyr", "skimr",
  "keras"
)

# Install missing packages
install.packages(setdiff(required_packages, rownames(installed.packages())), dependencies = TRUE)

# Load all required packages
lapply(required_packages, library, character.only = TRUE)

# Read datasets
spending <- read.csv("data/spending_dataset.csv", stringsAsFactors = FALSE)
cpi <- read.csv("data/Monthly CPI.csv", stringsAsFactors = FALSE)
unemployment <- read.csv("data/unemployment_rate.csv", stringsAsFactors = FALSE)

# Data Wrangling: Unemployment Rate Dataset
unemployment <- unemployment[, 1:2]  # select only required columns
names(unemployment)[c(1,2)] <- c("date", "rate")  # rename columns
unemployment$date <- sub(".*to\\s+", "", unemployment$date)  # select last month as date
unemployment$date[20] <- "Sep 2021"

# Fill missing values in unemployment data
feb_2020 <- unemployment %>% filter(date == "Feb 2020")
jan_2020 <- feb_2020 %>% mutate(date = "Jan 2020")
dec_2024 <- unemployment %>% filter(date == "Dec 2024")
jan_2025 <- dec_2024 %>% mutate(date = "Jan 2025")
unemployment_new <- bind_rows(jan_2020, unemployment, jan_2025)

# Data Wrangling: CPI Dataset
cpi_data_monthly <- cpi %>%
  filter(grepl("^[0-9]{4} [A-Z]{3}$", Date)) %>%
  mutate(
    Date = paste(
      toupper(substr(Date, 6, 8)),
      substr(Date, 1, 4),
      sep = " "
    ),
    Date = format(as.Date(paste0("01 ", Date), format = "%d %b %Y"), "%b %Y")
  )

# Filter CPI data for required date range
cpi_new <- cpi_data_monthly %>%
  mutate(
    Date_temp = as.Date(paste0("01 ", Date), format = "%d %b %Y")
  ) %>%
  filter(
    Date_temp >= as.Date("2020-01-01") & Date_temp <= as.Date("2025-01-01")
  ) %>%
  dplyr::select(Date, CPI.Index)

# Date Formatting
spending$Date <- as.Date(paste0(trimws(spending$Date), " 01"), format = "%b %Y %d")
cpi_new$Date <- as.Date(paste0(trimws(cpi_new$Date), " 01"), format = "%b %Y %d")
unemployment_new$date <- as.Date(paste0(trimws(unemployment_new$date), " 01"), format = "%b %Y %d")

# Create time series objects
spending_ts <- ts(spending$Total, start = c(2020, 1), frequency = 12)
cpi_ts <- ts(cpi_new$CPI.Index, start = c(2020, 1), frequency = 12)
unemployment_ts <- ts(unemployment_new$rate, start = c(2020, 1), frequency = 12)

# Handle outliers
handle_outliers <- function(ts_data) {
  outliers <- boxplot.stats(ts_data)$out
  if (length(outliers) > 0) {
    ts_data[ts_data %in% outliers] <- NA
    ts_data <- na.approx(ts_data)
  }
  return(ts_data)
}

spending_ts <- handle_outliers(spending_ts)
cpi_ts <- handle_outliers(cpi_ts)
unemployment_ts <- handle_outliers(unemployment_ts)

# Save processed data
save(spending_ts, cpi_ts, unemployment_ts, file = "data/processed_data.RData") 