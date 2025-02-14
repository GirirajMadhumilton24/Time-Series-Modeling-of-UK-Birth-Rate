# Load libraries
library(readxl) # Load the readxl package
library(ggplot2)
library(forecast)
library(dplyr)
library("TTR")

# Load the file without headers
data <- read_excel("C:\\Users\\girir\\OneDrive - University of Salford\\assignment asdv\\R time series\\Vital statistics in the UK.xlsx", sheet = "Birth", skip = 5)

# View the first few rows of the data
head(data)

# Select the desired columns and create a new data frame
birth_data <- data %>%
  select(
    `Year`,
    `Number of live births: England and Wales`,
  ) %>%
  rename(
    `Live Births EW` = `Number of live births: England and Wales`,
  )

# EDA--------------------------------------------------------------------------------------------------------------
summary(birth_data)

str(birth_data)

# Convert columns to appropriate types
birth_data$`Live Births EW` <- as.numeric(birth_data$`Live Births EW`)

# Check for missing values
colSums(is.na(birth_data))

# Create time series object
birth_ts_eng_wales <- ts(birth_data$`Live Births EW`, 
                         start = min(birth_data$Year), 
                         frequency = 1)

# Plot the time series
autoplot(birth_ts_eng_wales) +
  ggtitle("Live Births in England and Wales") +
  xlab("Year") +
  ylab("Number of Births") +
  theme_minimal()

# Decompose

birth_ts_eng_walesSMA8 <- SMA(birth_ts_eng_wales,n=5)
plot.ts(birth_ts_eng_wales)

# forecasting---1----------------------------------------------------------------------------------------

# Holt-Winters model with additive trend
birth_ew_forecast <- HoltWinters(birth_ts_eng_wales, beta = TRUE, gamma = FALSE)
print(birth_ew_forecast)

# Plot Holt-Winters forecast
plot(birth_ew_forecast, main = "Live Births in England and Wales (Holt-Winters)")

# RMSE Calculation for Holt-Winters
rmse_ew <- sqrt(birth_ew_forecast$SSE / length(birth_ts_eng_wales))
cat("RMSE for England and Wales (Holt-Winters):", rmse_ew, "\n")

# Apply model with custom starting level
l.start_ew <- mean(birth_ts_eng_wales[1:3])
holt_winters_model_ew <- HoltWinters(birth_ts_eng_wales, gamma = FALSE, l.start = l.start_ew)
print(holt_winters_model_ew)

# Forecast next 10 years using Holt-Winters
forecast_ew <- forecast(holt_winters_model_ew, h = 10)
plot(forecast_ew, main = "Forecast of Live Births in England and Wales", xlab = "Year", ylab = "Number of Births")

# Residual Analysis for Holt-Winters model
acf(forecast_ew$residuals, lag.max = 20, na.action = na.pass)

# Box-Ljung test
Box.test(forecast_ew$residuals, lag = 20, type = "Ljung-Box")

# residual plot
plot.ts(forecast_ew$residuals, main = "Residuals for Holt-Winters Model")

# Custom function to plot forecast errors
plotForecastErrors <- function(errors) {
  binsize <- IQR(errors) / 4
  mysd <- sd(errors)
  myrange <- range(c(errors, rnorm(10000, mean = 0, sd = mysd)))
  bins <- seq(myrange[1], myrange[2], binsize)
  
  hist(errors, col = "red", freq = FALSE, breaks = bins, main = "Forecast Errors", xlab = "Error")
  lines(density(rnorm(10000, mean = 0, sd = mysd)), col = "blue", lwd = 2)
}

# Plot forecast errors for Holt-Winters
forecast_ew$residuals <- na.omit(forecast_ew$residuals)
plotForecastErrors(forecast_ew$residuals)


# forecasting---2---------------------------------------------------------------------------------------------

# Differencing 
birth_ts_eng_wales_diff <- diff(birth_ts_eng_wales, differences = 1)
plot.ts(birth_ts_eng_wales_diff)

# ARIMA Forecasting (apply ARIMA model on the differenced series)
arima_forecast_ew <- arima(birth_ts_eng_wales_diff, order = c(1, 0, 1))

# Plot the ARIMA model's diagnostics
plot(arima_forecast_ew, main = "ARIMA Model for Live Births")

# ARIMA Forecasting for next 10 years
arima_forecast_ew <- forecast(arima_forecast_ew, h = 10)
# Plotting forecast
plot(arima_forecast_ew, main = "ARIMA Forecast for England and Wales Births")

# ARIMA RMSE Calculation (calculate RMSE using the residuals from the forecast object)
arima_rmse_ew <- sqrt(mean(arima_forecast_ew$residuals^2, na.rm = TRUE))
cat("RMSE for ARIMA (England & Wales):", arima_rmse_ew, "\n")

# Plot the ACF of the residuals from the ARIMA model
acf(arima_forecast_ew$residuals, lag.max = 20, na.action = na.pass,
    main = "ACF of ARIMA Residuals")

# Ljung-Box Test
Box.test(arima_forecast_ew$residuals, lag = 20, type = "Ljung-Box")

# Define a custom function to plot forecast errors (assuming it's defined elsewhere)
plotForecastErrors <- function(errors) {
  # Remove NA values from the residuals
  errors <- na.omit(errors)
  
  # Define the histogram bins to span the range of the data
  bins <- seq(min(errors), max(errors), length.out = 30)
  
  # Create a histogram of the residuals
  hist(errors, col = "red", freq = FALSE, breaks = bins,
       main = "Forecast Errors",
       xlab = "Errors", ylab = "Density")
  
  # Add a density curve
  lines(density(errors), col = "blue", lwd = 2)
}

# Plot the forecast errors for ARIMA model
plotForecastErrors(arima_forecast_ew$residuals)


# forecasting---3--------------------------------------------------------------------------------------

# ETS 

# Fit the ETS model (non-seasonal data will automatically select S = N)
ets_forecast_ew <- ets(birth_ts_eng_wales)

# Forecasting for the next 10 years
ets_forecast_ew <- forecast(ets_forecast_ew, h = 10)
# Plot the ETS forecast for the next 10 years
autoplot(ets_forecast_ew) + ggtitle("ETS Forecast (Non-Seasonal) for England and Wales Births")

# ETS RMSE Calculation
ets_rmse_ew <- sqrt(mean(ets_forecast_ew$residuals^2, na.rm = TRUE))
cat("RMSE for ETS (Non-Seasonal, England & Wales):", ets_rmse_ew, "\n")

# Plot the ACF of the residuals from the ETS model
acf(ets_forecast_ew$residuals, lag.max = 20, na.action = na.pass,
    main = "ACF of ETS Residuals")

# box Test for ETS 
Box.test(ets_forecast_ew$residuals, lag = 20, type = "Ljung-Box")

# Define a custom function to plot forecast errors
plotForecastErrors <- function(errors) {
  # Remove NA values from the residuals
  errors <- na.omit(errors)
  
  # Define the histogram bins to span the range of the data
  bins <- seq(min(errors), max(errors), length.out = 30)
  
  # Create a histogram of the residuals
  hist(errors, col = "red", freq = FALSE, breaks = bins,
       main = "Forecast Errors",
       xlab = "Errors", ylab = "Density")
  
  # Add a density curve
  lines(density(errors), col = "blue", lwd = 2)
}

# Plot the forecast errors for the ETS model
plotForecastErrors(ets_forecast_ew$residuals)




