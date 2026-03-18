#load all required libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(tsibble)
library(feasts)

#load the data 
data_2022 <- read_csv("C:/Users/abhin/OneDrive - University of Oklahoma/Desktop/EA Project/2022_hrl_load_metered.csv")
data_2023 <- read_csv("C:/Users/abhin/OneDrive - University of Oklahoma/Desktop/EA Project/2023_hrl_load_metered.csv")
data_2024 <- read_csv("C:/Users/abhin/OneDrive - University of Oklahoma/Desktop/EA Project/2024_hrl_load_metered.csv")

#combine the data
hourly_data <- bind_rows(data_2022, data_2023, data_2024)

#filter CE
CE_data <- hourly_data %>%
  filter(load_area == "CE") %>%
  mutate(datetime = mdy_hms(datetime_beginning_ept, tz = "US/Eastern")) %>%
  arrange(datetime) %>%
  select(datetime, mw)

head(CE_data)
nrow(CE_data)

#create hourly time series object 
CE_ts <- ts(
  CE_data$mw,
  frequency = 168
)

# TASK 1
# hourly electricity load
ggplot(CE_data, aes(x = datetime, y = mw)) +
  geom_line(color = "#2c3e50", linewidth = 0.25) +
  labs(
    title = "Hourly Electricity Load – (CE)",
    x = "Datetime (Hourly)",
    y = "Load (MW)"
  ) +
  theme_minimal()



# daily average electricity load
CE_data %>%
  mutate(
    date = as.Date(datetime),
    year = year(datetime)
  ) %>%
  group_by(year, date) %>%
  summarise(avg_load = mean(mw), .groups = "drop") %>%
  ggplot(aes(date, avg_load, color = factor(year))) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ year, scales = "free_x", ncol = 1) +
  labs(
    title = "Daily Average Electricity Load – (CE), Year-wise",
    x = "Date",
    y = "Average Load (MW)",
    color = "Year"
  ) +
  theme_minimal()

#Average Hourly Load Pattern by Year
CE_data %>%
  mutate(
    hour = hour(datetime),
    year = year(datetime)
  ) %>%
  group_by(year, hour) %>%
  summarise(avg_load = mean(mw), .groups = "drop") %>%
  ggplot(aes(hour, avg_load, color = factor(year))) +
  geom_line(linewidth = 1.1) +
  geom_point() +
  labs(
    title = "Average Hourly Load Pattern by Year",
    x = "Hour of Day",
    y = "Average Load (MW)",
    color = "Year"
  ) +
  theme_minimal()


# average monthly electricity load by year
CE_data %>%
  mutate(
    month = month(datetime, label = TRUE),
    year = factor(year(datetime))
  ) %>%
  group_by(year, month) %>%
  summarise(avg_load = mean(mw), .groups = "drop") %>%
  ggplot(aes(month, avg_load, fill = year)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Monthly Electricity Load by Year ",
    x = "Month",
    y = "Average Load (MW)",
    fill = "Year"
  ) +
  theme_minimal()


#TASK 2
# missing values
colSums(is.na(CE_data))
sum(is.na(CE_data))


#task 3 
#STL decomposition
CE_ts <- ts(
  CE_data$mw,
  frequency = 168
)

# Recompute STL decomposition
stl_fit <- stl(CE_ts, s.window = "periodic")

trend_df <- data.frame(
  time = CE_data$datetime,
  trend = stl_fit$time.series[, "trend"]
)
autoplot(stl_fit) +
  labs(
    title = "STL Decomposition of Hourly Electricity Load – CE REGION",
    x = "Time (Weeks)"
  )

# trend 
ggplot(trend_df, aes(time, trend)) +
  geom_line(color = "#1f77b4", linewidth = 1) +
  labs(
    title = "Trend Component from STL – CE ",
    x = "Time",
    y = "Trend Load (MW)"
  ) +
  theme_minimal()

#seasonal
seasonal_df <- data.frame(
  time = CE_data$datetime,
  seasonal = stl_fit$time.series[, "seasonal"]
)

seasonal_df %>%
  filter(time < min(time) + days(7)) %>%
  ggplot(aes(time, seasonal)) +
  geom_line(color = "#e67e22", linewidth = 1) +
  labs(
    title = "Seasonal Component (One-Week View) – CE",
    x = "Time",
    y = "Seasonal Effect (MW)"
  ) +
  theme_minimal()

# remainder
remainder_df <- data.frame(
  time = CE_data$datetime,
  remainder = stl_fit$time.series[, "remainder"]
)

ggplot(remainder_df, aes(time, remainder)) +
  geom_line(color = "#c0392b", linewidth = 0.4) +
  labs(
    title = "Remainder Component from STL – CE (CE)",
    x = "Time",
    y = "Remainder (MW)"
  ) +
  theme_minimal()



# task  4

lambda <- BoxCox.lambda(CE_ts)
lambda

CE_ts_bc <- BoxCox(CE_ts, lambda)

autoplot(CE_ts_bc) +
  labs(
    title = "Box–Cox Transformed Hourly Electricity Load – CE (CE)",
    y = "Transformed Load"
  )

# task 5

# Forecast horizon: 1 week (hourly data)
h <- 168

# Train–test split (CORRECT)
train_ts <- head(CE_ts_bc, -h)
test_ts  <- tail(CE_ts_bc, h)

# 1. Naive model
fit_naive <- naive(train_ts, h = h)

# 2. Seasonal Naive model
fit_snaive <- snaive(train_ts, h = h)

# 3. STL + ETS model
# (Handles high-frequency seasonality)
fc_stlf <- stlf(train_ts, h = h, method = "ets")

# 4. ARIMA model
fit_arima <- auto.arima(
  train_ts,
  seasonal = TRUE
)
fc_arima <- forecast(fit_arima, h = h)


perf_table <- rbind(
  Naive = accuracy(fit_naive,  test_ts)[2, c("MASE", "RMSE", "MAE", "MAPE")],
  Seasonal_Naive = accuracy(fit_snaive, test_ts)[2, c("MASE", "RMSE", "MAE", "MAPE")],
  STL_ETS = accuracy(fc_stlf, test_ts)[2, c("MASE", "RMSE", "MAE", "MAPE")],
  ARIMA = accuracy(fc_arima, test_ts)[2, c("MASE", "RMSE", "MAE", "MAPE")]
)

perf_table
best_model <- rownames(perf_table)[which.min(perf_table[, "RMSE"])]
best_model


# ARIMA forecast
autoplot(fc_arima) +
  autolayer(test_ts, series = "Actual") +
  labs(
    title = "ARIMA Forecast vs Actual (Transformed Load)",
    x = "Time",
    y = "Transformed Load"
  )

# STL + ETS forecast
autoplot(fc_stlf) +
  autolayer(test_ts, series = "Actual") +
  labs(
    title = "STL + ETS Forecast vs Actual (Transformed Load)",
    x = "Time",
    y = "Transformed Load"
  )



#TASK 6
#qs1
#step1: visual check for stationarity
autoplot(CE_ts_bc) +
  labs(
    title = "Transformed Hourly Electricity Load (CE)",
    x = "Time",
    y = "Transformed Load"
  )

#step2: Augmented Dickey–Fuller (ADF) test
adf.test(CE_ts_bc)
#step3: Non-seasonal differencing
ndiffs(CE_ts_bc)
nsdiffs(CE_ts_bc)
#step4: Differencing
diff_ts <- diff(CE_ts_bc)
autoplot(diff_ts) +
  labs(
    title = "Differenced Transformed Hourly Electricity Load",
    x = "Time",
    y = "Differenced Load"
  )


#2)
#ACF
Acf(diff_ts, lag.max = 500,
    main = "ACF of Differenced Transformed Hourly Load")

#PACF
Pacf(diff_ts, lag.max = 500,
     main = "PACF of Differenced Transformed Hourly Load")

#3)
# Candidate Model 1
fit_arima1 <- Arima(
  train_ts,
  order = c(1, 0, 0),
  seasonal = list(order = c(0, 1, 0), period = 168),
  method = "ML"
)

# Candidate Model 2
fit_arima2 <- Arima(
  train_ts,
  order = c(0, 0, 1),
  seasonal = list(order = c(0, 1, 0), period = 168),
  method = "ML"
)
fit_auto <- auto.arima(
  train_ts,
  seasonal = TRUE,
  stepwise = TRUE,
  approximation = TRUE
)

summary(fit_auto)
AIC(fit_arima1, fit_arima2, fit_auto)

best_model <- which.min(c(
  AIC(fit_arima1),
  AIC(fit_arima2),
  AIC(fit_auto)
))

best_model


#5 
# Parameter estimation summary
summary(fit_arima1)
# Residual diagnostic checks
checkresiduals(fit_arima1)

# Compare AIC values

best_model <- which.min(c(
  AIC(fit_arima1),
  AIC(fit_arima2),
  AIC(fit_auto)
))
best_model


#6 generate the 12 step ahead
fc_arima_12 <- forecast(fit_arima1, h = 12)
autoplot(fc_arima_12) +
  labs(
    title = "12-Hour Ahead Forecast Using ARIMA – CE ",
    x = "Time",
    y = "Transformed Load"
  )

fc_arima_12_df <- data.frame(
  datetime = seq(
    from = max(CE_data$datetime) + hours(1),
    by = "hour",
    length.out = 12
  ),
  forecast = as.numeric(fc_arima_12$mean),
  lo80 = fc_arima_12$lower[,1],
  hi80 = fc_arima_12$upper[,1],
  lo95 = fc_arima_12$lower[,2],
  hi95 = fc_arima_12$upper[,2]
)

ggplot(fc_arima_12_df, aes(datetime, forecast)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.2) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), alpha = 0.4) +
  labs(
    title = "12-Hour Ahead Forecast Using ARIMA – CE ",
    x = "Datetime",
    y = "Transformed Load"
  ) +
  theme_minimal()


#7)
fit_ets <- ets(train_ts)
summary(fit_ets)

#8)
# Residual diagnostics for ARIMA
checkresiduals(fit_arima1)

#9 
fc_ets_12 <- forecast(fit_ets, h = 12)

autoplot(fc_ets_12) +
  labs(
    title = "12-Hour Ahead Forecast Using ETS – CE (CE)",
    x = "Time",
    y = "Transformed Load"
  )
# Create datetime index for next 12 hours
forecast_time <- seq(
  from = max(CE_data$datetime) + hours(1),
  by = "hour",
  length.out = 12
)
fc_ets_df <- data.frame(
  datetime = forecast_time,
  forecast = as.numeric(fc_ets_12$mean),
  lo80 = fc_ets_12$lower[,1],
  hi80 = fc_ets_12$upper[,1],
  lo95 = fc_ets_12$lower[,2],
  hi95 = fc_ets_12$upper[,2]
)
ggplot(fc_ets_df, aes(x = datetime, y = forecast)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95),
              fill = "lightblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = lo80, ymax = hi80),
              fill = "blue", alpha = 0.2) +
  labs(
    title = "12-Hour Ahead Electricity Load Forecast Using ETS – CE (CE)",
    x = "Datetime (Hourly)",
    y = "Transformed Load"
  ) +
  theme_minimal()


#task 7
CE_data %>%
  mutate(
    day_type = ifelse(wday(datetime) %in% c(1, 7),
                      "Weekend", "Weekday")
  ) %>%
  group_by(day_type) %>%
  summarise(
    avg_load = mean(mw),
    sd_load = sd(mw)
  )
CE_data %>%
  mutate(
    day_type = ifelse(wday(datetime) %in% c(1, 7),
                      "Weekend", "Weekday")
  ) %>%
  ggplot(aes(day_type, mw, fill = day_type)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Electricity Load Distribution: Weekday vs Weekend",
    x = "",
    y = "Load (MW)"
  ) +
  theme_minimal()

#taSK 7.2
CE_data %>%
  mutate(hour = hour(datetime)) %>%
  group_by(hour) %>%
  summarise(avg_load = mean(mw)) %>%
  ggplot(aes(hour, avg_load)) +
  geom_line(color = "#2c3e50", linewidth = 1) +
  geom_point() +
  labs(
    title = "Average Hourly Electricity Load Profile – CE (CE)",
    x = "Hour of Day",
    y = "Average Load (MW)"
  ) +
  theme_minimal()


