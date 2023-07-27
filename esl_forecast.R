#ESL FORECAST-Running ARIMA and HoltWinters for the infiltration rates and comparing performance levels
# Farshad Ebrahimi, 7/20/2023

library(dplyr)
library(odbc)
library(ggplot2)
library(stats)
library(zoo)
library(forecast)
library(lubridate)
library(readxl)
library(DBI)
library(tidyverse)
library(TSstudio)
library(EnvStats)


options(scipen = 999)

# DB PG14
con <- dbConnect(odbc::odbc(), dsn = "mars14_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190)

# Residuals and model stats from Brian
infil_temp_models <- dbGetQuery(con, "SELECT * FROM metrics.tbl_infil_temp_models where model_type = 'linear'")

# Get OW_UID-System_ID
ow <- dbGetQuery(con, "SELECT *,admin.fun_smp_to_system(smp_id) as system_id FROM fieldwork.tbl_ow")

#get the rain event time and date

smp_radarcell <- dbGetQuery(con, "SELECT * FROM data.tbl_radar_event") %>%
  select(-eventdatastart_edt)
metric_residual <- infil_temp_models %>%
  inner_join(smp_radarcell, by = c("radar_event_uid")) %>%
  select(ow_uid, radar_event_uid, infiltration_inhr, eventdatastart_edt, residual) %>%
  na.omit() %>%
  distinct()

# mann-kendall test
metric_residual["p_value"] <- NA
metric_residual["slope_estimate"] <- NA
metric_residual["tau"] <- NA
metric_residual["intercept_estimate"] <- NA

output <- metric_residual[0,]

owid_unique <- metric_residual %>%
  select(ow_uid) %>%
  distinct()

for (i in 1:nrow(owid_unique)) {
  
  metric_df <- metric_residual %>%
    filter(ow_uid == owid_unique[i, ]) 
  
  kendall <- kendallTrendTest(residual ~ eventdatastart_edt , data = metric_df)
  
  metric_df["p_value"] <- kendall$p.value
  metric_df["intercept_estimate"] <- kendall$estimate[3]
  metric_df["slope_estimate"] <- kendall$estimate[2]
  metric_df["tau"] <- kendall$estimate[1]
  
  
  output <- rbind(output, metric_df)
  
}

output <- output %>%
  select(ow_uid, p_value, slope_estimate, intercept_estimate, tau) %>%
  distinct()

#Filter the negative slopes for forcasting

negative_slope_ow <- output %>%
  filter(slope_estimate < 0) %>%
  select(ow_uid)

#Filtr the infiltration data for those with negative slope
  infil_esl_data <- infil_temp_models %>%
    filter(ow_uid %in% negative_slope_ow$ow_uid) %>%
    inner_join(ow, by = "ow_uid") %>%
    select(ow_uid, smp_id, eventdatastart_edt, infiltration_inhr, residual) %>%
    na.omit()
  
  
###infil_esl_data for 10-1- Purpose: showcasing the methods
  
  infil_esl_data_10_1 <- infil_esl_data %>%
    filter(smp_id == "10-1-1") 

  #time stamp monthly-assign first day of the month to the monthly stamp
  infil_esl_data_10_1$Month_Yr <- format(as.Date(infil_esl_data_10_1$eventdatastart_edt), "%Y-%m")
  infil_esl_data_10_1$month <- ymd(paste(infil_esl_data_10_1$Month_Yr,"-01", sep = ""))
  
  #average monthly
  inf_monthly_10_1 <- infil_esl_data_10_1 %>%
    group_by(month) %>%
    dplyr::summarise(ave_rate = mean(infiltration_inhr))
  
  
  ### interpolation-there are some month with no infiltration data-Spliting the data: before and after the huge gap
  date_index_train <- data.frame(month = seq(from = ymd("2013-12-01"), to = ymd("2018-03-01"), by = "1 month"))
  date_index_test <- data.frame(month = seq(from = ymd("2020-11-01"), to = ymd("2023-03-01"), by = "1 month"))
  
  ts_data_train <- date_index_train %>%
    left_join(inf_monthly_10_1, by = "month")
  
  ts_data_test <- date_index_test %>%
    left_join(inf_monthly_10_1, by = "month")

  #interpolate missing values-this is to be done for smaller gaps 
  ts_data_train["inter_rate"] <- zoo::na.approx(ts_data_train$ave_rate)
  ts_data_test["inter_rate"] <- zoo::na.approx(ts_data_test$ave_rate)
  

  #turn the data into Time Series
  #this is train data
  ts_data_feed <- ts(ts_data_train$inter_rate, frequency = 12, start = c(2013,12))
  
  #this is test data
  ts_data_real <- ts(ts_data_test$inter_rate, frequency = 12, start = c(2020,11))
  
  
  
  # Fit the Holt-Winters model with additive seasonal components
  hw_model <- HoltWinters(ts_data_feed, seasonal = "additive", gamma = TRUE)
  
  # Forecast future values using the fitted model
  forecast_values_hw <- forecast(hw_model, h = 100)
  
  plot(forecast_values_hw)
  
  #this will calculate the performance metrics
  accuracy(forecast_values_hw, ts_data_real)
  
  # Model with ARIMA-non seasonal
  arima_model <- auto.arima(ts_data_feed)
  forecast_values_arima <- forecast(arima_model, h = 100)
  plot(forecast_values_arima)
  accuracy(forecast_values_arima, ts_data_real)

  
  
  