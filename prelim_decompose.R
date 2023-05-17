#Exploring Time Series of Infiltration Rates by Decomposing and Forecasting
# Farshad Ebrahimi, 5/15/2023

library(dplyr)
library(odbc)
library(ggplot2)
library(stats)
library(zoo)
library(hydroTSM)
library(tsibble)
library(fable)
library(forecast)
library(lubridate)
library(fpp2)

# DB PG14
con <- dbConnect(odbc::odbc(), dsn = "mars14_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190)

#Get metrics
folder <- "//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/48 Short-Circuiting GSI Data Analysis/Calculation Phase/Metrics Calculations"
analysis_date <- "2023-01-03"
csv_path <-   paste(folder,analysis_date,"metrics.csv" , sep ="/")
sc_metrics <- read.csv(csv_path)


#Filter for 179-5-1
metric_179_5_1 <- sc_metrics %>%
  filter(smp_id == "179-5-1") %>%
  select(smp_id, radar_event_uid, infiltration_inhr) %>%
  na.omit()

#get the rain event time and date

smp_radarcell <- dbGetQuery(con, "SELECT * FROM data.tbl_radar_event")
metric_ts_179_5_1 <- metric_179_5_1 %>%
  inner_join(smp_radarcell, by = "radar_event_uid" ) %>%
  select(smp_id, radar_event_uid, eventdatastart_edt, infiltration_inhr)

#prelim plot
inf_ts_all <- metric_ts_179_5_1 %>%
  select(Date = eventdatastart_edt, Rate = infiltration_inhr)

plot(inf_ts_all, main = " 179-5-1 Infiltration Rate (in/hr)")

### Seasonally separate the metrics
inf_ts <- inf_ts_all

#adding season
inf_ts["season"] <- time2season(inf_ts$Date, out.fmt = "seasons")

### Plot by Season to see if there is a trend
    inf_ts_spring <- inf_ts %>%
      filter(season == "spring") %>%
      select(Date, Rate)
    
    plot(inf_ts_spring, main = " 179-5-1, Spring, Infiltration Rate (in/hr)")
    
    # Separting By Season
    inf_ts_summer <- inf_ts %>%
      filter(season == "summer") %>%
      select(Date , Rate )
    plot(inf_ts_summer, main = " 179-5-1, Summer, Infiltration Rate (in/hr)")
    
    
    inf_ts_fall <- inf_ts %>%
      filter(season == "autumm") %>%
      select(Date , Rate)
    plot(inf_ts_fall, main = " 179-5-1, Fall, Infiltration Rate (in/hr)")
    
    
    inf_ts_winter <- inf_ts %>%
      filter(season == "winter") %>%
      select(Date, Rate )
    plot(inf_ts_winter, main = " 179-5-1, Winter, Infiltration Rate (in/hr)")


#start decomposing
# Convert 'Date' column to Date type
inf_ts_all$Date <- as.Date(inf_ts_all$Date)

#remove duplicate, more than one infiltration rate per a single date
inf_ts_all_uniq <- inf_ts_all %>%
  group_by(Date) %>%
  summarise(ave_rate = mean(Rate))

#time stamp monthly-assign first day of the month to the monthly stamp
inf_ts_all$Month_Yr <- format(as.Date(inf_ts_all$Date), "%Y-%m")
inf_ts_all$month <- ymd(paste(inf_ts_all$Month_Yr,"-01", sep = ""))

#average monthly
inf_ts_ave_monthly <- inf_ts_all %>%
  group_by(month) %>%
  summarise(ave_rate = mean(Rate))


### interpolation-there are some month with no infiltration data
date_index <- data.frame(month = seq(from = ymd("2015-04-01"), to = ymd("2022-05-01"), by = "1 month"))

#left join the complete set of months with the infiltration rates, this creates NA and gets filled with zoo::na.approx linear interpolation

ts_data <- date_index %>%
  left_join(inf_ts_ave_monthly, by = "month")

#interpolate missing values 
ts_data["inter_rate"] <- zoo::na.approx(ts_data$ave_rate)

#final data frame to turn into time-series object using ts()
ts_decom <- ts_data %>%
  select(month, inter_rate)

#turn the data into Time Series
ts_data <- ts(ts_decom$inter_rate, frequency = 12, start = c(2015,4))

#plot the series-time series plots are smooth lines
plot(ts_data)

#decompose using LOESS Method-break into seasonal, trend, remainder

decomposed <- stl(ts_data, s.window = "periodic")
# Access the seasonal, trend, and remainder components
seasonal <- decomposed$time.series[, "seasonal"]
trend <- decomposed$time.series[, "trend"]
remainder <- decomposed$time.series[, "remainder"]


#classic decompose into seasonal, trend, and random components
dec <- decompose(ts_data, type = "additive")
plot(dec$seasonal)
plot(dec$trend)
plot(dec$random)


#Forecasting using HoltWinters- seasonal data that propagates in additive way
train_data <- window(ts_data, end = c(2020,6)) 
  
test_data <- window(ts_data, start = c(2020,7))

# Fit the Holt-Winters model with additive seasonal components
hw_model <- HoltWinters(train_data, seasonal = "additive")

# Forecast future values using the fitted model
forecast_values <- forecast(hw_model, h = length(test_data))

forecast_values <- forecast(hw_model, h = 48)

plot(forecast_values)












