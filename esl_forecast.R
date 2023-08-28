#ESL FORECAST-Running Theil-sen, ARIMA and HoltWinters for the infiltration rates and comparing performance levels
#Create and save three compartment plots
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
library(mblm)
library(gganimate)
library(ggpubr)
library(plotly)
library(boot)
library(formattable)
library(plyr)
library("ggrepel")




options(scipen = 999)

# DB PG14
con <- dbConnect(odbc::odbc(), dsn = "mars14_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190)

### greenit tables for completion date to estimate age of smp
smpbdv <- dbGetQuery(con, "SELECT * from external.tbl_smpbdv")
cipit <- dbGetQuery(con, "SELECT * from external.tbl_cipit_project")


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
    select(ow_uid, smp_id, eventdatastart_edt, infiltration_inhr, fitted_value, residual) %>%
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

  
  
  #Create theil sen model plots
  ow_list <- infil_esl_data %>%
    select(smp_id, ow_uid) %>%
    distinct()
  
  for(i in 1:nrow(ow_list)) {
    
    temp <- infil_esl_data %>%
      filter(ow_uid == ow_list[i,]$ow_uid) 
    
    theilson <- output %>%
      filter(ow_uid == ow_list[i,]$ow_uid)
    
    #Theil-Sen model
    # temp_theilsen <- temp
    # temp_theilsen$eventdatastart_edt <- as.numeric(temp_theilsen$eventdatastart_edt)
    # model= mblm(infiltration_inhr ~ eventdatastart_edt, data=temp_theilsen)
    # summary(model)
    # Sum = summary(model)$coefficients
    
    
    infil_plot <- ggplot(temp, aes(temp$eventdatastart_edt, temp$infiltration_inhr))+
        geom_point()+
        labs(x = "", y = "Raw Inf. Rate (in/hr)")+
        geom_point(col = "black") +
      theme(axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 11),
            axis.title.y = element_text(size = 11),
            legend.position = "top",
            title = element_text(size = 16),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12))+
        #geom_abline(intercept = Sum[1], slope = Sum[2], color="blue", size=1.2) +
        ggtitle(paste0("Theil-Sen Model Fitting of Infiltration Rates (in/hr) VS Time for ", "SMP_ID = " , ow_list[i,]$smp_id," ( OW_UID = ",ow_list[i,]$ow_uid,")"))
    
    fit_plot <- ggplot(temp, aes(temp$eventdatastart_edt, temp$fitted_value))+
      geom_point()+
      theme(axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 11),
            axis.title.y = element_text(size = 11),
            legend.position = "top",
            title = element_text(size = 16),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12))+
      geom_point(col = "blue") +
      labs(x = "", y = "Fitted Inf. Rate (in/hr) by Temperature")

    resid_plot <- ggplot(temp, aes(temp$eventdatastart_edt, temp$residual))+
      geom_point()+
      geom_point(col = "red") +
      theme(axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 11),
            axis.title.y = element_text(size = 11),
            legend.position = "top",
            title = element_text(size = 16),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12))+
      geom_abline(intercept = theilson$intercept_estimate, slope = theilson$slope_estimate, color="blue", size=1.2) +
      labs(x = "Time", y = "Residual Inf. Rate (in/hr)")

    
      plot <- ggarrange(infil_plot, fit_plot, resid_plot, ncol = 1, nrow = 3)
    
      #ggplot2::ggsave( paste0("//pwdoows/oows/Watershed Sciences/GSI Monitoring/06 Special Projects/52 Long-Term GSI Performance Trends/Analysis/Theil-Sen Plots", "/", paste(ow_list[i,]$smp_id, ow_list[i,]$ow_uid, sep = "_"),".png"), plot = plot, width = 14, height = 8)
      
  }
  
  ### estimate system age 
  ow_list <- smpbdv %>%
    inner_join(cipit, by = "worknumber") %>%
    inner_join(ow_list, by = "smp_id") %>%
  select(smp_id, ow_uid, construction_complete_date)
  
  
  
  
  ### Estimate ESLs based on Theil-Sen, get the average of infiltration rate after removing outliers and devide by slope
  esl_estimates <- infil_esl_data %>%
    inner_join(output, by = "ow_uid") %>%
    select(smp_id, ow_uid, eventdatastart_edt, infiltration_inhr, slope_estimate)
  
  esl_df <- ow_list
  esl_df["years_span_from_built"] <- NA
  esl_df["years_to_zero"] <- NA
  esl_df["final_esl_yr"] <- NA
  
  final_df <- esl_df[0,] %>%
    select(-construction_complete_date)
  
  for (i in 1:nrow(ow_list)) {
    
    temp <- esl_estimates %>%
      filter(ow_uid == ow_list[i,]$ow_uid) 
    
    temp$years_span_from_built <- as.numeric(as.Date(max(temp$eventdatastart_edt))-ow_list[i,]$construction_complete_date)/365
    temp$years_to_zero <- abs(mean(temp$infiltration_inhr)/temp[1,]$slope_estimate)/(365*24*60*60)
    temp$final_esl_yr <- temp$years_span_from_built + temp$years_to_zero 
    
    temp <- temp %>%
      select(smp_id, ow_uid, years_span_from_built, years_to_zero, final_esl_yr) %>%
      distinct
    
    final_df <- rbind(final_df, temp)
    
  }
  
  final_df <- final_df %>%
    inner_join(ow_list, by = c("smp_id", "ow_uid"))
  
  ### bootstrapping
  #define function to calculate mean
  meanFunc <- function(x,i){mean(x[i])}
  
  # calculate standard error using 100 bootstrapped samples
  boot(final_df$final_esl_yr, meanFunc, 1000)
  
  
  
  # Round numbers
  final_df$final_esl_yr <- round_any(final_df$final_esl_yr, 0.01)
  
  final_df <- final_df %>%
    inner_join(smpbdv, by = "smp_id")
    
  # Trimming 
  memo_table <- final_df %>%
    arrange(final_esl_yr) %>%
    select(`SMP ID` = smp_id, Type = smp_smptype.x ,`Construction Date` = construction_complete_date, `Infiltration Rate to Zero (Years)`= final_esl_yr) 
  
  final_df <- final_df %>%
    arrange(final_esl_yr) 

  ggplot(final_df, aes(x=factor(smp_id, level=final_df$smp_id), y=final_esl_yr)) +
    geom_point( size = 4) +
    geom_hline(yintercept= mean(final_df$final_esl_yr), linetype="dashed", 
               color = "red", size=2) +
    scale_y_continuous(breaks = seq(0, 150, by = 10)) +
    labs(x = "SMP ID", y = "Infiltration Rate to Zero (Years)") +
    theme(panel.grid.major.y = element_line(size = 1), panel.grid.minor.y = element_blank(), text = element_text(size = 20)) +
    geom_text(x= "9-1-1", y=50, label="Average = 43.86 Years", size = 8 )
    
    
  
  

  
  # Generate the table using formattable
  formattable(memo_table, align = c("c", "c", "c", "c"))
  
  
  
  