#Mann Kendall Tests for select long term SMPs-this is normalized by water temperature
# Farshad Ebrahimi, 6/5/2023

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
library(EnvStats)
library(readxl)
library(DBI)
# DB PG14
con <- dbConnect(odbc::odbc(), dsn = "mars14_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190)

# Residuals and model stats from Brian
infil_temp_models <- dbGetQuery(con, "SELECT * FROM data.tbl_infil_temp_models")


#system categories

System_Categories <- read_excel("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\System Categories\\System Categories_Long-Term Trend Analysis.xlsx")

#retain categories
System_Categories_trend <- System_Categories %>%
  select(system_id = `System ID`, categories = `SC Category for Trend Analysis`)
  
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
metric_residual["tau_estimate"] <- NA

output <- metric_residual[0,]

owid_unique <- metric_residual %>%
  select(ow_uid) %>%
  distinct()

for (i in 1:nrow(owid_unique)) {
  
  metric_df <- metric_residual %>%
    filter(ow_uid == owid_unique[i, ]) 
  
  kendall <- kendallTrendTest(residual ~ eventdatastart_edt , data = metric_df)
  
  metric_df["p_value"] <- kendall$p.value
  metric_df["tau_estimate"] <- kendall$estimate[1]
  
  output <- rbind(output, metric_df)
  
}

output <- output %>%
  select(ow_uid, p_value, tau_estimate) %>%
  distinct()
  
  dbWriteTable(con, SQL("data.tbl_infil_temp_norm_mk"), output)


