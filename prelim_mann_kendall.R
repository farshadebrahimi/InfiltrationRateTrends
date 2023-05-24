#Mann Kendall Tests for select long term SMPs-this is the simple version on ow_uid/season combo. not seasonal version
# Farshad Ebrahimi, 5/22/2023

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

# DB PG14
con <- dbConnect(odbc::odbc(), dsn = "mars14_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190)

#Get metrics

sc_metrics <- read.csv("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\Infiltration Rate Recalcs\\metrics_with_recalcs.csv")


#get the data scope
data_scope <- read.csv("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\summary_statistics\\system_seasons_for_analysis.csv")


#get the rain event time and date

smp_radarcell <- dbGetQuery(con, "SELECT * FROM data.tbl_radar_event")
metric_rain <- sc_metrics %>%
  inner_join(smp_radarcell, by = "radar_event_uid" ) %>%
  select(ow_uid, radar_event_uid, eventdatastart_edt, infiltration_inhr)



#add season
metric_rain["season"] <- time2season(metric_rain$eventdatastart_edt, out.fmt = "seasons")


#Create the comprehensive DF for kendall test

metric_comp_df <- data_scope %>%
  inner_join(metric_rain, by = c("ow_uid","season")) %>%
  na.omit() %>%
  distinct()

#do count to double check
metric_comp_df_stats <- metric_comp_df %>%
  group_by(ow_uid, season) %>%
  summarise(ow_uid, season,Infil_count, count_season = n())
  


# kendall test-preparing the dataframes
metric_comp_df["p_value"] <- NA
metric_comp_df["tau_estimate"] <- NA

output <- metric_comp_df[0,]

owid_season_unique <- metric_comp_df %>%
  select(ow_uid, season) %>%
  distinct()

for (i in 1:nrow(owid_season_unique)) {
  
  temp_df <- owid_season_unique[i, ]

  metric_df <- metric_comp_df %>%
    filter(ow_uid == temp_df$ow_uid & season == temp_df$season) 
    
  kendall <- kendallTrendTest(infiltration_inhr ~ eventdatastart_edt , data = metric_df)
  
  metric_df["p_value"] <- kendall$p.value
  metric_df["tau_estimate"] <- kendall$estimate[1]
  
  output <- rbind(output, metric_df)
  

}

#summarize results-significant positive trends
kendal_summary_sig_pos <- output %>%
  select(system_id, ow_uid, season, p_value, tau_estimate) %>%
  distinct() %>%
  filter(p_value < 0.05 & tau_estimate > 0) %>%
  nrow()

#summarize results-significant negative trends
kendal_summary_sig_neg <- output %>%
  select(system_id, ow_uid, season, p_value, tau_estimate) %>%
  distinct() %>%
  filter(p_value < 0.05 & tau_estimate < 0) %>%
  nrow()

write.csv(output, file = "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\summary_statistics\\mann_kendall_prelim.csv", row.names = FALSE)




