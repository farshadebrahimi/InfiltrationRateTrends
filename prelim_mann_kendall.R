#Mann Kendall Tests for select long term SMPs-Seasonal and the simple version on ow_uid/season combo
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
library(readxl)

# DB PG14
con <- dbConnect(odbc::odbc(), dsn = "mars14_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190)

#Get metrics

sc_metrics <- read.csv("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\Infiltration Rate Recalcs\\metrics_with_recalcs.csv")


#get the data scope
data_scope <- read.csv("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\summary_statistics\\system_seasons_for_analysis.csv")

#system categories

System_Categories <- read_excel("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\System Categories\\System Categories_Long-Term Trend Analysis.xlsx")

#retain categories
System_Categories_trend <- System_Categories %>%
  select(system_id = `System ID`, categories = `SC Category for Trend Analysis`)
  
#get the rain event time and date

smp_radarcell <- dbGetQuery(con, "SELECT * FROM data.tbl_radar_event")
metric_rain <- sc_metrics %>%
  inner_join(smp_radarcell, by = "radar_event_uid" ) %>%
  select(ow_uid, radar_event_uid, eventdatastart_edt,eventdataend_edt, infiltration_inhr)


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

#write the summary
kendal_summary <- output %>%
  select(system_id, ow_uid, season, p_value, tau_estimate) %>%
  distinct()
write.csv(kendal_summary, file = "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\summary_statistics\\mann_kendall_simple_summary.csv", row.names = FALSE)



#seasonal version of the test

metric_comp_df["year"] <- year(metric_comp_df$eventdatastart_edt)
output_seasonal <- metric_comp_df[0,]




#remove the data that has a single type of data-only summers, only winters etc.

single_season_data <- owid_season_unique %>%
  group_by(ow_uid) %>%
  summarise(ow_uid, count_season = n()) %>%
  filter(count_season == 1)

#also remove a ow_id if there is not at least two years of data for a given season
single_year_season <- metric_comp_df %>%
  select(ow_uid, season, year) %>%
  distinct() %>%
  group_by(ow_uid, season) %>%
  summarise(ow_uid, season, counting = n()) %>%
  distinct() %>%
  filter(counting == 1)


#remove the single season ow_uid

metric_comp_df <- metric_comp_df %>%
  anti_join(single_season_data , by = "ow_uid") %>%
  anti_join(single_year_season , by = "ow_uid")

# get unique ids
owid_unique <- metric_comp_df %>%
  select(ow_uid) %>%
  distinct()





for (i in 1:nrow(owid_unique)) {
  
  temp_df <- owid_unique[i, ]
  
  metric_df <- metric_comp_df %>%
    filter(ow_uid == temp_df) 
  
  kendall <- kendallSeasonalTrendTest(infiltration_inhr ~ season + year, data = metric_df)
  
  metric_df["p_value"] <- kendall$p.value[2]
  metric_df["tau_estimate"] <- kendall$estimate[1]
  
  output_seasonal <- rbind(output_seasonal, metric_df)
  
  
}

#summarize results-significant positive trends
kendal_seasonal_summary_sig_pos <- output_seasonal %>%
  select(system_id, ow_uid, p_value, tau_estimate) %>%
  distinct() %>%
  filter(p_value < 0.05 & tau_estimate > 0) %>%
  nrow()

#summarize results-significant negative trends
kendal_seasonal_summary_sig_neg <- output_seasonal %>%
  select(system_id, ow_uid, p_value, tau_estimate) %>%
  distinct() %>%
  filter(p_value < 0.05 & tau_estimate < 0) %>%
  nrow()

write.csv(output_seasonal, file = "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\summary_statistics\\seasonal_mann_kendall_prelim.csv", row.names = FALSE)

#write summary
kendal_summary_seasonal <- output_seasonal %>%
  select(system_id, ow_uid, p_value, tau_estimate) %>%
  distinct()


write.csv(kendal_summary_seasonal, file = "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\summary_statistics\\kendal_summary_seasonal.csv", row.names = FALSE)

#add the categories
kendal_categories <- kendal_summary_seasonal %>%
  inner_join(System_Categories_trend, by = "system_id") %>%
  mutate(trend = case_when(p_value < 0.05 & tau_estimate > 0 ~ "sig_positive", 
                           p_value < 0.05 & tau_estimate < 0 ~ "sig_negative",
                           p_value > 0.05 & tau_estimate > 0 ~ "insig_positive",
                           p_value > 0.05 & tau_estimate < 0 ~ "insig_negative"))

# Calculate the count of each trend within each category
counts <- table(kendal_categories$categories, kendal_categories$trend)

# Convert the counts into a dataframe
counts_df <- as.data.frame.matrix(counts)

# Reshape the dataframe for plotting
counts_df <- as.data.frame(counts_df)
counts_df$category <- row.names(counts_df)
counts_df <- tidyr::gather(counts_df, key = "trend", value = "count", -category)

# Create the stacked bar chart
ggplot(counts_df, aes(x = category, y = count, fill = trend)) +
  geom_bar(stat = "identity") +
  labs(x = "Category", y = "Count", fill = "Trend")+
  scale_y_continuous(breaks = seq(0,15, by = 1))+
  theme(panel.grid.major.y = element_line(size = 1))+
  ggtitle("Break Down of the Trends based on SC categories")



