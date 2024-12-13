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
library(kable)
library(plyr)
library(tidyverse)
library(formattable)

#create negate of %in%
`%!in%` = Negate(`%in%`)


options(scipen = 999)


# DB PG14
con <- dbConnect(odbc::odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190)


# Residuals and model stats from Brian
infil_temp_models <- dbGetQuery(con, "SELECT * FROM metrics.tbl_infil_temp_models where model_type = 'linear'")

# Get OW_UID-System_ID
ow <- dbGetQuery(con, "SELECT *,admin.fun_smp_to_system(smp_id) as system_id FROM fieldwork.tbl_ow")


#get the design metrics to provide context
design_metrics <- dbGetQuery(con, "SELECT * FROM external.tbl_systembdv")

#system categories

System_Categories <- read_excel("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\System Categories\\System Categories_Long-Term Trend Analysis.xlsx", sheet = 2)

#years of data

years_data <- read.csv("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\summary_statistics\\monitoring_lengths.csv")


#smp type 
smp_types <- dbGetQuery(con, "SELECT * FROM external.tbl_smpbdv")


#retain categories
System_Categories_trend <- System_Categories[2:30,] %>%
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


#add the categories
kendal_categories <- output %>%
  inner_join(ow, by = "ow_uid") %>%
  inner_join(System_Categories_trend, by = "system_id") %>%
  mutate(trend = case_when(p_value <= 0.2 & slope_estimate > 0 ~ "Significant_Positive", 
                           p_value <= 0.2 & slope_estimate < 0 ~ "Significant_Negative",
                           p_value > 0.2 & slope_estimate > 0 ~ "Insignificant_Positive",
                           p_value > 0.2 & slope_estimate < 0 ~ "Insignificant_Negative")) %>%
  filter(categories != "Excluded")


#summarize results-significant positive trends
kendal_summary_sig_pos <- output %>%
  filter(p_value < 0.05 & slope_estimate > 0) %>%
  nrow()

#summarize results-significant negative trends
kendal_summary_sig_neg <- output %>%
  filter(p_value < 0.05 & slope_estimate < 0) %>%
  nrow()


# Create Table outputs and scatter plots
mk_table <- kendal_categories %>%
  select(smp_id, system_id, ow_uid, slope_estimate, p_value, categories)

mk_table$p_value <- round_any(mk_table$p_value, 0.001)

mk_table$slope_estimate <- round_any( mk_table$slope_estimate*3.15E+7 , 0.001)



mk_table_plot_df <- mk_table %>%
  mutate(trend = case_when(p_value <= 0.05 ~ "Statistically Significant", 
                           p_value > 0.05 ~ "Statistically Insignificant")) %>%
  arrange(slope_estimate)

# get the mean infiltration rates
ow_uid_rates <- infil_temp_models %>%
  select(ow_uid, infiltration_inhr) %>%
  na.omit() 


mean_rates <- ow_uid_rates %>%
  group_by(ow_uid) %>%
  summarise_at(vars(infiltration_inhr), list(ave_rate = mean))



mk_table_plot_df <- mk_table_plot_df %>%
  inner_join(mean_rates, by = "ow_uid") 

mk_table_plot_df$ave_rate <- round_any(mk_table_plot_df$ave_rate, 0.001)

mk_table_plot_df_norm <- mk_table_plot_df %>%
  mutate(norm = slope_estimate/ave_rate)

mk_table_plot_df_norm$norm <- round_any(mk_table_plot_df_norm$norm, 0.001)







### Create the table output with formattable package
table_df <- mk_table_plot_df_norm %>%
  inner_join(years_data, by = "system_id") %>%
  inner_join(smp_types, by = "smp_id") %>%
  select(smp_id, categories, slope_estimate, ave_rate ,norm, p_value) %>%
  arrange(norm)
#add asterisk to drainage wells
table_df <- table_df %>%
  mutate(smp_id = case_when(smp_id %in% c("1029-1-1", "1024-1-1", "1025-1-1") ~ paste0(smp_id, "*"),
                            smp_id %!in% c("1029-1-1", "1024-1-1", "1025-1-1") ~ smp_id))

names(table_df) <- c("SMP ID","Short-Circuiting (SC) Category", "Theil-Sen Slope (in/hr.yr)","Averaged Infiltration Rate (in/hr)", "Normalized Slope (1/yr)", "P-value")
#names(table_df) <- c("<center>SMP ID</center>", "<center>SMP Type</center>", "<center>SC Category</center>", "<center>Theil-Sen Slope (in/hr.yr)</center>", "<center>Averaged Rate (in/hr)</center>", "<center>Normalized Slope (1/yr)</center>", "<center>P-value</center>")


bold <- formatter(.tag = "span", style = function(x) style(
  display = "block", 
  padding = "0 4px", 
  font.weight = "bold"
))

costum_format <- formatter(.tag = "span", style = function(x) style(
  display = "block",
  padding = "0 4px",
  font.weight = "bold",
  `border-radius` = "4px",
  `background-color` = csscolor(gradient(abs(as.numeric(x)),  "white", "red")),
  color = "black"
  
))




costum_format_pval <- formatter(.tag = "span", style = function(x) style(
  display = "block", 
  padding = "0 4px", 
  font.weight = "bold",
  `border-radius` = "4px", 
  `background-color` = ifelse(x <= 0.05, "green","white") ,
  color = ifelse(x <= 0.05, "white","black")))


costum_format_sc <- formatter(.tag = "span", style = function(x) style(
  display = "block", 
  padding = "0 4px", 
  font.weight = "bold",
  `border-radius` = "4px", 
  `background-color` = ifelse(x == "No SC Confirmed" | x == "No SC Suspected" , "green","red2") ,
  color = ifelse(x == "No SC Confirmed" | x == "No SC Suspected", "white","black")
))

formattable(table_df, 
            align = c("c", "c", "c", "c", "c", "c"),
            list(
              "SMP ID" = bold,
              "Theil-Sen Slope (in/hr.yr)" = bold,
              "Averaged Infiltration Rate (in/hr)" = bold,
              "Normalized Slope (1/yr)" = costum_format,
              "P-value" =  bold,
              "Short-Circuiting (SC) Category" = bold ))





# figure 5
kw_df <- data.frame(Variable = c("Pretreatment Score", "Surface Inflow", "Sewer Connection", "Slow Release Orifice"),
                    `Kruskal-Wallis P-Value` = c(0.59, 0.90, 0.81, 0.69),
                    `Null Hypothesis Rejected?` = c("No", "No", "No", "No"))
names(kw_df) <- c("Variables", "Kruskal-Wallis P-Value", "Null Hypothesis Rejected?")
formattable(kw_df, align = c("c", "c", "c"))









