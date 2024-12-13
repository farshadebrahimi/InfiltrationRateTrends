#Mann Kendall Tests for select long term SMPs-this is normalized by water temperature
# Farshad Ebrahimi, 6/5/2023


library(plyr)
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
library(tidyverse)
library(formattable)
library(htmltools)
library(webshot)


options(scipen = 999)

# DB PG14
con <- dbConnect(odbc::odbc(), dsn = "mars14_datav2", MaxLongVarcharSize = 8190)

# Residuals and model stats from Brian
infil_temp_models <- dbGetQuery(con, "SELECT * FROM metrics.tbl_infil_temp_models where model_type = 'linear'")

# Get OW_UID-System_ID
ow <- dbGetQuery(con, "SELECT *,admin.fun_smp_to_system(smp_id) as system_id FROM fieldwork.tbl_ow")


#get the design metrics to provide context
design_metrics <- dbGetQuery(con, "SELECT * FROM external.tbl_systembdv")

#system categories

System_Categories <- read_excel("\\\\pwdoows\\OOWS\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\04 Analysis\\System Categories\\System Categories_Long-Term Trend Analysis.xlsx",
                                sheet = "System Variables")

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
  
### Write to db
  #dbWriteTable(con, SQL("data.tbl_linear_norm_mk"), output)

  #summarize results-significant positive trends
  kendal_summary_sig_pos <- output %>%
    filter(p_value < 0.05 & slope_estimate > 0) %>%
    nrow()
  
  #summarize results-significant negative trends
  kendal_summary_sig_neg <- output %>%
    filter(p_value < 0.05 & slope_estimate < 0) %>%
    nrow()
  
  
  
  #add the categories
  kendal_categories <- output %>%
    inner_join(ow, by = "ow_uid") %>%
    inner_join(System_Categories_trend, by = "system_id") %>%
    mutate(trend = case_when(p_value < 0.05 & slope_estimate > 0 ~ "sig_positive", 
                             p_value < 0.05 & slope_estimate < 0 ~ "sig_negative",
                             p_value > 0.05 & slope_estimate > 0 ~ "insig_positive",
                             p_value > 0.05 & slope_estimate < 0 ~ "insig_negative"))
  
 
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
    ggtitle("Breakdown of the Trends based on SC categories")
  
  
# Creating plots for the memo
  
  
  #add the categories
  kendal_categories <- output %>%
    inner_join(ow, by = "ow_uid") %>%
    inner_join(System_Categories_trend, by = "system_id") %>%
    mutate(trend = case_when(p_value < 0.05 & slope_estimate > 0 ~ "Significant_Positive", 
                             p_value < 0.05 & slope_estimate < 0 ~ "Significant_Negative",
                             p_value > 0.05 & slope_estimate > 0 ~ "Insignificant_Positive",
                             p_value > 0.05 & slope_estimate < 0 ~ "Insignificant_Negative")) %>%
    filter(categories != "Excluded")
  
  #TRIM AND WRITE TO csv
  temp_normalized_mk <- kendal_categories %>% 
    select(smp_id, ow_uid, slope_estimate_inhrsec = slope_estimate, intercept_estimate, tau, p_value, sc_category = categories)
  write.csv(x = temp_normalized_mk, file = paste0("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\04 Analysis\\temp_normalized_mk_",Sys.Date(),".csv"), row.names = FALSE)
  
  # Calculate the count of each trend within each category
  counts <- table(kendal_categories$categories, kendal_categories$trend)
  
  # Convert the counts into a dataframe
  counts_df <- as.data.frame.matrix(counts)
  
  # Reshape the dataframe for plotting
  counts_df <- as.data.frame(counts_df)
  counts_df$category <- row.names(counts_df)
  counts_df <- tidyr::gather(counts_df, key = "trend", value = "count", -category)
  
  # Create the stacked bar chart wit SC categories
  sc_barchart <- ggplot(counts_df, aes(x = category, y = count, fill = trend)) +
    geom_bar(stat = "identity") +
    labs(x = "Category", y = "Count", fill = "Trend")+
    scale_y_continuous(breaks = seq(0,15, by = 1))+
    theme(panel.grid.major.y = element_line(size = 1), panel.grid.minor.y = element_blank(), text = element_text(size = 20)) + 
    ggtitle("Breakdown of the Infiltration Rate Residual (in/hr) Mann-Kendall Trends based on Short-circuiting Categories")
  
  
  # Create Table outputs and scatter plots
  mk_table <- kendal_categories %>%
    select(smp_id, system_id, ow_uid, slope_estimate, p_value, categories)
  
  mk_table$p_value <- round_any(mk_table$p_value, 0.001)
  
  mk_table$slope_estimate <- round_any( mk_table$slope_estimate*3.15E+7 , 0.001)
  
  
  
  mk_table_plot_df <- mk_table %>%
    mutate(trend = case_when(p_value < 0.05 ~ "Statistically Significant", 
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
  
  
  #### Scattter Plot
  theilsen_plot <- ggplot(mk_table_plot_df, aes(x=factor(smp_id, level=mk_table_plot_df$smp_id), y=slope_estimate, fill = trend)) +
    geom_point(size=5, shape=23) +
    scale_y_continuous(breaks = seq(-1,1, by = 0.1)) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "red", size=2)+
    theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    labs(x = "SMP ID", y = "Slope of the Theil-Sen Line Estimate", fill = "Trend")+
    ggtitle("Slope of Theil-Sen Estimators (In/hr.Year)")

  ggplot(mk_table_plot_df, aes(x=factor(smp_id, level=mk_table_plot_df$smp_id), y=slope_estimate)) +
    geom_point(aes(color = categories, shape = trend), size = 5) +
    scale_color_manual(values=c('springgreen4','springgreen2', 'brown2', 'red4', 'tan2'))+
    scale_y_continuous(breaks = seq(-1,1, by = 0.1)) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "red", size=2)+
    theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    labs(x = "SMP ID", y = "Slope of the Theil-Sen Line Estimate", shape = "Trend", color = "SC Category")+
    ggtitle("Slope of Theil-Sen Estimators (In/hr.Year)")
  
  
  ### Create the table output with formattable package
  table_df <- mk_table_plot_df_norm %>%
    select(smp_id, categories, slope_estimate, ave_rate ,norm, p_value) %>%
    arrange(categories, norm)
  names(table_df) <- c("SMP ID","SC Category", "Theil-Sen Slope (in/hr.yr)","Average Observed Rate (in/hr)", "Normalized Slope (1/yr)", "P-value")
  
  

  
  costum_format <- formatter(.tag = "span", style = function(x) style(
    display = "block", 
    `font-weight` = "bold",
    padding = "0 4px", 
    `border-radius` = "4px", 
    `background-color` = csscolor(gradient(abs(as.numeric(x)), 
                                           customGreen0, customGreen)),
    # color = ifelse(x <= 0, "red","black")))
    color = ifelse(x <= 0, "black","black")))
  
  costum_format_pval <- formatter(.tag = "span", style = function(x) style(
    display = "block", 
    `font-weight` = "bold",
    padding = "0 4px", 
    `border-radius` = "4px", 
    color = ifelse(x <= 0.05, "black","black")))
  
  
  customGreen = "red"
  customGreen0 = "white"
  
  table_df$`Normalized Slope (1/yr)` <- round(table_df$`Normalized Slope (1/yr)`, 3)
  table_df <- table_df %>% dplyr::arrange(`Normalized Slope (1/yr)`)
  
  # Add asterisk for drainage well SMP's
  smp_ids <- c("9-1-1", "20-4-1",  "1029-1-1*", "171-2-1",
               "8-1-1", "10-1-1", "250-1-1",
               "18-1-1", "366-2-2", "1024-1-1*",
               "88-1-1", "187-3-3", "20-1-1",
               "20-8-1", "14-1-2", "9-2-1",
               "231-2-1", "1025-1-1*", "326-1-1",
               "1-3-1", "1-1-1", "179-5-1",
               "211-1-2", "1006-1-1")
  
  table_df$`SMP ID` <- smp_ids
  
  memo_table <- formattable(table_df, 
                            align =c("c","c","c"),
                            list("SMP ID" = costum_format_pval,
                                 "SC Category" = costum_format_pval,
                                 "Theil-Sen Slope (in/hr.yr)" = costum_format_pval,
                                 "Average Observed Rate (in/hr)" = costum_format_pval,
                                 "Normalized Slope (1/yr)" = costum_format,
                                 "P-value" =  costum_format_pval))
  
  # thank you: https://stackoverflow.com/questions/38833219/command-for-exporting-saving-table-made-with-formattable-package-in-r
  export_formattable <- function(f, file, width = "100%", height = NULL, 
                                 background = "white", delay = 0.2) {
          w <- as.htmlwidget(f, width = width, height = height)
          path <- html_print(w, background = background, viewer = NULL)
          url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
          webshot(url,
                  file = file,
                  selector = ".formattable_widget",
                  delay = delay)
  }
  
  export_formattable(memo_table, "\\\\pwdoows\\OOWS\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\05 Deliverables\\02 Final Deliverables\\03 Plots\\Theil-Sen_ESL.png")
  
  
  #### Bar chart reflecting inflow/outflow pathways 
  # Calculate the count of each trend within each category
  
  System_Categories_flow <- System_Categories[,c(1,9,10)] 
  names(System_Categories_flow) <- c("system_id","inflow","outflow")
  inflow_df <- mk_table_plot_df_norm %>% 
    inner_join(System_Categories_flow, by = "system_id") %>%
    mutate(inflow =  case_when(inflow == "Subsurface" ~ "Subsurface Only", 
                                       inflow != "Subsurface" ~ "With Surface")) %>%
    mutate(outflow =  case_when(outflow == "Sewer" ~ "Sewer", 
                                outflow != "Sewer" ~ "Non-Sewer")) %>%
    mutate(trend = case_when(p_value < 0.05 & slope_estimate > 0 ~ "Significant_Positive", 
                             p_value < 0.05 & slope_estimate < 0 ~ "Significant_Negative",
                             p_value > 0.05 & slope_estimate > 0 ~ "Insignificant_Positive",
                             p_value > 0.05 & slope_estimate < 0 ~ "Insignificant_Negative")) %>%
    mutate(flow = paste(inflow, outflow, sep = ", "))
    
  counts <- table(inflow_df$flow, inflow_df$trend)
  
  # Convert the counts into a dataframe
  counts_df <- as.data.frame.matrix(counts)
  
  # Reshape the dataframe for plotting
  counts_df <- as.data.frame(counts_df)
  counts_df$category <- row.names(counts_df)
  counts_df <- tidyr::gather(counts_df, key = "trend", value = "count", -category)
  
  # Create the stacked bar chart wit SC categories
  sc_barchart <- ggplot(counts_df, aes(x = category, y = count, fill = trend)) +
    geom_bar(stat = "identity") +
    labs(x = "Category", y = "Count", fill = "Trend")+
    scale_y_continuous(breaks = seq(0,15, by = 1))+
    theme(panel.grid.major.y = element_line(size = 1), panel.grid.minor.y = element_blank(), text = element_text(size = 20, face = "bold")) +
    ggtitle("Breakdown of MK Trends by The Inflow/Outflow Pathways")
  
