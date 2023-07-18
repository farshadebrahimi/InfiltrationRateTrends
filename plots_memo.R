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
  library(kable)
  library(plyr)
  library(tidyverse)
  library(formattable)
  
  #create negate of %in%
  `%!in%` = Negate(`%in%`)


  options(scipen = 999)

# DB PG14
  con <- dbConnect(odbc::odbc(), dsn = "mars14_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190)

# Residuals and model stats from Brian
  infil_temp_models <- dbGetQuery(con, "SELECT * FROM metrics.tbl_infil_temp_models where model_type = 'linear'")

# Get OW_UID-System_ID
  ow <- dbGetQuery(con, "SELECT *,admin.fun_smp_to_system(smp_id) as system_id FROM fieldwork.tbl_ow")


#get the design metrics to provide context
  design_metrics <- dbGetQuery(con, "SELECT * FROM external.tbl_systembdv")

#system categories

  System_Categories <- read_excel("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\System Categories\\System Categories_Long-Term Trend Analysis.xlsx")

#years of data
  
  years_data <- read.csv("\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\summary_statistics\\monitoring_lengths.csv")
  
  
#smp type 
  smp_types <- dbGetQuery(con, "SELECT * FROM external.tbl_smpbdv")
  
  
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


  

### Stacked SC bar chart

  #add the categories
  kendal_categories <- output %>%
    inner_join(ow, by = "ow_uid") %>%
    inner_join(System_Categories_trend, by = "system_id") %>%
    mutate(trend = case_when(p_value <= 0.2 & slope_estimate > 0 ~ "Significant_Positive", 
                             p_value <= 0.2 & slope_estimate < 0 ~ "Significant_Negative",
                             p_value > 0.2 & slope_estimate > 0 ~ "Insignificant_Positive",
                             p_value > 0.2 & slope_estimate < 0 ~ "Insignificant_Negative")) %>%
    filter(categories != "Excluded")
  
  # Calculate the count of each trend within each category
  counts <- table(kendal_categories$categories, kendal_categories$trend)
  
  # Convert the counts into a dataframe
  counts_df <- as.data.frame.matrix(counts)
  
  # Reshape the dataframe for plotting
  counts_df <- as.data.frame(counts_df)
  counts_df$category <- row.names(counts_df)
  counts_df <- tidyr::gather(counts_df, key = "trend", value = "count", -category)
  
  # Create the stacked bar chart wit SC categories
  stacked_sc_barchart <- ggplot(counts_df, aes(x = category, y = count, fill = factor(trend, levels=c("Significant_Positive","Insignificant_Positive","Insignificant_Negative","Significant_Negative")))) +
    geom_bar(stat = "identity") +
    labs(x = "Category", y = "Count", fill = "Trend")+
    scale_y_continuous(breaks = seq(0,15, by = 1))+
    scale_fill_viridis_d(direction = -1)+
    theme(panel.grid.major.y = element_line(size = 1), panel.grid.minor.y = element_blank(), text = element_text(size = 20)) + 
    ggtitle("Breakdown of the Infiltration Rate Residual (in/hr) Mann-Kendall Trends based on Short-circuiting Categories")
  

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
    inner_join(years_data, by = "system_id") %>%
    inner_join(smp_types, by = "smp_id") %>%
    select(smp_id, categories, slope_estimate, ave_rate ,norm, p_value) %>%
    arrange(norm)
  #add asterisk to drainage wells
  table_df <- table_df %>%
    mutate(smp_id = case_when(smp_id %in% c("1029-1-1", "1024-1-1", "1025-1-1") ~ paste0(smp_id, "*"),
                              smp_id %!in% c("1029-1-1", "1024-1-1", "1025-1-1") ~ smp_id))
  
  names(table_df) <- c("SMP ID","SC Category", "Theil-Sen Slope (in/hr.yr)","Averaged Rate (in/hr)", "Normalized Slope (1/yr)", "P-value")
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
                "Averaged Rate (in/hr)" = bold,
                "Normalized Slope (1/yr)" = costum_format,
                "P-value" =  costum_format_pval,
                "SC Category" = costum_format_sc ))
  
  
  

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
    theme(panel.grid.major.y = element_line(size = 1), panel.grid.minor.y = element_blank(), text = element_text(size = 20)) +
    ggtitle("Breakdown of MK Trends by The Inflow/Outflow Pathways")
  
  
### Adding loading ratio scatter plot
  table_all <- mk_table_plot_df %>%
    left_join(design_metrics, by="system_id") %>%
    mutate(loading_ratio = sys_lrtotalda_ft2 )
  
  ggplot(table_all, aes(x=slope_estimate, y=loading_ratio)) +
    geom_point(aes(color = categories, shape = trend), size = 5) +
    scale_color_manual(values=c('springgreen4','springgreen2', 'brown2', 'red4', 'tan2'))+
    theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    labs(x = "Theil-Sen Slope Estimate", y = "Total Loading Ratio ", shape = "Trend", color = "SC Category")+
    ggtitle("Loading Ratio VS Slope of Theil-Sen Estimators (In/hr.Year)")
  

### Rings of analysis
# Calculate the count of each trend within each category
  
  kendal_categories_inner <- kendal_categories %>%
    mutate(analysis_ring = case_when(categories == "No SC Confirmed" ~ "Group 1")) %>%
    filter(analysis_ring == "Group 1" )
  
  
  kendal_categories_middle <- kendal_categories %>%
    mutate(analysis_ring = case_when(categories == "No SC Confirmed" | categories == "No SC Suspected" | categories == "SC Confirmed (low)"  ~ "Group 2")) %>%
    filter(analysis_ring == "Group 2")
  
  kendal_categories_outer <- kendal_categories %>%
    mutate(analysis_ring = "Group 3") %>%
    filter(analysis_ring == "Group 3")
  
  
  kendal_categories_all <- rbind(kendal_categories_inner, kendal_categories_middle, kendal_categories_outer)
  
  counts <- table(kendal_categories_all$analysis_ring, kendal_categories_all$trend)
  
  # Convert the counts into a dataframe
  counts_df <- as.data.frame.matrix(counts)
  
  # Reshape the dataframe for plotting
  counts_df <- as.data.frame(counts_df)
  counts_df$category <- row.names(counts_df)
  counts_df <- tidyr::gather(counts_df, key = "trend", value = "count", -category)

# Create the stacked bar chart wit SC categories
  ring_barchart <- ggplot(counts_df, aes(x = category, y = count, fill = factor(trend, levels=c("Significant_Positive","Insignificant_Positive","Insignificant_Negative","Significant_Negative")))) +
    geom_bar(stat = "identity") +
    labs(x = "Ring of Analysis", y = "Count", fill = "Trend")+
    scale_y_continuous(breaks = seq(0,26, by = 1))+
    theme(panel.grid.major.y = element_line(size = 1), panel.grid.minor.y = element_blank(), text = element_text(size = 25)) + 
    scale_fill_viridis_d(direction = -1)+
    ggtitle("Breakdown of the Infiltration Rate Residual (in/hr) Mann-Kendall Trends")
  

