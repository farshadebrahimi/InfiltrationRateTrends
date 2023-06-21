#### Summary graphs, stats, and data cleaning
#### For systems to include in Mann-Kendall
#### Infiltration rate analysis
#### Written by: BPC

#### 0.5 library packages ####


#standard
library(tidyverse)
library(ggplot2)
library(magrittr)

#database
library(odbc)
library(DBI)

#plot colors
library(wesanderson)

#timeseries stuff
library(lubridate)
library(tsibble)
library(feasts)
library(hydroTSM)



#### 1.0 Read metrics, system storms, database ####

file_folder <- "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\48 Short-Circuiting GSI Data Analysis\\Calculation Phase\\"

sys_storms <- read.csv(paste0(file_folder,"system_storms.csv"))

metrics_raw <- read.csv(paste0(file_folder,"Metrics Calculations\\2023-01-03\\metrics.csv"))

metrics_raw$system_id <- gsub(pattern = '-\\d+$', x = metrics_raw$smp_id, replacement = '')
metrics <- metrics_raw %>% unique
metrics <- metrics %>% dplyr::select(-sc_category)

metrics <- metrics %>% dplyr::mutate(sys_storm_uid = paste0(system_id,"-",radar_event_uid)) 
sys_storms <- sys_storms %>% dplyr::mutate(sys_storm_uid = paste0(system_id,"-",radar_event_uid))


#new storms from Taylor's recalculation
new_storms_file <- "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\Infiltration Rate Recalcs\\920recalcs.csv"

new_storms_raw <- read.csv(new_storms_file)

# some DB stuff
mars_con <- dbConnect(odbc::odbc(), "mars14_data")

# details for rainfall events


radar_events <- dbGetQuery(mars_con, paste0("SELECT * FROM data.tbl_radar_event
                                      WHERE radar_event_uid IN (",
                                      paste(unique(metrics$radar_event_uid), collapse = ","),")"))

# error look ups
error_names <- dbGetQuery(mars_con, "SELECT * FROM metrics.tbl_error_lookup")


# full monitoring record
monitoring_periods <- dbGetQuery(mars_con,
                                 paste0(
                                 "SELECT (collection_dtime_est - deployment_dtime_est) as deploy_days, * FROM fieldwork.tbl_deployment
                                 WHERE ow_uid IN (",
                                 paste(unique(metrics$ow_uid), collapse = ",")
                                 ,
                                 ")
                                 AND sensor_purpose = 2
                                 AND download_error IS NOT TRUE
                                 AND (long_term_lookup_uid = 1 OR long_term_lookup_uid = 2)"))


#### 2.0 Data Manipulation ####

##### 2.1 Handle new metric calculations from Taylor for -920 events #####
# unique variable for ow_event
new_storms <- new_storms_raw %>% dplyr::mutate(ow_event = paste0(ow_uid," ",radar_event_uid))
metrics <- metrics %>% dplyr::mutate(ow_event = paste0(ow_uid," ",radar_event_uid))


# are all the new station storms in metrics?

if(sum(new_storms$ow_event %in% metrics$ow_event) == length(new_storms$ow_event)){
  print("Yes, all the new storms are in metrics")
}


#update the results for those storms


metrics_keep <- anti_join(metrics, new_storms, by = "ow_event")

# limit to needed fields
metrics_keep <- metrics_keep %>% dplyr::select(ow_uid, ow_suffix, radar_event_uid, ow_event,
                                               draindown_hr, draindown_error,
                                               infiltration_inhr, infiltration_error,
                                               system_id, sys_storm_uid)

#format new storms to match metrics_keep format

# add error columns
new_storms$infiltration_error <- new_storms$infiltration_inhr
new_storms$draindown_error <- new_storms$draindown_hr

#only include errors in error column
new_storms$infiltration_error[new_storms$infiltration_error > 0] <- NA
new_storms$draindown_error[new_storms$draindown_error > 0] <- NA

#only include non-errors in value column
new_storms$infiltration_inhr[new_storms$infiltration_inhr < 0] <- NA
new_storms$draindown_hr[new_storms$draindown_hr < 0] <- NA


# add missing columns to new_storms: system_id, sys_storm_uid, sc_category
new_storms <- new_storms %>% dplyr::left_join(metrics[,c("ow_event","system_id","sys_storm_uid", "ow_suffix")], by = "ow_event") %>%
                             dplyr::select(-percentstorageused_peak, -percentstorageused_relative)

# And now all columns match, we can combine the two
metrics <- rbind(metrics_keep, new_storms)


##### 2.2 grab monitoring periods; add to existing metrics #####

monitoring_periods <- monitoring_periods %>% mutate(deploy_days = gsub(deploy_days, pattern = " day.*", replacement = "")) %>% 
                      dplyr::filter(!is.na(deploy_days) & grepl(monitoring_periods$deploy_days, pattern = ":") == FALSE) %>%
                      dplyr::mutate(deploy_days = as.integer(deploy_days))

# sum by ow_uid
ow_days <- monitoring_periods %>% dplyr::group_by(ow_uid) %>%
                       summarise(MonLengthDays = sum(deploy_days),
                                 MonLengthYears = MonLengthDays/365.25) %>% ungroup()

sys_mon_length <- ow_days %>% left_join(distinct(metrics[,c("ow_uid","system_id")]), by = "ow_uid") %>%
                        group_by(system_id) %>%
                        summarise(SysMonLengthDays = max(MonLengthDays),
                                  SysMonLengthYears  = round(SysMonLengthDays/365.25, 2))

# create histogram of subsurface, infiltrating systems
sub_infil_list <- dbGetQuery(mars_con,
                       "WITH cwl AS (
                        	SELECT distinct ow_uid from data.tbl_ow_leveldata_raw),
                        	gi AS (
                        	SELECT * FROM external.viw_greenit_unified)
                        
                        SELECT distinct admin.fun_smp_to_system(o.smp_id) as system_id, gi.lined, gi.surface FROM fieldwork.tbl_ow o
                        LEFT JOIN cwl
                        ON  o.ow_uid = cwl.ow_uid
                        LEFT JOIN gi
                        ON gi.smp_id = o.smp_id
                        WHERE cwl.ow_uid IS NOT NULL
                        
                        AND o.smp_id LIKE '%-%'
                        AND gi.lined = FALSE
                        AND gi.surface = FALSE
                        ")

hist_sys <- sys_mon_length %>% dplyr::filter(system_id %in% sub_infil_list$system_id)

hist(hist_sys$SysMonLengthYears, ylab = "System Count", xlab = "Monitoring Length (Years)", main = "Histogram of System Monitoring Length")

# trim categories
cats <- sys_storms %>% dplyr::filter(batch_uid == max(batch_uid, na.rm = TRUE)) %>% dplyr::select(sys_storm_uid, sc_category)


#assign to metrics

metrics <- metrics %>% left_join(cats, by = "sys_storm_uid")

metrics %>% group_by(sc_category) %>% summarize(Storm_Count = n(),
                                                System_Count = length(unique(system_id)),
                                                System_infil_count = length(!is.na(infiltration_inhr)),
                                                Storms_per_system = n()/length(unique(system_id)))



category_sum <- metrics %>% group_by(sc_category) %>% summarize(Storm_Count = n(),
                                                System_Count = length(unique(system_id)),
                                                Infil_count = sum(!is.na(infiltration_inhr)),
                                                Infil_error_count = sum(!is.na(infiltration_error)),
                                                Storms_per_system = n()/length(unique(system_id)),
                                                Valid_infil_per_system = sum(!is.na(infiltration_inhr))/length(unique(system_id)))


ow_infil <- metrics %>% group_by(ow_uid) %>% summarize(Storm_Count = n(),
                                              Infil_count = sum(!is.na(infiltration_inhr)),
                                              Percent_storms_with_good_infil = Infil_count/Storm_Count,
                                              Infil_error_count = sum(!is.na(infiltration_error)),
                                              Infil_NoDescendingLimb_er = sum(infiltration_error == -900, na.rm = TRUE),
                                              Infil_RisingLimb_er = sum(infiltration_error == -910, na.rm = TRUE),
                                              Infil_RainDuringLimb_er = sum(infiltration_error == -920, na.rm = TRUE),
                                              Infil_NegligibleRate_er = sum(infiltration_error == -930, na.rm = TRUE)) %>%
  
                        left_join(distinct(metrics[,c("ow_uid","ow_suffix","system_id")]), by = "ow_uid") %>%
                        left_join(ow_days, by = "ow_uid") %>%
                        filter(grepl(ow_suffix, pattern = "OW")) %>%
                        mutate(Infil_per_year = Infil_count/MonLengthYears)
  



ow_draindown <- metrics %>% group_by(ow_uid) %>% summarize(Storm_Count = n(),
                                                               draindown_count = sum(!is.na(draindown_hr)),
                                                               Percent_storms_with_good_DD = draindown_count/Storm_Count,
                                                               DD_error_count = sum(!is.na(draindown_error)),
                                                               DD_NoResponse_er = sum(draindown_error == -810, na.rm = TRUE),
                                                               DD_NoReturnToBaseline_er = sum(draindown_error == -820, na.rm = TRUE),
                                                               DD_IncreaseDuringLimb_er = sum(draindown_error == -830, na.rm = TRUE),
                                                               DD_WLDoesNotDropBelowBaseline_er = sum(draindown_error == -840, na.rm = TRUE)) %>%
  
                            left_join(distinct(metrics[,c("ow_uid","ow_suffix","system_id")]), by = "ow_uid") %>%
                            left_join(ow_days, by = "ow_uid") %>%
                            filter(grepl(ow_suffix, pattern = "OW")) %>%
                            mutate(DD_per_year = draindown_count/MonLengthYears)




folder_loc <- "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\summary_statistics\\"
# Write output
write.csv(x = ow_infil,
          file = paste0(folder_loc,"ow_infiltration_summary.csv"))

write.csv(x = ow_draindown,
          file = paste0(folder_loc,"ow_draindown_summary.csv"))



#summary stat
sys_list <- ow_infil %>% dplyr::filter(Infil_count >= 50 & MonLengthYears >= 1.5) %>% 
dplyr::select(system_id) %>% unique


#### 3.0 Visualizations ####

##### 3.1 Summary graphs for "good" systems/ows ##### 

#summary by system
good_sum_graph_df <- ow_infil %>% dplyr::filter(system_id %in% sys_list$system_id) %>% 
                             dplyr::select(ow_uid,system_id,Infil_count,
                                           Infil_NoDescendingLimb_er,
                                           Infil_RisingLimb_er,
                                           Infil_RainDuringLimb_er,
                                           Infil_NegligibleRate_er) %>%
                            dplyr::group_by(system_id) %>%
                            dplyr::mutate(maxinfilcount = max(Infil_count, na.rm = TRUE)) %>%
                            dplyr::filter(Infil_count == maxinfilcount) %>% ungroup() %>%
                            dplyr::select(-maxinfilcount) %>%
                            dplyr::group_by(system_id) %>%
                            tidyr::gather(key = "calculationResult",
                                          value = "Count",
                                          Infil_count,
                                          Infil_NoDescendingLimb_er,
                                          Infil_RisingLimb_er,
                                          Infil_RainDuringLimb_er,
                                          Infil_NegligibleRate_er)



good_sum_plot <- ggplot(good_sum_graph_df, aes(x = system_id, y = Count, fill = calculationResult)) + 
            geom_col(col = "black") + theme_minimal() +
            ggtitle("Infiltration Rate Calculation Result") +
            ylab("Number of Observed Storm Events") + 
            xlab("System ID") +
            scale_fill_manual(values = c(wes_palettes$Chevalier1,"#FF6D6E"),
                              labels = c("Calculated Infiltration Rate",
                                     "Neglible Infiltration Rate Error",
                                     "No Data in Bottom 6 Inches Error",
                                     "Rain During Descending Limb Error",
                                     "Rising Limb in Bottom 6 Inches Error")) +
            guides(fill=guide_legend(title="Infiltration\nCalculation\nResult")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

good_sum_plot

#summary by event depth
good_ows <- ow_infil %>% dplyr::filter(system_id %in% sys_list$system_id) %>% 
            dplyr::select(ow_uid,system_id,Infil_count,
                          Infil_NoDescendingLimb_er,
                          Infil_RisingLimb_er,
                          Infil_RainDuringLimb_er,
                          Infil_NegligibleRate_er) %>%
            dplyr::group_by(system_id) %>%
            dplyr::mutate(maxinfilcount = max(Infil_count, na.rm = TRUE)) %>%
            dplyr::filter(Infil_count == maxinfilcount) %>%
            ungroup() %>%
            dplyr::select(ow_uid) %>% distinct()


event_depth_df <- metrics %>% dplyr::filter(ow_uid %in% good_ows$ow_uid) %>%
                      left_join(radar_events[,c("radar_event_uid","eventdepth_in")], by = "radar_event_uid")

event_depth_df$bins <- ""

event_depth_df$bins[event_depth_df$eventdepth_in <= 0.25] <- "[0.10 - 0.25] in"
event_depth_df$bins[event_depth_df$eventdepth_in > 0.25 & event_depth_df$eventdepth_in <= 0.5] <- "(0.25 - 0.50] in"
event_depth_df$bins[event_depth_df$eventdepth_in > 0.5 & event_depth_df$eventdepth_in <= 1.0] <- "(0.50 - 1.00] in"
event_depth_df$bins[event_depth_df$eventdepth_in > 1.0 & event_depth_df$eventdepth_in <= 1.5] <- "(1.00 - 1.50] in"
event_depth_df$bins[event_depth_df$eventdepth_in > 1.5 & event_depth_df$eventdepth_in <= 2.0] <- "(1.50 - 2.00] in"
event_depth_df$bins[event_depth_df$eventdepth_in > 2.0] <- "> 2.00 in"


good_event_plot_df <- event_depth_df %>% dplyr::select(eventdepth_in,
                                 infiltration_inhr,
                                 infiltration_error,
                                 bins) %>%
                  dplyr::group_by(bins) %>%
                  summarise(Infil_count = sum(!is.na(infiltration_inhr)),
                            Infil_NoDescendingLimb_er = sum(infiltration_error == -900, na.rm = TRUE),
                            Infil_RisingLimb_er = sum(infiltration_error == -910, na.rm = TRUE),
                            Infil_RainDuringLimb_er = sum(infiltration_error == -920, na.rm = TRUE),
                            Infil_NegligibleRate_er = sum(infiltration_error == -930, na.rm = TRUE)) %>%
                  tidyr::gather(key = "calculationResult",
                                value = "Count",
                                Infil_count,
                                Infil_NoDescendingLimb_er,
                                Infil_RisingLimb_er,
                                Infil_RainDuringLimb_er,
                                Infil_NegligibleRate_er)

good_event_plot_df$bins <- as.factor(good_event_plot_df$bins)
good_event_plot_df$bins <- ordered(good_event_plot_df$bins,
                              c("[0.10 - 0.25] in",
                                "(0.25 - 0.50] in",
                                "(0.50 - 1.00] in",
                                "(1.00 - 1.50] in",
                                "(1.50 - 2.00] in",
                                "> 2.00 in"))



good_event_sum_plot <- ggplot(good_event_plot_df, aes(x = bins, y = Count, fill = calculationResult)) + 
  geom_col(col = "black") + theme_minimal() +
  ggtitle("Infiltration Rate Calculation Result (1.5 years/50 infil. rates systems)") +
  ylab("Number of Observed Storm Events") + 
  xlab("Event Depth Bins") +
  scale_fill_manual(values = c(wes_palettes$Chevalier1,"#FF6D6E"),
                    labels = c("Calculated Infiltration Rate",
                               "Neglible Infiltration Rate Error",
                               "No Data in Bottom 6 Inches Error",
                               "Rain During Descending Limb Error",
                               "Rising Limb in Bottom 6 Inches Error")) +
  guides(fill=guide_legend(title="Infiltration\nCalculation\nResult")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5))

good_event_sum_plot

##### 3.2 Summary graphs for all systems/ows #####


# by system

#summary for all systems (minimum 50 storms)
sum_graph_df <- ow_infil %>% dplyr::filter(Storm_Count >= 50) %>%
  dplyr::select(ow_uid,system_id,Storm_Count,Infil_count,
                Infil_NoDescendingLimb_er,
                Infil_RisingLimb_er,
                Infil_RainDuringLimb_er,
                Infil_NegligibleRate_er) %>%
  dplyr::group_by(system_id) %>%
  dplyr::mutate(maxinfilcount = max(Infil_count, na.rm = TRUE)) %>%
  dplyr::mutate(maxstormcount = max(Storm_Count, na.rm = TRUE)) %>%
  dplyr::filter(Infil_count == maxinfilcount) %>% 
  dplyr::filter(Storm_Count == maxstormcount) %>%
  ungroup() %>%
  dplyr::select(-maxinfilcount, -maxstormcount) %>%
  dplyr::group_by(system_id) %>%
  tidyr::gather(key = "calculationResult",
                value = "Count",
                Infil_count,
                Infil_NoDescendingLimb_er,
                Infil_RisingLimb_er,
                Infil_RainDuringLimb_er,
                Infil_NegligibleRate_er) %>%
  dplyr::select(-ow_uid) %>% distinct()



sum_plot <- ggplot(sum_graph_df, aes(x = system_id, y = Count, fill = calculationResult)) + 
  geom_col(col = "black") + theme_minimal() +
  ggtitle("Infiltration Rate Calculation Result (min. of 50 storm events)") +
  ylab("Number of Observed Storm Events") + 
  xlab("System ID") +
  scale_fill_manual(values = c(wes_palettes$Chevalier1,"#FF6D6E"),
                    labels = c("Calculated Infiltration Rate",
                               "Neglible Infiltration Rate Error",
                               "No Data in Bottom 6 Inches Error",
                               "Rain During Descending Limb Error",
                               "Rising Limb in Bottom 6 Inches Error")) +
  guides(fill=guide_legend(title="Infiltration\nCalculation\nResult")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sum_plot


# by event size
event_depth_df <- metrics %>%
  left_join(radar_events[,c("radar_event_uid","eventdepth_in")], by = "radar_event_uid")

event_depth_df$bins <- ""

event_depth_df$bins[event_depth_df$eventdepth_in <= 0.25] <- "[0.10 - 0.25] in"
event_depth_df$bins[event_depth_df$eventdepth_in > 0.25 & event_depth_df$eventdepth_in <= 0.5] <- "(0.25 - 0.50] in"
event_depth_df$bins[event_depth_df$eventdepth_in > 0.5 & event_depth_df$eventdepth_in <= 1.0] <- "(0.50 - 1.00] in"
event_depth_df$bins[event_depth_df$eventdepth_in > 1.0 & event_depth_df$eventdepth_in <= 1.5] <- "(1.00 - 1.50] in"
event_depth_df$bins[event_depth_df$eventdepth_in > 1.5 & event_depth_df$eventdepth_in <= 2.0] <- "(1.50 - 2.00] in"
event_depth_df$bins[event_depth_df$eventdepth_in > 2.0] <- "> 2.00 in"


event_plot_df <- event_depth_df %>% dplyr::select(eventdepth_in,
                                                       infiltration_inhr,
                                                       infiltration_error,
                                                       bins) %>%
  dplyr::group_by(bins) %>%
  summarise(Infil_count = sum(!is.na(infiltration_inhr)),
            Infil_NoDescendingLimb_er = sum(infiltration_error == -900, na.rm = TRUE),
            Infil_RisingLimb_er = sum(infiltration_error == -910, na.rm = TRUE),
            Infil_RainDuringLimb_er = sum(infiltration_error == -920, na.rm = TRUE),
            Infil_NegligibleRate_er = sum(infiltration_error == -930, na.rm = TRUE)) %>%
  tidyr::gather(key = "calculationResult",
                value = "Count",
                Infil_count,
                Infil_NoDescendingLimb_er,
                Infil_RisingLimb_er,
                Infil_RainDuringLimb_er,
                Infil_NegligibleRate_er)

event_plot_df$bins <- as.factor(event_plot_df$bins)
event_plot_df$bins <- ordered(event_plot_df$bins,
                                   c("[0.10 - 0.25] in",
                                     "(0.25 - 0.50] in",
                                     "(0.50 - 1.00] in",
                                     "(1.00 - 1.50] in",
                                     "(1.50 - 2.00] in",
                                     "> 2.00 in"))



event_plot_labels <- event_plot_df %>% group_by(bins) %>%
                  mutate(binCount = sum(Count)) %>%
                  ungroup %>%
                  mutate(PercentCalcd = round((Count/binCount)*100,1)) %>%
                  dplyr::filter(calculationResult == "Infil_count") %>%
                  dplyr::select(bins,PercentCalcd, binCount) 

event_plot_labels$Count <- event_plot_labels$binCount + 1000
event_plot_labels$label <- paste0("Percent\nCalculated: ",event_plot_labels$PercentCalcd,"%")


event_sum_plot <- ggplot(event_plot_df, aes(x = bins, y = Count)) + 
  geom_col(col = "black", aes(fill = calculationResult)) + theme_minimal() +
  ggtitle("Infiltration Rate Calculation Result (All Systems)") +
  ylab("Number of Observed Storm Events") + 
  xlab("Event Depth Bins") +
  scale_fill_manual(values = c(wes_palettes$Chevalier1,"#FF6D6E"),
                    labels = c("Calculated Infiltration Rate",
                               "Neglible Infiltration Rate Error",
                               "No Data in Bottom 6 Inches Error",
                               "Rain During Descending Limb Error",
                               "Rising Limb in Bottom 6 Inches Error")) +
  guides(fill=guide_legend(title="Infiltration\nCalculation\nResult")) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
  geom_text(data = event_plot_labels, aes(x = bins, label = label))

event_sum_plot


##### 3.3 Metric Plots for "Good" systems #####
# raw, infil plots meeting criteria

# 1.5 years, 50 infil events
good_ow_data <- ow_infil %>% dplyr::filter(Infil_count >= 50 & MonLengthYears >= 1.5)

plot_ows <- unique(good_ow_data$ow_uid)

# graph folder
graph_folder <- "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\prelim_graphs\\"

for(i in 1:length(plot_ows)){
  
  plot_mets <- metrics %>% dplyr::filter(ow_uid == plot_ows[i]) %>%
              left_join(radar_events, by = 'radar_event_uid')
  
  infil_plot <- ggplot(plot_mets, aes(y = infiltration_inhr, x = eventdatastart_edt, color = eventdepth_in)) +
    geom_point(alpha = 0.5) + ylab("Infiltration Rate (in/hr)") + xlab("Time") + 
    ggtitle(paste0("Infiltration Rate Over Time for System ",plot_mets$system_id[1]," (",plot_mets$ow_suffix[1],")")) +
    labs(color = "Event Depth (in)")

  ggsave(plot = infil_plot,
         filename = paste0(graph_folder,"infil_rate_sys_",plot_mets$system_id[1],"_",plot_mets$ow_suffix[1],".jpg"),
                width = 8, height = 4.5, units = "in")
         
  
}


for(i in 1:length(plot_ows)){
  
  plot_mets <- metrics %>% dplyr::filter(ow_uid == plot_ows[i]) %>%
    left_join(radar_events, by = 'radar_event_uid')
  
  dd_plot <- ggplot(plot_mets, aes(y = draindown_hr, x = eventdatastart_edt, size = eventdepth_in, color = eventdepth_in)) +
    geom_point(alpha = 0.5) + ylab("Draindown Duration (hr)") + xlab("Time") + 
    ggtitle(paste0("Draindown Duration Over Time for System ",plot_mets$system_id[1]," (",plot_mets$ow_suffix[1],")"))
  
  ggsave(plot = dd_plot,
         filename = paste0(graph_folder,"draindown_sys_",plot_mets$system_id[1],"_",plot_mets$ow_suffix[1],".jpg"),
         width = 10, height = 8, units = "in")
  

}

for(i in 1:length(plot_ows)){
  plot_mets <- metrics %>% dplyr::filter(ow_uid == plot_ows[i]) %>%
    left_join(radar_events, by = 'radar_event_uid')
  
  dd_v_infil_plot <- ggplot(plot_mets, aes(x = draindown_hr, y = infiltration_inhr, size = eventdepth_in, color = eventdepth_in)) +
    geom_point(alpha = 0.5) + xlab("Draindown Duration (hr)") + ylab("Infiltration Rate (in/hr)") + 
    ggtitle(paste0("Draindown vs. Infiltration for System ",plot_mets$system_id[1]," (",plot_mets$ow_suffix[1],")"))
  
  ggsave(plot = dd_v_infil_plot,
         filename = paste0(graph_folder,"dd_vs_infil_sys_",plot_mets$system_id[1],"_",plot_mets$ow_suffix[1],".jpg"),
         width = 10, height = 8, units = "in")
  
}



#### 4.0 Summary by Season and Month #### 


##### 4.1 Create Counts of observations in each Month and each Season for a given OW #####
szn_table <- metrics %>% dplyr::filter(ow_uid %in% good_ows$ow_uid) %>%
            left_join(radar_events[,c("radar_event_uid","eventdepth_in", "eventdatastart_edt")], by = "radar_event_uid") %>%
            mutate(Month = months(eventdatastart_edt)) %>%
            mutate(YearMonth = yearmonth(eventdatastart_edt)) %>%
            mutate(Year = lubridate::year(eventdatastart_edt))
# 
# szn_table <- metrics %>%
#   left_join(radar_events[,c("radar_event_uid","eventdepth_in", "eventdatastart_edt")], by = "radar_event_uid") %>%
#   mutate(Month = months(eventdatastart_edt)) %>%
#   mutate(YearMonth = yearmonth(eventdatastart_edt)) %>%
#   mutate(Year = lubridate::year(eventdatastart_edt))

#Create Seasons
szn_table$season <- time2season(szn_table$eventdatastart_edt, out.fmt="seasons")

szn_table <- szn_table %>% dplyr::mutate(YearSeason = paste0(Year," ", season))

#spread data
mon_sys <- szn_table %>% dplyr::select(ow_uid, system_id,infiltration_inhr, YearMonth) %>%
              dplyr::group_by(ow_uid, system_id, YearMonth) %>%
              dplyr::summarise(Infil_count = sum(!is.na(infiltration_inhr))) %>%
              dplyr::ungroup() %>%
              dplyr::group_by(system_id,YearMonth) %>%
              dplyr::mutate(max_infil_count = max(Infil_count, na.rm = TRUE)) %>%
              dplyr::ungroup() %>%
              dplyr::select(-max_infil_count)


#list of months and seasons with the number of observations in each Year/Month and Year/Season for a unique OW
mon_sys_count <- szn_table %>% dplyr::select(ow_uid, system_id,infiltration_inhr, YearMonth) %>%
  dplyr::group_by(ow_uid, system_id, YearMonth) %>%
  dplyr::summarise(Infil_count = sum(!is.na(infiltration_inhr))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(system_id,YearMonth) %>%
  dplyr::mutate(max_infil_count = max(Infil_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-max_infil_count)

szn_sys_count <- szn_table %>% dplyr::select(ow_uid, system_id,infiltration_inhr, YearSeason,Year, season) %>%
  dplyr::filter(is.na(infiltration_inhr) == FALSE) %>%
  dplyr::group_by(ow_uid, system_id, season) %>%
  dplyr::summarise(Infil_count = sum(!is.na(infiltration_inhr)),
                   Year_count = length(unique(Year))) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(system_id, season) %>%
  dplyr::mutate(max_infil_count = max(Infil_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-max_infil_count)



##### 4.2 Add minimum requirements for including a site (n >= 10 across years for a season; at least 3 years of the same season) #####
szn_sys_count_good <- szn_sys_count %>% dplyr::filter(Infil_count >= 10 & Year_count >= 3) %>%
                                        group_by(ow_uid,system_id) %>%
                                        mutate(SeasonCount = n()) %>%
                                        ungroup() %>%
                                        dplyr::filter(SeasonCount >= 2)


system_szns <- szn_sys_count_good %>% group_by(ow_uid,system_id) %>%
               summarize(SeasonCount = n())

# systems with 4 seasons meeting the criteria
sys_4season <- system_szns$system_id[system_szns$SeasonCount == 4]

##### 4.3 Number of observations by System, Year, and Season #####


szn_sys_year_count <- szn_table %>% dplyr::select(ow_uid, system_id,infiltration_inhr, YearSeason,Year, season) %>%
  dplyr::filter(is.na(infiltration_inhr) == FALSE) %>%
  dplyr::group_by(ow_uid, system_id, Year,season) %>%
  dplyr::summarise(Infil_count_year_season = sum(!is.na(infiltration_inhr))) %>%
  dplyr::ungroup()

szn_sys_year_count <- szn_sys_year_count %>% dplyr::filter(system_id %in% system_szns$system_id)

##### 4.4 Cheeky graph summarizing Infiltration Rates by Season for our chosen ow's #####
test_ows <- system_szns$ow_uid %>% unique

test_sum_graph_df <- ow_infil %>% dplyr::filter(ow_uid %in% test_ows) %>% 
  dplyr::select(ow_uid,system_id,Infil_count,
                Infil_NoDescendingLimb_er,
                Infil_RisingLimb_er,
                Infil_RainDuringLimb_er,
                Infil_NegligibleRate_er) %>%
  dplyr::group_by(system_id) %>%
  dplyr::mutate(maxinfilcount = max(Infil_count, na.rm = TRUE)) %>%
  dplyr::filter(Infil_count == maxinfilcount) %>% ungroup() %>%
  dplyr::select(-maxinfilcount) %>%
  dplyr::group_by(system_id) %>%
  tidyr::gather(key = "calculationResult",
                value = "Count",
                Infil_count,
                Infil_NoDescendingLimb_er,
                Infil_RisingLimb_er,
                Infil_RainDuringLimb_er,
                Infil_NegligibleRate_er)


test_event_plot_labels <- test_sum_graph_df %>% group_by(system_id) %>%
  mutate(sysCount = sum(Count)) %>%
  ungroup %>%
  mutate(PercentCalcd = round((Count/sysCount)*100,1)) %>%
  dplyr::filter(calculationResult == "Infil_count") %>%
  dplyr::select(system_id,PercentCalcd, sysCount) 

test_event_plot_labels$Count <- test_event_plot_labels$sysCount
test_event_plot_labels$label <- paste0(test_event_plot_labels$PercentCalcd,"%")



test_sum_plot <- ggplot(test_sum_graph_df, aes(x = system_id, y = Count)) + 
  geom_col(col = "black", aes(fill = calculationResult)) + theme_minimal() +
  ggtitle("Infiltration Rate Calculation Result (Systems with 3+ Years of seasonal data)") +
  ylab("Number of Observed Storm Events") + 
  xlab("System ID") +
  geom_text(data = test_event_plot_labels, aes(x = system_id, y = (Count+50), label = label)) +
  scale_fill_manual(values = c(wes_palettes$Chevalier1,"#FF6D6E"),
                    labels = c("Calculated Infiltration Rate",
                               "Neglible Infiltration Rate",
                               "No Data in Bottom 6 Inches",
                               "Rain During Descending Limb",
                               "Rising Limb in Bottom 6 Inches")) +
  guides(fill=guide_legend(title="Result:")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = element_text(size = 16),
        legend.position = "top",
        legend.text = element_text(size = 12))

test_sum_plot

#### 5.0 Save dataset for Mann-Kendall Test ####

##### 5.1 Save a copy of the metrics with the new -920 results incorporated #####

met_folder <- "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\Infiltration Rate Recalcs\\"

write.csv(file = paste0(met_folder,"metrics_with_recalcs.csv"), x = metrics)

##### 5.2 Save a copy of the summary of the seasonality for review #####

write.csv(file = paste0(folder_loc,"system_seasons_for_analysis.csv"), x = szn_sys_count_good)


##### 5.3 Unformatted summary of  seasonality and months for review #####

# Save Each
write.csv(file = paste0(folder_loc,"statistics_by_season.csv"), x = szn_sys_count)
write.csv(file = paste0(folder_loc,"statistics_by_month.csv"), x = mon_sys_count)

#### 6.0 Additional Plots for Report/Powerpoint ####

##### 6.1 create plot with months of data for each system plot #####

# event dates
event_dates <- radar_events %>% dplyr::select(radar_event_uid, eventdatastart_edt)

# month/years with data
month_raster_df <- metrics %>% left_join(event_dates, by = "radar_event_uid") %>%
                   dplyr::filter(is.na(infiltration_error)) %>%
                   mutate(Month = month(eventdatastart_edt)) %>%
                   mutate(Year = year(eventdatastart_edt)) %>%
                   dplyr::select(system_id, ow_uid, Month, Year) %>%
                   group_by(system_id,ow_uid,Month,Year) %>%
                   summarize(InfilCount = n()) %>% ungroup() %>%
                   distinct() %>% mutate(MonthYear = Year + Month/12,
                                         Data = TRUE)
                    
# trim to "good ows"
good_raster_df <- month_raster_df %>% dplyr::filter(ow_uid %in% good_ows$ow_uid)

#raster vals
pal <- wes_palette("Zissou1", 21, type = "continuous")

# yvals
maj_x_breaks <- c(2012.95833:2022.95833)
man_x_breaks <- seq(2012.95833,2022.95833, (1/12))

infil_mon_raster <- ggplot(good_raster_df, aes(x = MonthYear, y = system_id, fill = InfilCount)) + geom_raster() +
                    scale_fill_gradientn(colors = pal) +  
                    theme_bw() + ylab("System ID") + xlab("Month") +
                    scale_x_continuous(breaks = maj_x_breaks, minor_breaks = man_x_breaks,
                                       labels = c(2013:2023)) +
                    labs(fill = "Infil.\nRates\nObserved") +
                    theme(axis.text.x = element_text(size = 12),
                          axis.text.y = element_text(size = 12),
                          axis.title.x = element_text(size = 14),
                          axis.title.y = element_text(size = 14)) +
                    ggtitle("Observed Infiltration Rates for Tested Systems")

infil_mon_raster

# folder
memo_plots <- "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\05 Memo\\02 Plots\\"

ggsave(filename = paste0(memo_plots,"monitoring_raster_plot.png"),
       plot = infil_mon_raster, width = 12, height = 6.75, units = "in")

# once more with feeling, for seasons


# month/years with data
szn_raster_df <- metrics %>% left_join(event_dates, by = "radar_event_uid") %>%
  dplyr::filter(is.na(infiltration_error)) %>%
  mutate(Season = time2season(eventdatastart_edt)) %>%
  mutate(Year = year(eventdatastart_edt)) %>%
  dplyr::select(system_id, ow_uid, Season, Year) %>%
  group_by(system_id,ow_uid,Season,Year) %>%
  summarize(InfilCount = n()) %>% ungroup() %>%
  distinct() %>% mutate(SeasonYear = Year + Season/4,
                        Data = TRUE)

# trim to "good ows"
good_raster_df <- szn_raster_df %>% dplyr::filter(ow_uid %in% good_ows$ow_uid)

# yvals
maj_x_breaks <- c(2012.95833:2022.95833)
man_x_breaks <- seq(2012.95833,2022.95833, (1/12))

infil_mon_raster <- ggplot(good_raster_df, aes(x = SeasonYear, y = system_id, fill = InfilCount)) + geom_raster() +
  scale_fill_gradientn(colors = pal) +  
  theme_bw() + ylab("System ID") + xlab("Season") +
  scale_x_continuous(breaks = maj_x_breaks, minor_breaks = man_x_breaks,
                     labels = c(2013:2023)) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  ggtitle("Observed Infiltration Rates for Tested Systems")

