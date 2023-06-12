# checking temperature values

##### 0.1 Packages #####

#Dplyr stuff
library(magrittr)
library(tidyverse)
library(lubridate)
library(data.table)

#plot
library(ggplot2)
library(gganimate)

#Database Stuff
library(RODBC)
library(odbc)

#stat stuff
library(broom)

#### 0.2 Database Connections and file extensions ####
# connect
mars_con <- dbConnect(odbc::odbc(), "mars14_data")


# read best metrics
folder_loc <- "\\\\pwdoows\\OOWS\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\"
metrics_raw <- read.csv(paste0(folder_loc,"sump depth recalcs\\20230607_sumpdepthrecalc_noPoplar_w_Systemid.csv"))


# read the ow's we want
ows <- read.csv(paste0(folder_loc,"summary_statistics\\system_seasons_for_analysis.csv")) %>%
       dplyr::filter(!is.na(ow_uid)) %>% select(ow_uid) %>% unique %>% pull

# remove poplar, 445-1, 256-1
ows <- ows[ows != 853 & ows != 901   & ows != 1010]


#### 1.0 Data Filtering ####

# metrics with these ows
metrics <- metrics_raw %>% dplyr::filter(ow_uid %in% ows)

# grab the storms
storms <- dbGetQuery(mars_con, paste0("SELECT * FROM data.tbl_radar_event
                            WHERE radar_event_uid IN (",
                            paste(metrics$radar_event_uid, collapse = ", "),
                            ")"))

# VERY FUN AND NOT ANNOYING daylight savings time stuff
storms$eventdatastart_edt <- ymd_hms(storms$eventdatastart_edt, tz = "America/New_York")
storms$eventdataend_edt <- ymd_hms(storms$eventdataend_edt, tz = "America/New_York")

storms %<>% dplyr::filter(!is.na(eventdatastart_edt))
storms %<>% dplyr::filter(!is.na(eventdataend_edt))

#Our water level data is not corrected for daylight savings time. ie it doesn't spring forwards
#So we must shift back any datetimes within the DST window
#Thankfully, the dst() function returns TRUE if a dtime is within that zone
  dst_index <- lubridate::dst(storms$eventdatastart_edt)
  storms$eventdatastart_edt %<>% lubridate::force_tz("EST") #Assign new TZ without changing dates
  storms$eventdatastart_edt[dst_index] <- storms$eventdatastart_edt[dst_index] - lubridate::hours(1)


  dst_index <- lubridate::dst(storms$eventdataend_edt)
  storms$eventdataend_edt %<>% lubridate::force_tz("EST") #Assign new TZ without changing dates
  storms$eventdataend_edt[dst_index] <- storms$eventdataend_edt[dst_index] - lubridate::hours(1)



# query across the events


#### 2.0 Main Loop: Query storm data, summarize temperature, perform LOESS/lm regression, Save Plots ####

##### 2.1 Set up structures #####

# initialize lists to truncate into df's and save
loess_75_list <- list()
loess_100_list <- list()
lm_list <- list()
temp_metrics <- list()

# folder location for plots
graph_fold <- "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\prelim_graphs\\Regression Models\\"

##### 2.2 Loop for all ows #####
for(j in 1:length(ows)){
  
  ow_x <- ows[j]
  metrics_x <- metrics %>% dplyr::filter(ow_uid == ow_x)
  temper_data <- as.data.frame(matrix(ncol = 5))
  colnames(temper_data) <- c("temperature_uid", "temperature_degf", "ow_uid","dtime_est","radar_event_uid" )
  
  temper_data_x <- dbGetQuery(mars_con, paste0("SELECT * FROM data.tbl_temperature WHERE ow_uid = ",ow_x))
  temper_data_x$dtime_est <- ymd_hms(temper_data_x$dtime_est, tz = "EST")
  
##### 2.3 Loop for all metrics #####  
  for(i in 1:nrow(metrics_x)){
    
    #start time
    start_time <- storms$eventdatastart_edt[storms$radar_event_uid == metrics_x$radar_event_uid[i]]
    #end time
    end_time <- storms$eventdataend_edt[storms$radar_event_uid == metrics_x$radar_event_uid[i]]
    
    ##### 2.4 Query temperature data for storm #####
    temper_data_y <- temper_data_x %>% dplyr::filter(dtime_est > start_time) %>%
                                       dplyr::filter(dtime_est < end_time)
    
    #make null if no data
    try(temper_data_y$radar_event_uid <- metrics_x$radar_event_uid[i])
    
    temper_data <- rbind(temper_data,temper_data_y)

  }  
        # # attempt to animate moving temperature for indiviudal storms
        # ggplot(temper_data, aes(x = dtime_est, y = temperature_degf)) + geom_point() +
        #   scale_y_continuous(limits = c(35, 85)) + ylab("Temperature (Deg. F)") +
        #   xlab("Date Time") +
        #   ## gganimate attempt
        #   transition_states(radar_event_uid,
        #                      transition_length = 2,
        #                      state_length = 4) +
        #   ease_aes('linear')
  
  # this was choking the whole damn thing
  # ggplot(temper_data, aes(group = radar_event_uid, y = temperature_degf, col = temperature_degf)) + geom_boxplot()
  
  
  ##### 2.5 Summarize temperature data for each radar event #####  
  
  summary_data <- temper_data %>% group_by(radar_event_uid) %>%
                  summarize(mean = mean(temperature_degf, na.rm = TRUE),
                            median = median(temperature_degf, na.rm = TRUE),
                            quartile_low = quantile(temperature_degf, na.rm = TRUE)[2],
                            quartile_high = quantile(temperature_degf, na.rm = TRUE)[4],
                            IQR = quartile_high - quartile_low,
                            diff = max(temperature_degf, na.rm = TRUE) - min(temperature_degf, na.rm = TRUE)
                            )
  
  metrics_x <- metrics_x %>% left_join(summary_data, by = "radar_event_uid")
  
  
  
  ##### 2.6 Specific removals for certain systems #####
  
  # add start date
  storm_start <- storms %>% dplyr::select(radar_event_uid,eventdatastart_edt)
  
  metrics_x <- metrics_x %>% left_join(storm_start, by = "radar_event_uid")
  
  # System 1-1 after the beginning of 2018
  if(ow_x == 660)
  {
    metrics_x <- metrics_x %>% dplyr::filter(eventdatastart_edt > ymd('2018-01-01'))
  }
  
  # 211-1 prior to fix on 7/27/2018
  if(ow_x == 937)
  {
    metrics_x <- metrics_x %>% dplyr::filter(eventdatastart_edt > ymd('2018-07-28'))
  }

  
  ##### 2.7 Regressions and regression plots #####
  # LOESS
  loess_mod_75 <- loess(infiltration_inhr ~ median, data = metrics_x, span = 0.50)
  loess_mod_50 <- loess(infiltration_inhr ~ median, data = metrics_x, span = 0.75)
  loess_mod_100 <- loess(infiltration_inhr ~ median, data = metrics_x, span = 1.00)
  
  #linear regression
  lm_mod <- lm(infiltration_inhr ~ median, data = metrics_x)
  
  #tidy it up
  model_res_75 <- augment(loess_mod_75, newdata = metrics_x)
  model_res_50 <- augment(loess_mod_50, newdata = metrics_x)
  model_res_100<- augment(loess_mod_100, newdata = metrics_x)
  lm_res <- augment(lm_mod, newdata = metrics_x)
  
  
  plot_x <- ggplot(metrics_x, aes(y = infiltration_inhr, x = median)) + 
            geom_point() + 
            geom_line(data = model_res_75, aes(y = .fitted, x = median, col = "red"), size = 1.2) +
            geom_line(data = model_res_50, aes(y = .fitted, x = median, col = "green3"), size = 1.2) +
            geom_line(data = model_res_100, aes(y = .fitted, x = median, col = "gold"), size = 1.2) +
            geom_line(data = lm_res, aes(y = .fitted, x = median, col = "blue"), size = 1.2) +
            scale_color_manual(values = c("blue", "red", "green3", "gold"), labels = c("Linear", "LOESS (100)", "LOESS (75)", "LOESS (50)")) + 
            labs(col = "Model Type") +
            ylab("Infiltration Rate (in/hr)") + 
            xlab("Median Event Temperature (F)") +
            ggtitle(paste0("Infiltration Rate vs Temperature for System ",metrics_x$system_id[1]))
  
  plot_y <- ggplot(metrics_x, aes(y = infiltration_inhr, x = median)) + 
            geom_point() + 
            # geom_line(data = model_res_75, aes(y = .fitted, x = median, col = "red"), size = 1.2) +
            # geom_line(data = model_res_50, aes(y = .fitted, x = median, col = "green3"), size = 1.2) +
            # geom_line(data = model_res_100, aes(y = .fitted, x = median, col = "gold"), size = 1.2) +
            geom_line(data = lm_res, aes(y = .fitted, x = median, col = "blue"), size = 1.2) +
            scale_color_manual(values = c("blue"), labels = c("Linear")) + 
            labs(col = "Model Type") +
            ylab("Infiltration Rate (in/hr)") + 
            xlab("Median Event Temperature (F)") +
            ggtitle(paste0("Infiltration Rate vs Temperature for System ",metrics_x$system_id[1]))
  
  plot_x
  
  ##### 2.8 Store Results, Save graph #####
  #save ow model results to list        
  lm_list[[j]] <- lm_res
  loess_75_list[[j]] <- model_res_75
  loess_100_list[[j]] <- model_res_100

  temp_metrics[[j]] <- metrics_x
    
  # save plot
  ggsave(filename = paste0(graph_fold,metrics_x$system_id[1],"_",metrics_x$ow_suffix[1],"_models.png"),
         plot = plot_x,
         height = 8, width = 10, units = "in", dpi = 300)
  
  ggsave(filename = paste0(graph_fold,metrics_x$system_id[1],"_",metrics_x$ow_suffix[1],"_linear_model.png"),
         plot = plot_y,
         height = 8, width = 10, units = "in", dpi = 300)

  
}


#### 3.0 Save the outputs ####


##### 3.1 Save Temperature Output #####

# temperature_storm_results <- rbindlist(temp_metrics) %>%
#                              dplyr::select(ow_uid,
#                                            radar_event_uid,
#                                            mean,median,
#                                            quartile_low,
#                                            quartile_high,
#                                            IQR,
#                                            diff)
# 
# colnames(temperature_storm_results) <- c("ow_uid",
#                                          "radar_event_uid",
#                                          "mean_temperature",
#                                          "median_temperature",
#                                          "low_quratile_temperature",
#                                          "high_quratile_temperature",
#                                          "temperature_IQR",
#                                          "temperature_range")
# 
# write_results <- dbWriteTable(mars_con,
#                               DBI::SQL("data.tbl_event_temperature"),
#                               temperature_storm_results,
#                               append = TRUE,
#                               row.names = FALSE)


#### 4.0 Save LOESS and LM models ####

model_values <- rbindlist(temp_metrics)

linear_df <- rbindlist(lm_list)
loess_75_df <- rbindlist(loess_75_list)
loess_100_df  <- rbindlist(loess_100_list)

#assign model id's
linear_df$model <- "linear"
loess_75_df$model <- "loess_75"
loess_100_df$model <- "loess_100"

model_df <- rbind(linear_df,
                  loess_75_df,
                  loess_100_df)


model_df <- model_df %>% select(ow_uid,
                                radar_event_uid,
                                eventdatastart_edt,
                                infiltration_inhr,
                                infiltration_error,
                                median,
                                model,
                                .fitted,
                                .resid)

colnames(model_df) <- c("ow_uid",
                        "radar_event_uid",
                        "eventdatastart_edt",
                        "infiltration_inhr",
                        "infiltration_error",
                        "median_temperature",
                        "model_type",
                        "fitted_value",
                        "residual")



write_results <- dbWriteTable(mars_con,
                              DBI::SQL("data.tbl_infil_temp_models"),
                              model_df,
                              append = TRUE,
                              row.names = FALSE)
