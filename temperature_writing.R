# Temperature Writing to database to describe exogenuous variables
# Crudely hacked together by Brian Cruice on 06/05/2023
# Based on update_ow_data.Rmd scripts written by Taylor Heffernan

# DON'T RUN THIS AGAIN! (NOT YET)
# I am actively working on measures to stop the script from writing the same data twice 


##### 0.1 Packages #####

#Dplyr stuff
library(magrittr)
library(tidyverse)
library(lubridate)

#Database Stuff
library(RODBC)
library(odbc)


##### 0.2 Connect to database #####

marsDBcon <- dbConnect(odbc::odbc(), "mars14_data")

##### 0.3 Import site names of interest #####

# folder location 

folder_loc <- "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\Analysis\\summary_statistics\\"

ow_list <- read.csv(paste0(folder_loc,"system_seasons_for_analysis.csv")) %>%
           dplyr::filter(is.na(ow_uid) == FALSE) %>%
           dplyr::select(ow_uid) %>% unique %>% pull


#### 1.0 Query Postgres DB ####

access_list <- dbGetQuery(marsDBcon, paste0("SELECT * FROM admin.tbl_accessdb WHERE ow_uid in (",paste(ow_list, collapse= ", "),")"))



#### 2.0 Iterate through Access DB's and write to Postgres DB ####

for( i in 1:nrow(access_list)){
  
# identify the ow
ow_x <- access_list$ow_uid[i]

# what's the latest date we already have in postgres?
latest_date_x <- dbGetQuery(marsDBcon,
                            paste0("SELECT max(dtime_est) FROM data.tbl_temperature where ow_uid = ",
                                   ow_x))
  
# read temperature data from access file
accessdbCon_x <- RODBC::odbcConnectAccess2007(access_list$filepath[i])

temp_query_x <- paste0("SELECT [",access_list$datatable[i],"].[Standard Dtime], ",
                              "[",access_list$datatable[i],"].[Temp BW (°F)] FROM [",access_list$datatable[i],"]")

temp_data_x <- sqlQuery(accessdbCon_x, temp_query_x, as.is = TRUE)


# handle datetime coming in as text and rename column
temp_data_x <- temp_data_x %>% dplyr::mutate(dtime_est = ymd_hms(`Standard Dtime`)) %>%
                               dplyr::select(-`Standard Dtime`)

# append ow_uid
temp_data_x$ow_uid <- access_list$ow_uid[i]

#rename columns
colnames(temp_data_x) <- c("temperature_degF", "dtime_est", "ow_uid")

#close access connection
odbcClose(accessdbCon_x)


# show first
head(temp_data_x)

# filter out NA values
temp_data_x <- temp_data_x %>% dplyr::filter(!is.na(temperature_degF))

# Write to postgres

write_results <- dbWriteTable(marsDBcon,
                 DBI::SQL("data.tbl_temperature"),
                 temp_data_x,
                 append = TRUE,
                 row.names = FALSE)

print(write_results)
}


#### 3.0 Close it out ####
dbDisconnect(marsDBcon)


