### power analysis



##### 0.1 Loading the usual suspects #####

# database packages
library(odbc)
library(DBI)

# tidyverse, etc
library(tidyverse)
library(magrittr)
library(lubridate)

#viz
library(ggplot2)

`%!in%` <- Negate(`%in%`)

##### 0.2 database connection ######
mars_con <- dbConnect(odbc::odbc(), "mars14_datav2")


##### 0.3 Let's read in the power analysis fx from USGS #####

# power.WMW
# Computes power function for Wilcoxon rank-sum test, one-sided alternative
# y1 = data (for both groups if stacked = TRUE; 1st group if stacked = FALSE)
# y2 = grouping variable if stacked = TRUE; 2nd data column if stacked = FALSE
# gmratio is the ratio of geometric means
# power is the set of powers for which you want sample sizes
# NOTE: you can enter power <- c(0.2, 0.5, 0.7) if you wish
# NOTE: sample sizes will be rounded up to the next largest integer using the
# ceiling function
power.WMW <- function(y1, y2, gmratio, conf = 95, stacked = TRUE,  
                      power = seq(0.5, 0.95, by = 0.05)) { 
  par(las = 1, tck = 0.02, xaxs = "i", yaxs = "i")
  y1name <- deparse(substitute(y1))
  y2name <- deparse(substitute(y2))
  if(class(y2) == "numeric") {
    stacked = FALSE
    cat("\n DATA ANALYZED:", y1name, y2name, "\n") 
  }
  cat("------------------ \n")
  if(stacked) {
    cat("\n DATA ANALYZED:", y1name, "by", y2name,"\n")
    cat("------------------ \n")
    xx <- data.frame(A = y1,B= y2)
    zz <- unstack(xx, A ~ B )
    x1 <- zz[[1]]
    x2 <- zz[[2]]
  } else{
    x1 <- y1
    x2 <- y2
  }
  conf <- conf / 100.0
  n1 <- length(x1)
  n2 <- length(x2)
  

  w1 <- log(x1)
  w2 <- log(x2)
  avediff <- log(gmratio)
  stddiff <- sqrt(sd(w1) ^ 2 + sd(w2) ^ 2)
  pplus <- pnorm(0, avediff, stddiff)

  #### Beginning BPC edits ####
  # USGS code, relies on natural log monotonic transfer
  
  
  # first test has fairly normal data, so no monotonic transfer needed
  w1 <- x1
  w2 <- x2
  avediff <- gmratio
  stddiff <- sqrt(sd(w1) ^ 2 + sd(w2) ^ 2)
  pplus <- pnorm(0, avediff, stddiff)
  # let's use a monotonic transfer that does not exclude negative values from being used
  
  
  #### END BPC edits ####
  #### Resume USGS code ####
  
    if(pplus < 0.5) {
    pplus <- 1.0 - pplus
  }
  ns <- n1+n2
  nxrat <- n1 / ns
  zalpha <- qnorm(conf)
  zbeta <- sqrt(12 * nxrat * (1 - nxrat) * ns * (pplus - .5) ^ 2) - zalpha
  pow <- pnorm(zbeta)
  RESULTSOBS3 <- data.frame(SampleSize = ns, Nxratio = nxrat, GMratio = gmratio,
                            PPlus = round(pplus, 3),
                            ObsrvPower = round(100 * pow, 2))
  cat("Results for Wilcoxon rank-sum test (one-tailed) \n     with specified gmratio. \n")
  cat("SampleSize is the required number of obs in both groups together. \n")
  cat("Nxratio is the proportion of SampleSize for 1st variable entered. \n")
  print(RESULTSOBS3)
  cat("------------------ \n")
  zbet <- qnorm(power)
  sampsize <- (zalpha + zbet) ^ 2 / (12 * nxrat * (1 - nxrat) * (pplus - 0.5) ^ 2)
  power <- 100*power
  maintitle <- expression("WILCOXON RANK SUM TEST")
  RESULTS <- data.frame(Power = power, SampleSize=sampsize)
  
  plot(power ~ sampsize, main = maintitle, ylab = "Power of test", xlab = "Total sample size")
  sampsize2 <- ceiling(sampsize)
  zbeta2 <- sqrt(12 * nxrat * (1 - nxrat) * sampsize2 * (pplus - 0.5) ^ 2) - zalpha
  power2 <- pnorm(zbeta2)
  power2 <- 100 * (round(power2, digits = 3))
  RESULTS2 <- data.frame(SampleSize = sampsize2, Power = power2)
  cat("SampleSize is integer for closest Power\n    not less than specified Power \n")
  cat("Sample sizes are rounded up to smallest integer\n     not less than the computed sample size \n")
  print(RESULTS2)
}


# test data
# load("\\\\pwdoows\\OOWS\\Watershed Sciences\\Reference Documents\\Statistics\\Helsel_etal_2020_USGS_Statistical_Methods_in_Water_Resources_Supporting_Information\\specapic.RData")
# source("\\\\pwdoows\\OOWS\\Watershed Sciences\\Reference Documents\\Statistics\\Helsel_etal_2020_USGS_Statistical_Methods_in_Water_Resources_Supporting_Information\\sdpool.R")
# 
# downgrad <- c(0.85, 0.39, 0.32, 0.3, 0.3, 0.205, 0.2,
#                 0.2, 0.14, 0.14, 0.09, 0.046, 0.035)
# upgrad <- c(6.9, 3.2, 1.7)
# 
# specapic2 <- specapic[specapic$rock == "Siliciclastic" | specapic$rock =="Dolomite",]
# 
# power.WMW(specapic2[rock == "Dolomite"], specapic2[rock == "Siliciclastic"],
#           gmratio = 5.27, power = seq(0.5, 0.99, by = 0.02))



# read existing clusters
unmon_clusters <- dbGetQuery(mars_con,"SELECT * FROM fieldwork.tbl_clustered_systems")
unmon_clusters <- unmon_clusters %>% dplyr::filter(date_generated == max(date_generated))







###  lol it's here
# install.packages("MultNonParam")
# library(MultNonParam)
### lol no it's not


# post-hoc power test of existing  theil-sen slopes
ts_slopes <- dbGetQuery(mars_con, "select * from metrics.tbl_trend_mktest")



ts_slopes %<>% dplyr::filter(batch_uid == max(batch_uid))
sys_vals <- dbGetQuery(mars_con,"select o.ow_uid, o.ow_suffix, o.smp_id, sys.system_id, sys.sys_lrsubsurfacedcia_ft2, mk.mkslope_inhrsec
                                 from metrics.tbl_trend_mktest mk
                                 left join fieldwork.tbl_ow o
                                 on o.ow_uid = mk.ow_uid
                                 left join external.tbl_systembdv sys
                                 on sys.system_id = admin.fun_smp_to_system(o.smp_id)")
# remove name dupes
sys_vals <- sys_vals[!duplicated(colnames(sys_vals))]

# slope to annual occurence
ts_slopes %<>%  dplyr::mutate(mkslope_inhryear = mkslope_inhrsec * 3600*24*365.25)
sys_vals %<>% dplyr::mutate(mkslope_inhryear = mkslope_inhrsec * 3600*24*365.25)

#split groups
group1 <- sys_vals %>% dplyr::filter(sys_lrsubsurfacedcia_ft2 >= 12) %>% dplyr::select(mkslope_inhryear, sys_lrsubsurfacedcia_ft2)
group2 <-sys_vals %>%  dplyr::filter(sys_lrsubsurfacedcia_ft2 <= 12) %>% dplyr::select(mkslope_inhryear, sys_lrsubsurfacedcia_ft2)

# we want to see a meaninful differenc of about 0.1 to 0.05 per year
exist.mean <- mean(ts_slopes$mkslope_inhryear) 
exist.median <- median(ts_slopes$mkslope_inhryear)   
exist.sd <- sd(ts_slopes$mkslope_inhryear)



mean_infil <- 1.440697
gguess <- ((mean_infil*.8)/mean_infil)
gguess <- (exist.mean + 0.01/exist.mean)

power.WMW(group1$mkslope_inhryear,group2$mkslope_inhryear,gmratio = gguess, stacked = FALSE)


near_miss_csv <- "\\\\pwdoows\\OOWS\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\04 Analysis\\summary_statistics\\near_miss_2023-08-30.csv"
near_miss <- read.csv(near_miss_csv)

infil_rates <- dbGetQuery(mars_con,
                          paste0("select * from metrics.tbl_infiltration where ow_uid in (",
                                 paste(near_miss$ow_uid, collapse = ", "),
                                 ")")) %>%
  dplyr::filter(!is.na(infiltration_rate_inhr))



#### 1.0 Calculate the known variability of tested data ####
exist.mean <- mean(ts_slopes$mkslope_inhryear)
exist.median <- median(ts_slopes$mkslope_inhryear)
exist.sd <- sd(ts_slopes$mkslope_inhryear)


## histogram plot
ggplot(data = ts_slopes, aes(x = mkslope_inhryear)) +
  geom_histogram(binwidth = 0.2, boundary = 0) + 
  geom_vline(xintercept = 0, size = 1, linetype = "dashed") +
  theme_bw() + scale_x_continuous(breaks = seq(-8,1.2,.4)) +
  xlab("Theil-Sen Slope (in/hr per year)") +
  ylab("Count") + scale_y_continuous(minor_breaks = seq(0,15,1), breaks = seq(0,15,5)) +
  ggtitle("Histogram of Theil-Sen Slopes")

# t.test plots for theil-sen
power.t.test(delta = 0.2,
             sd = exist.sd,
             power = 0.8,
             sig.level = 0.2)


# use transformed infiltration rates for a power t test
near_miss_csv <- "\\\\pwdoows\\OOWS\\Watershed Sciences\\GSI Monitoring\\06 Special Projects\\52 Long-Term GSI Performance Trends\\04 Analysis\\summary_statistics\\near_miss_2023-08-30.csv"
near_miss <- read.csv(near_miss_csv)

near_miss_10 <- near_miss %>% dplyr::filter(InfilCount >= 10)

infil_rates <- dbGetQuery(mars_con,
                          paste0("select * from metrics.tbl_infiltration where ow_uid in (",
                                 paste(near_miss$ow_uid, collapse = ", "),
                                 ")")) %>%
  dplyr::filter(!is.na(infiltration_rate_inhr)) %>%
  dplyr::filter(ow_uid %in% near_miss_10$ow_uid)

infil_mean <- mean(infil_rates$infiltration_rate_inhr)
infil_sd <- sd(infil_rates$infiltration_rate_inhr)
infil_hist <- hist(infil_rates$infiltration_rate_inhr)

log_infil_hist <- hist(log(infil_rates$infiltration_rate_inhr))
log_infil_mean <- mean(log(infil_rates$infiltration_rate_inhr))
log_infil_sd <- sd(log(infil_rates$infiltration_rate_inhr))

trans_infil_mean <- exp(log_infil_mean)
trans_infil_sd_low <- exp(log_infil_mean - log_infil_sd)
trans_infil_sd_high <- exp(log_infil_mean + log_infil_sd)
trans_infil_sd <- exp(log_infil_sd)

infil_rates %<>% dplyr::mutate(ln_infil_rate_inhr = log(infiltration_rate_inhr))


infil_rate_summary <- infil_rates %>% group_by(ow_uid) %>%
                      summarise(ln_infil_mean = mean(ln_infil_rate_inhr),
                          ln_infil_sd = sd(ln_infil_rate_inhr),
                          infil_mean = mean(infiltration_rate_inhr),
                          infil_sd = sd(infiltration_rate_inhr))

infil_rate_summary$b8_p05_n <- NA
for(i in 1:nrow(infil_rate_summary)){
  hist(infil_rates$ln_infil_rate_inhr[infil_rates$ow_uid == infil_rate_summary$ow_uid[i]])
  mean_x <- infil_rate_summary$ln_infil_mean[i]
  sd_x <- infil_rate_summary$ln_infil_sd[i]
    
  power_test_x <- power.t.test(delta = abs(mean_x - sd_x),
                                 sd = log_infil_sd,
                                 power = 0.8,
                                 sig.level = 0.05)
  
  infil_rate_summary$b8_p05_n[i] <- power_test_x$n

}

power.t.test(n = 39,
             sd = trans_infil_sd,
             sig.level = 0.05,
             power = 0.9)

power.t.test(delta = log_infil_sd_high,
             sd = log_infil_sd_high,
             sig.level = 0.05,
             power = 0.9)

# grab difference calculable

sigs <- c(5:30)*.01

deltas <- c(5:30)*0

for(i in 1:length(sigs)){
  results <- power.t.test(n = 39,
                          sd = log_infil_sd,
                          sig.level = sigs[i],
                          power = 0.8)
  dif_x <- results$delta
  deltas[i] <- dif_x
}

delta_frame <- cbind(sigs,deltas) %>% as.data.frame()

power_analysis_plot <- ggplot(data = delta_frame, aes(x = sigs, y = deltas)) + geom_point() + geom_line(col = "red") +
  ylab("Estimated Detectable Change in Infiltration Rate (in/hr)") + xlab("Test Significance (p-value)") +
  ggtitle("Power Analysis Results") + theme_bw() + scale_x_continuous(breaks = (0:6)*0.05)

#difference
exp(log_infil_mean) - exp(log_infil_mean - log_infil_sd)

srt_tested <- dbGetQuery(mars_con,"select distinct(s.system_id) from fieldwork.viw_srt_full s
                                    full join external.tbl_systembdv bdv
                                    on bdv.system_id = s.system_id
                                    where s.test_date >= '2018-08-01'
                                    and s.phase = 'Post-Construction'
                                    and type != 'Pre-Inspection Dye Test'
                                    and public = TRUE
                                    and (bdv.sys_modelinputcategory = 'Subsurface infiltration'
                                    OR bdv.sys_modelinputcategory = 'Bioinfiltration'
                                    OR bdv.sys_modelinputcategory = 'Subsurface slow release (unlined)'
                                    oR bdv.sys_modelinputcategory = 'Bioretention (unlined)')")

ab_test %>% dplyr::filter(system_id %in% srt_tested$system_id)
