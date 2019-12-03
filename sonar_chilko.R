
# Sonar infilling script

#library(XLConnect)
library(xlsx)       # for loading excel files 
library(openxlsx)   # for large excel files
library(dplyr)
library(janitor)    # for converting excel numeric date to Date
library(lubridate)  # convert to time series within dyplyr
library(stringr)    # pad 0 time series
library(withr)      # for with_options()
library(padr)       # to expand time series
library(tidyr)      # for fill()
library(tidyverse)

# set wd 
setwd("C:/DFO-MPO/Data - MACRO FILES")

# load excel count data - sheet #6
count_data <- read.xlsx("Chilko Sonar Tool 2019.xlsm", sheet=7, na.strings = c(""), startRow=5)

#################
# CLEANING CODE #                                                   # Run all of the code below, no need to change anything. This works for any sonar tool file.
#################

# change date format, clean up dataframe 
count_data$Date <- excel_numeric_to_date(count_data$Date)

count_data <- count_data %>% 
  rename(bank = Bank,
         observer = Observer,
         date = Date,
         count_hr_24 = Count.Hour,
         hr_block = Portion.of.hour,
         time_length_min = Time.counted_min,
         sox_us = Sox_us,
         sox_ds = Sox_ds,
         ch_us = CH_us,
         ch_ds = CH_ds,
         count_number = `Obs.Count.#`,
         comments = Comments) 

# make as time series 
count_data$count_hr_24 <- with_options(c(scipen = 999), str_pad(count_data$count_hr_24, 2, pad = "0"))

# add ':00' after every 2nd digit
count_data <- count_data %>% 
  mutate(count_hr_24 = gsub('(.{2})', '\\1:00', count_hr_24))

# as time-series
count_data$date_time <- as.POSIXct(paste(count_data$date, count_data$count_hr_24), format="%Y-%m-%d %H:%M", tz="")


#####################
# PAD MISSING HOURS #        
#####################

#####
# FILTER
#####

# select only 0-20 for infilling for prelims - don't need 20-40 or 40-60
count_020 <- count_data %>%
  filter(hr_block == "0-20 min") %>%
  print()

# expand for missing hours 
count_020 <- pad(count_020, interval="hour", group="bank", by="date_time")               

# fill metadata for infilled hours 
count_020 <- count_020 %>%
  mutate(observer = ifelse(is.na(observer), "INFILL", observer)) %>% 
  mutate(hr_block = "0-20 min") %>%
  mutate(time_length_min = ifelse(is.na(time_length_min), 20, time_length_min)) 




# New empty time series for length of whole season, 1 hour intervals
ts.hour <- data.frame(date_time=seq(ymd_hm("2019-08-09 12:00", tz=""), ymd_hm("2019-10-04 09:00", tz=""), by="1 hour"))
ts.hour$date <- as.Date(ts.hour$date_time)
ts.hour$count_hr_24 <- strftime(ts.hour$date_time, format="%H:%M")

# Merge counts and empty time series dataframes
counts_hour <- full_join(ts.hour, count_020, by=c("date", "count_hr_24")) 

counts_hour$date2 <- as.Date(counts_hour$date_time)












#### MATH


for (i in count_data) {
  t1 <- count_data[i,13]-(86400*i),
  t2 <- count_data[i,13]+(86400*i), 
  count_data$sox_us <- ifelse( is.na(count_data$sox_us), 
    
}


for(i in 1:length(tail.end$sox_total)){
  tail.end$propn_decl[i]<-(tail.end$sox_total[i] - tail.end$sox_total[i+1])/tail.end$sox_total[i]
}


count_data[1441,13]-(86400*i)       # specific cell 
count_data[,]$date_time-(86400*i)    #all rows in date_time 
count_data[,13]-(86400*i)           # all rows in column 13, but different format   

mean( c(count_data$sox_us[count_data[2026,]$date_time-(86400*i), count_data[2026,]$date_time+(86400*i)) )




