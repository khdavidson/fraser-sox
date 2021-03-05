
# Sonar infilling script
# KD
# 202

####################################################################################################################################################

library(tidyverse)
library(xlsx)       # for loading excel files 
library(openxlsx)   # for large excel files
library(janitor)    # for converting excel numeric date to Date, e.g., excel_numeric_to_date()
library(lubridate)  # convert to time series within dyplyr
library(stringr)    # pad 0 time series, e.g., str_pad()
library(withr)      # for with_options()
library(padr)       # to expand time series e.g., pad()
library(zoo)


# set wd 
setwd("~/Documents/ANALYSIS/data/Sonar")

# load excel count data - sheet #6
count.dat <- read.xlsx("Chilko_Sonar_tool_2020.xlsm", sheet=7, na.strings = c(""), startRow=5)

####################################################################################################################################################

#                                                                        CLEANING

# change date format, clean up dataframe 
count.dat$Date <- excel_numeric_to_date(count.dat$Date)

count.dat <- count.dat %>% 
  rename(bank = Bank,
         observer = Observer,
         date = Date,
         count_hr = Count.Hour,
         hr_block = Portion.of.hour,
         time_length_min = Time.counted_min,
         sox_us = Sox_us,
         sox_ds = Sox_ds,
         ch_us = CH_us,
         ch_ds = CH_ds,
         count_number = `Obs.Count.#`,
         comments = Comments) 

####################################################################################################################################################

#                                                                 TIME SERIES EXPANSION

#####################
# MAKE TS, REFORMAT #
#####################

# insert leading 0 (e.g. "9" becomes "09") for time series later 
# add ':00' after every 2nd digit
# make new date-time time series variable 
count_data <- count.dat %>% 
  mutate(count_hr = with_options(c(scipen = 999), str_pad(count_hr, 2, pad = "0"))) %>%
  mutate(count_hr = gsub('(.{2})', '\\1:00', count_hr)) %>%
  mutate(date_time = as.POSIXct(paste(date, count_hr), tz="")) %>%
  print() 


################
# PAD AND JOIN #
################
# PRIMARY COUNTS ONLY - expand for missing hours by bank  
count_data_1 <- pad(count_data %>% filter(count_number==1), by="date_time", interval="hour", group="bank") #2824 rows

# ADDITIONAL COUNTS - extract these and add back on to expanded primary counts
count_data_gt1 <- count_data %>%
  filter(count_number>1) %>% 
  print() #583
  
# Join 
count_data_pad <- rbind(count_data_1, count_data_gt1) #3407 rows


#################
# FILL METADATA #
#################
# fill metadata for infilled hours 
infill <- count_data_pad %>%
  mutate(observer = ifelse(is.na(observer), "INFILL", observer),
    date = if_else(is.na(date), as.Date(as.character(date_time)), date),
    count_hr = ifelse(is.na(count_hr), format(as.POSIXct(date_time, format='%Y-%m-%d %H:%M:%S'), format='%H:%M'), count_hr),
    hr_block = ifelse(is.na(hr_block), "0-20 min", hr_block), 
    time_length_min = ifelse(is.na(time_length_min), 20, time_length_min),
    count_number = ifelse(is.na(count_number), 1, count_number),
    ydate = lubridate::yday(date)) %>%
  print()

# This was compared to a manually-infilled spreadsheet (C:\DFO-MPO\Data - MACRO FILES\Chilko_Sonar_tool_2020_KDinfill.xlsm) and found to produce
# the same result.




####################################################################################################################################################

#                                                                      INFILLING


# mini dataframe to test with 
hrs <- c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00")
test <- infill %>% 
  filter(date > as.Date("2020-09-17"), date < as.Date("2020-09-21"), count_hr %in% hrs, bank=="Left") %>% 
  print()


test <- test %>% 
  group_by(count_hr) %>%
  mutate(sox_us_infilled = ifelse(is.na(sox_us), 
    rollapply(sox_us, width=1, FUN=function(x) mean(sox_us, na.rm = TRUE), fill=NA, align="center"), 
    sox_us)) %>%
      print()


# full run 
infill <- infill %>% 
  group_by(bank, count_hr) %>% 
  mutate(sox_us_infilled = ifelse(is.na(sox_us), 
                                  rollapply(sox_us, width=1, FUN=function(x) mean(sox_us, na.rm = TRUE), fill=NA, align="center"), 
                                  sox_us)) %>%
      print()




sub <- infill %>%
  filter(observer=="INFILL") %>% 
  print()

sum(sub$sox_us_infilled)


