
# Sonar infilling script

l#ibrary(XLConnect)
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
setwd("~/ANALYSIS/Data/Sonar")

# load excel count data - sheet #6
count_data <- read.xlsx("Quesnel Sonar Tool 2019_AG.xlsm", sheet = 6, na.strings = c(""), startRow=5)

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
count_data$date_time <- as.POSIXct(paste(count_data$date, count_data$count_hr_24), tz="")

# expand for missing hours 
count_data <- pad(count_data, by="date_time", interval="hour", group="bank")

# fill metadata for infilled hours 
count_data <- count_data %>%
  group_by(bank) %>%
  fill(date, hr_block, time_length_min) %>%
  mutate(observer = ifelse(is.na(observer), "INFILL", observer))


#### MATH


for (i in count_data) {
  t1 <- count_data[i,13]-(86400*i),
  t2 <- count_data[i,13]+(86400*i), 
  count_data$sox_us <- ifelse( is.na(count_data$sox_us), 
    
}




count_data[1441,13]-(86400*i)       # specific cell 
count_data[,]$date_time-(86400*i)    #all rows in date_time 
count_data[,13]-(86400*i)           # all rows in column 13, but different format   

mean( c(count_data$sox_us[count_data[2026,]$date_time-(86400*i), count_data[2026,]$date_time+(86400*i)) )




