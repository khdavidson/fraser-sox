
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
library(zoo)
library(fpp2)

# set wd 
setwd("C:/DFO-MPO/Data - MACRO FILES")

# load excel count data - sheet #6
count_data <- read.xlsx("Chilko Sonar Tool 2019- no infill.xlsm", sheet=7, na.strings = c(""), startRow=5)

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


    #~~~~~~~~~~~~~~~~~#
    # DUPLICATE CHECK #
    #~~~~~~~~~~~~~~~~~#
    
    # are there any duplicated entries?
    # this will show you all of the rows, excluding counts which are allowed to vary - this solves for replicated metadata cases only
    count_data %>% 
      select(bank, observer, date, count_hr_24, hr_block, time_length_min, count_number) %>% 
      filter(duplicated(.)) %>% 
      print()
    
    #---
    # PAUSE: YOU HAVE A CHOICE NOW TO CONSIDER 
    #---
    
    # in some cases, this will be an honest duplicate, and one row can be deleted: 
      count_data <- count_data %>% 
        distinct() %>% 
        print
    
    # BUT, sometimes it will be an entry error (e.g., time blocks were not correctly entered). In this case, it requires manual changing of the
    # metadata. To maintain the structure of the code and past data, I will change the values manually here, but this could simply be done in Excel
    # and re-loading the workbook above. 
    
      # The fields that need changing are count_hr_24 (column 4), rows 1909-1913 inclusive
      count_data[1909,4] <- "00:00"                                   # times to change to were obtained from hardcopy data sheets
      count_data[1910,4] <- "01:00"
      count_data[1911,4] <- "03:00"
      count_data[1912,4] <- "06:00"
      count_data[1913,4] <- "15:00"
    
      
    # Re-check duplicates again: 
    count_data %>% 
      select(bank, observer, date, count_hr_24, hr_block, time_length_min, count_number) %>% 
      filter(duplicated(.)) %>% 
      print()
    
        # NO DUPLICATES! :)


#####################
# PAD MISSING HOURS #        
#####################

# make as time-series
count_data$date_time <- as.POSIXct(paste(count_data$date, count_data$count_hr_24), format="%Y-%m-%d %H:%M", tz="UTC")

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
  mutate(time_length_min = ifelse(is.na(time_length_min), 20, time_length_min)) %>% 
  mutate(date=as.Date(strptime(count_020$date_time, "%Y-%m-%d"))) %>% 
  mutate(count_hr_24=as.character(strftime(count_020$date_time, format="%H:%M:%S", tz="UTC")))


    #~~~~~~~~~~~~~~~~~#
    # DUPLICATE CHECK #
    #~~~~~~~~~~~~~~~~~#
    # did the infill expansion work?  
    
    # New empty time series for length of whole season, 1 hour intervals
    ts.hour <- data.frame(date_time=seq(ymd_hm("2019-08-09 12:00", tz=""), ymd_hm("2019-10-04 09:00", tz=""), by="1 hour"))
    
    # Merge counts and empty time series dataframes
    counts_check <- full_join(ts.hour, count_020, by=c("date_time")) 
    
    # subsample just by count 1 (no double counts)
    counts_check1 <- counts_check %>% 
      filter(count_number=="1") %>%
      print()
    
    # Identify the row numbers of duplicates (used to index below)
    counts_check[duplicated(counts_check, by = key(counts_check$bank, counts_check$count_number)),]
    
        # NO DUPLICATES :)



#############
# INFILLING #        
#############
    
# split by L and R banks 
count_020L <- count_020 %>% 
      filter(bank == "Left") %>% 
      print()
    
count_020R <- count_020 %>% 
  filter(bank == "Right") %>% 
  print()

# Baby steps 

# this is how i get to the day before and day after 
# t minus 1
count_020L$tm1 <- ifelse(is.na(count_020L$sox_us), count_020L$date_time-86400, count_020L$date_time)  
attributes(count_020L$tm1) <- attributes(count_020L$date_time)
# t plus 1
count_020L$tp1 <- ifelse(is.na(count_020L$sox_us), count_020L$date_time+86400, count_020L$date_time)
attributes(count_020L$tp1) <- attributes(count_020L$date_time) 




count_020L["sox_us2"] <- lapply(count_020L$tm1, function(col) count_020L$sox_us[match(col, count_020L$tm1)])




for (i in 1:length(count_020L$sox_us)){
  count_020L$sox_us[i]<-ifelse(is.na(count_020L$sox_us[i]), )
}






for(i in 1:length(count_020)){
  t1[i] <- ifelse(is.na(count_020$sox_us[i]), 
   count_020$date_time[i-86400], 
    count_020$sox_us[i]) 
}




    
for (i in count_data) {
  t1 <- count_020[i,13]-(86400*i)
  t2 <- count_data[i,13]+(86400*i) 
  count_data$sox_us <- ifelse( is.na(count_data$sox_us))
    
}


for(i in 1:length(tail.end$sox_total)){
  tail.end$propn_decl[i]<-(tail.end$sox_total[i] - tail.end$sox_total[i+1])/tail.end$sox_total[i]
}


count_data[1441,13]-(86400*i)       # specific cell 
count_data[,]$date_time-(86400*i)    #all rows in date_time 
count_data[,13]-(86400*i)           # all rows in column 13, but different format   





