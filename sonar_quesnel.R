# Sonar analysis 

library(XLConnect)
library(tidyverse)
library(xlsx)
library(openxlsx)

# set wd
setwd("~/Data")

# read in just count form 
counts <- read.xlsx("Quesnel Sonar Tool 2019_KD_infill.xlsm", sheet = 6, startRow=5, cols=c(1:13), colNames=T, detectDates=T)

# clean up
counts <- counts %>% 
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
         comments = Comments,
         comments2 = Column1) %>%
  mutate(sox_net = sox_us-sox_ds)

#####
# 1. Infill 
#####

  # done manually this time in excel

#####
# 2+3. Calculate average net upstream and Expand for hourly net upstream
#####

# Total net, and hourly net 
counts.us <- counts %>% 
  select(bank:sox_net) %>%
  group_by(bank, date, count_hr_24, hr_block, time_length_min, count_number) %>% 
  summarize(sox_net_us = mean(sox_net)) %>% 
  mutate(sox_us_hrly = ifelse(sox_net_us==0, 0, (sox_net_us/time_length_min)*60)) %>%
  arrange(date, count_hr_24) %>%
  print()

    #>> at this point, if you wanted to do IOA, you would use this file which is at the observer level

#####
# 4. Average hourly upstream (observers combined into avgs)
#####

# averaged between observers
counts.avg.us <- counts.us %>% 
  group_by(bank, date, count_hr_24, hr_block, time_length_min) %>% 
  summarize(sox_hrly_avg = mean(sox_us_hrly)) %>% 
  print()

# summarized by date, bank
daily.counts <- counts.avg.us %>% 
  group_by(bank, date) %>% 
  summarize(total = sum(sox_hrly_avg)) %>% 
  print()

daily.counts <- as.data.frame(daily.counts)


##########################
# TAIL END EXTRAPOLATION #
##########################

#####
# 1. Make last day a total count (currently a partial count) - BASED ON % TOTAL PREVIOUS DAY
#####

# last full day % of total 
last <- counts.avg.us %>% 
  group_by(bank) %>%
  filter(date=="2019-09-29") %>% 
  summarize(propn = sum(sox_hrly_avg[count_hr_24 %in% c(0:7)])/sum(sox_hrly_avg)) %>%
  print()

    # Sept 29 0-7hrs    Left bank: 0%  |  Right bank: 80%

# last day -> total count using above % of total 
daily.counts.e <- daily.counts %>% 
  mutate(total = ifelse(date=="2019-09-30" & bank=="Right", total/0.8, 
                 ifelse(date=="2019-09-30" & bank=="Left", total/1, total))) %>%                          # note here divided by 1, not 0 because that gives an undefined number. this may need to be revised depending on our choice of expansion calculation
  print()

    # To me this way makes the most sense, rather than ignoring the first 8 hours of the day counted and just using a daily decay rate. 
    # but using a daily decay rate removes the issues around dividing by 0. 
    # this should be tested with the previous days data to see if this assumption holds up across days (or is the % of total daily counts different in the same hour chunks every time)


#####
# 2. Determine decay rate to zero to fill in tail end days 
#####

# select just tail end from last peak 
tail.end <- daily.counts.e %>%
  group_by(date) %>%
  filter(between(date, as.Date("2019-09-23"), as.Date("2019-09-30"))) %>% 
  summarize(sox_total = sum(total)) %>% 
  print()

    # could plot for exp model but no point as equation is not used 

# loop through calculating proportional declines 
for(i in 1:length(tail.end$sox_total)){
  tail.end$propn_decl[i]<-(tail.end$sox_total[i] - tail.end$sox_total[i+1])/tail.end$sox_total[i]
}

avg.decline <- mean(tail.end$propn_decl, na.rm=T)

    # average 35.9% decline in abundance per day from sept 23 to 30

#####
# 3. Apply decay rate
#####















