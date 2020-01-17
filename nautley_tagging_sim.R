# Nautley tagging sim ## 

setwd("~/ANALYSIS/Data")
library(tidyverse)
library(lubridate)

##########################
# 1999-2000 nautley data #
##########################
naut9900 <- read.csv("1999-2000 Nautley data.csv")

# reformat data 
nad.hist <- naut9900 %>%
  unite(date, date, year, sep="-") %>%
  mutate(year = paste(str_sub(date, -4))) %>%
  mutate(date = lubridate::dmy(date)) %>% 
  print

nad.hist$start_time <- factor(nad.hist$start_time, levels=c("20:00", "21:00", "22:00", "22:30", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00"), ordered=T)
nad.hist$end_time <- factor(nad.hist$end_time, levels=c("21:00", "22:00", "22:30", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00", "5:00"), ordered=T)

# proportion of run each hour
hr.propn <- nad.hist %>% 
  group_by(year, end_time) %>% 
  summarize(n = sum(total_sox)) %>%
  mutate(yearly_total = sum(n)) %>%
  mutate(propn = (n/yearly_total)*100) %>%
  print()


# tag 50% of hourly abundance up to 300 
nad.hist <- nad.hist %>%
  mutate(sox_50 = total_sox*0.5) %>% 
  mutate(sox_50 = ifelse(sox_50>300, 300, sox_50))

  # total tags by year
  y_tags <- nad.hist %>%
    group_by(year) %>%
    summarize(sum = sum(sox_50)) %>%
    print()
  
  # total tags by day
  d_tags <- nad.hist %>%
    group_by(date) %>%
    summarize(sum = sum(sox_50)) %>%
    print()

# hourly abundance 
ggplot(nad.hist, aes(x=hour, y=total_sox, group=date, colour=year)) + 
  geom_point(aes(colour=year), size=2.5, fill="white", pch=21, stroke=1.3) +
  geom_line(size=1) +
  facet_grid(~year)

#############
# 2018 data #
#############

# 2018 data 
nad.18 <- read.csv("2019 Nautley_dailydata.csv")

# time blocks of interest
time_of_interest <- c("20:00", "20:15", "20:20", "20:30", "20:40", "20:45", "21:00", "21:30", "22:00", "22:10", "22:20", "22:30", 
                      "23:00", "23:30", "23:40", "0:00", "0:30", "1:00", "1:10", "1:30", "1:45", "2:00", "2:15", "2:25", "2:30")

# clean data, filter, order
nad.18 <- nad.18 %>% 
  mutate(date = lubridate::dmy(date)) %>%
  mutate(start_date = lubridate::dmy(start_date)) %>%
  mutate(end_date = lubridate::dmy(end_date)) %>%
  filter(trap_type == "small RST") %>%
  filter(start_time %in% time_of_interest) %>%
  print()

nad.18$start_time <- factor(nad.18$start_time, 
  levels = c("20:00", "20:15", "20:20", "20:30", "20:40", "20:45", "21:00", "21:30", "22:00", "22:10", "22:20", "22:30", "23:00", "23:30", 
    "23:40", "0:00", "0:30", "1:00", "1:10", "1:30", "1:45", "2:00", "2:15", "2:25", "2:30"))

nad.18 <- nad.18 %>%
  mutate(sox_50 = sox_smolts*0.50) %>%
  mutate(sox_50m = ifelse(sox_50>300, 300, sox_50)) %>%
  print()

# tags by date
d.tag.18 <- nad.18 %>% 
  group_by(date) %>% 
  summarize(sum=sum(sox_50)) %>% 
  print

# plot 
ggplot(nad.18, aes(x=start_time, y=sox_smolts, group=date)) +
  geom_line()










