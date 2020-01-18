# Nautley tagging sim ## 

setwd("~/ANALYSIS/Data")
library(tidyverse)
library(lubridate)

#####################
# LOAD & CLEAN DATA #
#####################

#####
# 1999 & 2000 Data
#####
naut9900 <- read.csv("1999-2000 Nautley data.csv")

# reformat data 
nad.hist <- naut9900 %>%
  unite(date, date, year, sep="-") %>%
  mutate(year = paste(str_sub(date, -4))) %>%
  mutate(date = lubridate::dmy(date)) %>% 
  print

nad.hist$start_time <- factor(nad.hist$start_time, levels=c("20:00", "21:00", "22:00", "22:30", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00"), ordered=T)
nad.hist$end_time <- factor(nad.hist$end_time, levels=c("21:00", "22:00", "22:30", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00", "5:00"), ordered=T)

#####
# 2018 Data 
#####
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
    "23:40", "0:00", "0:30", "1:00", "1:10", "1:30", "1:45", "2:00", "2:15", "2:25", "2:30"), ordered=T)
nad.18$end_time <- factor(nad.18$end_time, 
  levels = c("20:00", "22:00", "22:15", "22:30", "23:00", "23:30", "0:00", "0:30", "1:00", "1:30", "1:45", "2:00", "2:15", "2:30",
    "3:00", "8:30", "9:00"), ordered=T)

################################################################################################################################################

#                                                                 HISTORICAL DATA

#######################
# RAW CATCH OVER TIME #
#######################

# For 1999 & 2000
daily <- nad.hist %>% 
  group_by(date, year) %>% 
  summarize(total = sum(total_sox)) %>% 
  print()

# For 2019
daily18 <- nad.18 %>% 
  group_by(date) %>% 
  summarize(total = sum(sox_smolts)) %>% 
  mutate(year = 2019) %>%
  select(date, year, total) %>%
  print()

# Join
daily2 <- rbind(as.data.frame(daily), as.data.frame(daily18))

# Plot
ggplot(daily2, aes(x=date, y=total, group=year)) +
  geom_line(size=0.9, aes(colour=year)) +
  geom_point(pch=21, size=3, fill="white", aes(colour=year), stroke=1.5) + 
  scale_x_date(labels = function(x) format(x, "%d-%b")) +
  facet_grid(rows=vars(year))


##################################################
# PROPORTION OF RUN EACH HOUR, FOR EACH DAY-YEAR #
##################################################

# For 1999 & 2000
hr.propn <- nad.hist %>% 
  group_by(year, date, end_time) %>% 
  summarize(n = sum(total_sox)) %>%
  mutate(daily_total = sum(n)) %>%
  mutate(propn = (n/daily_total)*100) %>%
  print()

avg.hr.propn <- hr.propn %>% 
  group_by(year, end_time) %>% 
  summarize(avg_propn = mean(propn), sd_propn = sd(propn)) %>% 
  mutate(sd_propn = ifelse(sd_propn == "NaN", 0, sd_propn)) %>%
  print()

# For 2018
hr.propn18 <- nad.18 %>% 
  group_by(date, end_time) %>% 
  summarize(n = sum(sox_smolts)) %>%
  mutate(daily_total = sum(n)) %>%
  mutate(propn = (n/daily_total)*100) %>%
  filter(date >= as.Date("2019-04-26")) %>%
  print()

avg.hr.propn.18 <- hr.propn18 %>% 
  group_by(end_time) %>% 
  summarize(avg_propn = mean(propn), sd_propn = sd(propn)) %>% 
  mutate(sd_propn = ifelse(sd_propn == "NaN", 0, sd_propn)) %>%
  mutate(year = 2018) %>%
  select(year, end_time, avg_propn, sd_propn) %>%
  print()

avg.propns <- rbind(as.data.frame(avg.hr.propn), as.data.frame(avg.hr.propn.18))
avg.propns$end_time <- factor(avg.propns$end_time, 
  levels = c("20:00","21:00", "22:00", "22:15", "22:30", "23:00", "23:30", "0:00", "0:30", "1:00", "1:30", "1:45", "2:00", "2:15", "2:30",
    "3:00","4:00","5:00", "8:30", "9:00"), ordered=T)
avg.propns <- avg.propns %>% 
  filter(end_time != "8:30")

# PLOT
ggplot(avg.propns, aes(x=end_time, y=avg_propn, group=year)) +
  geom_ribbon(aes(ymin=avg.propns$avg_propn-avg.propns$sd_propn, ymax=avg.propns$avg_propn+avg.propns$sd_propn), linetype=2, alpha=0.15) +
  geom_line(size=0.9, aes(colour=year)) +
  geom_point(pch=21, size=3, fill="white", aes(colour=year), stroke=1.5) + 
  facet_grid(rows=vars(year))

###############################################################################################################################################

#                                                         TAGGING SIMULATIONS

#######################################
# TAG 50% HOURLY ABUNDANCE, UP TO 300 #
#######################################

# For 1999 & 2000 
nad.hist <- nad.hist %>%
  mutate(sox_50 = total_sox*0.5) %>% 
  mutate(sox_50 = ifelse(sox_50>300, 300, sox_50))

# For 2018
nad.18 <- nad.18 %>%
  mutate(sox_50 = sox_smolts*0.50) %>%
  mutate(sox_50m = ifelse(sox_50>300, 300, sox_50)) %>%
  print()













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

# tags by date
d.tag.18 <- nad.18 %>% 
  group_by(date) %>% 
  summarize(sum=sum(sox_50)) %>% 
  print

# plot 
ggplot(nad.18, aes(x=start_time, y=sox_smolts, group=date)) +
  geom_line()