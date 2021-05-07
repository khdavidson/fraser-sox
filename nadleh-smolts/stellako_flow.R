
# Hydrograph comparisons for Stellako 
# May 2021

#################################################################################################################################################################

library(tidyverse)
library(lubridate)
library(stringr)   # for str_pad
library(egg)    # for ggarrange()

setwd("~/ANALYSIS/data")

flow.raw <- read.csv("hydroFLOW_stellako_2021_realtime_08JB002_QR_May-7-2021_07_52_46PM.csv")
level.raw <- read.csv("hydroLEVEL_stellako_2021_realtime_08JB002_HG_May-7-2021_07_54_53PM.csv")
hist.raw <- read.csv("hydroF+L_stellako_historical_08JB002_QR_May-7-2021_07_52_46PM.csv")

#################################################################################################################################################################

#                                                                  CLEAN

#------ REAL TIME DATA
flow.2021 <- flow.raw %>% 
  separate(date_time, into=c("date", "time"), sep=" ") %>%
  mutate(time = str_pad(time, 5, pad = "0")) %>%
  mutate(date = lubridate::mdy(date)) %>%
  mutate(date_time = as.POSIXct(paste(date, time))) %>%
  print()

flow.2021.sum <- flow.2021 %>%
  group_by(date) %>%
  summarize(mean_flow = mean(flow_cms), sd=sd(flow_cms)) %>%
  mutate(yday = yday(date)) %>%
  print()

level.2021 <- level.raw %>%
  separate(date_time, into=c("date", "time"), sep=" ") %>%
  mutate(time = str_pad(time, 5, pad = "0")) %>%
  mutate(date = lubridate::mdy(date)) %>%
  mutate(date_time = as.POSIXct(paste(date, time))) %>%
  print()

level.2021.sum <- level.2021 %>%
  group_by(date) %>%
  summarize(mean_level = mean(level_m), sd=sd(level_m)) %>%
  mutate(yday = yday(date)) %>%
  print()

# JOIN
FL.2021 <- full_join(flow.2021, level.2021, by=c("station_number", "station_name", "date", "time", "parameter", "date_time"))


#------ HISTORICAL DATA
hist <- hist.raw %>% 
  mutate(date = lubridate::mdy(date)) %>%
  mutate(yday = yday(date)) %>%
  filter(yday%in%c(91:145)) %>%
  print()

flow.hist.sum <- hist %>%
  filter(!is.na(flow_cms)) %>%
  group_by(yday) %>%
  summarize(max_flow = max(flow_cms, na.rm=T), min_flow=min(flow_cms, na.rm=T), mean_flow=mean(flow_cms, na.rm=T)) %>%
  print()

level.hist.sum <- hist %>%
  filter(!is.na(level_m)) %>%
  group_by(yday) %>%
  summarize(max_level = max(level_m, na.rm=T), min_level=min(level_m, na.rm=T), mean_level=mean(level_m, na.rm=T)) %>%
  print()


#################################################################################################################################################################


flow<-ggplot() +
  geom_ribbon(data=flow.hist.sum, aes(x=as.Date(yday, origin="2021-01-01"), ymin=min_flow, ymax=max_flow), fill="gray50", alpha=0.2) +
  geom_ribbon(data=flow.2021.sum, aes(x=as.Date(yday, origin="2021-01-01"), ymin=mean_flow-sd, ymax=mean_flow+sd), fill="green", alpha=0.3) +
  geom_line(data=flow.hist.sum, aes(x=as.Date(yday, origin="2021-01-01"), y=min_flow), colour="gray70", size=0.5) +
  geom_line(data=flow.hist.sum, aes(x=as.Date(yday, origin="2021-01-01"), y=mean_flow), colour="gray70", size=1) +
  geom_line(data=flow.hist.sum, aes(x=as.Date(yday, origin="2021-01-01"), y=max_flow), colour="gray70", size=0.5) +
  geom_line(data=flow.2021.sum, aes(x=as.Date(yday, origin="2021-01-01"), y=mean_flow), colour="green", size=1, linetype="dashed") +
  labs(x= "Date", y="Flow (cms)") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        axis.title = element_text(face="bold"))

level<-ggplot() +
  geom_ribbon(data=level.2021.sum, aes(x=as.Date(yday, origin="2021-01-01"),, ymin=mean_level-sd, ymax=mean_level+sd, fill=year), fill="blue", alpha=0.3) +
  geom_ribbon(data=level.hist.sum, aes(x=as.Date(yday, origin="2021-01-01"), ymin=min_level, ymax=max_level), fill="gray50", alpha=0.2) +
  geom_line(data=level.2021.sum, aes(x=as.Date(yday, origin="2021-01-01"), y=mean_level), colour="blue", size=1) +
  geom_line(data=level.hist.sum, aes(x=as.Date(yday, origin="2021-01-01"), y=min_level), colour="gray70", size=0.5) +
  geom_line(data=level.hist.sum, aes(x=as.Date(yday, origin="2021-01-01"), y=mean_level), colour="gray70", size=1) +
  geom_line(data=level.hist.sum, aes(x=as.Date(yday, origin="2021-01-01"), y=max_level), colour="gray70", size=0.5) +
  labs(x= "Date", y="Level (m)") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        axis.title = element_text(face="bold"))


ggarrange(flow, level, nrow=2)

































