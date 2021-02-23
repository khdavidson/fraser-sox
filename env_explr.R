
# env conditions over time 

library(tidyverse)
library(egg)
library(xlsx)
library(openxlsx)

setwd("~/Documents/ANALYSIS/data")

env.data.raw <- read.xlsx("Master Roving Analysis Spreadsheet.xlsx", sheet="environmental_data")

#####################################################################################################################################################

# CLEAN 

env.data <- env.data.raw %>%
  rename(year=Year,
    run_timing=Run.Timing,
    watershed_group=Watershed.Group,
    stream=`Stream/Shore`,
    survey=`Survey.#`,
    survey_type=Survey.Type,
    date=Date,
    pos=PeakSpawn,
    start_pos=`StartPeakSpawn.(e.g..20-Oct-14)`,
    end_pos=EndPeakSpawn,
    avg_pos=AvgPeakSpawnDate,
    bankfull=`%Bankfull`,
    brightness=Brightness,
    cloud_cover=`%Cloudy`,
    precip_type=PrecType,
    precip_int=PrecInt,
    fish_vis=FishVis,
    oe=Observer.Efficiency,
    water_temp=WTemp,
    water_clarity=WClarity,
    gauge=Gauge) %>% 
  mutate_at("date", as.numeric) %>%
  mutate(date=as.Date(date, origin = "1899-12-30")) %>%
  mutate(water_temp=ifelse(water_temp=="n/a", NA, water_temp)) %>%
  mutate_at("water_temp", as.numeric) %>%
  mutate(watershed_group=ifelse(watershed_group=="early South Thompson", "Early South Thompson", watershed_group)) %>%
  print()

#####################################################################################################################################################

# TEMP 

temp.avg.st <- env.data %>%
  filter(!is.na(water_temp)) %>%
  group_by(year, watershed_group, stream) %>% 
  summarize(mean_temp=mean(water_temp, na.rm=T), sd_temp=sd(water_temp, na.rm=T)) %>%
  print()

temp.avg.wg <- env.data %>%
  filter(!is.na(water_temp)) %>%
  group_by(year, watershed_group) %>% 
  summarize(mean_temp=mean(water_temp, na.rm=T), sd_temp=sd(water_temp, na.rm=T)) %>%
  print()

ggplot(temp.avg.wg, aes(x=year, y=mean_temp)) +
  geom_hline(data=env.data%>%filter(!is.na(water_temp))%>%group_by(watershed_group)%>%summarize(mean_temp=mean(water_temp), med_temp=median(water_temp)),
    aes(yintercept=mean_temp), colour="red") +
  geom_hline(data=env.data%>%filter(!is.na(water_temp))%>%group_by(watershed_group)%>%summarize(mean_temp=mean(water_temp), med_temp=median(water_temp)),
    aes(yintercept=med_temp), colour="red", linetype="dotted") +
  geom_point() +
  geom_errorbar(aes(ymin=mean_temp-sd_temp, ymax=mean_temp+sd_temp), width=0) +
  scale_x_continuous(breaks=seq(2003,2020,by=2)) +
  labs(x="", y="Average temperature (C)") +
  theme_bw() +
  facet_wrap(~watershed_group)









