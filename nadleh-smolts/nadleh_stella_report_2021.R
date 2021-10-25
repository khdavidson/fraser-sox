
# NORTHERN SMOLTS ANALYSIS 2019 + 2021
# PSC Final Report Year 2 code 
# Building on 'nautley_final_report.R' script generated from 2019 report


############################################################################################################################################

library(tidyverse)
library(readxl)
library(lubridate)

setwd("~/ANALYSIS/data")

catch.data <- read_excel("Northern_smolt_database_2019-2021.xlsx", sheet="nightly_catch")
enviro.data <- read_excel("Northern_smolt_database_2019-2021.xlsx", sheet="environmental")
lf.data <- read_excel("Northern_smolt_database_2019-2021.xlsx", sheet="length_frequency")
bio.data <- read_excel("Northern_smolt_database_2019-2021.xlsx", sheet="biosampling")


############################################################################################################################################


#                                                  CLEANING

catch.data <- catch.data %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>% 
  mutate(time_trap_open_date = if_else(time_trap_open%in%c("11:00", "11:15", "11:30",  "13:00", "13:05", "14:00", "15:00", "17:00", 
                                                           "20:00", "20:05", "20:10", "20:15", "20:20", "20:25", "20:30", "20:35", "20:40", "20:45",
                                                           "21:00", "21:05", "21:10", "21:15", "21:20", "21:30", "21:40", 
                                                           "22:00", "22:05", "22:10", "22:20", "22:30", "22:35", 
                                                           "23:00", "23:15", "23:30", "23:40"), 
                                       as.Date(date_opened), 
                                  if_else(time_trap_open%in%c("00:00", "00:10", "00:30", 
                                                              "01:00", "01:10", "01:30", "01:45",
                                                              "02:00", "02:15", "02:25", "02:30",
                                                              "03:00", "04:00", "05:00"), as.Date(date_closed), 
                                          as.Date(NA))),
         time_trap_closed_date = if_else(time_trap_closed%in%c("23:30", "23:00", "22:30", "22:15", "22:00", "21:40", "21:30", "21:15",
                                                               "21:10", "21:05", "21:00", "20:30", "20:00", "18:30", "16:00", "15:00",
                                                               "14:00", "13:00", "11:15"), 
                                         as.Date(date_opened), 
                                         if_else(time_trap_closed%in%c("09:15", "09:00", "08:30", "04:00", "03:00", "02:30", "02:15",
                                                                     "02:00", "01:45", "01:30", "01:00", "00:30", "00:00"), 
                                                 as.Date(date_closed), as.Date(NA))),
         time_trap_closed_date = if_else(date_opened==as.Date("2019-04-25")&time_trap_open=="21:00", 
                                        as.Date("2019-04-26"), as.Date(time_trap_closed_date)),
         open_datetime = as.POSIXct(paste(time_trap_open_date, time_trap_open), format="%Y-%m-%d %H:%M"),
         closed_datetime = as.POSIXct(paste(time_trap_closed_date, time_trap_closed), format="%Y-%m-%d %H:%M"),) %>%
  print()

enviro.data <- enviro.data %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>% 
  print()

lf.data <- lf.data %>% 
  mutate_at(vars(date_opened, date_closed), as.Date) %>% 
  print()  

bio.data <- bio.data %>% 
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  mutate_at("time_trap_closed", as.character) %>%
  mutate_at(vars(NEWregion1:NEWprob2, region3:prob6), as.numeric) %>% 
  print()  


##############################################################################################################################################


#                                                                CATCH SUMMARY

# Total number of unique sockeye caught, sampled, died (and death rate)
catch.data %>% 
  group_by(year, site) %>%
  summarize(total_caught=sum(total_unmarked, na.rm=T), total_dead=sum(n_unmarked_dead, na.rm=T), 
            total_sampled=sum(n_unmarked_sampled, na.rm=T), start_date=min(date_opened), end_date=max(date_closed)) %>%
  mutate(mort_rate = (total_dead/total_caught)*100)

# Chinook by-catch 
catch.data %>% 
  group_by(year, site) %>%
  summarize(total_ck_smolt=sum(n_chinook_smolts, na.rm=T), total_ck_fry=sum(n_chinook_fry, na.rm=T))

# Number with paired l/w/scales/DNA
bio.data %>% 
  filter(!is.na(length_mm) | !is.na(weight_g) | !is.na(PSC_uid) | !is.na(whatman_uid)) %>%
  group_by(year, site) %>%
  summarize(n=n())
## ***** resolve this - total #s biosampled are greater than those on the catch entry sheet

# Number LF'd
lf.data %>% 
  filter(data_type=="Routine") %>% 
  group_by(year, site) %>% 
  summarize(n=n())


##############################################################################################################################################


#                                               OVERALL MIGRATION / CPUE TRENDS



#--------- Calculate fished time interval, DOY, and interval CPUE 
catch.data <- catch.data %>%
  mutate(fished_time_interval = ifelse(grepl("Release location", location), NA,
                                       as.numeric(difftime(closed_datetime, open_datetime, units = "hours"))),
         DOY = lubridate::yday(date_opened),
         interval_CPUE = total_unmarked/fished_time_interval) %>%
  print()

# PLOT: Raw catch, not corrected
ggplot(cpue_night%>%filter(year=="2021", site=="Nadleh"), aes(x=as.Date(DOY, origin="2019-01-01"), y=total_caught)) +
  geom_bar(stat="identity") +
  scale_x_date(date_labels="%b %d", date_breaks="2 day") +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black", angle=45, hjust=1))
#facet_wrap(~paste(site, year, sep="-"),  nrow=3, scales="free_y")


#--------- Calculate NIGHTLY total CPUE 
cpue_night <- catch.data %>% 
  filter(!grepl("day shift", comments), trap_type!="large fyke") %>% 
  group_by(year, site, date_opened, DOY) %>% 
  summarize(hrs_fished = sum(fished_time_interval), total_caught = sum(total_unmarked,na.rm=T), CPUE=total_caught/hrs_fished) %>% 
  print()





#--------- HOURLY AVG % (night time)
hourly_passage <- catch.data %>%
  filter(year==2021, site=="Nadleh", !grepl("day shift", comments), !grepl("Release location", location)) %>% 
  group_by(date_opened, time_trap_closed) %>% 
  summarize(hourly_caught = sum(total_unmarked, na.rm=T)) %>%
  mutate(perc = hourly_caught/sum(hourly_caught, na.rm=T)) %>%
  group_by(time_trap_closed) %>%
  summarize(mean_perc = mean(perc, na.rm=T), sd_perc = sd(perc, na.rm=T)) %>%
  mutate(sd_perc = ifelse(is.na(sd_perc), 0, sd_perc)) %>%
  ungroup() %>%
  print()

hourly_passage$time_trap_closed <- factor(hourly_passage$time_trap_closed, levels=c("20:30", "21:00", "21:05", "22:00", "23:00",
                                                                                    "00:00", "01:00", "01:30", "02:00", "03:00", "04:00"),
                                          ordered=T)

ggplot(hourly_passage, aes(x=time_trap_closed, y=mean_perc)) +
  geom_ribbon(data=hourly_passage, aes(x=time_trap_closed, ymin=mean_perc-sd_perc, ymax=mean_perc+sd_perc, group=1), fill="gray85", alpha=0.7) +
  geom_point(size=3, shape=21, stroke=1.1, fill="dodger blue") +
  theme_bw()



