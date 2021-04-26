

# NADLEH LUNAR CYCLES 


setwd("~/ANALYSIS/data")

library(tidyverse)
library(lubridate)
library(xlsx)
library(openxlsx)

# Load data
naut9900 <- read.csv("1999-2000 Nautley data.csv")
nad.19raw <- read.xlsx("nadleh_ANALYTICAL_database_2019.xlsx", sheet=2, detectDates = T)
inseason2021 <- read.xlsx("nadleh_data_entry_2021.xlsx", sheet = "nightly_catch", detectDates = T)
moon <- read.csv("moon_phase_nadleh.csv")


##########################################################################################################################################################################

#                                                                                   CLEAN


#-------- Historical data cleaning 
nad.hist <- naut9900 %>%
  unite(date, date, year, sep="-") %>%
  mutate(year = paste(str_sub(date, -4))) %>%
  mutate(date = lubridate::dmy(date)) %>% 
  print

nad.hist$start_time <- factor(nad.hist$start_time, levels=c("20:00", "21:00", "22:00", "22:30", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00"), ordered=T)
nad.hist$end_time <- factor(nad.hist$end_time, levels=c("21:00", "22:00", "22:30", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00", "5:00"), ordered=T)


#-------- 2019 data cleaning
# time blocks of interest
time_of_interest <- c("20:00", "20:15", "20:20", "20:30", "20:40", "20:45", "21:00", "21:30", "22:00", "22:10", "22:20", "22:30", 
                      "23:00", "23:30", "23:40", "00:00", "00:30", "01:00", "01:10", "01:30", "01:45", "02:00", "02:15", "02:25", "02:30")

# clean data, filter, order
nad.19 <- nad.19raw %>% 
  filter(trap_type == "small RST") %>%
  filter(start_time %in% time_of_interest) %>%
  print()
nad.19$start_time <- factor(nad.19$start_time, 
                            levels = c("20:00", "20:15", "20:20", "20:30", "20:40", "20:45", "21:00", "21:30", "22:00", "22:10", "22:20", "22:30", "23:00", "23:30", 
                                       "23:40", "00:00", "00:30", "01:00", "01:10", "01:30", "01:45", "02:00", "02:15", "02:25", "02:30"), ordered=T)
nad.19$end_time <- factor(nad.19$end_time, 
                          levels = c("20:00", "22:00", "22:15", "22:30", "23:00", "23:30", "00:00", "00:30", "01:00", "01:30", "01:45", "02:00", "02:15", "02:30",
                                     "03:00", "08:30", "09:00"), ordered=T)


#-------- Moon data clean 
moon <- moon %>%
  mutate(date=ifelse(date=="18-May", "18-May-00", date)) %>% 
  mutate(date=as.Date(date, format="%d-%b-%y")) %>% 
  mutate(total=2000) %>% 
  mutate(yday=lubridate::yday(date)) %>%
  rename(moon=moon_phase) %>%
  select(date, year, total, yday, moon) %>%
  print()


########################################################################################################################################################################

#                                                                 SUMMARIZE


#-------- RAW CATCH OVER TIME 

# For 1999 & 2000
daily <- nad.hist %>% 
  group_by(date, year) %>% 
  summarize(total = sum(total_sox)) %>% 
  mutate(yday=lubridate::yday(date)) %>%
  mutate(moon=NA) %>%
  print()

# For 2019
daily19 <- nad.19 %>% 
  group_by(date) %>% 
  summarize(total = sum(sox_smolts)) %>% 
  mutate(year = 2019) %>%
  mutate(yday=lubridate::yday(date)) %>%
  mutate(moon=NA) %>%
  print()

# For 2021
inseason2021summary <- inseason2021 %>% 
  filter(!is.na(time_trap_closed)) %>%
  group_by(date_closed) %>%
  summarize(night_count = sum(n_unmarked_sampled+n_unmarked_dead+n_unmarked_spilled)) %>%
  rename(date=date_closed,
         total=night_count) %>%
  mutate(year=2021) %>%
  mutate(yday=lubridate::yday(date)) %>%
  select(date, year, total, yday) %>% 
  mutate(moon=NA) %>%
  print()

#-------- Join
daily2 <- rbind(as.data.frame(daily), as.data.frame(daily19), as.data.frame(inseason2021summary), as.data.frame(moon))


#-------- Plot
ggplot() +
  geom_line(data=daily2%>%filter(is.na(moon)), aes(x=as.Date(yday, origin="2000-01-01"), y=total, group=year, colour=year), size=0.9) +
  geom_point(data=daily2%>%filter(is.na(moon)), aes(x=as.Date(yday, origin="2000-01-01"), y=total, group=year, colour=year, fill=year), stroke=1.5, pch=21, size=3, alpha=0.7) + 
  geom_point(data=daily2%>%filter(!is.na(moon)), aes(x=as.Date(yday, origin="2000-01-01"), y=total, group=year), colour="red", fill="red", stroke=1.5, pch=25, size=3) + 
  
  scale_x_date(date_labels="%b %d", date_breaks="5 day") +
  labs(x="", y="Raw catch") +
  facet_wrap(.~year, scales = "free_y", nrow=4) +
  theme_bw() +
  theme(legend.position=c(0.1,0.9),
        legend.title=element_blank(),
        legend.text=element_text(size=13),
        legend.spacing.y = unit(0, "mm"),
        legend.background = element_rect(colour="black"),
        axis.text = element_text(colour="black", size=13), 
        axis.title = element_text(face="bold", size=15),
        strip.text = element_text(size=13))



