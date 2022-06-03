
# NORTHERN SMOLTS ANALYSIS 2019 + 2021
# PSC Final Report Year 2 code 
# Building on 'nautley_final_report.R' script generated from 2019 report


############################################################################################################################################

library(tidyverse)
library(readxl)
library(cowplot)    # for plot_grid (superior to egg::ggarrange!)
library(ggpubr)     # for stat_cor in ggplot
library(padr)
library(withr)
library(recapr)     # for basic M-R analysis 


setwd("~/ANALYSIS/data/nadleh_raw_files")
options(scipen = 9999999)

catch.data.raw <- read_excel("Northern_smolt_database_99-00_19-21.xlsx", sheet="nightly_catch")
enviro.data.raw <- read_excel("Northern_smolt_database_99-00_19-21.xlsx", sheet="environmental")
lf.data.raw <- read_excel("Northern_smolt_database_99-00_19-21.xlsx", sheet="length_frequency")
bio.data.raw <- read_excel("Northern_smolt_database_99-00_19-21.xlsx", sheet="biosampling", guess_max = 10000)
stella.disch.raw <- read.csv("STELLA_DISC_2021_08JB002_QR_Nov-1-2021_09_27_39PM.csv")
nad19.disch.raw <- read.csv("NAUT_DISCH_2019_08JB003_QR_Dec-19-2019_12_44_31AM.csv")
nad21.disch.raw <- read.csv("NAUT_DISCH_2021_08JB003_QR_Nov-1-2021_09_25_53PM.csv")
nadhist.disch.raw <- read.csv("NAUT_DISCH_1999-2000_Daily__Nov-3-2021_08_38_13PM.csv")
nad.hist.raw <- read.csv("1999-2000 Nautley data.csv")
moon.raw <- read.csv("moon_phases.csv")
chilko.raw <- read.csv("chilko_smolt_database_template 1956-2019 - daily data.csv")


############################################################################################################################################


#                                                  CLEANING


# ==================== SMOLT DATA ====================

# CATCH DATA 2019-2021 ----------------
catch.data <- catch.data.raw %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>% 
  # create dates that correspond to open/close times to create date-time objects
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
         # create date-time objects
         open_datetime = as.POSIXct(paste(time_trap_open_date, time_trap_open), format="%Y-%m-%d %H:%M"),
         closed_datetime = as.POSIXct(paste(time_trap_closed_date, time_trap_closed), format="%Y-%m-%d %H:%M"),
         DOY_closed = lubridate::yday(date_closed),
         fished_time_interval_true = 
           ifelse(grepl("Release location", location), 
                  NA, 
                  round(as.numeric(difftime(closed_datetime, open_datetime, units = "hours")),3))) %>%
  select(-c(time_trap_open_date,time_trap_closed_date)) %>%
  print()


# HISTORICAL CATCH DATA 1999-2000 ----------------
nad.hist <- nad.hist.raw %>% 
  rename(time_trap_open = start_time,
         time_trap_closed = end_time) %>%
  unite(col="date", c(date,year), sep="-", remove=F) %>%
  mutate(date = lubridate::dmy(date),
         date_opened = date,
         date_closed = date+1,
         time_trap_open = with_options(c(scipen = 999), str_pad(time_trap_open, 5, pad = "0")),
         time_trap_closed = with_options(c(scipen = 999), str_pad(time_trap_closed, 5, pad = "0")),
         time_trap_open_date = if_else(time_trap_open%in%c("20:00", "21:00", "22:00", "22:30", "23:00"), as.Date(date_opened), as.Date(date_closed)),
         time_trap_closed_date = if_else(time_trap_closed%in%c("21:00", "22:00", "22:30", "23:00"), as.Date(date_opened), as.Date(date_closed)),
         open_datetime = as.POSIXct(paste(time_trap_open_date, time_trap_open), format="%Y-%m-%d %H:%M"),
         closed_datetime = as.POSIXct(paste(time_trap_closed_date, time_trap_closed), format="%Y-%m-%d %H:%M"),
         fished_time_interval_true = round(as.numeric(difftime(closed_datetime, open_datetime, units = "hours")),3),
         CPUE_hourly = round(total_sox/fished_time_interval_true,0),
         DOY_closed = lubridate::yday(date_closed)) %>%
  select(-c(date)) %>%
  select(year, date_opened, date_closed, DOY_closed, time_trap_open, time_trap_closed, total_sox, 
         time_trap_open_date, time_trap_closed_date, open_datetime, closed_datetime, fished_time_interval_true, CPUE_hourly) %>%
  print()



# BIOLOGICAL DATA ----------------
lf.data <- lf.data.raw %>% 
  mutate_at(vars(date_opened, date_closed), as.Date) %>% 
  print()  

bio.data <- bio.data.raw %>% 
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  mutate_at(vars(contains('_reg')), funs(ifelse(.=="4","Nadina", ifelse(.=="12","Stellako",.)))) %>%
  mutate(cf_k = (weight_g/(length_mm)^3)*100000,
         DOY_closed = lubridate::yday(date_closed)) %>%
  print()  



# ==================== ENVIRO DATA ====================

# FIELD ENVIRO DATA ----------------
enviro.data <- enviro.data.raw %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>% 
  print()

# Define ice flow periods for plotting 
ice_flows <- data.frame(xstart = as.Date('2019-04-21'), xend = as.Date('2019-04-25'), year="2019", site="Nadleh") %>%
  bind_rows(., data.frame(xstart = as.Date('2021-04-14'), xend = as.Date('2021-04-15'), year="2021", site="Nadleh")) %>% 
  bind_rows(., data.frame(xstart = as.Date('2021-04-16'), xend = as.Date('2021-04-17'), year="2021", site="Nadleh")) %>% 
  bind_rows(., data.frame(xstart = as.Date('2021-04-18'), xend = as.Date('2021-04-19'), year="2021", site="Nadleh")) %>% 
  bind_rows(., data.frame(xstart = as.Date('2021-04-21'), xend = as.Date('2021-04-23'), year="2021", site="Nadleh")) %>% 
  bind_rows(., data.frame(xstart = NA, xend = NA, year="2021", site="Stellako")) %>%
  mutate(xstartDOY = lubridate::yday(xstart),
         xendDOY = lubridate::yday(xend)) %>%
  print()


# EC DISCHARGE DATA ----------------
# Contemporary real-time 2019-2021
discharge.19_21 <- rbind(stella.disch.raw%>%mutate(year="2021",site="Stellako"), 
                   nad19.disch.raw%>%mutate(year="2019",site="Nadleh"), 
                   nad21.disch.raw%>%mutate(year="2021",site="Nadleh")) %>%
  rename(date_time = `Date..PST.`,
         param_code=Parameter,
         discharge_cms=`Value..m3.s.`) %>% 
  mutate(date=as.Date(substring(date_time, 1, 10), format="%m/%d/%Y")) %>%
  group_by(year,site,date) %>% 
  summarize(mean_dis = mean(discharge_cms), sd=sd(discharge_cms), min_dis=min(discharge_cms), max_dis=max(discharge_cms)) %>%
  mutate(DOY=lubridate::yday(date)) %>%
  filter(DOY>=100) %>%
  print()


# Historical data 1999-2000
discharge.hist <- nadhist.disch.raw %>% 
  select(-c(ID, PARAM, SYM)) %>%
  rename(date=Date,
         discharge_cms=Value) %>%
  mutate(date = lubridate::mdy(date),
         year = paste(format(date, format="%Y")),
         DOY=lubridate::yday(date),
         DOY=ifelse(year=="2000", DOY-1,DOY),     # change DOY because 2000 was a leap year - for the purposes of plotting this is OK
         site="Nadleh") %>%
  filter(year%in%c("1999", "2000"),
         DOY%in%c(100:151)) %>%
  print()


# MOON MOON ----------------
moon <- moon.raw %>%
  mutate(date = lubridate::dmy(date)) %>%
  print()
  

# Moon data clean -------------------
moon <- moon.raw %>%
  mutate(date=ifelse(date=="18-May", "18-May-00", date)) %>% 
  mutate(date=as.Date(date, format="%d-%b-%y")) %>% 
  mutate(total=100) %>% 
  mutate(yday=lubridate::yday(date)) %>%
  rename(moon=moon_phase) %>%
  select(date, year, total, yday, moon) %>%
  print()


# Chilko data clean ------------------- 
chilko <- chilko.raw %>%
  mutate_at("total_abundance", as.numeric) %>% 
  mutate_at("age1_daily_abundance", as.numeric) %>% 
  mutate_at("age2_daily_abundance", as.numeric) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  mutate(yday = lubridate::yday(date)) %>%
  filter(year > 2000) %>%
  print()


##############################################################################################################################################


#                                                                CATCH SUMMARIES

# Total number of unique sockeye caught, sampled, LFed, died (and death rate)
catch.data %>%
  filter(!grepl("Release location", location)) %>%
  group_by(year, site) %>%
  summarize(total_caught=sum(total_unmarked, na.rm=T), total_spill = sum(n_unmarked_spilled,na.rm=T), 
            total_dead=sum(n_unmarked_dead, na.rm=T), total_sampled=sum(n_unmarked_sampled, na.rm=T), total_LF = sum(n_unmarked_lf, na.rm=T),
            start_date=min(date_opened), end_date=max(date_closed)) %>%
  mutate(mort_rate = (total_dead/total_caught)*100)

# Number EWatch samples    ** 2019 data wrong for ewatch ** 
bio.data %>% 
  filter(!is.na(ewatch_uid)) %>% 
  group_by(year, site) %>% 
  summarize(n_ewatch = n()) 

# Chinook by-catch 
catch.data %>% 
  group_by(year, site) %>%
  summarize(total_ck_smolt=sum(n_chinook_smolts, na.rm=T), total_ck_fry=sum(n_chinook_fry, na.rm=T))

# Number smolts with paired l/w/scales/DNA
bio.data %>% 
  filter(!is.na(length_mm) & !is.na(weight_g) & !is.na(PSC_uid) & !is.na(whatman_uid)) %>%
  group_by(year, site) %>%
  summarize(n=n())

# Number LF'd
lf.data %>% 
  filter(data_type=="Routine") %>% 
  group_by(year, site) %>% 
  summarize(n=n())


# DIURNAL PATTERNS ----------------
# TOTAL daytime catch as % of night catch OVERALL
catch.data %>%
  filter(year=="2021", !grepl("Release location", location)) %>% 
  mutate(diurnal_group = ifelse(grepl("day shift", comments), "day shift", "night shift")) %>%
  group_by(year,site,diurnal_group) %>%
  summarize(total_fish = sum(total_unmarked)) %>% 
  mutate(perc_total = (total_fish[1]/total_fish[2])) %>%
  .[[1,5]] -> nadleh_day_propn_2021

catch.data %>%
  filter(year=="2021", !grepl("Release location", location)) %>% 
  mutate(diurnal_group = ifelse(grepl("day shift", comments), "day shift", "night shift")) %>%
  group_by(year,site,diurnal_group) %>%
  summarize(total_fish = sum(total_unmarked)) %>% 
  mutate(perc_total = (total_fish[1]/total_fish[2])) %>%
  .[[3,5]] -> stella_day_propn_2021


# Nadleh ----
# MOVING WINDOW daytime catch as % of night catch
day.shifts <- catch.data %>% 
  filter(site=="Nadleh", year=="2021", grepl("day shift", comments)) %>% 
  group_by(date_opened) %>%
  summarize(total_caught = sum(total_unmarked)) %>% 
  arrange(date_opened) %>% 
  #group_by(site) %>%
  mutate(day_shift_no = rep(seq_len(n()), length.out = n()),
         day_night = "day") %>%
  rename(date=date_opened) %>%
  print()

night.window1 <- catch.data %>%    
  filter(site=="Nadleh", year=="2021", !grepl("day shift", comments), !grepl("Release location",location)) %>%
  #group_by(site) %>%
  filter(date_closed %in% c(day.shifts$date)) %>%
  group_by(date_closed) %>% 
  summarize(total_caught = sum(total_unmarked)) %>%
  arrange(date_closed) %>% 
  #group_by(site) %>%
  mutate(day_shift_no = rep(seq_len(n()), length.out = n()),
         day_night = "night1") %>%
  rename(date=date_closed) %>%
  print()

night.window2 <- catch.data %>%  
  filter(site=="Nadleh", year=="2021", !grepl("day shift", comments), !grepl("Release location",location), 
         date_closed %in% c(day.shifts$date+1)) %>%
  group_by(date_closed) %>% 
  summarize(total_caught = sum(total_unmarked)) %>%
  arrange(date_closed) %>% 
  #group_by(site) %>%
  mutate(day_shift_no = rep(seq_len(n()), length.out = n()),
         day_night = "night2") %>%
  rename(date=date_closed) %>%
  print()

day.night.window <- rbind(day.shifts, night.window1, night.window2) %>%
  group_by(day_shift_no) %>%
  mutate(shift_reference_date = min(date)) %>%
  select(-c(date)) %>%
  pivot_wider(names_from="day_night", values_from="total_caught") %>%
  mutate(propn1 = round((day/night1),3), propn2 = round((day/night2),3), 
         perc1=propn1*100, perc2=propn2*100, avg_perc = mean(c(perc1, perc2))) %>%
  arrange(day_shift_no) %T>%
  write.csv('nad_rept_tab1.csv', row.names = F) %>%
  print()

daily.propns.ranges <- day.night.window %>% 
  ungroup() %>%
  summarize(meanPERC=mean(c(perc1, perc2)), sdPERC=sd(c(perc1,perc2)), 
            minPERC=min(c(perc1,perc2)), maxPERC=max(c(perc1,perc2)),
            
            meanPROPN=mean(c(propn1, propn2)), sdPROPN=sd(c(propn1, propn2)), 
            minPROPN=min(c(propn1, propn2)), maxPROPN=max(c(propn1, propn2)),) %>% 
  print()


# Stellako ----
# MOVING WINDOW daytime catch as % of night catch
day.shiftsS <- catch.data %>% 
  filter(site=="Stellako", year=="2021", grepl("day shift", comments)) %>% 
  group_by(date_opened) %>%
  summarize(total_caught = sum(total_unmarked)) %>% 
  arrange(date_opened) %>% 
  #group_by(site) %>%
  mutate(day_shift_no = rep(seq_len(n()), length.out = n()),
         day_night = "day") %>%
  rename(date=date_opened) %>%
  print()

night.window1S <- catch.data %>%    
  filter(site=="Stellako", year=="2021", !grepl("day shift", comments), !grepl("Release location",location)) %>%
  #group_by(site) %>%
  filter(date_closed %in% c(day.shiftsS$date)) %>%
  group_by(date_closed) %>% 
  summarize(total_caught = sum(total_unmarked)) %>%
  arrange(date_closed) %>% 
  #group_by(site) %>%
  mutate(day_shift_no = rep(seq_len(n()), length.out = n()),
         day_night = "night1") %>%
  rename(date=date_closed) %>%
  print()

night.window2S <- catch.data %>%  
  filter(site=="Stellako", year=="2021", !grepl("day shift", comments), !grepl("Release location",location), 
         date_closed %in% c(day.shiftsS$date+1)) %>%
  group_by(date_closed) %>% 
  summarize(total_caught = sum(total_unmarked)) %>%
  arrange(date_closed) %>% 
  #group_by(site) %>%
  mutate(day_shift_no = rep(seq_len(n()), length.out = n()),
         day_night = "night2") %>%
  rename(date=date_closed) %>%
  print()

day.night.windowS <- rbind(day.shiftsS, night.window1S, night.window2S) %>%
  group_by(day_shift_no) %>%
  mutate(shift_reference_date = min(date)) %>%
  select(-c(date)) %>%
  pivot_wider(names_from="day_night", values_from="total_caught") %>%
  mutate(propn1 = round((day/night1),3), propn2 = round((day/night2),3), 
         perc1=propn1*100, perc2=propn2*100, avg_perc = mean(c(perc1, perc2))) %>%
  arrange(day_shift_no) %T>%
  write.csv('stel_rept_tab1.csv', row.names = F) %>%
  print()

daily.propns.rangesS <- day.night.windowS %>% 
  filter(avg_perc != "NaN", avg_perc < 100) %>%   #toggle with/without "avg_perc < 100" to see without the 115% data point
  ungroup() %>%
  summarize(mean_perc=mean(c(perc1, perc2),na.rm=T), sd=sd(c(perc1,perc2),na.rm=T), 
            min=min(c(perc1,perc2),na.rm=T), max=max(c(perc1,perc2),na.rm=T)) %>% 
  print()



##############################################################################################################################################


#                                               OVERALL MIGRATION / CPUE TRENDS


# ==================== NIGHTLY ====================

# Calculate average/min/max fishing time (effort, i.e., hours) each year and site, excluding the really long days in early 2019 
catch.data %>% 
  filter(date_opened>=as.Date("2019-04-26") & !date_opened%in%c(as.Date("2019-05-26"),as.Date("2019-05-27")), 
         !grepl("Release location", location), !grepl("day shift", comments)) %>%
  group_by(year, site, date_closed) %>% 
  summarize(night_fishing_hrs = sum(fished_time_interval_true)) %>% 
  group_by(year, site) %>% 
  summarize(avg_fishtime = mean(night_fishing_hrs), sd=sd(night_fishing_hrs), median=median(night_fishing_hrs), 
            min=min(night_fishing_hrs), max=max(night_fishing_hrs)) %>%
  .[[1,3]] -> avg_effort_2019


# Calculate CPUE ----------------
## Two methods for early 2019 catch: 
### a) using the average fishing time effort for 2019 (CPUE_hourly), and 
### b) adjusting downward by subtracting average % daily catch for early 2019   <-- this makes more sense as a) produces two different types of data (early 2019 is per hour while late 2019 is rolled up per night)
catch.data <- catch.data %>%
  mutate(fished_time_interval_applied = 
           ifelse(date_opened>=as.Date("2019-04-12") & date_opened<=as.Date("2019-04-25") & fished_time_interval_true>9.5, 
                  avg_effort_2019,
                  fished_time_interval_true),

         CPUE_hourly = round(total_unmarked/fished_time_interval_applied,0),
         
         CPUE_and_propndayMEAN = 
           ifelse(date_opened>=as.Date("2019-04-12") & date_opened<=as.Date("2019-04-25") & fished_time_interval_true>9.5,
                  total_unmarked-(total_unmarked*daily.propns.ranges$meanPROPN),
                  round(total_unmarked/fished_time_interval_applied,0)),
         CPUE_and_propndayUPPER = 
           ifelse(date_opened>=as.Date("2019-04-12") & date_opened<=as.Date("2019-04-25") & fished_time_interval_true>9.5,
                  total_unmarked-(total_unmarked*daily.propns.ranges$minPROPN),
                  round(total_unmarked/fished_time_interval_applied,0)),
         CPUE_and_propndayLOWER = 
           ifelse(date_opened>=as.Date("2019-04-12") & date_opened<=as.Date("2019-04-25") & fished_time_interval_true>9.5,
                  total_unmarked-(total_unmarked*daily.propns.ranges$maxPROPN),
                  round(total_unmarked/fished_time_interval_applied,0))) %>%
  print()


# NIGHTLY CPUE CATCH TABLE - for ease of plotting and visual analysis ----------------  
# 2019-2021
catch.nightly <- catch.data %>% 
  filter(trap_type != "large fyke",                                         # remove fyke net
         !grepl("Release location", location),                              # remove MR release cases
         date_opened!=as.Date("2019-04-25")|time_trap_open!="11:00",        # remove the one day shift in 2019
         year!="2021"|!grepl("day shift", comments)) %>%                    # remove day shifts in 2021
  group_by(year, site, date_closed) %>% 
  summarize(raw_total = sum(total_unmarked),
            nightly_CPUE = sum(CPUE_hourly),
            hourly_CPUE_propnMEAN = sum(CPUE_and_propndayMEAN),
            hourly_CPUE_propnUPPER = sum(CPUE_and_propndayUPPER),
            hourly_CPUE_propnLOWER = sum(CPUE_and_propndayLOWER)) %>% 
  ungroup() %>%
  mutate(DOY_closed=lubridate::yday(date_closed),
         site_year=paste(site, year, sep=" ")) %>%
  print()


# Historical 
catch.hist <- nad.hist %>% 
  group_by(year, DOY_closed, date_closed) %>% 
  summarize(raw_total = sum(total_sox),
            nightly_CPUE = sum(CPUE_hourly)) %>% 
  mutate_at("year", as.factor) %>%
  mutate(site = "Nadleh",
         DOY_closed=ifelse(year=="2000",DOY_closed-1,DOY_closed)) %>%
  print()




# PLOT: FIGURE 1. CPUE MIGRATION CORRECTED ----------------
plot_grid(
# HISTORICAL
  ggplot() +
    geom_point(data=moon%>%filter(year%in%c("1999"), date!=as.Date("1999-05-30")), 
               aes(x=as.Date(lubridate::yday(date), origin="1998-12-31"),y=2200),colour="gray70", fill="gray70",
               shape=21,size=7,stroke=1,alpha=0.6) +
    geom_point(data=moon%>%filter(year%in%c("2000"), date!=as.Date("2000-04-18")), 
               aes(x=as.Date(lubridate::yday(date), origin="1998-12-31"),y=2200),shape=21,col="black",fill="black",
               size=7,stroke=1,alpha=0.7) +
    
    annotate(geom="text", label="A", x=as.Date(101, origin="1998-12-31"), y=2300, fontface=2, size=7, hjust = 0) +
    annotate(geom="text", label="Nautley 1999, 2000", x=as.Date(101, origin="1998-12-31"), fontface=3, y=1900, size=5, hjust = 0) +
    geom_line(data=discharge.hist, 
              aes(x=as.Date(DOY, origin="1998-12-31"), y=discharge_cms*25, group=year, colour=year), alpha=0.6) +
    geom_bar(data=catch.hist%>%filter(year==1999)%>%mutate(nightly_CPUE=ifelse(nightly_CPUE==0,NA,nightly_CPUE)),
             aes(x=as.Date(DOY_closed, origin="1998-12-31"), y=nightly_CPUE, group=year, fill=year, colour=year), 
             stat="identity", size=0.2, alpha=0.4, width=0.8) +
    geom_bar(data=catch.hist%>%filter(year==2000)%>%mutate(nightly_CPUE=ifelse(nightly_CPUE==0,NA,nightly_CPUE)),
             aes(x=as.Date(DOY_closed, origin="1998-12-31"), y=nightly_CPUE, group=year, fill=year, colour=year), 
             stat="identity", size=0.2, alpha=0.3, width=0.8) +
    labs(x="", y="") +
    scale_colour_manual(values=c("gray70", "black")) +
    scale_fill_manual(values=c("gray70", "black")) +
    scale_x_date(limits=c(as.Date(101, origin="1998-12-31"), as.Date(150, origin="1998-12-31")), 
                 date_labels="%b %d", date_breaks="3 day", expand=c(0.01,0.01)) +
    scale_y_continuous(sec.axis = sec_axis(~./25, name="")) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.text = element_text(colour="black", size=15),
          axis.title = element_text(face="bold", size=18),
          panel.grid  = element_blank(),
          panel.border = element_rect(size=1.2)) +
    guides(colour="none") +
    guides(fill="none"),

# NADLEH 2019
ggplot() +
  geom_rect(data=ice_flows%>%filter(year=="2019"), 
            aes(xmin=as.Date(xstartDOY, origin="2018-12-31"), xmax=as.Date(xendDOY, origin="2018-12-31"), ymin=-Inf, ymax=Inf), 
            fill="gray60", alpha=0.5) +
  annotate(geom="text", label="B", x=as.Date(101, origin="2018-12-31"), y=7000, fontface=2, size=7, hjust = 0) +
  annotate(geom="text", label="Nautley 2019", x=as.Date(101, origin="2018-12-31"), fontface=3, y=5800, size=5, hjust = 0) +
  geom_ribbon(data=discharge.19_21%>%filter(year=="2019"), 
              aes(x=as.Date(DOY, origin="2018-12-31"), ymin=min_dis*75, ymax=max_dis*75), fill="#8bc2fd", alpha=0.6) +
  geom_line(data=discharge.19_21%>%filter(year=="2019"), 
            aes(x=as.Date(DOY, origin="2018-12-31"), y=mean_dis*75), colour="#1785fc", alpha=0.6) +
  geom_bar(data=catch.nightly%>%filter(year=="2019")%>%mutate(hourly_CPUE_propnMEAN=ifelse(hourly_CPUE_propnMEAN==0,NA,hourly_CPUE_propnMEAN)), 
           aes(x=as.Date(DOY_closed, origin="2018-12-31"), y=hourly_CPUE_propnMEAN), 
           stat="identity", colour="forest green", fill="forest green", alpha=0.75, size=0.2, width=0.8) +
  geom_point(data=moon%>%filter(year=="2019", date!=as.Date("2019-05-18")), 
             aes(x=as.Date(lubridate::yday(date), origin="2018-12-31"),y=7000),shape=21,col="#ffd966",fill="#ffe599",size=7,stroke=1) +
  geom_errorbar(data=catch.nightly%>%filter(year=="2019", date_closed%in%c(as.Date("2019-04-13"):as.Date("2019-04-25")))%>%
                 mutate(hourly_CPUE_propnLOWER=ifelse(hourly_CPUE_propnLOWER==0.000,NA,hourly_CPUE_propnLOWER))%>%
                 filter(!is.na(hourly_CPUE_propnLOWER)), 
                  aes(x=as.Date(DOY_closed, origin="2018-12-31"), ymin=hourly_CPUE_propnLOWER, ymax=hourly_CPUE_propnUPPER), 
                width=0.3, size=0.5, na.rm=F) +   
  geom_point(data=catch.nightly%>%filter(year=="2019", date_closed%in%c(as.Date("2019-04-13"):as.Date("2019-04-25")))%>%
              mutate(hourly_CPUE_propnMEAN=ifelse(hourly_CPUE_propnMEAN==0,NA,hourly_CPUE_propnMEAN)),
               aes(x=as.Date(DOY_closed, origin="2018-12-31"), y=hourly_CPUE_propnMEAN), 
               color="black", fill="black", shape=21, size=2) +
  scale_x_date(limits=c(as.Date(101, origin="2018-12-31"), as.Date(150, origin="2018-12-31")), 
               date_labels="%b %d", date_breaks="3 day", expand=c(0.01,0.01)) +  
  scale_y_continuous(sec.axis = sec_axis(~./75, name = expression(bold("Discharge"~m^3/s)))) +
  labs(x="", y="Nightly total") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=18),
        panel.grid  = element_blank(),
        panel.border = element_rect(size=1.2)),

# NADLEH 2021
ggplot() +
  geom_rect(data=ice_flows%>%filter(year=="2021",site=="Nadleh"), 
            aes(xmin=as.Date(xstartDOY, origin="2020-12-31"), xmax=as.Date(xendDOY, origin="2020-12-31"), ymin=-Inf, ymax=Inf), 
            fill="gray60", alpha=0.5) +
  annotate(geom="text", label="C", x=as.Date(101, origin="2020-12-31"), y=5500, fontface=2, size=7, hjust = 0) +
  annotate(geom="text", label="Nautley 2021", x=as.Date(101, origin="2020-12-31"), fontface=3, y=4500, size=5, hjust = 0) +
  geom_ribbon(data=discharge.19_21%>%filter(year=="2021",site=="Nadleh"), 
              aes(x=as.Date(DOY, origin="2020-12-31"), ymin=min_dis*30, ymax=max_dis*30), fill="#8bc2fd", alpha=0.6) +
  geom_line(data=discharge.19_21%>%filter(year=="2021",site=="Nadleh"), 
            aes(x=as.Date(DOY, origin="2020-12-31"), y=mean_dis*30), colour="#1785fc", alpha=0.6) +
  geom_bar(data=catch.nightly%>%filter(year=="2021",site=="Nadleh")%>%mutate(nightly_CPUE=ifelse(nightly_CPUE==0,NA,nightly_CPUE)),               
           aes(x=as.Date(DOY_closed, origin="2020-12-31"), y=nightly_CPUE), 
           stat="identity", fill="forest green", colour="forest green", alpha=0.75, size=0.2, width=0.8) +
  geom_point(data=moon%>%filter(year=="2021"), 
             aes(x=as.Date(lubridate::yday(date), origin="2020-12-31"),y=5400),shape=21,col="#ffd966",fill="#ffe599",size=7,stroke=1) +
  scale_x_date(limits=c(as.Date(101, origin="2020-12-31"), as.Date(150, origin="2020-12-31")),
               date_labels="%b %d", date_breaks="3 day", expand=c(0.01,0.01)) +
  scale_y_continuous(limits=c(0,6000), sec.axis = sec_axis(~./30, name="")) +
  labs(x="", y="") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=18),
        panel.grid  = element_blank(),
        panel.border = element_rect(size=1.2)) , 

# STELLAKO 2021
ggplot() +
  annotate(geom="text", label="D", x=as.Date(101, origin="2020-12-31"), y=360, fontface=2, size=7, hjust = 0) +
  annotate(geom="text", label="Stellako 2021", x=as.Date(101, origin="2020-12-31"), y=290, fontface=3, size=5, hjust = 0) +  
  geom_ribbon(data=discharge.19_21%>%filter(year=="2021",site=="Stellako"), 
              aes(x=as.Date(DOY, origin="2020-12-31"), ymin=min_dis*4, ymax=max_dis*4), fill="#8bc2fd", alpha=0.6) +
  geom_line(data=discharge.19_21%>%filter(year=="2021",site=="Stellako"), 
            aes(x=as.Date(DOY, origin="2020-12-31"), y=mean_dis*4), colour="#1785fc", alpha=0.6) +
  geom_bar(data=catch.nightly%>%filter(year=="2021", site=="Stellako")%>%mutate(nightly_CPUE=ifelse(nightly_CPUE==0,NA,nightly_CPUE)), 
           aes(x=as.Date(DOY_closed, origin="2020-12-31"), y=nightly_CPUE), 
           stat="identity", fill="forest green", colour="forest green", alpha=0.75, size=0.2, width=0.8) +
  geom_point(data=moon%>%filter(year=="2021"), 
             aes(x=as.Date(lubridate::yday(date), origin="2020-12-31"),y=355),shape=21,col="#ffd966",fill="#ffe599",size=7,stroke=1) +
  scale_x_date(limits=c(as.Date(101, origin="2020-12-31"), as.Date(150, origin="2020-12-31")),
               date_labels="%b %d", date_breaks="3 day", expand=c(0.01,0.01)) +
  scale_y_continuous(limits=c(0,400), sec.axis = sec_axis(~./4, name="")) +
  labs(x="", y="") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=18),
        panel.grid  = element_blank(),
        panel.border = element_rect(size=1.2)),  

ncol=1, align="v", rel_heights = c(1,1.35,1,1.3)
)



# ==================== HOURLY % ====================

# Calculate hourly proportions ----------------
# 2019/2021 data
hourly_passage <- catch.data %>%
  filter(!grepl("day shift", comments), !grepl("Release location", location), date_closed>=as.Date("2019-04-27"), 
         !date_closed%in%c(as.Date("2019-05-27"), as.Date("2019-05-28"))) %>% 
  group_by(site, year, date_closed, time_trap_closed) %>% 
  summarize(hourly_caught = sum(CPUE_hourly, na.rm=T)) %>%
  mutate(perc = hourly_caught/sum(hourly_caught, na.rm=T)) %>%
  group_by(site, year, time_trap_closed) %>%
  summarize(mean_perc = mean(perc, na.rm=T), sd_perc = sd(perc, na.rm=T), n=n()) %>%
  filter(n>1) %>%
  mutate(site_year=ifelse(year=="2019", "Nautley 2019", ifelse(year=="2021"&site=="Nadleh", "Nautley 2021", "Stellako 2021")),
         site = ifelse(site=="Nadleh", "Nautley", site)) %>%
  ungroup() %>%
  print()

# Historical (1999-2000) data
hourly.hist <- nad.hist %>% 
  group_by(year, date_closed, time_trap_closed) %>%
  summarize(hourly_caught = sum(CPUE_hourly, na.rm=T)) %>%
  mutate(perc = hourly_caught/sum(hourly_caught, na.rm=T)) %>%
  group_by(year, time_trap_closed) %>%
  summarize(mean_perc = mean(perc, na.rm=T), sd_perc = sd(perc, na.rm=T), n=n()) %>%
  filter(n>1) %>%
  ungroup() %>%
  mutate(site_year = ifelse(year=="1999", "Nautley 1999", "Nautley 2000"),
         site = "Nautley") %>%
  select(site, year, time_trap_closed, mean_perc, sd_perc, n, site_year) %>%
  print()


# Join ----------------
hourly_all <- rbind(hourly_passage, hourly.hist) %>%
  ungroup()
hourly_all$time_trap_closed <- factor(hourly_all$time_trap_closed, 
                                       levels=c("21:00", "22:00", "23:00", "23:30", "00:00", "00:30",
                                                "01:00", "01:30", "02:00", "02:15", "02:30", "03:00", "04:00", "05:00"), ordered=T)


# PLOT: FIGURE 2. HOURLY MIGRATION ---------------- 
ggplot(hourly_all%>%arrange(year), aes(x=time_trap_closed, y=mean_perc, group=year, colour=year, fill=year, shape=year, alpha=year)) +
  geom_ribbon(aes(ymin=mean_perc-sd_perc, ymax=mean_perc+sd_perc), colour="transparent", alpha=0.13) +
  geom_line(aes(colour=year, alpha=year, linetype=year), size=1) +
  geom_point(aes(colour=year, fill=year, alpha=year, size=year), stroke=1.1) +
  scale_shape_manual(values=c(24, 22, 21, 21)) +
  scale_fill_manual(values=c("gray50", "gray50", "black", "#00E500")) +
  scale_colour_manual(values=c("gray50", "gray50", "black", "#00E500")) +
  scale_alpha_manual(values=c(0.5,0.5,0.6,0.8)) +
  scale_size_manual(values=c(2.7,2.7,3.5,3.5)) +
  scale_linetype_manual(values=c("dotted", "dotted", "solid", "solid")) +
  labs(x="Time of capture", y="Proportion of catch", fill="Year", colour="Year", size="Year", shape="Year", alpha="Year", linetype="Year") +
  theme_bw() +
  theme(axis.text.y = element_text(colour="black", size=18),
        axis.text.x = element_text(colour="black", size=16),
        axis.title = element_text(face="bold", size=20),
        axis.line=element_line(),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(size=1.2),
        legend.title = element_text(face="bold", size=20),
        legend.text = element_text(size=18),
        legend.position = c(0.92,0.87),
        legend.background = element_rect(colour="black"))+
  facet_wrap(~site, nrow=2) +
  geom_text(data=data.frame(lab=c("A", "B"), site=c("Nautley","Stellako"), x=c(1, 1), y=c(0.6,0.6)) , 
            aes(label=lab, x=x, y=y), inherit.aes=F, size=7, fontface=2, hjust=0) +
  geom_text(data=data.frame(lab=c("A", "B"), site=c("Nautley","Stellako"), x=c(1, 1), y=c(0.55,0.55)) , 
            aes(label=site, x=x, y=y), inherit.aes=F, size=5, fontface=3, hjust=0)



##############################################################################################################################################


#                                                 BIOLOGICAL DATA (no GSI)


# TEMPORAL (nightly) TRENDS IN RAW DATA ---------------- 
# Length, weight, CF
plot_grid(ggplot() +
  geom_point(data=bio.data%>%filter(year=="2021"), 
             aes(x=as.Date(date_closed), y=length_mm, group=site,fill=site,colour=site), shape=21, alpha=0.5, size=3) +
  geom_smooth(data=bio.data%>%filter(year=="2021"), 
              aes(x=as.Date(date_closed), y=length_mm, group=site,fill=site,colour=site),method="lm") +
  stat_cor(data=bio.data%>%filter(year=="2021"), 
           aes(x=as.Date(date_closed), y=length_mm, group=site, colour=site, label=..rr.label..), size=3, r.digits=2, p.digits=3, 
           label.y=c(200,200), label.x=c(as.Date("2021-04-15"),as.Date("2021-04-20")), show.legend = F) +
  stat_cor(data=bio.data%>%filter(year=="2021"), 
           aes(x=as.Date(date_closed), y=length_mm, group=site, colour=site, label=..p.label..), size=3, r.digits=2, p.digits=3, 
           label.y=c(150,150), label.x=c(as.Date("2021-04-15"),as.Date("2021-04-20")), show.legend = F) +
  labs(x="") +
  theme_bw() +
  theme(legend.position = "none"),

ggplot() +
  geom_point(data=bio.data%>%filter(year=="2021"), 
             aes(x=as.Date(date_closed), y=weight_g, group=site,fill=site,colour=site), shape=21, alpha=0.5, size=3) +
  geom_smooth(data=bio.data%>%filter(year=="2021"), 
              aes(x=as.Date(date_closed), y=weight_g, group=site,fill=site,colour=site),method="lm") +
  stat_cor(data=bio.data%>%filter(year=="2021"), 
           aes(x=as.Date(date_closed), y=length_mm, group=site, colour=site, label=..rr.label..), size=3, r.digits=2, p.digits=3,
           label.y=c(60,60), label.x=c(as.Date("2021-04-15"),as.Date("2021-04-20")), show.legend = F) +
  stat_cor(data=bio.data%>%filter(year=="2021"), 
           aes(x=as.Date(date_closed), y=length_mm, group=site, colour=site, label=..p.label..), size=3, r.digits=2, p.digits=3,
           label.y=c(40,40), label.x=c(as.Date("2021-04-15"),as.Date("2021-04-20")), show.legend = F) +
  labs(x="") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8)),

ggplot() +
  geom_point(data=bio.data%>%filter(year=="2021"), 
             aes(x=as.Date(date_closed), y=cf_k, group=site,fill=site,colour=site), shape=21, alpha=0.5, size=3) +
  geom_smooth(data=bio.data%>%filter(year=="2021"), 
              aes(x=as.Date(date_closed), y=cf_k, group=site,fill=site,colour=site),method="lm") +
  stat_cor(data=bio.data%>%filter(year=="2021"), 
           aes(x=as.Date(date_closed), y=length_mm, group=site, colour=site, label=..rr.label..), size=3, r.digits=2, p.digits=3,
           label.y=c(1.75,1.75), label.x=c(as.Date("2021-04-15"),as.Date("2021-04-20")), show.legend = F) +
  stat_cor(data=bio.data%>%filter(year=="2021"), 
           aes(x=as.Date(date_closed), y=length_mm, group=site, colour=site, label=..p.label..), size=3, r.digits=2, 
           label.y=c(1.25,1.25), label.x=c(as.Date("2021-04-15"),as.Date("2021-04-20")), show.legend = F) +
  labs(x="") +
  theme_bw() +
  theme(legend.position = "none"),

ncol=1, align="v")


# TEMPORAL (hourly) TRENDS IN RAW DATA ----------------  
bio.data$time_trap_closed <- factor(bio.data$time_trap_closed, 
                                      levels=c("21:00", "21:30", "21:40", "22:00", "23:00", "00:00", 
                                               "01:00", "02:00", "03:00", "04:00"), ordered=T)

# Length, weight, CF
plot_grid(ggplot() +
            geom_violin(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                        aes(x=time_trap_closed, y=length_mm, fill=site,colour=site, group=interaction(site, time_trap_closed)), alpha=0.5) +
            geom_point(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                       aes(x=time_trap_closed, y=length_mm, group=site,fill=site,colour=site), shape=21, alpha=0.5, size=3) +
            geom_smooth(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                        aes(x=time_trap_closed, y=length_mm, group=site,fill=site,colour=site),method="lm") +
            stat_cor(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                     aes(x=time_trap_closed, y=length_mm, group=site, colour=site, label=..rr.label..), size=3, r.digits=2, p.digits=3, 
                     label.y=c(200,200), label.x=c(1, 2), show.legend = F) +
            stat_cor(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                     aes(x=time_trap_closed, y=length_mm, group=site, colour=site, label=..p.label..), size=3, r.digits=2, p.digits=3, 
                     label.y=c(150,150), label.x=c(1, 2), show.legend = F) +
            labs(x="") +
            theme_bw() +
            theme(legend.position = "none"),
          
          ggplot() +
            geom_violin(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                        aes(x=time_trap_closed, y=weight_g, fill=site,colour=site, group=interaction(site, time_trap_closed)), alpha=0.5) +
            geom_point(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                       aes(x=time_trap_closed, y=weight_g, group=site,fill=site,colour=site), shape=21, alpha=0.5, size=3) +
            geom_smooth(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                        aes(x=time_trap_closed, y=weight_g, group=site,fill=site,colour=site),method="lm") +
            stat_cor(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                     aes(x=time_trap_closed, y=length_mm, group=site, colour=site, label=..rr.label..), size=3, r.digits=2, p.digits=3,
                     label.y=c(60,60), label.x=c(1,2), show.legend = F) +
            stat_cor(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                     aes(x=time_trap_closed, y=length_mm, group=site, colour=site, label=..p.label..), size=3, r.digits=2, p.digits=3,
                     label.y=c(40,40), label.x=c(1,2), show.legend = F) +
            labs(x="") +
            theme_bw() +
            theme(legend.position = c(0.4,0.8)),
          
          ggplot() +
            geom_violin(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                        aes(x=time_trap_closed, y=cf_k, fill=site,colour=site, group=interaction(site, time_trap_closed)), alpha=0.5) +
            geom_point(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                       aes(x=time_trap_closed, y=cf_k, group=site,fill=site,colour=site), shape=21, alpha=0.5, size=3) +
            geom_smooth(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                        aes(x=time_trap_closed, y=cf_k, group=site,fill=site,colour=site),method="lm") +
            stat_cor(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                     aes(x=time_trap_closed, y=length_mm, group=site, colour=site, label=..rr.label..), size=3, r.digits=2, p.digits=3,
                     label.y=c(1.75,1.75), label.x=c(1,2), show.legend = F) +
            stat_cor(data=bio.data%>%filter(year=="2021", !is.na(time_trap_closed), time_trap_closed!="21:30-23:00"), 
                     aes(x=time_trap_closed, y=length_mm, group=site, colour=site, label=..p.label..), size=3, r.digits=2, 
                     label.y=c(1.25,1.25), label.x=c(1,2), show.legend = F) +
            labs(x="") +
            theme_bw() +
            theme(legend.position = "none"),
          
          ncol=1, align="v")


# Length-freq compared to biodata ----------------  

# Nadleh
ggplot() +
  geom_bar(data=lf.data%>%filter(year=="2021", site=="Nadleh")%>%group_by(length_mm)%>%summarize(n=n()), 
           aes(x=length_mm, y=n), stat="identity", fill="gray80", colour="gray80", alpha=0.7) +
  geom_histogram(data=bio.data%>%filter(year=="2021", site=="Nadleh",!is.na(length_mm))%>%group_by(length_mm)%>%summarize(n=n()),
                 aes(x=length_mm, y=n), stat="identity", fill="blue", colour="blue", alpha=0.7) +
  scale_x_continuous(breaks=seq(0,200,by=10)) +
  theme_bw()

# Stellako
ggplot() +
  geom_bar(data=lf.data%>%filter(year=="2021", site=="Stellako")%>%group_by(length_mm)%>%summarize(n=n()), 
           aes(x=length_mm, y=n), stat="identity", fill="gray80", colour="gray80", alpha=0.7) +
  geom_histogram(data=bio.data%>%filter(year=="2021", site=="Stellako",!is.na(length_mm))%>%group_by(length_mm)%>%summarize(n=n()),
                 aes(x=length_mm, y=n), stat="identity", fill="blue", colour="blue", alpha=0.7) +
  scale_x_continuous(breaks=seq(0,200,by=10)) +
  theme_bw()



##############################################################################################################################################


#                                                               GSI AND AGE


# ====================== GSI FINAL SELECTION AND STOCK-SWITCHING =================

# ASSESS STOCK SWTICHING ---------------- 

# Stock-switching cases regardless of %
View(bio.data %>% 
       filter(site=="Nadleh", !is.na(gsi_subset_reg1) | !is.na(gsi_full_reg1)) %>% 
       filter(gsi_subset_reg1 != gsi_full_reg1) %>%
       select(year, site, date_closed, gsi_subset_reg1, gsi_subset_prob1, gsi_full_reg1, gsi_full_prob1,
              gsi_subset_reg2, gsi_subset_prob2, gsi_full_reg2, gsi_full_prob2, MGL_identifier))

# Cases where % were <80% in one and/or both cases, and flag if the stock changed
View(bio.data %>%
       filter(site=="Nadleh", !is.na(gsi_subset_reg1) | !is.na(gsi_full_reg1)) %>% 
       filter(gsi_subset_prob1>=0.8 | gsi_full_prob1>=0.8) %>%
       select(year, site, date_closed, gsi_subset_reg1, gsi_subset_prob1, gsi_full_reg1, gsi_full_prob1,
              MGL_identifier) %>%
       mutate(dif = ifelse(gsi_subset_prob1<0.8 | gsi_full_prob1<0.8, gsi_subset_prob1-gsi_full_prob1, NA),
              is_equal = ifelse(gsi_subset_reg1==gsi_full_reg1, "", "FLAG")) %>%
       filter(!is.na(dif)))


# ASSIGN FINAL GSI ALLOCATION ---------------- 
# Rules: both subsample and full batches must be >= 80%, no stock-switching; for Nad & Stella
bio.data <- bio.data %>%  
  mutate(gsi_final_reg = ifelse(site=="Nadleh" & gsi_subset_prob1<0.8 | gsi_full_prob1<0.8, NA, gsi_full_reg1),
         gsi_final_prob = ifelse(site=="Nadleh" & gsi_subset_prob1<0.8 | gsi_full_prob1<0.8, NA, gsi_full_prob1))

# View to confirm:
ggplot() +
  geom_point(data=bio.data%>%filter(year=="2021",site=="Nadleh"), aes(x=gsi_subset_reg1, y=gsi_subset_prob1), colour="blue") +
  geom_point(data=bio.data%>%filter(year=="2021",site=="Nadleh"), aes(x=gsi_full_reg1, y=gsi_full_prob1)) + 
  geom_point(data=bio.data%>%filter(year=="2021",site=="Nadleh"), aes(x=gsi_final_reg, y=gsi_final_prob), color="red") 




# ====================== GSI SAMPLING SUMMARIES (INCL/EXCL) ======================

# Number and proportion of the total samples submitted that did not amplify ---------------- 
bio.data %>%
  filter(!is.na(MGL_identifier)) %>%
  group_by(year, site, is.na(gsi_full_reg1)) %>%
  summarize(n=n()) %>%
  group_by(year, site) %>%
  mutate(total=sum(n),
         propn = n/total)

# Number and proportion of the total samples submitted above/below 80% **AND** that did not amplify ----------------  
bio.data %>%
  filter(!is.na(MGL_identifier)) %>%
  group_by(year, site, is.na(gsi_full_reg1)) %>%
  summarize(n=n()) %>%
  group_by(year, site) %>%
  mutate(total=sum(n),
         propn = n/total)

# Number and proportion of the total samples submitted that stock switched ---------------- 
bio.data %>%
  filter(!is.na(MGL_identifier)) %>%
  group_by(year, site, is.na(gsi_final_reg)) %>%
  summarize(n=n()) %>%
  group_by(year, site) %>%
  mutate(total=sum(n),
         propn = n/total)

# Average assignment probability of Nadina @ Stellako ----------------  
bio.data %>% 
  filter(site=="Stellako") %>%
  group_by(site, year) %>% 
  summarize(mean = mean(gsi_final_prob,na.rm=T), sd=sd(gsi_final_prob,na.rm=T))


# OVERALL GSI SUMMARY @ NADLEH ---------------- 
bio.data %>%
  filter(!is.na(gsi_final_reg)) %>% 
  group_by(site, year, gsi_final_reg) %>% 
  summarize(n=n()) %>%
  group_by(site, year) %>%
  mutate(total=sum(n), propn=n/total)




# ====================== GSI HOURLY ======================
# Summary average over migration
GSI.hourly <- bio.data %>% 
  filter(year=="2021", site=="Nadleh", !is.na(gsi_final_reg)) %>% 
  group_by(date_closed, time_trap_closed, gsi_final_reg) %>% 
  summarize(n=n())%>%
  mutate(propn=n/sum(n)) %>%
  print()
GSI.hourly$time_trap_closed <- factor(GSI.hourly$time_trap_closed, levels=c("21:00", "21:30", "21:40",  "22:00", "23:00", 
                                                                            "00:00", "01:00", "02:00", "03:00", "04:00", 
                                                                            ordered=T))

ggplot(GSI.hourly, 
       aes(x=time_trap_closed, y=propn, group=gsi_final_reg, fill=gsi_final_reg, colour=gsi_final_reg)) +
  geom_hline(yintercept = 0.5, colour="red", linetype="dotted") +
  geom_point(shape=21, size=5, alpha=0.6) +
  labs(x="Time closed", y="Proportion of samples") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=17),
        axis.title = element_text(size=19, face="bold"),
        panel.grid = element_blank(),
        panel.border = element_rect(size=1.2),
        legend.position = c(0.8,0.2),
        legend.background = element_rect(colour="black"),
        legend.title = element_blank())




# ====================== GSI NIGHTLY ======================
GSI.nightly <- bio.data %>% 
  filter(site=="Nadleh", !is.na(gsi_final_reg)) %>% 
  group_by(year, date_closed, DOY_closed, gsi_final_reg) %>% 
  summarize(n=n())%>%
  group_by(year, date_closed) %>% 
  mutate(propn=n/sum(n)) %>%
  print()

# PLOT: FIGURE 3. GSI ~ date ----------------   
ggplot(data=GSI.nightly, 
       aes(x=as.Date(DOY_closed, origin="2018-12-31"), y=propn, group=gsi_final_reg, 
           colour=gsi_final_reg, fill=gsi_final_reg)) +
  geom_segment(data=data.frame(year=c("2019", "2021"), 
                               x1=c(as.Date("2019-04-20"), as.Date("2021-04-26")), 
                               xend1=c(as.Date("2019-04-21"), as.Date("2021-04-27")), 
                               x2=c(as.Date("2019-05-03"), as.Date("2021-05-08")), 
                               xend2=c(as.Date("2019-05-10"), as.Date("2021-05-13"))), 
               aes(x=as.Date(lubridate::yday(x1), origin="2018-12-31"), 
                   xend=as.Date(lubridate::yday(xend1), origin="2018-12-31"), 
                   y=-0.2, yend=-0.2), inherit.aes = F, size=3, alpha=0.7, colour="gray50") + 
  geom_segment(data=data.frame(year=c("2019", "2021"), 
                               x1=c(as.Date("2019-04-20"), as.Date("2021-04-26")), 
                               xend1=c(as.Date("2019-04-21"), as.Date("2021-04-27")), 
                               x2=c(as.Date("2019-05-03"), as.Date("2021-05-08")), 
                               xend2=c(as.Date("2019-05-10"), as.Date("2021-05-13"))), 
               aes(x=as.Date(lubridate::yday(x2), origin="2018-12-31"), 
                   xend=as.Date(lubridate::yday(xend2), origin="2018-12-31"), 
                   y=-0.2, yend=-0.2), inherit.aes = F, size=3, alpha=0.7, colour="gray50") + 
  geom_hline(yintercept=0.5, col="red", linetype="dashed") +
  geom_smooth(method="loess", alpha=0.24, size=0.8, span=0.5) +
  geom_point(shape=21, stroke=1.1, alpha=0.7, size=2.8) +
  labs(x="", y="Nightly proportion") + 
  scale_x_date(date_breaks = "2 day", date_labels="%b %d") +
  scale_y_continuous(breaks=seq(0,1,by=0.25))+
  scale_colour_manual(values=c("orange", "blue")) +
  scale_fill_manual(values=c("orange", "blue")) +
  theme_bw() + 
  theme(axis.text = element_text(colour="black", size=17),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=20),
        legend.position = c(0.1,0.85),
        legend.background = element_rect(colour="black"),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(t=5, r=30, b=5, l=5, unit = "pt"),
        panel.border = element_rect(size=1.2),
        panel.grid = element_blank())  +
  facet_wrap(.~year, ncol=1, scales="free_y") +
  geom_text(data=data.frame(lab=c("A", "B"), year=c("2019","2021"), 
                            x=c(as.Date("2019-04-20"), as.Date("2019-04-20")), y=c(1.2,1.4)) , 
            aes(label=lab, x=x, y=y), inherit.aes=F, size=7, fontface=2, hjust=0) +
  geom_text(data=data.frame(lab=c("A", "B"), year=c("2019","2021"), 
                            x=c(as.Date("2019-04-20"), as.Date("2019-04-20")), y=c(1.1,1.25)) , 
            aes(label=year, x=x, y=y), inherit.aes=F, size=5, fontface=3, hjust=0)



# ====================== GSI SIZE ======================

# Plot barplot  
GSI.size <- bio.data %>% 
  filter(!is.na(gsi_final_reg), site=="Nadleh") %>% 
  group_by(year, gsi_final_reg) %>% 
  summarize(n=n(), mean_FL = mean(length_mm, na.rm=T), se_FL=sd(length_mm, na.rm=T)/sqrt(length(length_mm)),
            mean_W = mean(weight_g, na.rm=T), se_W=sd(weight_g, na.rm=T)/sqrt(length(weight_g)),
            mean_CF = mean(cf_k, na.rm=T), se_CF=sd(cf_k, na.rm=T)/sqrt(length(cf_k))) %>%
  print()

plot_grid(
  ggplot(GSI.size) +
    geom_bar(aes(x=year, y=mean_FL, group=gsi_final_reg, fill=gsi_final_reg, colour=gsi_final_reg), 
             position="dodge", stat="identity", alpha=0.6, size=0.6) +
    geom_errorbar(aes(x=year, ymin=mean_FL-se_FL, ymax=mean_FL+se_FL, group=gsi_final_reg, colour=gsi_final_reg),
                  position = position_dodge(width = .9), width=0, size=1.2) +
    scale_fill_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_colour_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_y_continuous(limits=c(0,175), breaks=seq(0,175,by=50)) +
    labs(x="", y="Fork length (mm)") +
    theme_bw() + 
    theme(axis.text = element_text(colour="black", size=17),
          axis.title = element_text(face="bold", size=19),
          panel.grid = element_blank(),
          panel.border = element_rect(size=1.2),
          legend.position = c(0.8,0.85),
          legend.background = element_rect(colour="black"),
          legend.title = element_blank(),
          legend.text = element_text(size=17)) + 
    annotate(geom="text", label="A", x=0.45, y=170, fontface=2, size=7, hjust = 0) +
  guides(colour = guide_legend(override.aes = list(colour = "transparent"))),
  
  ggplot(GSI.size) +
    geom_bar(aes(x=year, y=mean_W, group=gsi_final_reg, fill=gsi_final_reg, colour=gsi_final_reg), 
             position="dodge", stat="identity", alpha=0.6, size=0.6) +
    geom_errorbar(aes(x=year, ymin=mean_W-se_W, ymax=mean_W+se_W, group=gsi_final_reg, colour=gsi_final_reg),
                  position = position_dodge(width = .9), width=0, size=1.2) +
    scale_fill_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_colour_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_y_continuous(breaks=seq(0,30,by=5), limits=c(0,30)) +
    labs(x="", y="Weight (g)") +
    theme_bw() +
    theme(axis.text = element_text(colour="black", size=17),
          axis.title = element_text(face="bold", size=19),
          panel.grid = element_blank(),
          panel.border = element_rect(size=1.2)) + 
    guides(fill="none", colour="none") + 
    annotate(geom="text", label="B", x=0.45, y=29, fontface=2, size=7, hjust = 0),
  
  ggplot(GSI.size) +
    geom_bar(aes(x=year, y=mean_CF, group=gsi_final_reg, fill=gsi_final_reg, colour=gsi_final_reg), 
             position="dodge", stat="identity", alpha=0.6, size=0.6) +
    geom_errorbar(aes(x=year, ymin=mean_CF-se_CF, ymax=mean_CF+se_CF, group=gsi_final_reg, colour=gsi_final_reg),
                  position = position_dodge(width = .9), width=0, size=1.2) +
    scale_fill_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_colour_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_y_continuous(breaks=seq(0,1,by=0.25), limits=c(0,1.05)) +
    labs(x="", y="Condition factor (k)") +
    theme_bw() +
    theme(axis.text = element_text(colour="black", size=17),
          axis.title = element_text(face="bold", size=19),
          panel.grid = element_blank(),
          panel.border = element_rect(size=1.2)) + 
    guides(fill="none", colour="none") +
    annotate(geom="text", label="C", x=0.45, y=1, fontface=2, size=7, hjust = 0),
  
  ncol=2, nrow=2)


# Plot: old Figure 4. Boxplot size ~ GSI ----------------                           
plot_grid(
  ggplot(data=bio.data %>% filter(!is.na(gsi_final_reg), site=="Nadleh")) +
    geom_boxplot(aes(x=year, y=length_mm, fill=gsi_final_reg, colour=gsi_final_reg), alpha=0.6, size=0.5, outlier.size=4) +
    scale_fill_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_colour_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_y_continuous(breaks=seq(75,200,by=25), limits=c(75,200)) +
    labs(x="", y="Fork length (mm)") +
    theme_bw() + 
    theme(axis.text = element_text(colour="black", size=17),
          axis.title = element_text(face="bold", size=19),
          panel.grid = element_blank(),
          panel.border = element_rect(size=1.2),
          legend.position = c(0.17,0.8),
          legend.background = element_rect(colour="black"),
          legend.title = element_blank(),
          legend.text = element_text(size=17)) + 
    annotate(geom="text", label="A", x=0.45, y=200, fontface=2, size=7, hjust = 0) +
    guides(colour = guide_legend(override.aes = list(colour = "transparent"))),
  
  ggplot(data=bio.data %>% filter(!is.na(gsi_final_reg), site=="Nadleh")) +
    geom_boxplot(aes(x=year, y=weight_g, fill=gsi_final_reg, colour=gsi_final_reg), alpha=0.6, size=0.5, outlier.size=4) +
    scale_fill_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_colour_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_y_continuous(breaks=seq(5,65,by=10), limits=c(5,65)) +
    labs(x="", y="Weight (g)") +
    theme_bw() + 
    theme(axis.text = element_text(colour="black", size=17),
          axis.title = element_text(face="bold", size=19),
          panel.grid = element_blank(),
          panel.border = element_rect(size=1.2)) + 
    annotate(geom="text", label="B", x=0.45, y=65, fontface=2, size=7, hjust = 0) +
    guides(fill="none", colour="none"),
  
  ggplot(data=bio.data %>% filter(!is.na(gsi_final_reg), site=="Nadleh")) +
    geom_boxplot(aes(x=year, y=cf_k, fill=gsi_final_reg, colour=gsi_final_reg), alpha=0.6, size=0.5, outlier.size=4) +
    scale_fill_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_colour_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_y_continuous(breaks=seq(0.60,1.2,by=0.1), limits=c(0.65,1.2)) +
    labs(x="", y="Condition factor 'k'") +
    theme_bw() + 
    theme(axis.text = element_text(colour="black", size=17),
          axis.title = element_text(face="bold", size=19),
          panel.grid = element_blank(),
          panel.border = element_rect(size=1.2)) + 
    annotate(geom="text", label="C", x=0.45, y=1.2, fontface=2, size=7, hjust = 0) +
    guides(fill="none", colour="none"),
  
  ncol=2, nrow=2)




# ====================== GSI, SIZE AND AGE ======================

# Age sample summary/failure rate ----------------
bio.data %>% 
  filter(scale_condition!=0) %>%
  group_by(year, site, scale_condition) %>%
  summarize(n=n()) %>%
  group_by(year, site) %>%
  mutate(total=sum(n),
         discarded = ifelse(scale_condition%in%c(7,8,9), "bad", "good")) %>%
  ungroup() %>%
  group_by(year, site, discarded) %>%
  summarize(fail_n = sum(n), total=unique(total)) %>%
  mutate(fail_rate = fail_n/total)




# Overall age breakdown ----------------
bio.data %>% 
  filter(!is.na(age), age!=0, !ufid%in%c("2021-N-2008", "2021-N-2059", "2019-N-1111")) %>%
  group_by(year, site, age) %>%
  summarize(n=n(), meanL=mean(length_mm,na.rm=T), sdL=sd(length_mm,na.rm=T), meanW=mean(weight_g,na.rm=T), sdW=sd(weight_g,na.rm=T),
            MINL=min(length_mm,na.rm=T), MAXL=max(length_mm,na.rm=T),
            MINW=min(weight_g,na.rm=T), MAXW=max(weight_g,na.rm=T)) %>%
  group_by(year, site) %>%
  mutate(total=sum(n),
         propn=n/total) 


# PREDICT Age ~ size ----------------
# Fit
glm.df <- bio.data %>% 
  mutate(age = ifelse(age==0 , NA, age)) %>%
  filter(!is.na(age) &
         !is.na(length_mm))%>%
         #!ufid%in%c("2021-N-2008", "2021-N-2059", "2019-N-1111")) %>%     # 183mm age-1, 119mm age-2, 135mm age-2 outlier smolts removed
  mutate_at("age", as.factor)

ggplot(glm.df, aes(x=length_mm, y=age)) +
  geom_point()

as.glm <- glm(age ~ length_mm, family=binomial(link='logit'), data=glm.df)
#asg.glm <- glm(age ~ length_mm:weight_g, family=binomial(link='logit'), data=glm.df)

summary(as.glm)
anova(as.glm)
library(pscl)
pR2(as.glm)


# Predict
# Make prediction df:
predict.df <- bio.data %>% 
  mutate(age = ifelse(age==0 , NA, age)) %>%
  filter(is.na(age) & 
           !is.na(length_mm)) 
           #!ufid%in%c("2021-N-2008", "2021-N-2059")) %>%

# View distribution of predictor variable length: 
ggplot(predict.df, aes(x=length_mm, y=1)) +
  geom_point()

# Predict values and put in dataframe:
bio.dataT <- bio.data %>%
  mutate(age = ifelse(age==0 , NA, age),
         age_from = ifelse(is.na(age), "model", "scale"),
         
         age = ifelse(age_from=="model" & !is.na(length_mm), 
                      predict(as.glm, newdata=predict.df, type='response'),
                      age),
         age_clean = ifelse(age_from=="model" & age>=0.5, 2, 
                  ifelse(age_from=="model" & age<0.5, 1, age)))
                    #ifelse(age_from=="model" & age<0.75 & age>0.2, NA, age))))

# Plot predicted and known values / view data:
ggplot(bio.dataT%>%filter(!is.na(length_mm)), aes(x=length_mm, y=as.factor(age_clean), colour=age_from)) +
  geom_point()

View(bio.dataT %>% select(year,age, age_clean, age_from, ufid, length_mm) %>% arrange(year, age, desc(age_from)))

bio.dataT$age_from <- factor(bio.dataT$age_from, levels=c("scale", "model", ordered=T))

ggplot(bio.dataT%>%filter(!is.na(length_mm)), 
       aes(x=interaction(as.factor(age_clean), age_from, sep=" ", lex.order=T), y=length_mm,
           colour=age_from, fill=age_from)) +
  geom_point(shape=21, alpha=0.2, size=3) +
  scale_fill_discrete(labels=c("Scale reading", "Modelled")) +
  scale_colour_discrete(labels=c("Scale reading", "Modelled")) +
  scale_y_continuous(breaks=seq(50,200,by=25)) +
  labs(x="", y="Fork length (mm)", fill="Age derived from", colour="Age derived from") +
  theme_bw() +
  theme(axis.text.x = element_text(colour="black", angle=45, hjust=1),
        axis.text = element_text(colour="black", size=17),
        axis.title = element_text(face="bold", size=19),
        panel.grid = element_blank(),
        panel.border = element_rect(size=1.2)) +
  facet_wrap(.~year)

# MODELLING ULTIMATELY DOESN'T PRODUCE BIOLOGICALLY PLAUSIBLE RESULTS 


# VISUALLY INFILL Age ~ size ----------------
# Histogram to visually assess & Table of values: 
# Function to remove baseline geom_hist colour:
StatBin2 <- ggproto(
  "StatBin2", 
  StatBin,
  compute_group = function (data, scales, binwidth = NULL, bins = NULL, 
                            center = NULL, boundary = NULL, 
                            closed = c("right", "left"), pad = FALSE, 
                            breaks = NULL, origin = NULL, right = NULL, 
                            drop = NULL, width = NULL) {
    if (!is.null(breaks)) {
      if (!scales$x$is_discrete()) {
        breaks <- scales$x$transform(breaks)
      }
      bins <- ggplot2:::bin_breaks(breaks, closed)
    }
    else if (!is.null(binwidth)) {
      if (is.function(binwidth)) {
        binwidth <- binwidth(data$x)
      }
      bins <- ggplot2:::bin_breaks_width(scales$x$dimension(), binwidth, 
                                         center = center, boundary = boundary, 
                                         closed = closed)
    }
    else {
      bins <- ggplot2:::bin_breaks_bins(scales$x$dimension(), bins, 
                                        center = center, boundary = boundary, 
                                        closed = closed)
    }
    res <- ggplot2:::bin_vector(data$x, bins, weight = data$weight, pad = pad)
    
    # drop 0-count bins completely before returning the dataframe
    res <- res[res$count > 0, ] 
    
    res
  })

ggplot(data=bio.data%>%filter(!is.na(age) & age!=0), 
       aes(x=length_mm, group=as.factor(age), colour=as.factor(age), fill=as.factor(age))) +
  geom_histogram(alpha=0.5, stat = StatBin2, show.legend = F) +
  theme_bw() +
  facet_wrap(.~interaction(year,site))

# Observed age ratio:
bio.data %>%
  filter(!is.na(length_mm)) %>%
  mutate(age_from = ifelse(is.na(age) | age==0, "infill", "scales"),
         age = ifelse(age_from=="infill" & length_mm>=150, 2, 
                  ifelse(age_from=="infill" & length_mm<150, 1, age))) %>%
  group_by(year,site, age_from, age) %>%
  summarize(n=n()) %>%
  arrange(year, site, age, age_from) %>%
  group_by(site, year, age_from) %>%
  mutate(total_obs=ifelse(age_from=="scales", sum(n), NA) ,
         propn_obs=n/total_obs) %>%
# Age ratio with "infilled" ages"  
  group_by(site, year, age) %>%
  summarize(total_inf=sum(n)) %>%
  group_by(site, year) %>%
  mutate(TOTAL=sum(total_inf),
         propn_inf = total_inf/TOTAL)
  

# PLOT: FIGURE 4. AGE, SIZE ~ GSI ----------------
ggarrange(
  # Length
  ggplot(data=bio.data%>%filter(!is.na(age), age!=0, !is.na(gsi_final_reg)), 
         aes(x=year, y=length_mm, colour=interaction(gsi_final_reg, as.factor(age)), fill=interaction(gsi_final_reg, as.factor(age)))) +
    geom_boxplot(aes(outlier.colour=interaction(gsi_final_reg, as.factor(age))),
                 alpha=0.6, size=0.5, outlier.size=4, outlier.shape=21, outlier.alpha=0.7) +
    labs(x="", y="Fork length (mm)") +
    scale_colour_manual(values=c("black", "black", "orange", "blue"), 
                        labels=c("Nadina age-1", "Stellako age-1", "Nadina age-2", "Stellako age-2")) +
    scale_fill_manual(values=c("orange", "blue", "orange", "blue"), 
                      labels=c("Nadina age-1", "Stellako age-1", "Nadina age-2", "Stellako age-2")) +
    theme_bw() + 
    theme(axis.text = element_text(colour="black", size=17),
          axis.title = element_text(face="bold", size=19),
          panel.grid = element_blank(),
          panel.border = element_rect(size=1.2),
          legend.position = c(0.25,0.73),
          legend.background = element_rect(colour="black", fill="transparent"),
          legend.title = element_blank(),
          legend.text = element_text(size=17)) +
    annotate(geom="text", label="A", x=0.45, y=200, fontface=2, size=7, hjust = 0),

  # Weight
  ggplot(data=bio.data%>%filter(!is.na(age), age!=0, !is.na(gsi_final_reg)), 
         aes(x=year, y=weight_g, colour=interaction(gsi_final_reg, as.factor(age)), fill=interaction(gsi_final_reg, as.factor(age)))) +
    geom_boxplot(aes(outlier.colour=interaction(gsi_final_reg, as.factor(age))),
                     alpha=0.6, size=0.5, outlier.size=4, outlier.shape=21, outlier.alpha=0.7) +
    labs(x="", y="Weight (g)") +
    scale_colour_manual(values=c("black", "black", "orange", "blue"), 
                        labels=c("Nadina age-1", "Stellako age-1", "Nadina age-2", "Stellako age-2")) +
    scale_fill_manual(values=c("orange", "blue", "orange", "blue"), 
                      labels=c("Nadina age-1", "Stellako age-1", "Nadina age-2", "Stellako age-2")) +
    theme_bw() + 
    theme(axis.text = element_text(colour="black", size=17),
          axis.title = element_text(face="bold", size=19),
          panel.grid = element_blank(),
          panel.border = element_rect(size=1.2),
          legend.position = "none") +
    annotate(geom="text", label="B", x=0.45, y=60, fontface=2, size=7, hjust = 0),

  # Condition factor
  ggplot(data=bio.data%>%filter(!is.na(age), age!=0, !is.na(gsi_final_reg)), 
         aes(x=year, y=cf_k, colour=interaction(gsi_final_reg, as.factor(age)), fill=interaction(gsi_final_reg, as.factor(age)))) +
    geom_boxplot(aes(outlier.colour=interaction(gsi_final_reg, as.factor(age))),
                 alpha=0.6, size=0.5, outlier.size=4, outlier.shape=21, outlier.alpha=0.7) +
    labs(x="", y="Condition factor 'k'") +
    scale_colour_manual(values=c("black", "black", "orange", "blue"), 
                        labels=c("Nadina age-1", "Stellako age-1", "Nadina age-2", "Stellako age-2")) +
    scale_fill_manual(values=c("orange", "blue", "orange", "blue"), 
                      labels=c("Nadina age-1", "Stellako age-1", "Nadina age-2", "Stellako age-2")) +
    theme_bw() + 
    theme(axis.text = element_text(colour="black", size=17),
          axis.title = element_text(face="bold", size=19),
          panel.grid = element_blank(),
          panel.border = element_rect(size=1.2),
          legend.position = "none") +
    annotate(geom="text", label="C", x=0.45, y=1.2, fontface=2, size=7, hjust = 0),
  nrow=2, ncol=2)


# Age-2s closer look ----------------
bio.data %>%
  filter(age==2) %>%
  select(year, date_closed, length_mm, weight_g, gsi_final_reg, comments)






# NADINA vs STELLAKO, 2019 vs 2021-------------------    *** here next day: 2 way anova b/w GSI and year? 
# Length
lm2L <- lm(length_mm ~ gsi_final_reg + year, data=bio.data%>%filter(!is.na(length_mm), !is.na(gsi_final_reg), age==1))
r2L <- resid(lm2L)
hist(r2L)
qqnorm(r2L)
qqline(r2L)
plot(lm2L)
aov2L <- aov(length_mm ~ gsi_final_reg + year, data=bio.data%>%filter(!is.na(length_mm), !is.na(gsi_final_reg), age==1))
summary(aov2L)
TukeyHSD(aov2L)
## removing outlier "2021-N-2008" doesn't change results


# Weight
lm2W <- lm(weight_g ~ gsi_final_reg + year, data=bio.data%>%filter(!is.na(weight_g), !is.na(gsi_final_reg), age==1))
r2W <- resid(lm2W)
hist(r2W)
qqnorm(r2W)
qqline(r2W)
plot(lm2W)
aov2W <- aov(weight_g ~ gsi_final_reg + year, data=bio.data%>%filter(!is.na(weight_g), !is.na(gsi_final_reg), age==1))
summary(aov2W)
TukeyHSD(aov2W)

# CF
lm2CF <- lm(cf_k ~ gsi_final_reg + year, data=bio.data%>%filter(!is.na(cf_k), !is.na(gsi_final_reg), age==1))
r2CF <- resid(lm2CF)
hist(r2CF)
qqnorm(r2CF)
qqline(r2CF)
plot(lm2CF)
aov2CF <- aov(cf_k ~ gsi_final_reg + year, data=bio.data%>%filter(!is.na(cf_k), !is.na(gsi_final_reg), age==1))
summary(aov2CF)
TukeyHSD(aov2CF)



# 2019 vs 2021 -------------------          
# Length

length.lm <- lm(length_mm ~ gsi_final_reg, data=bio.data%>%filter(site=="Nadleh", ))
length.r <- resid(length.lm)
hist(length.r)
qqnorm(length.r)
qqline(length.r)
summary(aov(length_mm ~ gsi_final_reg, data=bio.data%>%filter(year==2021, site=="Nadleh")))
wilcox.test(length_mm ~ gsi_final_reg, data=bio.data%>%filter(year==2021, site=="Nadleh"))
t.test(length_mm ~ gsi_final_reg, data=bio.data%>%filter(year==2021, site=="Nadleh"))

# Weight
weight.lm <- lm(weight_g ~ gsi_final_reg, data=bio.data%>%filter(year==2021, site=="Nadleh"))
weight.r <- resid(weight.lm)
hist(weight.r)
qqnorm(weight.r)
qqline(weight.r)
summary(aov(weight_g ~ gsi_final_reg, data=bio.data%>%filter(year==2021, site=="Nadleh")))
wilcox.test(weight_g ~ gsi_final_reg, data=bio.data%>%filter(year==2021, site=="Nadleh"))
t.test(weight_g ~ gsi_final_reg, data=bio.data%>%filter(year==2021, site=="Nadleh"))

# CF
cf.lm <- lm(cf_k ~ gsi_final_reg, data=bio.data%>%filter(year==2021, site=="Nadleh"))
cf.r <- resid(cf.lm)
hist(cf.r)
qqnorm(cf.r)
qqline(cf.r)
summary(aov(cf_k ~ gsi_final_reg, data=bio.data%>%filter(year==2021, site=="Nadleh")))
wilcox.test(cf_k ~ gsi_final_reg, data=bio.data%>%filter(year==2021, site=="Nadleh"))
t.test(cf_k ~ gsi_final_reg, data=bio.data%>%filter(year==2021, site=="Nadleh"))






##############################################################################################################################################

#                                                      COPEPODS

bio.data <- bio.data %>%
  mutate(copepod_PA = ifelse(grepl("opepod", comments), 1, 0 )) %>%
  print()


# ====================== OVERALL INFECTION PREVALENCE BY SITE: infected vs. uninfected size ======================
bio.data %>% 
  filter(year=="2021") %>%
  group_by(site, copepod_PA) %>%
  summarize(n=n(), meanL=mean(length_mm,na.rm=T), sdL=sd(length_mm, na.rm=T), 
            meanW=mean(weight_g,na.rm=T), sdW=sd(weight_g,na.rm=T), meanCF=mean(cf_k, na.rm=T), sdCF=sd(cf_k, na.rm=T)) %>%
  group_by(site) %>%
  mutate(propn=n/sum(n))


# STATS @ NADLEH: Infected vs Uninfected ------------------
nad.cop.dat <- bio.data %>% filter(site=="Nadleh", year==2021)
## length 
copepod_length <- lm(nad.cop.dat$length_mm ~ nad.cop.dat$copepod_PA)
r_length <- resid(copepod_length)
hist(r_length)
qqnorm(r_length)
qqline(r_length)
summary(lm(nad.cop.dat$length_mm ~ nad.cop.dat$copepod_PA))
summary(aov(nad.cop.dat$length_mm ~ nad.cop.dat$copepod_PA))
wilcox.test(nad.cop.dat$length_mm ~ nad.cop.dat$copepod_PA) 
t.test(nad.cop.dat$length_mm ~ nad.cop.dat$copepod_PA)
ggplot(nad.cop.dat, aes(x=as.factor(copepod_PA), y=length_mm)) + geom_boxplot()

## weight
copepod_weight <- lm(nad.cop.dat$weight_g ~ nad.cop.dat$copepod_PA)
r_weight <- resid(copepod_weight)
hist(r_weight)
qqnorm(r_weight)
qqline(r_weight)
summary(lm(nad.cop.dat$weight_g ~ nad.cop.dat$copepod_PA))
summary(aov(nad.cop.dat$weight_g ~ nad.cop.dat$copepod_PA))
wilcox.test(nad.cop.dat$weight_g ~ nad.cop.dat$copepod_PA) 
t.test(nad.cop.dat$weight_g ~ nad.cop.dat$copepod_PA)
ggplot(nad.cop.dat, aes(x=as.factor(copepod_PA), y=weight_g)) + geom_boxplot()

## CF
copepod_cf <- lm(nad.cop.dat$cf_k ~ nad.cop.dat$copepod_PA)
r_cf <- resid(copepod_cf)
hist(r_cf)
qqnorm(r_cf)
qqline(r_cf)
summary(lm(nad.cop.dat$cf_k ~ nad.cop.dat$copepod_PA))
summary(aov(nad.cop.dat$cf_k ~ nad.cop.dat$copepod_PA))
wilcox.test(nad.cop.dat$cf_k ~ nad.cop.dat$copepod_PA) 
t.test(nad.cop.dat$cf_k ~ nad.cop.dat$copepod_PA)
ggplot(nad.cop.dat, aes(x=as.factor(copepod_PA), y=cf_k)) + geom_boxplot()


# STATS @ STELLAKO: Infected vs Uninfected ------------------
stel.cop.dat <- bio.data %>% filter(site=="Stellako")
## length 
copepod_length <- lm(stel.cop.dat$length_mm ~ stel.cop.dat$copepod_PA)
r_length <- resid(copepod_length)
hist(r_length)
qqnorm(r_length)
qqline(r_length)
summary(lm(stel.cop.dat$length_mm ~ stel.cop.dat$copepod_PA))
summary(aov(stel.cop.dat$length_mm ~ stel.cop.dat$copepod_PA))
wilcox.test(stel.cop.dat$length_mm ~ stel.cop.dat$copepod_PA) 
t.test(stel.cop.dat$length_mm ~ stel.cop.dat$copepod_PA)
ggplot(stel.cop.dat, aes(x=as.factor(copepod_PA), y=length_mm)) + geom_boxplot()

## weight
copepod_weight <- lm(stel.cop.dat$weight_g ~ stel.cop.dat$copepod_PA)
r_weight <- resid(copepod_weight)
hist(r_weight)
qqnorm(r_weight)
qqline(r_weight)
summary(lm(stel.cop.dat$weight_g ~ stel.cop.dat$copepod_PA))
summary(aov(stel.cop.dat$weight_g ~ stel.cop.dat$copepod_PA))
wilcox.test(stel.cop.dat$weight_g ~ stel.cop.dat$copepod_PA) 
t.test(stel.cop.dat$weight_g ~ stel.cop.dat$copepod_PA)
ggplot(stel.cop.dat, aes(x=as.factor(copepod_PA), y=weight_g)) + geom_boxplot()

## CF
copepod_cf <- lm(stel.cop.dat$cf_k ~ stel.cop.dat$copepod_PA)
r_cf <- resid(copepod_cf)
hist(r_cf)
qqnorm(r_cf)
qqline(r_cf)
summary(lm(stel.cop.dat$cf_k ~ stel.cop.dat$copepod_PA))
summary(aov(stel.cop.dat$cf_k ~ stel.cop.dat$copepod_PA))
wilcox.test(stel.cop.dat$cf_k ~ stel.cop.dat$copepod_PA) 
t.test(stel.cop.dat$cf_k ~ stel.cop.dat$copepod_PA)
ggplot(stel.cop.dat, aes(x=as.factor(copepod_PA), y=cf_k)) + geom_boxplot()


# ====================== COPEPOD INFECTION AND GSI ======================
# INFECTED SMOLTS: 
bio.data %>% 
  filter(year=="2021", !is.na(gsi_final_reg), copepod_PA==1) %>%
  group_by(site, gsi_final_reg) %>%
  summarize(n=n(), meanL=mean(length_mm,na.rm=T), sdL=sd(length_mm,na.rm=T), 
            meanW=mean(weight_g,na.rm=T), sdW=sd(weight_g,na.rm=T)) %>%
  group_by(site) %>%
  mutate(perc=n/sum(n))


# ====================== NADINA ONLY INVESTIGATIONS ======================

# NADINA @ NADLEH: Infected vs. Uninfected ------------------ 
t.test(length_mm ~ copepod_PA, data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, site=="Nadleh", age!=2))
ggplot(data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, site=="Nadleh",
                              aes(x=as.factor(copepod_PA), y=length_mm))) + geom_boxplot()

t.test(weight_g ~ copepod_PA, data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, site=="Nadleh"))
ggplot(data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, site=="Nadleh",
                              aes(x=as.factor(copepod_PA), y=weight_g))) + geom_boxplot()

t.test(cf_k ~ copepod_PA, data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, site=="Nadleh", age!=2))
ggplot(data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, site=="Nadleh",
                              aes(x=as.factor(copepod_PA), y=cf_k))) + geom_boxplot()


# NADINA @ STELLAKO: Infected vs. Uninfected ------------------ 
t.test(length_mm ~ copepod_PA, data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, site=="Stellako", age!=2))
ggplot(data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, site=="Stellako"), 
       aes(x=as.factor(copepod_PA), y=length_mm)) + geom_boxplot()

t.test(weight_g ~ copepod_PA, data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, site=="Stellako"))
ggplot(data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, site=="Stellako"), 
       aes(x=as.factor(copepod_PA), y=weight_g)) + geom_boxplot()

t.test(cf_k ~ copepod_PA, data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, site=="Stellako", age!=2))
ggplot(data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, site=="Stellako"), 
       aes(x=as.factor(copepod_PA), y=cf_k)) + geom_boxplot()


# NADINA INFECTION @ BOTH SITES ------------------ 
nadina.infection.full <- bio.data %>% 
  filter(gsi_final_reg=="Nadina", year==2021, age!=2) %>%
  print()

# 2-way anova (not used)
copepod.aov <- aov(length_mm ~ site + copepod_PA, data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021, !is.na(copepod_PA) & !is.na(length_mm)))
summary(copepod.aov)
TukeyHSD(copepod.aov, which="site")

copepod.faov <- anova(lm(length_mm ~ copepod_PA + site, data=bio.data%>%filter(gsi_final_reg=="Nadina", year==2021)))
summary(copepod.faov)

summary(aov(length_mm ~ site + copepod_PA, data = bio.data%>%filter(gsi_final_reg=="Nadina", year==2021)))



# Plot: Figure 6 copepods --------------
ggplot(data=nadina.infection.full, aes(x=site, y=length_mm, group=interaction(site, as.factor(copepod_PA)), 
                                       colour=as.factor(copepod_PA), fill=as.factor(copepod_PA))) + 
  geom_boxplot(alpha=0.4, size=1, width=0.5, outlier.size = 6) +
  scale_y_continuous(breaks=seq(80,200,by=20)) +
  scale_x_discrete(limits = c("Stellako", "Nadleh"), labels=c("Stellako", "Nautley")) +
  scale_colour_discrete(labels=c("Uninfected", "Infected")) +
  scale_fill_discrete(labels=c("Uninfected", "Infected")) +
  labs(x="", y="Length (mm)", colour="Copepod infection", fill="Copepod infection") +
  #stat_summary(aes(group=interaction(site, as.factor(copepod_PA)), colour=as.factor(copepod_PA)), 
  #             fun=mean, geom="point", shape=24, size=7, alpha=0.8, stroke=1.5, show.legend = FALSE) +
  #stat_summary(aes(label=round(..y.., digits=1)), fun=mean, colour="red", geom="text", show.legend = FALSE, vjust=0, hjust=0) +
  annotate(geom="text", label="Nadina (Nadina-Francois-ES) smolts only", x=1.4, y=200, fontface=3, size=7, hjust=0) +
  theme_bw() + 
  theme(axis.text = element_text(colour="black", size=23),
        axis.title = element_text(face="bold", size=26),
        panel.grid = element_blank(),
        panel.border = element_rect(size=1.2),
        legend.position = c(0.15,0.9),
        legend.background = element_rect(colour="black"),
        legend.title = element_text(face="bold", size=20),
        legend.text = element_text(size=18))

t.test(length_mm ~ site, data=bio.data %>% filter(gsi_final_reg=="Nadina", year==2021, age!=2))
ggplot(data=bio.data %>% filter(gsi_final_reg=="Nadina", year==2021), aes(x=site, y=length_mm)) + geom_boxplot()








##############################################################################################################################################


#                                                  MARK-RECAPTURE

# Remove day shifts, beginning of program, and only 2021 data
mr.dat <- catch.data %>%
  filter(year==2021, date_opened >= as.Date("2021-04-28"), !grepl("day shift", comments)) %>%
  mutate(release_group = ifelse(date_closed>=as.Date("2021-04-29") & date_closed<=as.Date("2021-05-01") & site=="Nadleh", 1, 
                          ifelse(date_closed>=as.Date("2021-05-02") & date_closed<=as.Date("2021-05-10") & site=="Nadleh", 2,
                            ifelse(date_closed>=as.Date("2021-05-11") & date_closed<=as.Date("2021-05-19") & site=="Nadleh", 3,
                              ifelse(date_closed>=as.Date("2021-05-20") & site=="Nadleh", 4, 
                                ifelse(date_closed==as.Date("2021-05-11") & time_trap_closed=="22:00" & site=="Nadleh", 2,
                                  ifelse(date_closed>=as.Date("2021-05-06") & date_closed<=as.Date("2021-05-09") & site=="Stellako", 1,
                                    ifelse(date_closed>=as.Date("2021-05-10") & date_closed<=as.Date("2021-05-14") & site=="Stellako", 2,
                                      ifelse(date_closed>=as.Date("2021-05-15") & site=="Stellako", 3, NA)))))))))

# Summarize as release groups start/end dates, number of days for each release group, and total # recaps
mr.dat.summary <- mr.dat %>%
  filter(!is.na(release_group)) %>%
  group_by(site, release_group) %>%
  summarize(release_start = min(date_closed), release_end = max(date_closed), n_days = paste0(n_distinct(date_closed), " days"),
            n_recaps = sum(n_recaps_lower_clip, na.rm=T)) %>%
  print()

View(mr.dat %>% filter(n_recaps_lower_clip>0))

# Number of days to get recaps
mr.dat %>% 
  filter(n_recaps_lower_clip>0 | n_recaps_upper_clip>0) %>% 
  group_by(site, release_group) %>%
  summarize(dif = max(date_closed) - min(date_closed-1))

# Recap rate
mr.dat %>% 
  filter(!is.na(release_group)) %>%
  group_by(site, release_group) %>%
  summarize(released = sum(n_marked_released, na.rm=T), recaps_Nad=sum(n_recaps_lower_clip, na.rm=T), 
            recaps_Stel=sum(n_recaps_upper_clip, na.rm=T), release_date=min(date_opened)) %>%
  mutate(recap_rate = case_when(site=="Nadleh" ~ recaps_Nad/released,
                                site=="Stellako" ~ recaps_Stel/released))


# Plot release groups and recaps by # recaps
ggplot() +
  geom_segment(data=mr.dat.summary, aes(x=release_start-1, xend=release_end, y=1, yend=1, colour=as.factor(release_group)), 
               size=4, alpha=0.5) +
  geom_point(data=mr.dat%>%
               filter(site=="Nadleh", n_recaps_lower_clip>0)%>%
               group_by(release_group, site, date_closed)%>%
               summarize(n=sum(n_recaps_lower_clip)), 
             aes(x=date_closed, y=1, colour=as.factor(release_group), size=n)) +
    
  geom_point(data=mr.dat%>%
               filter(site=="Stellako", n_recaps_upper_clip>0)%>%
               group_by(release_group, site, date_closed)%>%
               summarize(n=sum(n_recaps_upper_clip)), 
             aes(x=date_closed, y=1, colour=as.factor(release_group), size=n)) +
    
  geom_text(data=mr.dat.summary, aes(x=release_start, y = 1, label=paste0("(",n_days,")"), colour=as.factor(release_group)), 
            vjust=1.7, hjust=0.3, show.legend = FALSE) +
  
  geom_text(data=mr.dat %>% 
              filter(!is.na(release_group)) %>%
              group_by(site, release_group) %>%
              summarize(released = sum(n_marked_released, na.rm=T), release_date=min(date_opened)), 
            aes(x=release_date, y = 1, label=released, colour=as.factor(release_group)), 
            vjust=-1, hjust=-0.2, show.legend = FALSE, fontface=2) +
  
  scale_size_continuous(range=c(2,6)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  labs(x="", y="") +
  facet_wrap(~site, nrow=2) +
  theme_bw() + 
  theme(axis.text = element_text(colour="black", size=17),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=19),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_rect(size=1.2),
        
        legend.position = "none")


ggplot() +
  stat_smooth(data=catch.nightly%>%filter(year=="2021",site=="Nadleh"),               
              aes(x=as.Date(DOY_closed, origin="2020-12-31"), y=nightly_CPUE), 
              geom="line", colour="black", span=0.1, se=F, alpha=0.5, size=1) +
  stat_smooth(data=catch.nightly%>%filter(year=="2021",site=="Stellako"),               
              aes(x=as.Date(DOY_closed, origin="2020-12-31"), y=nightly_CPUE), 
              geom="line", colour="black", span=0.1, se=F, alpha=0.5, size=1) +
  
  geom_segment(data=mr.dat.summary%>%filter(site=="Nadleh"), 
               aes(x=release_start-1, xend=release_end, y=-1, yend=-1, colour=as.factor(release_group)), 
               size=4, alpha=0.5, show.legend = F) +
  geom_segment(data=mr.dat.summary%>%filter(site=="Stellako"), 
               aes(x=release_start-1, xend=release_end, y=-1, yend=-1, colour=as.factor(release_group)), 
               size=4, alpha=0.5, show.legend = F) +
  geom_point(data=mr.dat%>%
               filter(site=="Nadleh", n_recaps_lower_clip>0)%>%
               group_by(release_group, site, date_closed)%>%
               summarize(n=sum(n_recaps_lower_clip)), 
             aes(x=date_closed, y=1, colour=as.factor(release_group), size=n), show.legend = F, alpha=0.7) +
  geom_point(data=mr.dat%>%
               filter(site=="Stellako", n_recaps_upper_clip>0)%>%
               group_by(release_group, site, date_closed)%>%
               summarize(n=sum(n_recaps_upper_clip)), 
             aes(x=date_closed, y=1, colour=as.factor(release_group), size=n), show.legend = F, alpha=0.7) +

  geom_label(data=mr.dat %>% 
              filter(!is.na(release_group),site=="Nadleh") %>%
              group_by(site, release_group) %>%
              summarize(released = sum(n_marked_released, na.rm=T), release_date=min(date_opened)), 
            aes(x=release_date, y = 1, label=released, colour=as.factor(release_group)), 
            vjust=-0.4, hjust=-0, show.legend = FALSE, fontface=2, size=6, alpha=0.7, label.size = NA) +
  
  geom_label(data=mr.dat %>% 
              filter(!is.na(release_group), site=="Stellako") %>%
              group_by(site, release_group) %>%
              summarize(released = sum(n_marked_released, na.rm=T), release_date=min(date_opened)), 
            aes(x=release_date, y = 1, label=released, colour=as.factor(release_group)), 
            vjust=-0.3, hjust=0, show.legend = FALSE, fontface=2, size=6, alpha=0.7, label.size = NA) +
  
  
  
  geom_text(data=data.frame(site = c("Nadleh", "Stellako"), label=c("A", "B"), 
                           x=c(as.Date("2021-04-13"), as.Date("2021-04-13")),
                           y=c(5000,350)), 
           aes(label=label, x=x, y=y), fontface=2, size=7, hjust=0) +
  scale_size_continuous(range=c(3,7)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d", limits=c(as.Date("2021-04-13"), as.Date("2021-05-27"))) +
  labs(x="", y="Nightly total") +
  facet_wrap(~site, nrow=2, scales="free_y") +
  theme_bw() + 
  theme(axis.text = element_text(colour="black", size=17),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=19),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        #panel.grid.major = element_line(colour="gray82", size=0.8),
        panel.border = element_rect(size=1.2),
        strip.text = element_blank(),
        legend.position = "none")



#================= MR ANALYSIS ====================

# NADLEH -----------------
# Point estimate via pooled Chapman and Petersen 
NChapman(n1=2557, n2=28058, m2=15)
vChapman(n1=2557, n2=28058, m2=15) 
seChapman(n1=2557, n2=28058, m2=15)
ciChapman(n1=2557, n2=28058, m2=15)

NPetersen(n1=2557, n2=28058, m2=15)
vPetersen(n1=2557, n2=28058, m2=15)
sePetersen(n1=2557, n2=28058, m2=15)
ciPetersen(n1=2557, n2=28058, m2=15)


# Stellako -----------------
# Point estimate via pooled Chapman and Petersen 
NChapman(n1=413, n2=1633, m2=2)
vChapman(n1=413, n2=1633, m2=2) 
seChapman(n1=413, n2=1633, m2=2)
ciChapman(n1=413, n2=1633, m2=2)

NPetersen(n1=413, n2=1633, m2=2)
vPetersen(n1=413, n2=1633, m2=2)
sePetersen(n1=413, n2=1633, m2=2)
ciPetersen(n1=413, n2=1633, m2=2)


##############################################################################################################################################


#                                                          ENVIRONMENTALS


# ====================== RPMs and RST DEBRIS =================================

# Recode debris load to be quantitative
enviro.data <- enviro.data %>%
  mutate(debris_load = case_when(debris_load=="med"~"medium",
                                 debris_load=="Medium"~"medium",
                                 debris_load=="medium/high"~"high",
                                 TRUE ~ as.character(debris_load)),
         debris_load_rc = case_when(debris_load=="low"~"1",
                                    debris_load=="medium"~"2",
                                    debris_load=="high"~"3"),
         DOY_closed = lubridate::yday(date_closed)) %>%
  mutate_at("debris_load_rc", as.numeric) 

View(enviro.data %>% 
  filter(year==2021, site=="Nadleh") %>% 
  group_by(date_closed) %>%
  summarize(debris_load_rc = mean(debris_load_rc, na.rm=T)))


# PLOT: RPMs ~ debris load + time (Nautley) ------------------------
ggplot() +
  geom_bar(data=enviro.data%>%filter(year==2021,site=="Nadleh")%>%group_by(site,date_closed)%>%
             summarize(debris_load_rc=max(debris_load_rc,na.rm=T)), 
           aes(x=as.Date(date_closed), y=debris_load_rc*2), stat="identity", position="dodge", alpha=0.5) +
  geom_point(data=enviro.data%>%filter(year==2021,site=="Nadleh"), aes(x=as.Date(date_closed), y=rst_rpms), 
             size=3, alpha=0.7) +
  geom_line(data=enviro.data%>%filter(year==2021,site=="Nadleh"), aes(x=as.Date(date_closed), y=rst_rpms), 
            size=1, alpha=0.5) +
  scale_colour_manual(values=c("green")) +
  scale_fill_manual(values=c("green")) +
  scale_x_date(date_breaks="3 day", date_labels="%b %d") +
  scale_y_continuous(sec.axis = sec_axis(~./2, name="Debris load")) +
  labs(x="", y="RST RPMs") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=21),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=23),
        panel.grid = element_blank(),
        panel.border = element_rect(size=1.2),
        strip.text = element_blank(),
        legend.position = "none")


# PLOT: RPMs ~ debris load + time (Stellako) ------------------------
ggplot() +
  geom_bar(data=enviro.data%>%filter(year==2021,site=="Stellako",!is.na(debris_load))%>%group_by(site,date_closed)%>%
             summarize(debris_load_rc=max(debris_load_rc,na.rm=T)), 
           aes(x=as.Date(date_closed), y=debris_load_rc*2), stat="identity", position="dodge", alpha=0.5) +
  geom_point(data=enviro.data%>%filter(year==2021,site=="Stellako"), aes(x=as.Date(date_closed), y=rst_rpms), 
             size=3, alpha=0.7) +
  geom_line(data=enviro.data%>%filter(year==2021,site=="Stellako"), aes(x=as.Date(date_closed), y=rst_rpms), 
            size=1, alpha=0.5) +
  scale_colour_manual(values=c("blue")) +
  scale_fill_manual(values=c("blue")) +
  scale_x_date(date_breaks="3 day", date_labels="%b %d") +
  scale_y_continuous(sec.axis = sec_axis(~./2, name="Debris load")) +
  labs(x="", y="RST RPMs") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=21),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=23),
        panel.grid = element_blank(),
        panel.border = element_rect(size=1.2),
        strip.text = element_blank(),
        legend.position = "none") 



# ====================== MOON MOON =================================

# At Chilko -------------------

ggplot() +
  geom_line(data=chilko%>%filter(year>=2001, year<2021), 
            aes(x=as.Date(yday, origin="2000-01-01"), y=total_abundance/1000), colour="gray50", size=1) +
  geom_point(data=chilko%>%filter(year>=2001, year<2021), 
             aes(x=as.Date(yday, origin="2000-01-01"), y=total_abundance/1000), 
             fill="gray30", colour="black", stroke=1.5, pch=21, size=2, alpha=0.6) +
  geom_vline(data=moon%>%filter(year>=2001, year<2021), 
             aes(xintercept=as.Date(yday, origin="2000-01-01"), y=Inf), colour="red", size=1) +

  #geom_point(data=moon%>%filter(year>=2001, year<2021), 
  #           aes(x=as.Date(yday, origin="2000-01-01"), y=total), colour="red", fill="red", stroke=1.5, pch=25, size=3) +
  facet_wrap(.~year, scales = "free_y") +
  theme_bw() +
  theme(legend.position=c(0.1,0.9),
        legend.title=element_blank(),
        legend.text=element_text(size=13),
        legend.spacing.y = unit(0, "mm"),
        legend.background = element_rect(colour="black"),
        axis.text = element_text(colour="black", size=13), 
        axis.title = element_text(face="bold", size=15),
        strip.text = element_text(size=13))


# ====================== TEMP =================================

mig.rect <- data.frame(site = c("Nadleh","Nadleh","Nadleh","Nadleh", "Stellako") ,
                       year=c("2019", "2021", "2019", "2021", "2021"),
                       start=c(lubridate::yday("2019-04-20"), lubridate::yday("2021-04-26"), lubridate::yday("2019-05-03"), 
                               lubridate::yday("2021-05-08"), lubridate::yday("2021-05-04")),
                       end=c(lubridate::yday("2019-04-21"), lubridate::yday("2021-04-27"), lubridate::yday("2019-05-10"), 
                             lubridate::yday("2021-05-13"), lubridate::yday("2021-05-14")) )

ggplot() +
  geom_rect(data=mig.rect, aes(xmin=as.Date(start, origin="2019-12-31"), xmax=as.Date(end, origin="2019-12-31"), ymin=-Inf, ymax=Inf), 
            fill="gray60", alpha=0.5) +  
  geom_point(data=enviro.data%>%group_by(site,year, DOY_closed)%>%summarize(water_temp_C=mean(water_temp_C)), 
             aes(x=as.Date(DOY_closed, origin="2019-12-31"), y=water_temp_C)) +
  geom_line(data=enviro.data%>%group_by(site,year, DOY_closed)%>%summarize(water_temp_C=mean(water_temp_C)), 
            aes(x=as.Date(DOY_closed, origin="2019-12-31"), y=water_temp_C)) +
  facet_wrap(~interaction(site, year), nrow=3) +
  scale_x_date(date_breaks="2 day", date_labels = "%b %d")+
  scale_y_continuous(breaks=seq(0,15, by=2))






                             









##############################################################################################################################################


#                                                               BY-CATCH

# chinook
bio.data %>%
  group_by(site, year) %>%
  filter(species=="Chinook") %>%
  summarize(n=n())

catch.data %>%
  filter(grepl("hinook", comments)) %>%
  group_by(site, year) %>%
  summarize(n=n())
  
















