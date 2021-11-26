
# NORTHERN SMOLTS ANALYSIS 2019 + 2021
# PSC Final Report Year 2 code 
# Building on 'nautley_final_report.R' script generated from 2019 report


############################################################################################################################################

library(tidyverse)
library(readxl)
library(lubridate)
library(cowplot)    # for plot_grid (superior to egg::ggarrange!)
library(padr)
library(withr)

setwd("~/ANALYSIS/data")
options(scipen = 9999999)

catch.data.raw <- read_excel("Northern_smolt_database_2019-2021.xlsx", sheet="nightly_catch")
enviro.data.raw <- read_excel("Northern_smolt_database_2019-2021.xlsx", sheet="environmental")
lf.data.raw <- read_excel("Northern_smolt_database_2019-2021.xlsx", sheet="length_frequency")
bio.data.raw <- read_excel("Northern_smolt_database_2019-2021.xlsx", sheet="biosampling", guess_max = 10000)
stella.disch.raw <- read.csv("STELLA_DISC_2021_08JB002_QR_Nov-1-2021_09_27_39PM.csv")
nad19.disch.raw <- read.csv("NAUT_DISCH_2019_08JB003_QR_Dec-19-2019_12_44_31AM.csv")
nad21.disch.raw <- read.csv("NAUT_DISCH_2021_08JB003_QR_Nov-1-2021_09_25_53PM.csv")
nadhist.disch.raw <- read.csv("NAUT_DISCH_1999-2000_Daily__Nov-3-2021_08_38_13PM.csv")
nad.hist.raw <- read.csv("1999-2000 Nautley data.csv")
moon.raw <- read.csv("moon_phases.csv")


############################################################################################################################################


#                                                  CLEANING


#=======================================================
#                 SMOLT DATA
#=======================================================

#--------- CATCH DATA 2019-2021
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


# HISTORICAL CATCH DATA 1999-2000
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



#--------- BIOLOGICAL DATA 
lf.data <- lf.data.raw %>% 
  mutate_at(vars(date_opened, date_closed), as.Date) %>% 
  print()  

bio.data <- bio.data.raw %>% 
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  mutate_at(vars(contains('_reg')), funs(ifelse(.=="4","Nadina", ifelse(.=="12","Stellako",.)))) %>%
  mutate(cf_k = (weight_g/(length_mm)^3)*100000,
         DOY_closed = lubridate::yday(date_closed)) %>%
  print()  






#=======================================================
#                 ENVIRONMENTAL DATA
#=======================================================


#--------- FIELD ENVIRO DATA
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


#--------- EC DISCHARGE DATA
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


#--------- MOON MOON
moon <- moon.raw %>%
  mutate(date = lubridate::dmy(date)) %>%
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


#----------- DIURNAL PATTERS
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


#--- Nadleh:
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


#--- Stellako:
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


#============================================================================
#                               NIGHTLY
#============================================================================

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


#--------- Calculate CPUE
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


#--------- NIGHTLY CPUE CATCH TABLE - for ease of plotting and visual analysis  
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




#--------- PLOT: CPUE MIGRATION CORRECTED
plot_grid(
ggplot() +
  geom_point(data=moon%>%filter(year%in%c("1999"), date!=as.Date("1999-05-30")), 
             aes(x=as.Date(lubridate::yday(date), origin="1998-12-31"),y=2000),colour="gray70", fill="gray70",
             shape=24,size=4,alpha=0.6) +
  geom_point(data=moon%>%filter(year%in%c("2000"), date!=as.Date("2000-04-18")), 
             aes(x=as.Date(lubridate::yday(date), origin="1998-12-31"),y=2000),colour="black", fill="black",
             shape=24,size=4,alpha=0.7) +
  
  annotate(geom="text", label="A", x=as.Date(101, origin="1998-12-31"), y=2300, fontface=2, size=7, hjust = 0) +
  annotate(geom="text", label="Nautley 1999, 2000", x=as.Date(101, origin="1998-12-31"), fontface=3, y=1900, size=5, hjust = 0) +
  geom_line(data=discharge.hist, 
            aes(x=as.Date(DOY, origin="1998-12-31"), y=discharge_cms*25, group=year, colour=year), alpha=0.6) +
  geom_bar(data=catch.hist,
           aes(x=as.Date(DOY_closed, origin="1998-12-31"), y=nightly_CPUE, group=year, fill=year, colour=year), 
           stat="identity", alpha=0.5, size=0.2, width=0.8) +
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
  geom_point(data=moon%>%filter(year=="2019", date!=as.Date("2019-05-18")), 
             aes(x=as.Date(lubridate::yday(date), origin="2018-12-31"),y=7000),shape=24,col="red",fill="red",size=4) +
  geom_rect(data=ice_flows%>%filter(year=="2019"), 
            aes(xmin=as.Date(xstartDOY, origin="2018-12-31"), xmax=as.Date(xendDOY, origin="2018-12-31"), ymin=-Inf, ymax=Inf), 
            fill="gray60", alpha=0.5) +
  annotate(geom="text", label="B", x=as.Date(101, origin="2018-12-31"), y=7000, fontface=2, size=7, hjust = 0) +
  annotate(geom="text", label="Nautley 2019", x=as.Date(101, origin="2018-12-31"), fontface=3, y=5800, size=5, hjust = 0) +
  geom_ribbon(data=discharge.19_21%>%filter(year=="2019"), 
              aes(x=as.Date(DOY, origin="2018-12-31"), ymin=min_dis*75, ymax=max_dis*75), fill="#8bc2fd", alpha=0.6) +
  geom_line(data=discharge.19_21%>%filter(year=="2019"), 
            aes(x=as.Date(DOY, origin="2018-12-31"), y=mean_dis*75), colour="#1785fc", alpha=0.6) +
  geom_bar(data=catch.nightly%>%filter(year=="2019"), aes(x=as.Date(DOY_closed, origin="2018-12-31"), y=hourly_CPUE_propnMEAN), 
           stat="identity", colour="forest green", fill="forest green", alpha=0.75, size=0.2, width=0.8) +
  geom_errorbar(data=catch.nightly%>%filter(year=="2019", date_closed%in%c(as.Date("2019-04-13"):as.Date("2019-04-25"))), 
                aes(x=as.Date(DOY_closed, origin="2018-12-31"), ymin=hourly_CPUE_propnLOWER, ymax=hourly_CPUE_propnUPPER), width=0.3, size=0.5) +   
  geom_point(data=catch.nightly%>%filter(year=="2019", date_closed%in%c(as.Date("2019-04-13"):as.Date("2019-04-25"))),
               aes(x=as.Date(DOY_closed, origin="2018-12-31"), y=hourly_CPUE_propnMEAN), 
               color="black", fill="black", shape=21, size=2) +
  scale_x_date(limits=c(as.Date(101, origin="2018-12-31"), as.Date(150, origin="2018-12-31")), 
               date_labels="%b %d", date_breaks="3 day", expand=c(0.01,0.01)) +  
  scale_y_continuous(sec.axis = sec_axis(~./75, name = expression(bold("Discharge"~m^3/s)))) +
  labs(x="", y="CPUE") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=18),
        panel.grid  = element_blank(),
        panel.border = element_rect(size=1.2)),

# NADLEH 2021
ggplot() +
  geom_point(data=moon%>%filter(year=="2021"), 
             aes(x=as.Date(lubridate::yday(date), origin="2020-12-31"),y=5000),shape=24,col="red",fill="red",size=4) +
  geom_rect(data=ice_flows%>%filter(year=="2021",site=="Nadleh"), 
            aes(xmin=as.Date(xstartDOY, origin="2020-12-31"), xmax=as.Date(xendDOY, origin="2020-12-31"), ymin=-Inf, ymax=Inf), 
            fill="gray60", alpha=0.5) +
  annotate(geom="text", label="C", x=as.Date(101, origin="2020-12-31"), y=5500, fontface=2, size=7, hjust = 0) +
  annotate(geom="text", label="Nautley 2021", x=as.Date(101, origin="2020-12-31"), fontface=3, y=4500, size=5, hjust = 0) +
  geom_ribbon(data=discharge.19_21%>%filter(year=="2021",site=="Nadleh"), 
              aes(x=as.Date(DOY, origin="2020-12-31"), ymin=min_dis*30, ymax=max_dis*30), fill="#8bc2fd", alpha=0.6) +
  geom_line(data=discharge.19_21%>%filter(year=="2021",site=="Nadleh"), 
            aes(x=as.Date(DOY, origin="2020-12-31"), y=mean_dis*30), colour="#1785fc", alpha=0.6) +

  geom_bar(data=catch.nightly%>%filter(year=="2021",site=="Nadleh"),               
           aes(x=as.Date(DOY_closed, origin="2020-12-31"), y=nightly_CPUE), 
           stat="identity", fill="forest green", colour="forest green", alpha=0.75, size=0.2, width=0.8) +
  
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
  geom_point(data=moon%>%filter(year=="2021"), 
             aes(x=as.Date(lubridate::yday(date), origin="2020-12-31"),y=300),shape=24,col="red",fill="red",size=4) +
  annotate(geom="text", label="D", x=as.Date(101, origin="2020-12-31"), y=360, fontface=2, size=7, hjust = 0) +
  annotate(geom="text", label="Stellako 2021", x=as.Date(101, origin="2020-12-31"), y=290, fontface=3, size=5, hjust = 0) +  
  geom_ribbon(data=discharge.19_21%>%filter(year=="2021",site=="Stellako"), 
              aes(x=as.Date(DOY, origin="2020-12-31"), ymin=min_dis*4, ymax=max_dis*4), fill="#8bc2fd", alpha=0.6) +
  geom_line(data=discharge.19_21%>%filter(year=="2021",site=="Stellako"), 
            aes(x=as.Date(DOY, origin="2020-12-31"), y=mean_dis*4), colour="#1785fc", alpha=0.6) +
  geom_bar(data=catch.nightly%>%filter(year=="2021", site=="Stellako"), 
           aes(x=as.Date(DOY_closed, origin="2020-12-31"), y=nightly_CPUE), 
           stat="identity", fill="forest green", colour="forest green", alpha=0.75, size=0.2, width=0.8) +
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



#============================================================================
#                               HOURLY %
#============================================================================

#--------- Calculate hourly proportions
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


#--------- Join
hourly_all <- rbind(hourly_passage, hourly.hist) %>%
  ungroup()
hourly_all$time_trap_closed <- factor(hourly_all$time_trap_closed, 
                                       levels=c("21:00", "22:00", "23:00", "23:30", "00:00", "00:30",
                                                "01:00", "01:30", "02:00", "02:15", "02:30", "03:00", "04:00", "05:00"), ordered=T)


#--------- PLOT 
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
  #coord_capped_cart(bottom='both') +
  geom_text(data=data.frame(lab=c("A", "B"), site=c("Nautley","Stellako"), x=c(1, 1), y=c(0.6,0.6)) , 
            aes(label=lab, x=x, y=y), inherit.aes=F, size=7, fontface=2, hjust=0) +
  geom_text(data=data.frame(lab=c("A", "B"), site=c("Nautley","Stellako"), x=c(1, 1), y=c(0.55,0.55)) , 
            aes(label=site, x=x, y=y), inherit.aes=F, size=5, fontface=3, hjust=0)



##############################################################################################################################################


#                                                 BIOLOGICAL DATA (no GSI)


#--------- TEMPORAL (nightly) TRENDS IN RAW DATA 
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


#--------- TEMPORAL (hourly) TRENDS IN RAW DATA 
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


#--------- Length-freq compared to biodata 
#all.l <- bio.data %>% 
#  select(site, year, date_opened, date_closed, length_mm) %>%
#  filter(!is.na(length_mm)) %>%
#  mutate(source="biodata") %>% 
#  left_join(., lf.data) %>% 
#  print()

ggplot() +
  geom_bar(data=lf.data%>%filter(year=="2021", site=="Nadleh")%>%group_by(length_mm)%>%summarize(n=n()), 
           aes(x=length_mm, y=n), stat="identity", fill="gray80", colour="gray80", alpha=0.7) +
  geom_histogram(data=bio.data%>%filter(year=="2021", site=="Nadleh",!is.na(length_mm))%>%group_by(length_mm)%>%summarize(n=n()),
                 aes(x=length_mm, y=n), stat="identity", fill="blue", colour="blue", alpha=0.7) +
  scale_x_continuous(breaks=seq(0,200,by=10)) +
  theme_bw()

ggplot() +
  geom_bar(data=lf.data%>%filter(year=="2021", site=="Stellako")%>%group_by(length_mm)%>%summarize(n=n()), 
           aes(x=length_mm, y=n), stat="identity", fill="gray80", colour="gray80", alpha=0.7) +
  geom_histogram(data=bio.data%>%filter(year=="2021", site=="Stellako",!is.na(length_mm))%>%group_by(length_mm)%>%summarize(n=n()),
                 aes(x=length_mm, y=n), stat="identity", fill="blue", colour="blue", alpha=0.7) +
  scale_x_continuous(breaks=seq(0,200,by=10)) +
  theme_bw()



##############################################################################################################################################


#                                                               GSI

#--------- GSI check
ggplot(data=bio.data%>%filter(year=="2021",site=="Nadleh"), aes(x=b1_reg1,y=b1_prob1)) +
  geom_point()

# Number with GSI certainty >= 80% 
bio.data %>%
  filter(year=="2021", dna_select_bin==1) %>%
  group_by(site, b1_prob1>=0.8) %>%
  summarize(n=n())

bio.data %>%
  filter(year=="2021",site=="Nadleh",b1_prob1<0.8) %>%
  select(ufid,b1_reg1:b1_prob2) %>%
  mutate(diff_GSI = abs(b1_prob1-b1_prob2)) %>%
  filter(diff_GSI>=0.5)


#--------- GSI Summary
bio.data %>% 
  filter(year=="2021",b1_prob1>=0.8) %>%
  group_by(site, b1_reg1) %>%
  summarize(n=n()) %>%
  mutate(propn = n/sum(n))


#--------- GSI nightly 
GSI.nightly <- bio.data %>% 
  filter(year=="2021", b1_prob1>=0.8) %>% 
  group_by(date_closed, DOY_closed, b1_reg1) %>% 
  summarize(n=n())%>%
  group_by(date_closed) %>% 
  mutate(propn=n/sum(n), year="2021", DOY_closed_offset=DOY_closed) %>%
  rename(region=b1_reg1) %>% 
  ungroup() %>%
  bind_rows(., bio.data %>% 
              filter(year=="2019", b12_prob1>=0.8) %>% 
              group_by(date_closed, DOY_closed, b12_reg1) %>% 
              summarize(n=n())%>%
              group_by(date_closed) %>% 
              mutate(propn=n/sum(n), year="2019", DOY_closed_offset=DOY_closed+7)%>%
              rename(region=b12_reg1) %>% 
              ungroup()) 


ggplot(data=GSI.nightly, 
       aes(x=as.Date(DOY_closed, origin="2018-12-31"), y=propn, group=region, 
           colour=region, fill=region)) +
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
  geom_smooth(method="loess", alpha=0.3, size=1, span=0.5) +
  geom_point(alpha=0.6, shape=21, size=3, stroke=1.3) +
  labs(x="", y="Daily proportion") + 
  scale_x_date(date_breaks = "2 day", date_labels="%b %d") +
  scale_y_continuous(breaks=seq(0,1,by=0.25))+
  scale_colour_manual(values=c("orange", "blue")) +
  scale_fill_manual(values=c("orange", "blue")) +
  theme_bw() + 
  theme(axis.text = element_text(colour="black", size=17),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=20),
        legend.position = c(0.9,0.1),
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



#--------- GSI hourly                             **** COME BACK TO THIS ONCE MORE DNA SAMPLES ARE ANALYZED ****
GSI.hourly <- bio.data %>% 
  filter(year=="2021", b1_prob1>=0.8) %>% 
  group_by(date_closed, time_trap_closed, b1_reg1) %>% 
  summarize(n=n())%>%
  mutate(propn=n/sum(n)) %>%
  group_by(time_trap_closed, b1_reg1) %>% 
  summarize(mean_perc = mean(propn, na.rm=T), sd_perc=sd(propn,na.rm=T)) %>%
  rename(region=b1_reg1) %>% 
  ungroup() %>%
  print()
GSI.hourly$time_trap_closed <- factor(GSI.hourly$time_trap_closed, levels=c("22:00", "23:00", "00:00", "01:00", "02:00",
                                                                            "03:00", "04:00", ordered=T))

ggplot(GSI.hourly, 
       aes(x=time_trap_closed, y=mean_perc, group=region, fill=region, colour=region)) +
  geom_hline(yintercept = 0.5, colour="red", linetype="dotted") +
  #geom_smooth(method="loess") +
  #geom_errorbar(aes(ymin=mean_perc-sd_perc, ymax=mean_perc+sd_perc, group=region)) +
  geom_point(shape=21, size=4, alpha=0.6) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=17),
        axis.title = element_text(size=19, face="bold"),
        panel.grid = element_blank(),
        panel.border = element_rect(size=1.2),
        legend.position = c(0.8,0.2),
        legend.background = element_rect(colour="black"),
        legend.title = element_blank())


#--------- GSI SIZE 
GSI.size <- bio.data %>% 
  filter(year=="2021", site=="Nadleh", b1_prob1>=0.8, !is.na(length_mm)|!is.na(weight_g)|!is.na(cf_k)) %>% 
  group_by(b1_reg1) %>% 
  summarize(n=n(), mean_FL = mean(length_mm, na.rm=T), se_FL=sd(length_mm, na.rm=T)/sqrt(length(length_mm)),
            mean_W = mean(weight_g, na.rm=T), se_W=sd(weight_g, na.rm=T)/sqrt(length(weight_g)),
            mean_CF = mean(cf_k, na.rm=T), se_CF=sd(cf_k, na.rm=T)/sqrt(length(cf_k)))%>%
  mutate(year="2021") %>%
  rename(region=b1_reg1) %>%
  ungroup() %>%
  bind_rows(., bio.data %>% 
              filter(year=="2019", b12_prob1>=0.8, !is.na(length_mm)|!is.na(weight_g)|!is.na(cf_k)) %>% 
              group_by(b12_reg1) %>% 
              summarize(n=n(), mean_FL = mean(length_mm, na.rm=T), se_FL=sd(length_mm, na.rm=T)/sqrt(length(length_mm)),
                        mean_W = mean(weight_g, na.rm=T), se_W=sd(weight_g, na.rm=T)/sqrt(length(weight_g)),
                        mean_CF = mean(cf_k, na.rm=T), se_CF=sd(cf_k, na.rm=T)/sqrt(length(cf_k)))%>%
              mutate(year="2019") %>%
              rename(region=b12_reg1) %>%
              ungroup())

# Plot                            
plot_grid(
  ggplot(GSI.size) +
    geom_bar(aes(x=year, y=mean_FL, group=region, fill=region), 
             position="dodge", stat="identity", colour="black", alpha=0.8, size=0.7) +
    geom_errorbar(aes(x=year, ymin=mean_FL-se_FL, ymax=mean_FL+se_FL, group=region),
                  position = position_dodge(width = .9), width=0, size=1.2) +
    scale_fill_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
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
    annotate(geom="text", label="A", x=0.45, y=170, fontface=2, size=7, hjust = 0),
  
  ggplot(GSI.size) +
    geom_bar(aes(x=year, y=mean_W, group=region, fill=region), 
             position="dodge", stat="identity", colour="black", alpha=0.8, size=0.7) +
    geom_errorbar(aes(x=year, ymin=mean_W-se_W, ymax=mean_W+se_W, group=region),
                  position = position_dodge(width = .9), width=0, size=1.2) +
    scale_fill_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
    scale_y_continuous(breaks=seq(0,20,by=5), limits=c(0,20)) +
    labs(x="", y="Weight (g)") +
    theme_bw() +
    theme(axis.text = element_text(colour="black", size=17),
          axis.title = element_text(face="bold", size=19),
          panel.grid = element_blank(),
          panel.border = element_rect(size=1.2)) + 
    guides(fill="none", colour="none") + 
    annotate(geom="text", label="B", x=0.45, y=19, fontface=2, size=7, hjust = 0),
  
  ggplot(GSI.size) +
    geom_bar(aes(x=year, y=mean_CF, group=region, fill=region), 
             position="dodge", stat="identity", colour="black", alpha=0.8, size=0.7) +
    geom_errorbar(aes(x=year, ymin=mean_CF-se_CF, ymax=mean_CF+se_CF, group=region),
                  position = position_dodge(width = .9), width=0, size=1.2) +
    scale_fill_manual(breaks=c("Nadina", "Stellako"), values=c("orange", "blue")) +
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


#--------- GSI big smolts 
bio.data %>% 
  filter(year=="2021", length_mm > 130) %>% 
  group_by(site) %>%
  summarize(n=n())

bio.data %>% 
  filter(year=="2021", length_mm > 130, b1_prob1>=0.8) %>% 
  group_by(site, b1_reg1) %>%
  summarize(n=n())




##############################################################################################################################################

#                                                      COPEPODS


# COPEPODS OVERALL 
bio.data %>% 
  filter(year=="2021") %>%
  group_by(site, grepl("opepod", comments)) %>%
  summarize(n=n(), meanL=mean(length_mm,na.rm=T), meanW=mean(weight_g,na.rm=T)) %>%
  group_by(site) %>%
  mutate(perc=n/sum(n))


# COPEPODS GSI 
bio.data %>% 
  filter(year=="2021", b1_prob1>=0.8, grepl("opepod", comments)) %>%
  group_by(site, b1_reg1) %>%
  summarize(n=n(), meanL=mean(length_mm,na.rm=T), sdL=sd(length_mm,na.rm=T), 
            meanW=mean(weight_g,na.rm=T), sdW=sd(weight_g,na.rm=T)) %>%
  group_by(site) %>%
  mutate(perc=n/sum(n))


# COPEPODS GSI & SIZE
bio.data %>% 
  filter(year=="2021", b1_prob1>=0.8) %>%
  group_by(site, grepl("opepod", comments)) %>%
  summarize(n=n(), meanL=mean(length_mm,na.rm=T), sdL=sd(length_mm,na.rm=T), 
            meanW=mean(weight_g,na.rm=T), sdW=sd(weight_g,na.rm=T)) %>%
  group_by(site) %>%
  mutate(perc=n/sum(n))

