# Nadleh / Stella smolt progress updates code
# Started Aug 2021 

################################################################################################################################################

# libraries 
library(tidyverse)
library(readxl)
library(egg)

# set wd
setwd("~/ANALYSIS/data")

# read data 
## nadleh data: 
nadleh.catch.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="nightly_catch", n_max=337, na="NA")
nadleh.enviro.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="environmentals", na="NA")
nadleh.bio.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="biosampling", na="NA")
nadleh.lf.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="length_frequency")
chilko.lw.raw <- read_excel("Chilko 2021 - Final.xlsm", sheet="Length-weight_data_entry", range="A1:C1116")

################################################################################################################################################

#                                                                      CLEANING

nadleh.lf <- nadleh.lf.raw %>% 
  mutate(comments = ifelse(comments=="NA", NA, comments)) %>%
  print()
nadleh.lf <- nadleh.lf[rep(1:nrow(nadleh.lf), nadleh.lf[["count"]]), ]

nadleh.bio <- nadleh.bio.raw %>% 
  filter(whatman_uid != "-") %>% 
  mutate(condition_k = (weight_g/length_mm^3)*100000) %>%
  print()

nadleh.catch <- nadleh.catch.raw %>% 
  filter(!grepl("day shift", comments)) %>%
  mutate_at("date_closed", as.Date) %>%
  mutate(date_time = as.POSIXct(paste(date_true, time_trap_closed), format="%Y-%m-%d %H:%M")) %>%
  mutate(date_time_rounded = format(round(date_time, units="hours"), format="%H:%M")) %>%
  print()

chilko.lw <- chilko.lw.raw %>% 
  rename(date=Date,
         length_mm=`Length (mm)`,
         weight_g=`Weight (g)`) %>%
  mutate(date = as.Date(date)) %>%
  mutate(condition_k = (weight_g/length_mm^3)*100000) %>%
  print()

################################################################################################################################################

#                                                                   SAMPLING SUMMARY

# Total # sockeye encountered by RST 
# bycatch done by eye in Excel 
nadleh.catch %>% 
  filter(location != "Release Location") %>%
  summarize(fish = sum(total_unmarked, na.rm=T)+sum(total_recaps, na.rm=T)) %>% 
  print()
 
# Total # biosampled 
nadleh.bio %>% 
  filter(!grepl("hinook", comments)) %>% 
  summarize(n = n()) %>%
  print()
# GSI sampled
nadleh.bio %>% 
  filter(!grepl("hinook", comments), !grepl("don't run", comments), whatman_uid != "NA-NA", !is.na(whatman_uid)) %>% 
  summarize(n = n()) %>%
  print()
# age samped 
nadleh.bio %>% 
  filter(!grepl("hinook", comments), !grepl("don't run", comments), PSC_uid != "NA-NA", ) %>% 
  summarize(n = n()) %>%
  print()
# paired length-weight samped 
nadleh.bio %>% 
  filter(!grepl("hinook", comments), !grepl("don't run", comments), !is.na(weight_g), !is.na(length_mm)) %>% 
  summarize(n = n()) %>%
  print()
# full workup
nadleh.bio %>% 
  filter(!grepl("hinook", comments), !grepl("don't run", comments), !is.na(weight_g), !is.na(length_mm), PSC_uid != "NA-NA", 
         whatman_uid != "NA-NA") %>% 
  summarize(n = n()) %>%
  print()

#################################################################################################################################################

#                                                                ENVIRONMENTALS 



#################################################################################################################################################

#                                                                     MIGRATION SUMMARY


#-------- ABUNDANCE-BASED MIGRATION PATTERNS
# Total nightly catch 
nightly.catch <- nadleh.catch %>% 
  filter(!is.na(time_trap_closed), location != "Release Location") %>%
  group_by(date_closed) %>%
  summarize(night_count = sum(total_unmarked)+sum(total_recaps, na.rm=T)) %>%
  print()

night<-ggplot(data=nightly.catch, aes(x=as.Date(date_closed), y=night_count)) +
  annotate(geom="text", x=as.Date("2021-04-12"), y=5500, label="A", size=5) +
  geom_bar(stat="identity", colour="black", fill="#1E90FF", width=1, size=0.2) +
  scale_y_continuous(breaks=seq(0, 6000, by=1000), limits=c(0,6000)) +
  scale_x_date(date_breaks="3 day", date_labels="%b %d") +   #limits=c(as.Date("2021-04-12"), as.Date("2021-04-30")), 
  labs(x="Date", y="Total nightly catch") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=9),
        panel.grid = element_blank(), 
        axis.title = element_text(face="bold", size=11),
        axis.text.x = element_text(angle=45, hjust=1))

# Hourly catch 
hourly.catch <- nadleh.catch %>% 
  filter(!is.na(date_closed), location != "Release Location", 
         date_time_rounded%in%c("21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00")) %>%
  group_by(date_closed, date_time_rounded) %>%
  summarize(hour_count = sum(total_unmarked)+sum(total_recaps, na.rm=T)) %>%
  mutate(propn=hour_count/sum(hour_count)) %>%
  #mutate(propn=ifelse(is.nan(propn), NA, propn)) %>%
  group_by(date_time_rounded) %>% 
  summarize(avg_propn = mean(propn, na.rm=T), sd=sd(propn, na.rm=T)) %>%
  print()

hour<-ggplot(data=hourly.catch, 
             aes(x=factor(hourly.catch$date_time_rounded, levels=c("21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00"), ordered=T), 
                 y=avg_propn, group=1)) +
  geom_ribbon(aes(ymin=avg_propn-sd, ymax=avg_propn+sd), fill="gray85", alpha=0.7) +
  geom_line(size=0.3, colour="black") +
  geom_point(size=2, shape=21, colour="black", fill="#1E90FF") +
  annotate(geom="text", x="21:00", y=0.9, label="B", size=5) +
  scale_y_continuous(breaks=seq(0,1,by=0.2), limits=c(0-0.1,1)) +
  labs(x="Time", y=expression(bold(atop("Average hourly", bold("proportion (mean"%+-%"SD)"))))) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=9),
        panel.grid = element_blank(), 
        axis.title.x = element_text(face="bold", size=11, margin=margin(t=10,b=0,l=0,r=0)),
        axis.title.y = element_text(face="bold", size=11, margin=margin(t=0,b=0,l=10,r=10)))

# combo migration plot 
ggarrange(night, hour, nrow=2)


#-------- DAYTIME FISHING EFFORT 
day.catch <- nadleh.catch.raw %>%
  filter(grepl("day shift", comments)) %>%
  select(date_opened:date_true, location:time_trap_closed, n_unmarked_sampled:comments) %>%
  print()
sum(day.catch$total_unmarked)


#-------- LENGTH-BASED MIGRATION PATTERNS
# Nightly lengths
ggplot(nadleh.bio, aes(x=as.Date(date_closed), y=length_mm)) +
  geom_point()

# Hourly lengths 
ggplot(nadleh.bio%>%filter(!time_trap_closed%in%c("21:30", "21:40")), 
       aes(x=factor(time_trap_closed, levels=c("21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00"), ordered=T), 
           y=length_mm)) +
  geom_point()




#################################################################################################################################################

#                                                           BIOSAMPLING SUMMARIES 

#-------- LENGTH-FREQUENCY
min(nadleh.lf$length_mm)
max(nadleh.lf$length_mm)
min(nadleh.bio$length_mm, na.rm=T)
max(nadleh.bio$length_mm, na.rm=T)

# Length-freq histo and density overlay WITH CHILKO COMPARISON
ggplot() +
  geom_histogram(data=chilko.lw, aes(length_mm, y=..density..), fill="green", colour="transparent", alpha=0.2, bins=50) +
  geom_density(data=chilko.lw, aes(length_mm), colour="green", fill="green", size=1, alpha=0.1, adjust=1.5) +  
  geom_histogram(data=nadleh.lf, aes(length_mm, y=..density..), fill="orange", colour="transparent", alpha=0.3, bins=50) +
  geom_density(data=nadleh.lf, aes(length_mm), colour="orange", fill="orange", size=1, alpha=0.3, adjust=1.5) +
  scale_x_continuous(breaks=seq(70,205, by=2)) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=9),
        panel.grid = element_blank(), 
        axis.title.x = element_text(face="bold", size=11, margin=margin(t=10,b=0,l=0,r=0)),
        axis.title.y = element_text(face="bold", size=11, margin=margin(t=0,b=0,l=10,r=10)))

# Length by # records 
ggplot() +
geom_bar(data=nadleh.lf%>%group_by(length_mm)%>%summarize(total=sum(count)), 
           aes(x=length_mm, y=total), stat="identity", fill="green", colour="black", size=0.5) +
  scale_x_continuous(breaks=seq(70,205, by=2)) +
  labs(x="Length (mm)", y="Frequency") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=9),
        panel.grid = element_blank(), 
        axis.title.x = element_text(face="bold", size=11, margin=margin(t=10,b=0,l=0,r=0)),
        axis.title.y = element_text(face="bold", size=11, margin=margin(t=0,b=0,l=10,r=10)))

# Length > 140mm 
ggplot(data=nadleh.bio%>%filter(!is.na(length_mm), length_mm>140), aes(length_mm)) +
  geom_histogram(aes(y=..density..), fill="gray50", colour="transparent", alpha=0.3, bins=50) +
  geom_density(colour="orange", fill="orange", size=1, alpha=0.4, adjust=1.5) +
  scale_x_continuous(breaks=seq(70,205, by=2)) 

nadleh.bio %>% 
  filter(length_mm>140) %>%
  group_by(length_mm) %>%
  summarize(n=n()) %>%
  summarize(sum(n))



#-------- CONDITION FACTOR 
# With Chilko comparison 
CFs <- rbind(
  nadleh.bio%>%summarize(meanCF = mean(condition_k, na.rm=T), seCF=sd(condition_k, na.rm=T)/sqrt(length(condition_k))) %>%
  mutate(site="Nadleh"),
  
  chilko.lw%>%summarize(meanCF = mean(condition_k, na.rm=T), seCF=sd(condition_k, na.rm=T)/sqrt(length(condition_k))) %>%
    mutate(site="Chilko")
)

ggplot() +
  geom_bar(data=CFs, aes(x=site, y=meanCF), stat="identity", fill="gray60", colour="gray20") +
  #geom_errorbar(data=CFs, aes(x=site, ymax=meanCF+seCF, ymin=meanCF-seCF), width=0.1) +
  scale_y_continuous(limits=c(0,1)) +
  labs(x="", y="Average condition factor (k)") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        axis.title = element_text(face="bold"))



#################################################################################################################################################

#                                                         MARK-RECAPTURE PRELIM ASSESSMENT


# Release cohorts 
mark.cohorts <- nadleh.catch %>%
  filter(location=="Release Location") %>%
  print()
# releases occurred on 2021-04-29, 2021-05-02, 2021-05-10, and 2021-05-19

# Use release cohort dates to filter nightly_catch for calculating recapture rates 
nadleh.catchtest <- nadleh.catch %>% 
  mutate(cohort_event = ifelse(date_closed>=as.Date("2021-04-29")&date_closed<as.Date("2021-05-02"), 1, 
                               ifelse(date_closed>=as.Date("2021-05-02")&date_closed<as.Date("2021-05-10"), 2, 
                                      ifelse(date_closed>=as.Date("2021-05-10")&date_closed<as.Date("2021-05-19"), 3,
                                             ifelse(date_closed>=as.Date("2021-05-19"), 4, NA))))) %>%
  group_by(cohort_event) %>%
  summarize(release_date=min(date_closed), n_release=unique(n_marked_released), n_recaps = sum(n_recaps_lower_clip, na.rm=T)) %>%
  filter(!is.na(n_release)) %>%
  mutate(recovery_rate = (n_recaps/n_release)*100) %>%
  print()


###################################################################

# SIZE DISTRIBUTIONS OF MARK, RELEASE AND 'ROUTINE' POPULATIONS


# Histograms 
ggplot() +
  geom_histogram(data=nadleh.lf%>%filter(site=="Nadleh" & data_type=="Routine"), aes(length_mm), fill="gray50", alpha=0.3) +
  geom_histogram(data=nadleh.lf%>%filter(site=="Nadleh" & data_type%in%c("Application", "Recapture")), aes(length_mm, group=data_type, fill=data_type, colour=data_type), alpha=0.3) 

# Density plots 
ggplot() +
  geom_density(data=nadleh.lf%>%filter(site=="Nadleh" & data_type=="Routine"), aes(length_mm), fill="gray60", colour="gray60", alpha=0.3) +
  geom_density(data=nadleh.lf%>%filter(site=="Nadleh" & data_type%in%c("Application", "Recapture")), aes(length_mm, group=data_type, fill=data_type, colour=data_type), alpha=0.3)


#-------- PRELIM STATS
# Can't test Recovery sample as it is too small :( 
# Komolgorov-Smirnov test of distribution 
ks.test(x=nadleh.lf[nadleh.lf$data_type=="Application",]$length_mm, y=nadleh.lf[nadleh.lf$data_type=="Routine",]$length_mm)
# T-test of means
t.test(x=nadleh.lf[nadleh.lf$data_type=="Application",]$length_mm, y=nadleh.lf[nadleh.lf$data_type=="Routine",]$length_mm)



