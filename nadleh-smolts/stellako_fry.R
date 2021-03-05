# stellako flat file 

library(tidyverse)
library(xlsx)
library(openxlsx)
library(lubridate)
library(ggpubr)      # for ggarrange with common.legend
library(ggsci)       # for colour palettes

setwd("~/Documents/ANALYSIS/data")

bio.raw <- read.xlsx("stellako_fry_database.xlsx", sheet="biosamples")
count.raw <- read.xlsx("stellako_fry_database.xlsx", sheet="count", detectDates=T)

options(scipen=9999)

#####################################################################################################################################################

#                                                               CLEANING

bio <- bio.raw %>%
  mutate(date = format(as.Date(date, "%B %d %Y"), "%Y-%m-%d")) %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate_at("year", as.integer) %>%
  mutate(ufid = paste(date, fish_no, sep="-")) %>%
  mutate(cf_k = (100000*ww_g)/(length_mm^3)) %>%
  print()

count <- count.raw %>% 
  mutate(year = lubridate::year(date)) %>% 
  mutate(yday = lubridate::yday(date)) %>%
  mutate_at("year", as.integer) %>%
  mutate(recovery_rate=ifelse(marks_recovered==0, lag(recovery_rate), ((marks_recovered+1)/(marks_released+1)))) %>%
  print()

# checked recovery rate re-calculations using:
##flags <- count %>%
##  mutate(check = ifelse(recovery_rate2-recovery_rate==0, "", "FLAG")) %>%
##  filter(check=="FLAG") %>%
##  print()
# just recovery rates in 1989 don't match because back then they didn't use the '+1' in the formula, so these have been re-calculated for 
# comparison here, but note they won't match the original Excel files. 


#####################################################################################################################################################

#                                                            BIOSAMPLE SUMMARY INFO

###########
# NIGHTLY #
###########

bio.d <- bio %>%
  group_by(year, yday) %>%
  summarize(mean_fl=mean(length_mm), sd_fl=sd(length_mm), mean_ww=mean(ww_g), sd_ww=mean(ww_g), mean_k=mean(cf_k), sd_k=sd(cf_k)) %>%
  print()


fln<-ggplot(bio.d, aes(x=as.Date(yday, origin="1989-01-01"), y=mean_fl, group=year)) +
  geom_ribbon(aes(ymin=mean_fl-sd_fl, ymax=mean_fl+sd_fl, fill=year), alpha=0.1) +
  geom_line(aes(colour=year), size=1.1, alpha=0.7) +
  scale_x_date(date_labels="%d %b", date_breaks="5 day") +
  scale_y_continuous(breaks=seq(20,40,by=2)) +
  labs(x="", y="Fork length (mm)", colour="", fill="") +
  theme_bw() +
  theme(panel.grid = element_blank(),
    axis.text.y = element_text(colour="black", size=11),
    axis.text.x = element_blank(),
    axis.title = element_text(face="bold", size=13)) +
  guides(fill=guide_legend(ncol=2))

wwn<-ggplot(bio.d, aes(x=as.Date(yday, origin="1989-01-01"), y=mean_ww, group=year)) +
  geom_ribbon(aes(ymin=mean_ww-sd_ww, ymax=mean_ww+sd_ww, fill=year), alpha=0.1) +
  geom_line(aes(colour=year), size=1.1, alpha=0.7) +
  scale_x_date(date_labels="%d %b", date_breaks="5 day") +
  scale_y_continuous(breaks=seq(0,0.8,by=0.1)) +
  labs(x="", y="Weight (g w/w)", colour="", fill="") +
  theme_bw() +
  theme(panel.grid = element_blank(),
    axis.text.y = element_text(colour="black", size=11),
    axis.text.x = element_text(colour="black", size=11, angle=45, hjust=1),
    axis.title = element_text(face="bold", size=13)) +
  guides(fill=guide_legend(ncol=2))

cfn<-ggplot(bio.d, aes(x=as.Date(yday, origin="1989-01-01"), y=mean_k, group=year)) +
  geom_ribbon(aes(ymin=mean_k-sd_k, ymax=mean_k+sd_k, fill=year), alpha=0.1) +
  geom_line(aes(colour=year), size=1.1, alpha=0.7) +
  scale_x_date(date_labels="%d %b", date_breaks="5 day") +
  scale_y_continuous(breaks=seq(0.5,1,by=0.1), limits=c(0.5,1)) +
  labs(x="", y="Weight (g w/w)", colour="", fill="") +
  theme_bw() +
  theme(panel.grid = element_blank(),
    axis.text.y = element_text(colour="black", size=11),
    axis.text.x = element_text(colour="black", size=11, angle=45, hjust=1),
    axis.title = element_text(face="bold", size=13)) +
  guides(fill=guide_legend(ncol=2))

ggarrange(fln, wwn, cfn, nrow=2, ncol=2, common.legend=T, legend="none")


##########
# YEARLY #
##########

bio.y <- bio %>%
  group_by(year) %>%
  summarize(mean_fl=mean(length_mm), sd_fl=sd(length_mm), mean_ww=mean(ww_g), sd_ww=sd(ww_g), mean_k=mean(cf_k), sd_k=sd(cf_k)) %>%
  mutate_at("year", as.factor) %>%
  print()

fly<-ggplot(bio.y, aes(x=year, y=mean_fl)) +
  geom_errorbar(aes(x=year, ymin=mean_fl-sd_fl, ymax=mean_fl+sd_fl), width=0, size=0.6) +
  geom_point(shape=21, stroke=1.3, size=4, colour="black", fill="#5ebeae") +
  labs(x="", y="Fork length (mm)") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=12),
    axis.title = element_text(face="bold", size=13))

wwy<-ggplot(bio.y, aes(x=year, y=mean_ww)) +
  geom_errorbar(aes(x=year, ymin=mean_ww-sd_ww, ymax=mean_ww+sd_ww), width=0, size=0.6) +
  geom_point(shape=21, stroke=1.3, size=4, colour="black", fill="#ffad05") +
  scale_y_continuous(breaks=seq(0.05,0.3,by=0.05), limits=c(0.04,0.3)) +
  labs(x="Year", y="Weight (g w/w)") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=12),
    axis.title = element_text(face="bold", size=13))

cfy<-ggplot(bio.y, aes(x=year, y=mean_k)) +
  geom_errorbar(aes(x=year, ymin=mean_k-sd_k, ymax=mean_k+sd_k), width=0, size=0.6) +
  geom_point(shape=21, stroke=1.3, size=4, colour="black", fill="#af3326") +
  scale_y_continuous(breaks=seq(0.5,0.8,by=0.1), limits=c(0.5,0.8)) +
  labs(x="Year", y="Condition factor") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=12),
    axis.title = element_text(face="bold", size=13))

ggarrange(fly, wwy, cfy, nrow=2, ncol=2)


#####################################################################################################################################################

#                                                                   COUNT SUMMARIES 

#############################
# RAW CATCH & POP EST (M-R) #
#############################

ggplot(data=count%>%mutate_at("year", as.factor), aes(x=as.Date(yday, origin="1989-01-01"))) +
  geom_line(aes(y=total_sockeye), size=1, colour="gray70", alpha=0.8) +
  geom_line(aes(y=pop_est/100), size=1, colour="gray30", alpha=0.75) +
  scale_x_date(date_labels="%d %b", date_breaks="14 day") +
  scale_y_continuous(sec.axis = sec_axis(trans=~./100, name="Population estimate (100s)")) +
  labs(x="", y="Raw nightly catch") +
  theme_bw() +
  theme(panel.grid = element_blank(),
    axis.title.y = element_text(face="bold", colour="gray60", size=13),
    axis.title.y.right = element_text(face="bold", colour="gray20", size=13),
    axis.text.y = element_text(colour="black", size=10),
    axis.text.x = element_text(colour="black", angle=45, hjust=1, size=12)) +
  facet_wrap(~year, scales="free_y")



#####################################################################################################################################################

#                                                          M-R ASSESSMENT

#################
# RECOVERY RATE #
#################
pre2000rr<-ggplot(data=count%>%filter(year<2000)%>%mutate_at("year", as.factor), aes(x=as.Date(yday, origin="1989-01-01"), y=recovery_rate, colour=year, group=year)) +
  geom_line(size=1.2, alpha=0.8) +
  scale_x_date(date_labels="%d %b", date_breaks="5 day", limits=c(as.Date(98, origin="1989-01-01"), as.Date(159, origin="1989-01-01"))) +
  labs(x="", y="Nightly recovery rate") +
  theme_bw()

post2000rr<-ggplot(data=count%>%filter(year>=2000)%>%mutate_at("year", as.factor), aes(x=as.Date(yday, origin="1989-01-01"), y=recovery_rate, colour=year, group=year)) +
  geom_line(size=1.2, alpha=0.8) +
  scale_x_date(date_labels="%d %b", date_breaks="5 day", limits=c(as.Date(98, origin="1989-01-01"), as.Date(159, origin="1989-01-01"))) +
  scale_colour_npg() +
  labs(x="", y="Nightly recovery rate") +
  theme_bw()

ggarrange(pre2000rr, post2000rr, nrow=2)


# Marks, recovery & rate 
ggplot(data=count%>%filter(marks_released>0 | marks_recovered>0)%>%mutate_at("year", as.factor), 
  aes(x=as.Date(yday, origin="1989-01-01"))) +
  geom_line(aes(y=marks_released, group=year), colour="gray60", size=0.8, alpha=0.7) +
  geom_point(aes(y=marks_released, group=year), fill="gray60", colour="gray60", stroke=1.1, shape=21, size=1, alpha=0.5) +
    
  geom_line(aes(y=marks_recovered, group=year), colour="gray30", size=0.8, alpha=0.7) +
  geom_point(aes(y=marks_recovered, group=year), fill="gray30", colour="gray30", stroke=1.1, shape=21, size=1, alpha=0.5) +

  geom_line(aes(y=recovery_rate*100000, group=year), colour="#00b8ff", size=0.8, alpha=0.7) +
  geom_point(aes(y=recovery_rate*100000, group=year), fill="#00b8ff", colour="#00b8ff", stroke=1.1, shape=21, size=1, alpha=0.6) +
    
  scale_x_date(date_labels="%d %b", date_breaks="8 day") +
  scale_y_continuous(sec.axis = sec_axis(trans=~./100000, name="Recovery rate")) +
  labs(x="", y="Number of marks released and recovered") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.y = element_text(face="bold", colour="gray40"),
    axis.text.y = element_text(colour="gray40"),
    axis.title.y.right = element_text(face="bold", colour="#00a5e5"),
    axis.text.y.right = element_text(colour="#00a5e5")) +
  facet_wrap(~year, scales="free_y")




###########
# SUMMARY #
###########
# in 1992 and 1995 marks were released at coarser intervals as cohorts. In all other years nightly marking and recapturing occurred, so calculating
# average recovery rates for each year will vary (in the cohort release years, recovery rates were applied between release intervals to calculate
# a nightly population estimate, but it doesn't represent true observed recovery rates). 




# filter for the cohort release years and only observed recovery rates based on actual mark and release events (i.e., remove interpolated recovery
# rates)
cohort.mr <- count %>% 
  filter(year%in%c(1989,1992,1995), marks_released>0 | marks_recovered>0) %>% 
  group_by(year) %>%
  summarize(mean_rr=mean(recovery_rate)*100, sd_rr=sd(recovery_rate)*100) %>%
  print()

night.mr <- count %>% 
  filter(!year%in%c(1989,1992,1995), !is.na(marks_released), !is.na(marks_recovered)) %>% 
  group_by(year) %>% 
  summarize(mean_rr=mean(recovery_rate)*100, sd_rr=sd(recovery_rate)*100) %>%
  print()



















