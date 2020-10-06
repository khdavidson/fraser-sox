# SONAR infilling - double hours missing 
# Sept 28 2020

####################################################################################################################################################

library(tidyverse)
library(broom)         # for multiple linear models through groupings e.g., do()  
library(imputeTS)
library(grid)          # for grid.draw()

setwd("~/Documents/ANALYSIS/data/Sonar")


####################################################################################################################################################


nad.dat.raw <- read.csv("Nadina Sonar_tool_2020_KDinfill_DATAENTRY.csv")

#                                                          CLEANING

nad.dat <- nad.dat.raw %>% 
  rename(bank=Bank,
    observer=Observer,
    date=Date,
    count_hr=Count.Hour,
    hr_portion=Portion.of.hour,
    time_mins=Time.counted_min,
    sox_us=Sox_us,
    sox_ds=Sox_ds,
    ch_us=CH_us,
    ch_ds=CH_ds,
    obs_count=Obs.Count..,
    comments=Comments) %>% 
  mutate(date = lubridate::dmy(date)) %>%
  mutate(ydate = lubridate::yday(date)) %>% 
  print()


####################################################################################################################################################

#                                                 DOUBLE HR ISSUES - DATA EXPLORATION

# Sept 10 and 11 both have hours 0300-0800 which makes infilling difficult. 

#####################################
# RAW DATA SELECTION - NO INFILLING #
#####################################

# Select the hours of interest 
hrs_of_interest <- c(3, 4, 5, 6, 7, 8)

hrs_subset <- nad.dat %>% 
  filter(count_hr %in% hrs_of_interest, obs_count=="1") %>% 
  print()

# check them out 
ggplot(hrs_subset %>% filter(date >= as.Date("2020-08-24")), aes(x=ydate, y=sox_us)) +
  geom_point() +
  geom_line() +
  facet_wrap(~count_hr, ncol=3)

# Minimized dataset just 4 days before/after the gap
day_subset <- hrs_subset %>% 
  filter(date > "2020-09-05" & date < "2020-09-16", obs_count=="1") %>%
  print()

ggplot(day_subset, aes(x=ydate, y=sox_us)) +
  geom_point() +
  geom_line() +
  geom_smooth(aes(group=count_hr), method="lm") +
  facet_wrap(~count_hr, ncol=3)



####################################################################################################################################################

#                                                                 0300 FILES

#----------------------------------------------------------


#######################
# CREATE DATA SUBSETS #
#######################
# No lead up 0s
dat.3 <- nad.dat %>%
  filter(count_hr=="3", date > as.Date("2020-08-27"), obs_count=="1") %>% 
  print()

# 8-day window (for means) 
dat.3.sub <- nad.dat %>% 
  filter(count_hr=="3", date > "2020-09-05" & date < "2020-09-16", obs_count=="1") %>%
  print()


#----------------------------------------------------------


################### 
# METHOD 1: MEANS #
###################
## 1a. First mutate(): 8 day window overall mean applied to both Sept 10 & 11
## 1b. Second mutate(): 4 day window lead-up mean applied to Sept 10 and 4-day window tail-out applied to Sept 11
## 1c. Third mutate(): Mean of Sept 9 & 12 applied to Sept 10 (BBB way)
##     Fourth mutate(), new pipe: Mean of Sept 10 & 13 applied to Sept 11 (BBB way)

# first pipe for 1a,b and first part of 1c
dat.3 <- dat.3 %>% 
  mutate(sox_us_infill_fullmean = ifelse(is.na(dat.3$sox_us), mean(dat.3.sub$sox_us, na.rm=T), dat.3$sox_us)) %>% 
  mutate(sox_us_infill_partmean = ifelse(is.na(dat.3$sox_us) & dat.3$date==as.Date("2020-09-10"), 
                                       mean(dat.3.sub[dat.3.sub$date < as.Date("2020-09-10"),]$sox_us, na.rm=T),
                                  ifelse(is.na(dat.3$sox_us) & dat.3$date==as.Date("2020-09-11"), 
                                         mean(dat.3.sub[dat.3.sub$date > as.Date("2020-09-11"),]$sox_us, na.rm=T), 
                                       dat.3$sox_us))) %>%
  mutate(sox_us_infill_2mean = 
      ifelse(date==as.Date("2020-09-10"), 
             mean(dat.3[dat.3$date==as.Date("2020-09-09"),]$sox_us : dat.3[dat.3$date==as.Date("2020-09-12"),]$sox_us), 
             sox_us)) %>% 
  print()

# second pipe for second part of 1c
dat.3 <- dat.3 %>% 
  mutate(sox_us_infill_2mean = 
      ifelse(date==as.Date("2020-09-11"),
             mean((dat.3[dat.3$date==as.Date("2020-09-10"),]$sox_us_infill_2mean + dat.3[dat.3$date==as.Date("2020-09-13"),]$sox_us_infill_2mean)/2), 
             sox_us_infill_2mean)) %>%
  print()


#----------------------------------------------------------


######################
# CREATE TIME SERIES #
######################
ts.us.all3 <- ts(dat.3$sox_us)
ts.us.sub3 <- ts(dat.3.sub$sox_us)



#----------------------------------------------------------


################################
# METHOD 2: na_interpolation() #
################################

# Note: na_interpolation() uses the full time series regardless of where it's subsetted, so for this exercise I just used the full timeseries 

####
# 1a. Linear interpolation - interpolation using 'approx'
#### "linearly interpolate given data points, or a function performing the linear (or constant) interpolation."

# infill using INTERPOLATION over FULL SERIES
infill_linterp3all <- na_interpolation(ts.us.all3, option="linear")

# make the infilled time series a data frame and rename it 
infill_linterp3all.df <- as.data.frame(infill_linterp3all)

infill_linterp3all.df <- infill_linterp3all.df %>% 
  rename(sox_us_infill_linterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.3 <- cbind(dat.3, infill_linterp3all.df)


####
# 1b. Spline interpolation
#### "Perform cubic (or Hermite) spline interpolation of given data points"

# infill using INTERPOLATION over FULL SERIES
infill_spinterp3all <- na_interpolation(ts.us.all3, option="spline")

# make the infilled time series a data frame and rename it 
infill_spinterp3all.df <- as.data.frame(infill_spinterp3all)

infill_spinterp3all.df <- infill_spinterp3all.df %>% 
  rename(sox_us_infill_spinterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.3 <- cbind(dat.3, infill_spinterp3all.df)



####
# 1c. Stine interpolation
#### "Returns the values of an interpolating function that runs through a set of points in the xy-plane according to the algorithm of Stineman (1980)"

# infill using INTERPOLATION over FULL SERIES
infill_stinterp3all <- na_interpolation(ts.us.all3, option="stine")

# make the infilled time series a data frame and rename it 
infill_stinterp3all.df <- as.data.frame(infill_stinterp3all)

infill_stinterp3all.df <- infill_stinterp3all.df %>% 
  rename(sox_us_infill_stinterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.3 <- cbind(dat.3, infill_stinterp3all.df)


#----------------------------------------------------------


#########################
# METHOD 3: na_kalman() #
#########################

# Kalman smoothing over structural time series - can take into account seasonality, but we don't have that so may end up same as interpolation
# results above.

# infill using KALMAN over FULL SERIES
infill_kal3all <- na_kalman(ts.us.all3, model="StructTS")

# make the infilled time series a data frame and rename it 
infill_kal3all.df <- as.data.frame(infill_kal3all)

infill_kal3all.df <- infill_kal3all.df %>% 
  rename(sox_us_infill_kal=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.3 <- cbind(dat.3, infill_kal3all.df)


#----------------------------------------------------------


############
# Evaluate #
############

# Re-format and create dummy variable for easy plotting
dat.3 <- dat.3 %>% 
  mutate(dummy = ifelse(observer=="INFILL", "y", "n"))
  print()

dat.3.long <- dat.3 %>% 
  mutate(dummy = ifelse(observer=="INFILL", "y", "n")) %>%
  gather("method", "sox_us_infill", 14:17) %>%
  mutate(method = ifelse(dummy=="n", "data", method)) %>% 
  print()
    

# Full TS
ggplot() +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_fullmean), linetype="dashed", colour="blue", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_partmean), linetype="dashed", colour="purple", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="purple", colour="black",shape=23) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_2mean), linetype="dashed", colour="red", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_2mean), size=5.3, fill="red", colour="black",shape=22) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_linterp), linetype="dashed", colour="dark green", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_spinterp), linetype="dashed", colour="green", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_stinterp), linetype="dashed", colour="light green", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_kal), linetype="dashed", colour="orange", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us), colour="gray70", size=1.2) +
  geom_point(data=dat.3, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0300", x=as.Date("2020-09-22"), y=135, size=10) +
  scale_y_continuous(limits=c(0,140), breaks=seq(0,140, by=20)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())


# Truncated TS
ggplot() +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_fullmean), linetype="dashed", colour="blue", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_partmean), linetype="dashed", colour="purple", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="purple", colour="black",shape=23) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_2mean), linetype="dashed", colour="red", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_2mean), size=5.3, fill="red", colour="black",shape=22) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_linterp), linetype="dashed", colour="dark green", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_spinterp), linetype="dashed", colour="green", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_stinterp), linetype="dashed", colour="light green", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_kal), linetype="dashed", colour="orange", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us), colour="gray70", size=1.2) +
  geom_point(data=dat.3, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0300", x=as.Date("2020-09-22"), y=135, size=10) +
  scale_y_continuous(limits=c(0,30), breaks=seq(0,30, by=5)) +
  scale_x_date(date_breaks="1 day", date_labels="%b %d", limits=c(as.Date("2020-09-06"), as.Date("2020-09-15"))) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())



####################################################################################################################################################

#                                                                 0400 FILES

#----------------------------------------------------------


#######################
# CREATE DATA SUBSETS #
#######################
# No lead up 0s
dat.4 <- nad.dat %>%
  filter(count_hr=="4", date > as.Date("2020-08-27"), obs_count=="1") %>% 
  print()

# 8-day window (for means) 
dat.4.sub <- nad.dat %>% 
  filter(count_hr=="4", date > "2020-09-05" & date < "2020-09-16", obs_count=="1") %>%
  print()


#----------------------------------------------------------


################### 
# METHOD 1: MEANS #
###################
## 1a. First mutate(): 8 day window overall mean applied to both Sept 10 & 11
## 1b. Second mutate(): 4 day window lead-up mean applied to Sept 10 and 4-day window tail-out applied to Sept 11
## 1c. Third mutate(): Mean of Sept 9 & 12 applied to Sept 10 (BBB way)
##     Fourth mutate(), new pipe: Mean of Sept 10 & 13 applied to Sept 11 (BBB way)

# first pipe for 1a,b and first part of 1c
dat.4 <- dat.4 %>% 
  mutate(sox_us_infill_fullmean = ifelse(is.na(dat.4$sox_us), mean(dat.4.sub$sox_us, na.rm=T), dat.4$sox_us)) %>% 
  mutate(sox_us_infill_partmean = ifelse(is.na(dat.4$sox_us) & dat.4$date==as.Date("2020-09-10"), 
                                       mean(dat.4.sub[dat.4.sub$date < as.Date("2020-09-10"),]$sox_us, na.rm=T),
                                  ifelse(is.na(dat.4$sox_us) & dat.4$date==as.Date("2020-09-11"), 
                                         mean(dat.4.sub[dat.4.sub$date > as.Date("2020-09-11"),]$sox_us, na.rm=T), 
                                       dat.4$sox_us))) %>%
  mutate(sox_us_infill_2mean = 
      ifelse(date==as.Date("2020-09-10"), 
             mean(dat.4[dat.4$date==as.Date("2020-09-09"),]$sox_us : dat.4[dat.4$date==as.Date("2020-09-12"),]$sox_us), 
             sox_us)) %>% 
  print()

# second pipe for second part of 1c
dat.4 <- dat.4 %>% 
  mutate(sox_us_infill_2mean = 
      ifelse(date==as.Date("2020-09-11"),
             mean((dat.4[dat.4$date==as.Date("2020-09-10"),]$sox_us_infill_2mean + dat.4[dat.4$date==as.Date("2020-09-13"),]$sox_us_infill_2mean)/2), 
             sox_us_infill_2mean)) %>%
  print()

#----------------------------------------------------------


######################
# CREATE TIME SERIES #
######################
ts.us.all4 <- ts(dat.4$sox_us)
ts.us.sub4 <- ts(dat.4.sub$sox_us)



#----------------------------------------------------------


################################
# METHOD 2: na_interpolation() #
################################

# Note: na_interpolation() uses the full time series regardless of where it's subsetted, so for this exercise I just used the full timeseries 

####
# 1a. Linear interpolation - interpolation using 'approx'
#### "linearly interpolate given data points, or a function performing the linear (or constant) interpolation."

# infill using INTERPOLATION over FULL SERIES
infill_linterp4all <- na_interpolation(ts.us.all4, option="linear")

# make the infilled time series a data frame and rename it 
infill_linterp4all.df <- as.data.frame(infill_linterp4all)

infill_linterp4all.df <- infill_linterp4all.df %>% 
  rename(sox_us_infill_linterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.4 <- cbind(dat.4, infill_linterp4all.df)


####
# 1b. Spline interpolation
#### "Perform cubic (or Hermite) spline interpolation of given data points"

# infill using INTERPOLATION over FULL SERIES
infill_spinterp4all <- na_interpolation(ts.us.all4, option="spline")

# make the infilled time series a data frame and rename it 
infill_spinterp4all.df <- as.data.frame(infill_spinterp4all)

infill_spinterp4all.df <- infill_spinterp4all.df %>% 
  rename(sox_us_infill_spinterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.4 <- cbind(dat.4, infill_spinterp4all.df)


####
# 1c. Stine interpolation
#### "Returns the values of an interpolating function that runs through a set of points in the xy-plane according to the algorithm of Stineman (1980)"

# infill using INTERPOLATION over FULL SERIES
infill_stinterp4all <- na_interpolation(ts.us.all4, option="stine")

# make the infilled time series a data frame and rename it 
infill_stinterp4all.df <- as.data.frame(infill_stinterp4all)

infill_stinterp4all.df <- infill_stinterp4all.df %>% 
  rename(sox_us_infill_stinterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.4 <- cbind(dat.4, infill_stinterp4all.df)


#----------------------------------------------------------


#########################
# METHOD 3: na_kalman() #
#########################

# infill using KALMAN over FULL SERIES
infill_kal4all <- na_kalman(ts.us.all4, model="StructTS")

# make the infilled time series a data frame and rename it 
infill_kal4all.df <- as.data.frame(infill_kal4all)

infill_kal4all.df <- infill_kal4all.df %>% 
  rename(sox_us_infill_kal=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.4 <- cbind(dat.4, infill_kal4all.df)


#----------------------------------------------------------


############
# Evaluate #
############

# Re-format and create dummy variable for easy plotting
dat.4 <- dat.4 %>% 
  mutate(dummy = ifelse(observer=="INFILL", "y", "n"))
  print()

dat.4.long <- dat.4 %>% 
  mutate(dummy = ifelse(observer=="INFILL", "y", "n")) %>%
  gather("method", "sox_us_infill", 14:17) %>%
  mutate(method = ifelse(dummy=="n", "data", method)) %>% 
  print()
    

# Full TS
ggplot() +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_fullmean), colour="blue", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_partmean), colour="purple", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="purple", colour="black", shape=23) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_2mean), colour="red", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_2mean), size=5.3, fill="red", colour="black", shape=22) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_linterp), colour="dark green", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_spinterp), colour="green", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
    
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_stinterp), colour="light green", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_kal), colour="orange", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us), fill="gray70", size=1.2) +
  geom_point(data=dat.4, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0400", x=as.Date("2020-09-22"), y=125, size=10) +
  scale_y_continuous(limits=c(0,125), breaks=seq(0,125,by=20)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())


# Truncated TS
ggplot() +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_fullmean), colour="blue", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_partmean), colour="purple", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="purple", colour="black", shape=23) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_2mean), colour="red", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_2mean), size=5.3, fill="red", colour="black", shape=22) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_linterp), colour="dark green", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_spinterp), colour="green", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
    
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_stinterp), colour="light green", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_kal), colour="orange", linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us), fill="gray70", size=1.2) +
  geom_point(data=dat.4, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0400", x=as.Date("2020-09-22"), y=125, size=10) +
  scale_y_continuous(limits=c(0,35), breaks=seq(0,35,by=5)) +
  scale_x_date(date_breaks="1 day", date_labels="%b %d", limits=c(as.Date("2020-09-06"), as.Date("2020-09-15"))) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())



####################################################################################################################################################

#                                                                 0500 FILES

#----------------------------------------------------------


#######################
# CREATE DATA SUBSETS #
#######################
# No lead up 0s
dat.5 <- nad.dat %>%
  filter(count_hr=="5", date > as.Date("2020-08-27"), obs_count=="1") %>% 
  print()

# 8-day window (for means) 
dat.5.sub <- nad.dat %>% 
  filter(count_hr=="5", date > "2020-09-05" & date < "2020-09-16", obs_count=="1") %>%
  print()



#----------------------------------------------------------


################### 
# METHOD 1: MEANS #
###################
## 1a. First mutate(): 8 day window overall mean applied to both Sept 10 & 11
## 1b. Second mutate(): 4 day window lead-up mean applied to Sept 10 and 4-day window tail-out applied to Sept 11
## 1c. Third mutate(): Mean of Sept 9 & 12 applied to Sept 10 (BBB way)
##     Fourth mutate(), new pipe: Mean of Sept 10 & 13 applied to Sept 11 (BBB way)

# first pipe for 1a,b and first part of 1c
dat.5 <- dat.5 %>% 
  mutate(sox_us_infill_fullmean = ifelse(is.na(dat.5$sox_us), mean(dat.5.sub$sox_us, na.rm=T), dat.5$sox_us)) %>% 
  mutate(sox_us_infill_partmean = ifelse(is.na(dat.5$sox_us) & dat.5$date==as.Date("2020-09-10"), 
                                       mean(dat.5.sub[dat.5.sub$date < as.Date("2020-09-10"),]$sox_us, na.rm=T),
                                  ifelse(is.na(dat.5$sox_us) & dat.5$date==as.Date("2020-09-11"), 
                                         mean(dat.5.sub[dat.5.sub$date > as.Date("2020-09-11"),]$sox_us, na.rm=T), 
                                       dat.5$sox_us))) %>%
  mutate(sox_us_infill_2mean = 
      ifelse(date==as.Date("2020-09-10"), 
             mean(dat.5[dat.5$date==as.Date("2020-09-09"),]$sox_us : dat.5[dat.5$date==as.Date("2020-09-12"),]$sox_us), 
             sox_us)) %>% 
  print()

# second pipe for second part of 1c
dat.5 <- dat.5 %>% 
  mutate(sox_us_infill_2mean = 
      ifelse(date==as.Date("2020-09-11"),
             mean((dat.5[dat.5$date==as.Date("2020-09-10"),]$sox_us_infill_2mean + dat.5[dat.5$date==as.Date("2020-09-13"),]$sox_us_infill_2mean)/2), 
             sox_us_infill_2mean)) %>%
  print()


#----------------------------------------------------------


######################
# CREATE TIME SERIES #
######################
ts.us.all5 <- ts(dat.5$sox_us)
ts.us.sub5 <- ts(dat.5.sub$sox_us)


#----------------------------------------------------------


################################
# METHOD 2: na_interpolation() #
################################

# Note: na_interpolation() uses the full time series regardless of where it's subsetted, so for this exercise I just used the full timeseries 

####
# 1a. Linear interpolation - interpolation using 'approx'
#### "linearly interpolate given data points, or a function performing the linear (or constant) interpolation."

# infill using INTERPOLATION over FULL SERIES
infill_linterp5all <- na_interpolation(ts.us.all5, option="linear")

# make the infilled time series a data frame and rename it 
infill_linterp5all.df <- as.data.frame(infill_linterp5all)

infill_linterp5all.df <- infill_linterp5all.df %>% 
  rename(sox_us_infill_linterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.5 <- cbind(dat.5, infill_linterp5all.df)


####
# 1b. Spline interpolation
#### "Perform cubic (or Hermite) spline interpolation of given data points"

# infill using INTERPOLATION over FULL SERIES
infill_spinterp5all <- na_interpolation(ts.us.all5, option="spline")

# make the infilled time series a data frame and rename it 
infill_spinterp5all.df <- as.data.frame(infill_spinterp5all)

infill_spinterp5all.df <- infill_spinterp5all.df %>% 
  rename(sox_us_infill_spinterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.5 <- cbind(dat.5, infill_spinterp5all.df)



####
# 1c. Stine interpolation
#### "Returns the values of an interpolating function that runs through a set of points in the xy-plane according to the algorithm of Stineman (1980)"

# infill using INTERPOLATION over FULL SERIES
infill_stinterp5all <- na_interpolation(ts.us.all5, option="stine")

# make the infilled time series a data frame and rename it 
infill_stinterp5all.df <- as.data.frame(infill_stinterp5all)

infill_stinterp5all.df <- infill_stinterp5all.df %>% 
  rename(sox_us_infill_stinterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.5 <- cbind(dat.5, infill_stinterp5all.df)


#----------------------------------------------------------


#########################
# METHOD 3: na_kalman() #
#########################

# infill using KALMAN over FULL SERIES
infill_kal5all <- na_kalman(ts.us.all5, model="StructTS")

# make the infilled time series a data frame and rename it 
infill_kal5all.df <- as.data.frame(infill_kal5all)

infill_kal5all.df <- infill_kal5all.df %>% 
  rename(sox_us_infill_kal=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.5 <- cbind(dat.5, infill_kal5all.df)


#----------------------------------------------------------


############
# Evaluate #
############

# Re-format and create dummy variable for easy plotting
dat.5 <- dat.5 %>% 
  mutate(dummy = ifelse(observer=="INFILL", "y", "n"))
  print()

dat.5.long <- dat.5 %>% 
  mutate(dummy = ifelse(observer=="INFILL", "y", "n")) %>%
  gather("method", "sox_us_infill", 14:17) %>%
  mutate(method = ifelse(dummy=="n", "data", method)) %>% 
  print()
    

# Full TS
ggplot() +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_fullmean), colour="blue", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_partmean), colour="purple", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="purple", colour="black",shape=23) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_2mean), colour="red", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_2mean), size=5.3, fill="red", colour="black", shape=22) +

  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_linterp), colour="dark green", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_spinterp), colour="green", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_stinterp), colour="light green", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_kal), colour="orange", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us), size=1.2, colour="gray70") +
  geom_point(data=dat.5, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0500", x=as.Date("2020-09-22"), y=190, size=10) +
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,by=25)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())


# Amended TS
ggplot() +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_fullmean), colour="blue", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_partmean), colour="purple", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="purple", colour="black",shape=23) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_2mean), colour="red", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_2mean), size=5.3, fill="red", colour="black", shape=22) +

  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_linterp), colour="dark green", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_spinterp), colour="green", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_stinterp), colour="light green", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_kal), colour="orange", linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us), size=1.2, colour="gray70") +
  geom_point(data=dat.5, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0500", x=as.Date("2020-09-22"), y=190, size=10) +
  scale_y_continuous(limits=c(0,60), breaks=seq(0,60,by=20)) +
  scale_x_date(date_breaks="1 day", date_labels="%b %d", limits=c(as.Date("2020-09-06"), as.Date("2020-09-15"))) +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())



####################################################################################################################################################

#                                                                 0600 FILES

#----------------------------------------------------------


#######################
# CREATE DATA SUBSETS #
#######################

# No lead up 0s
dat.6 <- nad.dat %>%
  filter(count_hr=="6", date > as.Date("2020-08-27"), obs_count=="1") %>% 
  print()

# 8-day window (for means) 
dat.6.sub <- nad.dat %>% 
  filter(count_hr=="6", date > "2020-09-05" & date < "2020-09-16", obs_count=="1") %>%
  print()


#----------------------------------------------------------


################### 
# METHOD 1: MEANS #
###################
## 1a. First mutate(): 8 day window overall mean applied to both Sept 10 & 11
## 1b. Second mutate(): 4 day window lead-up mean applied to Sept 10 and 4-day window tail-out applied to Sept 11
## 1c. Third mutate(): Mean of Sept 9 & 12 applied to Sept 10 (BBB way)
##     Fourth mutate(), new pipe: Mean of Sept 10 & 13 applied to Sept 11 (BBB way)

# first pipe for 1a,b and first part of 1c
dat.6 <- dat.6 %>% 
  mutate(sox_us_infill_fullmean = ifelse(is.na(dat.6$sox_us), mean(dat.6.sub$sox_us, na.rm=T), dat.6$sox_us)) %>% 
  mutate(sox_us_infill_partmean = ifelse(is.na(dat.6$sox_us) & dat.6$date==as.Date("2020-09-10"), 
                                       mean(dat.6.sub[dat.6.sub$date < as.Date("2020-09-10"),]$sox_us, na.rm=T),
                                  ifelse(is.na(dat.6$sox_us) & dat.6$date==as.Date("2020-09-11"), 
                                         mean(dat.6.sub[dat.6.sub$date > as.Date("2020-09-11"),]$sox_us, na.rm=T), 
                                       dat.6$sox_us))) %>%
  mutate(sox_us_infill_2mean = 
      ifelse(date==as.Date("2020-09-10"), 
             mean(dat.6[dat.6$date==as.Date("2020-09-09"),]$sox_us : dat.6[dat.6$date==as.Date("2020-09-12"),]$sox_us), 
             sox_us)) %>% 
  print()

# second pipe for second part of 1c
dat.6 <- dat.6 %>% 
  mutate(sox_us_infill_2mean = 
      ifelse(date==as.Date("2020-09-11"),
             mean((dat.6[dat.6$date==as.Date("2020-09-10"),]$sox_us_infill_2mean + dat.6[dat.6$date==as.Date("2020-09-13"),]$sox_us_infill_2mean)/2), 
             sox_us_infill_2mean)) %>%
  print()


#----------------------------------------------------------


######################
# CREATE TIME SERIES #
######################
ts.us.all6 <- ts(dat.6$sox_us)
ts.us.sub6 <- ts(dat.6.sub$sox_us)


#----------------------------------------------------------


################################
# METHOD 2: na_interpolation() #
################################

# Note: na_interpolation() uses the full time series regardless of where it's subsetted, so for this exercise I just used the full timeseries 

####
# 1a. Linear interpolation - interpolation using 'approx'
#### "linearly interpolate given data points, or a function performing the linear (or constant) interpolation."

# infill using INTERPOLATION over FULL SERIES
infill_linterp6all <- na_interpolation(ts.us.all6, option="linear")

# make the infilled time series a data frame and rename it 
infill_linterp6all.df <- as.data.frame(infill_linterp6all)

infill_linterp6all.df <- infill_linterp6all.df %>% 
  rename(sox_us_infill_linterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.6 <- cbind(dat.6, infill_linterp6all.df)


####
# 1b. Spline interpolation
#### "Perform cubic (or Hermite) spline interpolation of given data points"

# infill using INTERPOLATION over FULL SERIES
infill_spinterp6all <- na_interpolation(ts.us.all6, option="spline")

# make the infilled time series a data frame and rename it 
infill_spinterp6all.df <- as.data.frame(infill_spinterp6all)

infill_spinterp6all.df <- infill_spinterp6all.df %>% 
  rename(sox_us_infill_spinterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.6 <- cbind(dat.6, infill_spinterp6all.df)



####
# 1c. Stine interpolation
#### "Returns the values of an interpolating function that runs through a set of points in the xy-plane according to the algorithm of Stineman (1980)"

# infill using INTERPOLATION over FULL SERIES
infill_stinterp6all <- na_interpolation(ts.us.all6, option="stine")

# make the infilled time series a data frame and rename it 
infill_stinterp6all.df <- as.data.frame(infill_stinterp6all)

infill_stinterp6all.df <- infill_stinterp6all.df %>% 
  rename(sox_us_infill_stinterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.6 <- cbind(dat.6, infill_stinterp6all.df)


#----------------------------------------------------------


#########################
# METHOD 3: na_kalman() #
#########################

# infill using KALMAN over FULL SERIES
infill_kal6all <- na_kalman(ts.us.all6, model="StructTS")

# make the infilled time series a data frame and rename it 
infill_kal6all.df <- as.data.frame(infill_kal6all)

infill_kal6all.df <- infill_kal6all.df %>% 
  rename(sox_us_infill_kal=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.6 <- cbind(dat.6, infill_kal6all.df)


#----------------------------------------------------------


############
# Evaluate #
############

# Re-format and create dummy variable for easy plotting
dat.6 <- dat.6 %>% 
  mutate(dummy = ifelse(observer=="INFILL", "y", "n")) %>%
  print()

dat.6.long <- dat.6 %>% 
  mutate(dummy = ifelse(observer=="INFILL", "y", "n")) %>%
  gather("method", "sox_us_infill", 14:17) %>%
  mutate(method = ifelse(dummy=="n", "data", method)) %>% 
  print()
    

# Full TS
ggplot() +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_fullmean), colour="blue", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_partmean), colour="purple", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="purple", colour="black", shape=23) +

  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_2mean), colour="red", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_2mean), size=5.3, fill="red", colour="black", shape=22) +
 
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_linterp), colour="dark green", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_spinterp), colour="green", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_stinterp), colour="light green", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_kal), colour="orange", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us), colour="gray70", size=1.2) +
  geom_point(data=dat.6, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0600", x=as.Date("2020-09-22"), y=290, size=10) +
  scale_y_continuous(limits=c(0,300), breaks=seq(0,300,by=50)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())


# Truncated TS
ggplot() +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_fullmean), colour="blue", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_partmean), colour="purple", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="purple", colour="black", shape=23) +

  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_2mean), colour="red", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_2mean), size=5.3, fill="red", colour="black", shape=22) +
 
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_linterp), colour="dark green", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_spinterp), colour="green", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_stinterp), colour="light green", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_kal), colour="orange", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us), colour="gray70", size=1.2) +
  geom_point(data=dat.6, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0600", x=as.Date("2020-09-22"), y=290, size=10) +
  scale_y_continuous(limits=c(0,50), breaks=seq(0,50,by=10)) +
  scale_x_date(date_breaks="1 day", date_labels="%b %d", limits=c(as.Date("2020-09-06"), as.Date("2020-09-15"))) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())



####################################################################################################################################################

#                                                                 0700 FILES


#----------------------------------------------------------


#######################
# CREATE DATA SUBSETS #
#######################

# No lead up 0s
dat.7 <- nad.dat %>%
  filter(count_hr=="7", date > as.Date("2020-08-27"), obs_count=="1") %>% 
  print()

# 8-day window (for means) 
dat.7.sub <- nad.dat %>% 
  filter(count_hr=="7", date > "2020-09-05" & date < "2020-09-16", obs_count=="1") %>%
  print()


#----------------------------------------------------------


################### 
# METHOD 1: MEANS #
###################
## 1a. First mutate(): 8 day window overall mean applied to both Sept 10 & 11
## 1b. Second mutate(): 4 day window lead-up mean applied to Sept 10 and 4-day window tail-out applied to Sept 11
## 1c. Third mutate(): Mean of Sept 9 & 12 applied to Sept 10 (BBB way)
##     Fourth mutate(), new pipe: Mean of Sept 10 & 13 applied to Sept 11 (BBB way)

# first pipe for 1a,b and first part of 1c
dat.7 <- dat.7 %>% 
  mutate(sox_us_infill_fullmean = ifelse(is.na(dat.7$sox_us), mean(dat.7.sub$sox_us, na.rm=T), dat.7$sox_us)) %>% 
  mutate(sox_us_infill_partmean = ifelse(is.na(dat.7$sox_us) & dat.7$date==as.Date("2020-09-10"), 
                                       mean(dat.7.sub[dat.7.sub$date < as.Date("2020-09-10"),]$sox_us, na.rm=T),
                                  ifelse(is.na(dat.7$sox_us) & dat.7$date==as.Date("2020-09-11"), 
                                         mean(dat.7.sub[dat.7.sub$date > as.Date("2020-09-11"),]$sox_us, na.rm=T), 
                                       dat.7$sox_us))) %>%
  mutate(sox_us_infill_2mean = 
      ifelse(date==as.Date("2020-09-10"), 
             mean(dat.7[dat.7$date==as.Date("2020-09-09"),]$sox_us : dat.7[dat.7$date==as.Date("2020-09-12"),]$sox_us), 
             sox_us)) %>% 
  print()

# second pipe for second part of 1c
dat.7 <- dat.7 %>% 
  mutate(sox_us_infill_2mean = 
      ifelse(date==as.Date("2020-09-11"),
             mean((dat.7[dat.7$date==as.Date("2020-09-10"),]$sox_us_infill_2mean + dat.7[dat.7$date==as.Date("2020-09-13"),]$sox_us_infill_2mean)/2), 
             sox_us_infill_2mean)) %>%
  print()


#----------------------------------------------------------


######################
# CREATE TIME SERIES #
######################
ts.us.all7 <- ts(dat.7$sox_us)
ts.us.sub7 <- ts(dat.7.sub$sox_us)


#----------------------------------------------------------


################################
# METHOD 2: na_interpolation() #
################################

# Note: na_interpolation() uses the full time series regardless of where it's subsetted, so for this exercise I just used the full timeseries 

####
# 1a. Linear interpolation - interpolation using 'approx'
#### "linearly interpolate given data points, or a function performing the linear (or constant) interpolation."

# infill using INTERPOLATION over FULL SERIES
infill_linterp7all <- na_interpolation(ts.us.all7, option="linear")

# make the infilled time series a data frame and rename it 
infill_linterp7all.df <- as.data.frame(infill_linterp7all)

infill_linterp7all.df <- infill_linterp7all.df %>% 
  rename(sox_us_infill_linterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.7 <- cbind(dat.7, infill_linterp7all.df)


####
# 1b. Spline interpolation
#### "Perform cubic (or Hermite) spline interpolation of given data points"

# infill using INTERPOLATION over FULL SERIES
infill_spinterp7all <- na_interpolation(ts.us.all7, option="spline")

# make the infilled time series a data frame and rename it 
infill_spinterp7all.df <- as.data.frame(infill_spinterp7all)

infill_spinterp7all.df <- infill_spinterp7all.df %>% 
  rename(sox_us_infill_spinterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.7 <- cbind(dat.7, infill_spinterp7all.df)



####
# 1c. Stine interpolation
#### "Returns the values of an interpolating function that runs through a set of points in the xy-plane according to the algorithm of Stineman (1980)"

# infill using INTERPOLATION over FULL SERIES
infill_stinterp7all <- na_interpolation(ts.us.all7, option="stine")

# make the infilled time series a data frame and rename it 
infill_stinterp7all.df <- as.data.frame(infill_stinterp7all)

infill_stinterp7all.df <- infill_stinterp7all.df %>% 
  rename(sox_us_infill_stinterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.7 <- cbind(dat.7, infill_stinterp7all.df)


#----------------------------------------------------------


#########################
# METHOD 3: na_kalman() #
#########################

# infill using KALMAN over FULL SERIES
infill_kal7all <- na_kalman(ts.us.all7, model="StructTS")

# make the infilled time series a data frame and rename it 
infill_kal7all.df <- as.data.frame(infill_kal7all)

infill_kal7all.df <- infill_kal7all.df %>% 
  rename(sox_us_infill_kal=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.7 <- cbind(dat.7, infill_kal7all.df)


#----------------------------------------------------------


############
# Evaluate #
############

# Re-format and create dummy variable for easy plotting
dat.7 <- dat.7 %>% 
  mutate(dummy = ifelse(observer=="INFILL", "y", "n")) %>%
  print()

dat.7.long <- dat.7 %>% 
  mutate(dummy = ifelse(observer=="INFILL", "y", "n")) %>%
  gather("method", "sox_us_infill", 14:17) %>%
  mutate(method = ifelse(dummy=="n", "data", method)) %>% 
  print()
    

# Full TS
ggplot() +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_fullmean), colour="blue", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_partmean), colour="purple", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="purple", colour="black",shape=23) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_2mean), colour="red", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_2mean), size=5.3, fill="red", colour="black", shape=22) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_linterp), colour="dark green", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_spinterp), colour="green", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_stinterp), colour="light green", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_kal), colour="orange", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us), colour="gray70", size=1.2) +
  geom_point(data=dat.7, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0700", x=as.Date("2020-09-22"), y=195, size=10) +
  scale_y_continuous(limits=c(-10,200), breaks=seq(-10,200,by=30)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())


# Truncated TS
ggplot() +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_fullmean), colour="blue", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_partmean), colour="purple", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="purple", colour="black",shape=23) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_2mean), colour="red", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_2mean), size=5.3, fill="red", colour="black", shape=22) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_linterp), colour="dark green", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_spinterp), colour="green", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_stinterp), colour="light green", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_kal), colour="orange", linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us), colour="gray70", size=1.2) +
  geom_point(data=dat.7, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0700", x=as.Date("2020-09-22"), y=195, size=10) +
  scale_y_continuous(limits=c(-10,30,by=10)) +
  scale_x_date(date_breaks=(by="1 day"), date_labels="%b %d", limits=c(as.Date("2020-09-06"), as.Date("2020-09-15"))) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"))



####################################################################################################################################################

#                                                                 0800 FILES

#----------------------------------------------------------


#######################
# CREATE DATA SUBSETS #
#######################

# No lead up 0s
dat.8 <- nad.dat %>%
  filter(count_hr=="8", date > as.Date("2020-08-27"), obs_count=="1") %>% 
  print()

# 8-day window (for means) 
dat.8.sub <- nad.dat %>% 
  filter(count_hr=="8", date > "2020-09-05" & date < "2020-09-16", obs_count=="1") %>%
  print()


#----------------------------------------------------------


################### 
# METHOD 1: MEANS #
###################
## 1a. First mutate(): 8 day window overall mean applied to both Sept 10 & 11
## 1b. Second mutate(): 4 day window lead-up mean applied to Sept 10 and 4-day window tail-out applied to Sept 11
## 1c. Third mutate(): Mean of Sept 9 & 12 applied to Sept 10 (BBB way)
##     Fourth mutate(), new pipe: Mean of Sept 10 & 13 applied to Sept 11 (BBB way)

# first pipe for 1a,b and first part of 1c
dat.8 <- dat.8 %>% 
  mutate(sox_us_infill_fullmean = ifelse(is.na(dat.8$sox_us), mean(dat.8.sub$sox_us, na.rm=T), dat.8$sox_us)) %>% 
  mutate(sox_us_infill_partmean = ifelse(is.na(dat.8$sox_us) & dat.8$date==as.Date("2020-09-10"), 
                                       mean(dat.8.sub[dat.8.sub$date < as.Date("2020-09-10"),]$sox_us, na.rm=T),
                                  ifelse(is.na(dat.8$sox_us) & dat.8$date==as.Date("2020-09-11"), 
                                         mean(dat.8.sub[dat.8.sub$date > as.Date("2020-09-11"),]$sox_us, na.rm=T), 
                                       dat.8$sox_us))) %>%
  mutate(sox_us_infill_2mean = 
      ifelse(date==as.Date("2020-09-10"), 
             mean(dat.8[dat.8$date==as.Date("2020-09-09"),]$sox_us : dat.8[dat.8$date==as.Date("2020-09-12"),]$sox_us), 
             sox_us)) %>% 
  print()

# second pipe for second part of 1c
dat.8 <- dat.8 %>% 
  mutate(sox_us_infill_2mean = 
      ifelse(date==as.Date("2020-09-11"),
             mean((dat.8[dat.8$date==as.Date("2020-09-10"),]$sox_us_infill_2mean + dat.8[dat.8$date==as.Date("2020-09-13"),]$sox_us_infill_2mean)/2), 
             sox_us_infill_2mean)) %>%
  print()


#----------------------------------------------------------


######################
# CREATE TIME SERIES #
######################
ts.us.all8 <- ts(dat.8$sox_us)
ts.us.sub8 <- ts(dat.8.sub$sox_us)


#----------------------------------------------------------


################################
# METHOD 2: na_interpolation() #
################################

# Note: na_interpolation() uses the full time series regardless of where it's subsetted, so for this exercise I just used the full timeseries 

####
# 1a. Linear interpolation - interpolation using 'approx'
#### "linearly interpolate given data points, or a function performing the linear (or constant) interpolation."

# infill using INTERPOLATION over FULL SERIES
infill_linterp8all <- na_interpolation(ts.us.all8, option="linear")

# make the infilled time series a data frame and rename it 
infill_linterp8all.df <- as.data.frame(infill_linterp8all)

infill_linterp8all.df <- infill_linterp8all.df %>% 
  rename(sox_us_infill_linterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.8 <- cbind(dat.8, infill_linterp8all.df)


####
# 1b. Spline interpolation
#### "Perform cubic (or Hermite) spline interpolation of given data points"

# infill using INTERPOLATION over FULL SERIES
infill_spinterp8all <- na_interpolation(ts.us.all8, option="spline")

# make the infilled time series a data frame and rename it 
infill_spinterp8all.df <- as.data.frame(infill_spinterp8all)

infill_spinterp8all.df <- infill_spinterp8all.df %>% 
  rename(sox_us_infill_spinterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.8 <- cbind(dat.8, infill_spinterp8all.df)



####
# 1c. Stine interpolation
#### "Returns the values of an interpolating function that runs through a set of points in the xy-plane according to the algorithm of Stineman (1980)"

# infill using INTERPOLATION over FULL SERIES
infill_stinterp8all <- na_interpolation(ts.us.all8, option="stine")

# make the infilled time series a data frame and rename it 
infill_stinterp8all.df <- as.data.frame(infill_stinterp8all)

infill_stinterp8all.df <- infill_stinterp8all.df %>% 
  rename(sox_us_infill_stinterp=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.8 <- cbind(dat.8, infill_stinterp8all.df)


#----------------------------------------------------------


#########################
# METHOD 3: na_kalman() #
#########################

# infill using KALMAN over FULL SERIES
infill_kal8all <- na_kalman(ts.us.all8, model="StructTS")

# make the infilled time series a data frame and rename it 
infill_kal8all.df <- as.data.frame(infill_kal8all)

infill_kal8all.df <- infill_kal8all.df %>% 
  rename(sox_us_infill_kal=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.8 <- cbind(dat.8, infill_kal8all.df)


#----------------------------------------------------------


############
# Evaluate #
############

# Re-format and create dummy variable for easy plotting
dat.8 <- dat.8 %>% 
  mutate(dummy = ifelse(observer=="INFILL", "y", "n")) %>%
  print()

dat.8.long <- dat.8 %>% 
  mutate(dummy = ifelse(observer=="INFILL", "y", "n")) %>%
  gather("method", "sox_us_infill", 14:17) %>%
  mutate(method = ifelse(dummy=="n", "data", method)) %>% 
  print()
    

# full TS
ggplot() +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_fullmean), colour="blue", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_partmean), colour="purple", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="purple", colour="black", shape=23) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_2mean), colour="red", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_2mean), size=5.3, fill="red", colour="black", shape=22) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_linterp), colour="dark green", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_spinterp), colour="green", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_stinterp), colour="light green", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_kal), colour="orange", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us), colour="gray70", size=1.2) +
  geom_point(data=dat.8, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0800", x=as.Date("2020-09-22"), y=590, size=10) +
  scale_y_continuous(limits=c(0,600), breaks=seq(0,600,by=75)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())


# truncated TS
ggplot() +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_fullmean), colour="blue", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_partmean), colour="purple", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="purple", colour="black", shape=23) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_2mean), colour="red", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_2mean), size=5.3, fill="red", colour="black", shape=22) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_linterp), colour="dark green", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_spinterp), colour="green", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_stinterp), colour="light green", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_kal), colour="orange", linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us), colour="gray70", size=1.2) +
  geom_point(data=dat.8, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0800", x=as.Date("2020-09-22"), y=590, size=10) +
  scale_y_continuous(limits=c(0,10), breaks=seq(0,10,by=1)) +
  scale_x_date(date_breaks="1 day", date_labels="%b %d", limits=c(as.Date("2020-09-06"), as.Date("2020-09-15"))) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())










####################################################################################################################################################

####################################################################################################################################################

####################################################################################################################################################





#                                                                TEST REMOVAL

# Try removing known counts within each hour subset and see how well each infilling method performs 
# 20% of the points with data is ~6 therefore remove 6 data points to test

####################################################################################################################################################

#                                                                 TEST 0300 

#----------------------------------------------------------


#######################
# CREATE DATA SUBSETS #
#######################
# No lead up 0s
dat.3 <- nad.dat %>%
  filter(count_hr=="3", date > as.Date("2020-08-27"), obs_count=="1") %>% 
  print()

# Random removal of 6 values and replace with NA
dat.3$sox_us_check <- dat.3$sox_us
dat.3$sox_us_check[c(4,8,12,19,22,26)] <- NA


#----------------------------------------------------------


#######################
# METHOD 1: AVERAGING #
#######################
## 1a. First mutate(): calculates the average for the missing day based on count on either side 
## 1b. Second mutate(): calculates average for Sept10 based on Sept9&12
##     Third mutate(), new pipe: Calculates average for Sept11 based on Sept10(infill)&Sept13
dat.3 <- dat.3 %>% 
  mutate(sox_us_infill_avg_test = 
      ifelse(date==as.Date("2020-08-31"), 
             mean(dat.3[dat.3$date==as.Date("2020-08-30"),]$sox_us : dat.3[dat.3$date==as.Date("2020-09-01"),]$sox_us),
        ifelse(date==as.Date("2020-09-04"),
               mean(dat.3[dat.3$date==as.Date("2020-09-03"),]$sox_us : dat.3[dat.3$date==as.Date("2020-09-05"),]$sox_us),
          ifelse(date==as.Date("2020-09-08"),
                 mean(dat.3[dat.3$date==as.Date("2020-09-07"),]$sox_us : dat.3[dat.3$date==as.Date("2020-09-09"),]$sox_us),
            ifelse(date==as.Date("2020-09-15"),
                   mean(dat.3[dat.3$date==as.Date("2020-09-14"),]$sox_us : dat.3[dat.3$date==as.Date("2020-09-16"),]$sox_us),
              ifelse(date==as.Date("2020-09-18"),
                     mean(dat.3[dat.3$date==as.Date("2020-09-17"),]$sox_us : dat.3[dat.3$date==as.Date("2020-09-19"),]$sox_us),
                ifelse(date==as.Date("2020-09-22"),
                       mean(dat.3[dat.3$date==as.Date("2020-09-21"),]$sox_us : dat.3[dat.3$date==as.Date("2020-09-23"),]$sox_us),
                  sox_us))))))) %>% 
  mutate(sox_us_infill_avg_test = 
    ifelse(date==as.Date("2020-09-10"), 
           mean(dat.3[dat.3$date==as.Date("2020-09-09"),]$sox_us : dat.3[dat.3$date==as.Date("2020-09-12"),]$sox_us), 
           sox_us_infill_avg_test)) %>%
  print()


# second pipe for second part of 1b
dat.3 <- dat.3 %>% 
  mutate(sox_us_infill_avg_test = 
      ifelse(date==as.Date("2020-09-11"),
             mean((dat.3[dat.3$date==as.Date("2020-09-10"),]$sox_us_infill_avg_test + dat.3[dat.3$date==as.Date("2020-09-13"),]$sox_us)/2), 
             sox_us_infill_avg_test)) %>%
  print()



#for(i in 1:length(dat.3t$sox_us_check[i])){
#  ifelse(is.na(dat.3t$sox_us_check[i]), 
#  mean(dat.3t[dat.3t$ydate[i-1], ]$sox_us_check[i-1] : dat.3t[dat.3$ydate[i+1], ]$sox_us_check[i+1]) , 
#    dat.3t$sox_us_check[i])
#}


#----------------------------------------------------------


######################
# CREATE TIME SERIES #
######################
ts.us.all3test <- ts(dat.3$sox_us_check)


#----------------------------------------------------------



################################
# METHOD 2: na_interpolation() #
################################

# Note: na_interpolation() uses the full time series regardless of where it's subsetted, so for this exercise I just used the full timeseries 

####
# 2a. Linear interpolation - interpolation using 'approx'
#### "linearly interpolate given data points, or a function performing the linear (or constant) interpolation."

# infill using INTERPOLATION over FULL SERIES
infill_linterp3alltest <- na_interpolation(ts.us.all3test, option="linear")

# make the infilled time series a data frame and rename it 
infill_linterp3alltest.df <- as.data.frame(infill_linterp3alltest)

infill_linterp3alltest.df <- infill_linterp3alltest.df %>% 
  rename(sox_us_infill_linterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.3 <- cbind(dat.3, infill_linterp3alltest.df)


####
# 2b. Spline interpolation
#### "Perform cubic (or Hermite) spline interpolation of given data points"

# infill using INTERPOLATION over FULL SERIES
infill_spinterp3alltest <- na_interpolation(ts.us.all3test, option="spline")

# make the infilled time series a data frame and rename it 
infill_spinterp3alltest.df <- as.data.frame(infill_spinterp3alltest)

infill_spinterp3alltest.df <- infill_spinterp3alltest.df %>% 
  rename(sox_us_infill_spinterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.3 <- cbind(dat.3, infill_spinterp3alltest.df)



####
# 2c. Stine interpolation
#### "Returns the values of an interpolating function that runs through a set of points in the xy-plane according to the algorithm of Stineman (1980)"

# infill using INTERPOLATION over FULL SERIES
infill_stinterp3alltest <- na_interpolation(ts.us.all3test, option="stine")

# make the infilled time series a data frame and rename it 
infill_stinterp3alltest.df <- as.data.frame(infill_stinterp3alltest)

infill_stinterp3alltest.df <- infill_stinterp3alltest.df %>% 
  rename(sox_us_infill_stinterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.3 <- cbind(dat.3, infill_stinterp3alltest.df)


#----------------------------------------------------------


#########################
# METHOD 3: na_kalman() #
#########################

# Kalman smoothing over structural time series - can take into account seasonality, but we don't have that so may end up same as interpolation
# results above.

# infill using KALMAN over FULL SERIES
infill_kal3alltest <- na_kalman(ts.us.all3test, model="StructTS")

# make the infilled time series a data frame and rename it 
infill_kal3alltest.df <- as.data.frame(infill_kal3alltest)

infill_kal3alltest.df <- infill_kal3alltest.df %>% 
  rename(sox_us_infill_kal_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.3 <- cbind(dat.3, infill_kal3alltest.df)


#----------------------------------------------------------


############
# Evaluate #
############

# Assess
ggplot() +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_avg_test), colour="red", linetype="dashed", colour="red", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_avg_test), size=5.4, fill="red", colour="black", shape=22) +

  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_linterp_test), colour="dark green", linetype="dashed", colour="dark green", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_linterp_test), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_spinterp_test), colour="green", linetype="dashed", colour="green", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_spinterp_test), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_stinterp_test), colour="light green", linetype="dashed", colour="light green", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_stinterp_test), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_kal_test), colour="orange", linetype="dashed", colour="orange", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_kal_test), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us), colour="gray70", size=1.2) +
  geom_point(data=dat.3, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0300 (test)", x=as.Date("2020-09-20"), y=145, size=10) +
  scale_y_continuous(limits=c(0,150), breaks=seq(0,150,by=25)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())



################################################################################################################################################


#                                                                  TEST 0400 

#----------------------------------------------------------


#######################
# CREATE DATA SUBSETS #
#######################
# No lead up 0s
dat.4 <- nad.dat %>%
  filter(count_hr=="4", date > as.Date("2020-08-27"), obs_count=="1") %>% 
  print()

# Random removal of 6 values and replace with NA
dat.4$sox_us_check <- dat.4$sox_us
dat.4$sox_us_check[c(4,8,12,19,22,26)] <- NA


#----------------------------------------------------------


#######################
# METHOD 1: AVERAGING #
#######################
## 1a. First mutate(): calculates the average for the missing day based on count on either side 
## 1b. Second mutate(): calculates average for Sept10 based on Sept9&12
##     Third mutate(), new pipe: Calculates average for Sept11 based on Sept10(infill)&Sept13
dat.4 <- dat.4 %>% 
  mutate(sox_us_infill_avg_test = 
      ifelse(date==as.Date("2020-08-31"), 
             mean(dat.4[dat.4$date==as.Date("2020-08-30"),]$sox_us : dat.4[dat.4$date==as.Date("2020-09-01"),]$sox_us),
        ifelse(date==as.Date("2020-09-04"),
               mean(dat.4[dat.4$date==as.Date("2020-09-03"),]$sox_us : dat.4[dat.4$date==as.Date("2020-09-05"),]$sox_us),
          ifelse(date==as.Date("2020-09-08"),
                 mean(dat.4[dat.4$date==as.Date("2020-09-07"),]$sox_us : dat.4[dat.4$date==as.Date("2020-09-09"),]$sox_us),
            ifelse(date==as.Date("2020-09-15"),
                   mean(dat.4[dat.4$date==as.Date("2020-09-14"),]$sox_us : dat.4[dat.4$date==as.Date("2020-09-16"),]$sox_us),
              ifelse(date==as.Date("2020-09-18"),
                     mean(dat.4[dat.4$date==as.Date("2020-09-17"),]$sox_us : dat.4[dat.4$date==as.Date("2020-09-19"),]$sox_us),
                ifelse(date==as.Date("2020-09-22"),
                       mean(dat.4[dat.4$date==as.Date("2020-09-21"),]$sox_us : dat.4[dat.4$date==as.Date("2020-09-23"),]$sox_us),
                  sox_us))))))) %>% 
  mutate(sox_us_infill_avg_test = 
    ifelse(date==as.Date("2020-09-10"), 
           mean(dat.4[dat.4$date==as.Date("2020-09-09"),]$sox_us : dat.4[dat.4$date==as.Date("2020-09-12"),]$sox_us), 
           sox_us_infill_avg_test)) %>%
  print()


# second pipe for second part of 1b
dat.4 <- dat.4 %>% 
  mutate(sox_us_infill_avg_test = 
      ifelse(date==as.Date("2020-09-11"),
             mean((dat.4[dat.4$date==as.Date("2020-09-10"),]$sox_us_infill_avg_test + dat.4[dat.4$date==as.Date("2020-09-13"),]$sox_us)/2), 
             sox_us_infill_avg_test)) %>%
  print()


#----------------------------------------------------------


######################
# CREATE TIME SERIES #
######################
ts.us.all4test <- ts(dat.4$sox_us_check)


#----------------------------------------------------------


################################
# METHOD 2: na_interpolation() #
################################

# Note: na_interpolation() uses the full time series regardless of where it's subsetted, so for this exercise I just used the full timeseries 

####
# 2a. Linear interpolation - interpolation using 'approx'
#### "linearly interpolate given data points, or a function performing the linear (or constant) interpolation."

# infill using INTERPOLATION over FULL SERIES
infill_linterp4alltest <- na_interpolation(ts.us.all4test, option="linear")

# make the infilled time series a data frame and rename it 
infill_linterp4alltest.df <- as.data.frame(infill_linterp4alltest)

infill_linterp4alltest.df <- infill_linterp4alltest.df %>% 
  rename(sox_us_infill_linterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.4 <- cbind(dat.4, infill_linterp4alltest.df)


####
# 2b. Spline interpolation
#### "Perform cubic (or Hermite) spline interpolation of given data points"

# infill using INTERPOLATION over FULL SERIES
infill_spinterp4alltest <- na_interpolation(ts.us.all4test, option="spline")

# make the infilled time series a data frame and rename it 
infill_spinterp4alltest.df <- as.data.frame(infill_spinterp4alltest)

infill_spinterp4alltest.df <- infill_spinterp4alltest.df %>% 
  rename(sox_us_infill_spinterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.4 <- cbind(dat.4, infill_spinterp4alltest.df)



####
# 2c. Stine interpolation
#### "Returns the values of an interpolating function that runs through a set of points in the xy-plane according to the algorithm of Stineman (1980)"

# infill using INTERPOLATION over FULL SERIES
infill_stinterp4alltest <- na_interpolation(ts.us.all4test, option="stine")

# make the infilled time series a data frame and rename it 
infill_stinterp4alltest.df <- as.data.frame(infill_stinterp4alltest)

infill_stinterp4alltest.df <- infill_stinterp4alltest.df %>% 
  rename(sox_us_infill_stinterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.4 <- cbind(dat.4, infill_stinterp4alltest.df)


#----------------------------------------------------------


#########################
# METHOD 4: na_kalman() #
#########################

# Kalman smoothing over structural time series - can take into account seasonality, but we don't have that so may end up same as interpolation
# results above.

# infill using KALMAN over FULL SERIES
infill_kal4alltest <- na_kalman(ts.us.all4test, model="StructTS")

# make the infilled time series a data frame and rename it 
infill_kal4alltest.df <- as.data.frame(infill_kal4alltest)

infill_kal4alltest.df <- infill_kal4alltest.df %>% 
  rename(sox_us_infill_kal_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.4 <- cbind(dat.4, infill_kal4alltest.df)


#----------------------------------------------------------


############
# Evaluate #
############

# Assess
ggplot() +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_avg_test), colour="red", linetype="dashed", colour="red", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_avg_test), size=5.4, fill="red", colour="black", shape=22) +

  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_linterp_test), colour="dark green", linetype="dashed", colour="dark green", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_linterp_test), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_spinterp_test), colour="green", linetype="dashed", colour="green", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_spinterp_test), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_stinterp_test), colour="light green", linetype="dashed", colour="light green", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_stinterp_test), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_kal_test), colour="orange", linetype="dashed", colour="orange", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_kal_test), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us), colour="gray70", size=1.2) +
  geom_point(data=dat.4, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0400 (test)", x=as.Date("2020-09-20"), y=120, size=10) +
  scale_y_continuous(limits=c(0,125), breaks=seq(0,120,by=20)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())



##################################################################################################################################################


#                                                                  TEST 0500

#----------------------------------------------------------


#######################
# CREATE DATA SUBSETS #
#######################
# No lead up 0s
dat.5 <- nad.dat %>%
  filter(count_hr=="5", date > as.Date("2020-08-27"), obs_count=="1") %>% 
  print()

# Random removal of 6 values and replace with NA
dat.5$sox_us_check <- dat.5$sox_us
dat.5$sox_us_check[c(4,8,12,19,22,26)] <- NA


#----------------------------------------------------------


#######################
# METHOD 1: AVERAGING #
#######################
## 1a. First mutate(): calculates the average for the missing day based on count on either side 
## 1b. Second mutate(): calculates average for Sept10 based on Sept9&12
##     Third mutate(), new pipe: Calculates average for Sept11 based on Sept10(infill)&Sept13
dat.5 <- dat.5 %>% 
  mutate(sox_us_infill_avg_test = 
      ifelse(date==as.Date("2020-08-31"), 
             mean(dat.5[dat.5$date==as.Date("2020-08-30"),]$sox_us : dat.5[dat.5$date==as.Date("2020-09-01"),]$sox_us),
        ifelse(date==as.Date("2020-09-04"),
               mean(dat.5[dat.5$date==as.Date("2020-09-03"),]$sox_us : dat.5[dat.5$date==as.Date("2020-09-05"),]$sox_us),
          ifelse(date==as.Date("2020-09-08"),
                 mean(dat.5[dat.5$date==as.Date("2020-09-07"),]$sox_us : dat.5[dat.5$date==as.Date("2020-09-09"),]$sox_us),
            ifelse(date==as.Date("2020-09-15"),
                   mean(dat.5[dat.5$date==as.Date("2020-09-14"),]$sox_us : dat.5[dat.5$date==as.Date("2020-09-16"),]$sox_us),
              ifelse(date==as.Date("2020-09-18"),
                     mean(dat.5[dat.5$date==as.Date("2020-09-17"),]$sox_us : dat.5[dat.5$date==as.Date("2020-09-19"),]$sox_us),
                ifelse(date==as.Date("2020-09-22"),
                       mean(dat.5[dat.5$date==as.Date("2020-09-21"),]$sox_us : dat.5[dat.5$date==as.Date("2020-09-23"),]$sox_us),
                  sox_us))))))) %>% 
  mutate(sox_us_infill_avg_test = 
    ifelse(date==as.Date("2020-09-10"), 
           mean(dat.5[dat.5$date==as.Date("2020-09-09"),]$sox_us : dat.5[dat.5$date==as.Date("2020-09-12"),]$sox_us), 
           sox_us_infill_avg_test)) %>%
  print()


# second pipe for second part of 1b
dat.5 <- dat.5 %>% 
  mutate(sox_us_infill_avg_test = 
      ifelse(date==as.Date("2020-09-11"),
             mean((dat.5[dat.5$date==as.Date("2020-09-10"),]$sox_us_infill_avg_test + dat.5[dat.5$date==as.Date("2020-09-13"),]$sox_us)/2), 
             sox_us_infill_avg_test)) %>%
  print()


#----------------------------------------------------------


######################
# CREATE TIME SERIES #
######################
ts.us.all5test <- ts(dat.5$sox_us_check)


#----------------------------------------------------------


################################
# METHOD 2: na_interpolation() #
################################

# Note: na_interpolation() uses the full time series regardless of where it's subsetted, so for this exercise I just used the full timeseries 

####
# 1a. Linear interpolation - interpolation using 'approx'
#### "linearly interpolate given data points, or a function performing the linear (or constant) interpolation."

# infill using INTERPOLATION over FULL SERIES
infill_linterp5alltest <- na_interpolation(ts.us.all5test, option="linear")

# make the infilled time series a data frame and rename it 
infill_linterp5alltest.df <- as.data.frame(infill_linterp5alltest)

infill_linterp5alltest.df <- infill_linterp5alltest.df %>% 
  rename(sox_us_infill_linterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.5 <- cbind(dat.5, infill_linterp5alltest.df)


####
# 1b. Spline interpolation
#### "Perform cubic (or Hermite) spline interpolation of given data points"

# infill using INTERPOLATION over FULL SERIES
infill_spinterp5alltest <- na_interpolation(ts.us.all5test, option="spline")

# make the infilled time series a data frame and rename it 
infill_spinterp5alltest.df <- as.data.frame(infill_spinterp5alltest)

infill_spinterp5alltest.df <- infill_spinterp5alltest.df %>% 
  rename(sox_us_infill_spinterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.5 <- cbind(dat.5, infill_spinterp5alltest.df)



####
# 1c. Stine interpolation
#### "Returns the values of an interpolating function that runs through a set of points in the xy-plane according to the algorithm of Stineman (1980)"

# infill using INTERPOLATION over FULL SERIES
infill_stinterp5alltest <- na_interpolation(ts.us.all5test, option="stine")

# make the infilled time series a data frame and rename it 
infill_stinterp5alltest.df <- as.data.frame(infill_stinterp5alltest)

infill_stinterp5alltest.df <- infill_stinterp5alltest.df %>% 
  rename(sox_us_infill_stinterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.5 <- cbind(dat.5, infill_stinterp5alltest.df)



#----------------------------------------------------------


#########################
# METHOD 5: na_kalman() #
#########################

# Kalman smoothing over structural time series - can take into account seasonality, but we don't have that so may end up same as interpolation
# results above.

# infill using KALMAN over FULL SERIES
infill_kal5alltest <- na_kalman(ts.us.all5test, model="StructTS")

# make the infilled time series a data frame and rename it 
infill_kal5alltest.df <- as.data.frame(infill_kal5alltest)

infill_kal5alltest.df <- infill_kal5alltest.df %>% 
  rename(sox_us_infill_kal_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.5 <- cbind(dat.5, infill_kal5alltest.df)



#----------------------------------------------------------


############
# Evaluate #
############

# Assess
ggplot() +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_avg_test), colour="red", linetype="dashed", colour="red", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_avg_test), size=5.4, fill="red", colour="black", shape=22) +

  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_linterp_test), colour="dark green", linetype="dashed", colour="dark green", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_linterp_test), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_spinterp_test), colour="green", linetype="dashed", colour="green", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_spinterp_test), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_stinterp_test), colour="light green", linetype="dashed", colour="light green", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_stinterp_test), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_kal_test), colour="orange", linetype="dashed", colour="orange", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_kal_test), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us), colour="gray70", size=1.2) +
  geom_point(data=dat.5, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0500 (test)", x=as.Date("2020-09-20"), y=195, size=10) +
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,by=25)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())



####################################################################################################################################################


#                                                                  TEST 0600

#----------------------------------------------------------


#######################
# CREATE DATA SUBSETS #
#######################
# No lead up 0s
dat.6 <- nad.dat %>%
  filter(count_hr=="6", date > as.Date("2020-08-27"), obs_count=="1") %>% 
  print()

# Random removal of 6 values and replace with NA
dat.6$sox_us_check <- dat.6$sox_us
dat.6$sox_us_check[c(4,8,12,19,22,26)] <- NA


#----------------------------------------------------------


#######################
# METHOD 1: AVERAGING #
#######################
## 1a. First mutate(): calculates the average for the missing day based on count on either side 
## 1b. Second mutate(): calculates average for Sept10 based on Sept9&12
##     Third mutate(), new pipe: Calculates average for Sept11 based on Sept10(infill)&Sept13
dat.6 <- dat.6 %>% 
  mutate(sox_us_infill_avg_test = 
      ifelse(date==as.Date("2020-08-31"), 
             mean(dat.6[dat.6$date==as.Date("2020-08-30"),]$sox_us : dat.6[dat.6$date==as.Date("2020-09-01"),]$sox_us),
        ifelse(date==as.Date("2020-09-04"),
               mean(dat.6[dat.6$date==as.Date("2020-09-03"),]$sox_us : dat.6[dat.6$date==as.Date("2020-09-05"),]$sox_us),
          ifelse(date==as.Date("2020-09-08"),
                 mean(dat.6[dat.6$date==as.Date("2020-09-07"),]$sox_us : dat.6[dat.6$date==as.Date("2020-09-09"),]$sox_us),
            ifelse(date==as.Date("2020-09-15"),
                   mean(dat.6[dat.6$date==as.Date("2020-09-14"),]$sox_us : dat.6[dat.6$date==as.Date("2020-09-16"),]$sox_us),
              ifelse(date==as.Date("2020-09-18"),
                     mean(dat.6[dat.6$date==as.Date("2020-09-17"),]$sox_us : dat.6[dat.6$date==as.Date("2020-09-19"),]$sox_us),
                ifelse(date==as.Date("2020-09-22"),
                       mean(dat.6[dat.6$date==as.Date("2020-09-21"),]$sox_us : dat.6[dat.6$date==as.Date("2020-09-23"),]$sox_us),
                  sox_us))))))) %>% 
  mutate(sox_us_infill_avg_test = 
    ifelse(date==as.Date("2020-09-10"), 
           mean(dat.6[dat.6$date==as.Date("2020-09-09"),]$sox_us : dat.6[dat.6$date==as.Date("2020-09-12"),]$sox_us), 
           sox_us_infill_avg_test)) %>%
  print()


# second pipe for second part of 1b
dat.6 <- dat.6 %>% 
  mutate(sox_us_infill_avg_test = 
      ifelse(date==as.Date("2020-09-11"),
             mean((dat.6[dat.6$date==as.Date("2020-09-10"),]$sox_us_infill_avg_test + dat.6[dat.6$date==as.Date("2020-09-13"),]$sox_us)/2), 
             sox_us_infill_avg_test)) %>%
  print()


#----------------------------------------------------------


######################
# CREATE TIME SERIES #
######################
ts.us.all6test <- ts(dat.6$sox_us_check)


#----------------------------------------------------------


################################
# METHOD 2: na_interpolation() #
################################

# Note: na_interpolation() uses the full time series regardless of where it's subsetted, so for this exercise I just used the full timeseries 

####
# 1a. Linear interpolation - interpolation using 'approx'
#### "linearly interpolate given data points, or a function performing the linear (or constant) interpolation."

# infill using INTERPOLATION over FULL SERIES
infill_linterp6alltest <- na_interpolation(ts.us.all6test, option="linear")

# make the infilled time series a data frame and rename it 
infill_linterp6alltest.df <- as.data.frame(infill_linterp6alltest)

infill_linterp6alltest.df <- infill_linterp6alltest.df %>% 
  rename(sox_us_infill_linterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.6 <- cbind(dat.6, infill_linterp6alltest.df)


####
# 1b. Spline interpolation
#### "Perform cubic (or Hermite) spline interpolation of given data points"

# infill using INTERPOLATION over FULL SERIES
infill_spinterp6alltest <- na_interpolation(ts.us.all6test, option="spline")

# make the infilled time series a data frame and rename it 
infill_spinterp6alltest.df <- as.data.frame(infill_spinterp6alltest)

infill_spinterp6alltest.df <- infill_spinterp6alltest.df %>% 
  rename(sox_us_infill_spinterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.6 <- cbind(dat.6, infill_spinterp6alltest.df)



####
# 1c. Stine interpolation
#### "Returns the values of an interpolating function that runs through a set of points in the xy-plane according to the algorithm of Stineman (1980)"

# infill using INTERPOLATION over FULL SERIES
infill_stinterp6alltest <- na_interpolation(ts.us.all6test, option="stine")

# make the infilled time series a data frame and rename it 
infill_stinterp6alltest.df <- as.data.frame(infill_stinterp6alltest)

infill_stinterp6alltest.df <- infill_stinterp6alltest.df %>% 
  rename(sox_us_infill_stinterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.6 <- cbind(dat.6, infill_stinterp6alltest.df)



#----------------------------------------------------------


#########################
# METHOD 6: na_kalman() #
#########################

# Kalman smoothing over structural time series - can take into account seasonality, but we don't have that so may end up same as interpolation
# results above.

# infill using KALMAN over FULL SERIES
infill_kal6alltest <- na_kalman(ts.us.all6test, model="StructTS")

# make the infilled time series a data frame and rename it 
infill_kal6alltest.df <- as.data.frame(infill_kal6alltest)

infill_kal6alltest.df <- infill_kal6alltest.df %>% 
  rename(sox_us_infill_kal_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.6 <- cbind(dat.6, infill_kal6alltest.df)


#----------------------------------------------------------


############
# Evaluate #
############

# Assess
ggplot() +

  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_avg_test), colour="red", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_avg_test), size=5.4, fill="red", colour="black", shape=22) +

  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_linterp_test), colour="dark green", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_linterp_test), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_spinterp_test), colour="green", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_spinterp_test), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_stinterp_test), colour="light green", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_stinterp_test), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_kal_test), colour="orange", linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_kal_test), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us), colour="gray70", size=1.2) +
  geom_point(data=dat.6, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0600 (test)", x=as.Date("2020-09-20"), y=290, size=10) +
  scale_y_continuous(limits=c(0,300), breaks=seq(0,300,by=50)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())



####################################################################################################################################################


#                                                                  TEST 0700

#----------------------------------------------------------


#######################
# CREATE DATA SUBSETS #
#######################
# No lead up 0s
dat.7 <- nad.dat %>%
  filter(count_hr=="7", date > as.Date("2020-08-27"), obs_count=="1") %>% 
  print()

# Random removal of 6 values and replace with NA
dat.7$sox_us_check <- dat.7$sox_us
dat.7$sox_us_check[c(4,8,12,19,22,26)] <- NA


#----------------------------------------------------------


#######################
# METHOD 1: AVERAGING #
#######################
## 1a. First mutate(): calculates the average for the missing day based on count on either side 
## 1b. Second mutate(): calculates average for Sept10 based on Sept9&12
##     Third mutate(), new pipe: Calculates average for Sept11 based on Sept10(infill)&Sept13
dat.7 <- dat.7 %>% 
  mutate(sox_us_infill_avg_test = 
      ifelse(date==as.Date("2020-08-31"), 
             mean(dat.7[dat.7$date==as.Date("2020-08-30"),]$sox_us : dat.7[dat.7$date==as.Date("2020-09-01"),]$sox_us),
        ifelse(date==as.Date("2020-09-04"),
               mean(dat.7[dat.7$date==as.Date("2020-09-03"),]$sox_us : dat.7[dat.7$date==as.Date("2020-09-05"),]$sox_us),
          ifelse(date==as.Date("2020-09-08"),
                 mean(dat.7[dat.7$date==as.Date("2020-09-07"),]$sox_us : dat.7[dat.7$date==as.Date("2020-09-09"),]$sox_us),
            ifelse(date==as.Date("2020-09-15"),
                   mean(dat.7[dat.7$date==as.Date("2020-09-14"),]$sox_us : dat.7[dat.7$date==as.Date("2020-09-16"),]$sox_us),
              ifelse(date==as.Date("2020-09-18"),
                     mean(dat.7[dat.7$date==as.Date("2020-09-17"),]$sox_us : dat.7[dat.7$date==as.Date("2020-09-19"),]$sox_us),
                ifelse(date==as.Date("2020-09-22"),
                       mean(dat.7[dat.7$date==as.Date("2020-09-21"),]$sox_us : dat.7[dat.7$date==as.Date("2020-09-23"),]$sox_us),
                  sox_us))))))) %>% 
  mutate(sox_us_infill_avg_test = 
    ifelse(date==as.Date("2020-09-10"), 
           mean(dat.7[dat.7$date==as.Date("2020-09-09"),]$sox_us : dat.7[dat.7$date==as.Date("2020-09-12"),]$sox_us), 
           sox_us_infill_avg_test)) %>%
  print()


# second pipe for second part of 1b
dat.7 <- dat.7 %>% 
  mutate(sox_us_infill_avg_test = 
      ifelse(date==as.Date("2020-09-11"),
             mean((dat.7[dat.7$date==as.Date("2020-09-10"),]$sox_us_infill_avg_test + dat.7[dat.7$date==as.Date("2020-09-13"),]$sox_us)/2), 
             sox_us_infill_avg_test)) %>%
  print()


#----------------------------------------------------------

######################
# CREATE TIME SERIES #
######################
ts.us.all7test <- ts(dat.7$sox_us_check)


#----------------------------------------------------------


################################
# METHOD 2: na_interpolation() #
################################

# Note: na_interpolation() uses the full time series regardless of where it's subsetted, so for this exercise I just used the full timeseries 

####
# 1a. Linear interpolation - interpolation using 'approx'
#### "linearly interpolate given data points, or a function performing the linear (or constant) interpolation."

# infill using INTERPOLATION over FULL SERIES
infill_linterp7alltest <- na_interpolation(ts.us.all7test, option="linear")

# make the infilled time series a data frame and rename it 
infill_linterp7alltest.df <- as.data.frame(infill_linterp7alltest)

infill_linterp7alltest.df <- infill_linterp7alltest.df %>% 
  rename(sox_us_infill_linterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.7 <- cbind(dat.7, infill_linterp7alltest.df)


####
# 1b. Spline interpolation
#### "Perform cubic (or Hermite) spline interpolation of given data points"

# infill using INTERPOLATION over FULL SERIES
infill_spinterp7alltest <- na_interpolation(ts.us.all7test, option="spline")

# make the infilled time series a data frame and rename it 
infill_spinterp7alltest.df <- as.data.frame(infill_spinterp7alltest)

infill_spinterp7alltest.df <- infill_spinterp7alltest.df %>% 
  rename(sox_us_infill_spinterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.7 <- cbind(dat.7, infill_spinterp7alltest.df)



####
# 1c. Stine interpolation
#### "Returns the values of an interpolating function that runs through a set of points in the xy-plane according to the algorithm of Stineman (1980)"

# infill using INTERPOLATION over FULL SERIES
infill_stinterp7alltest <- na_interpolation(ts.us.all7test, option="stine")

# make the infilled time series a data frame and rename it 
infill_stinterp7alltest.df <- as.data.frame(infill_stinterp7alltest)

infill_stinterp7alltest.df <- infill_stinterp7alltest.df %>% 
  rename(sox_us_infill_stinterp_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.7 <- cbind(dat.7, infill_stinterp7alltest.df)



#----------------------------------------------------------


#########################
# METHOD 7: na_kalman() #
#########################

# Kalman smoothing over structural time series - can take into account seasonality, but we don't have that so may end up same as interpolation
# results above.

# infill using KALMAN over FULL SERIES
infill_kal7alltest <- na_kalman(ts.us.all7test, model="StructTS")

# make the infilled time series a data frame and rename it 
infill_kal7alltest.df <- as.data.frame(infill_kal7alltest)

infill_kal7alltest.df <- infill_kal7alltest.df %>% 
  rename(sox_us_infill_kal_test=x) %>% 
  print()

# bind existing dataframe and the infilled time series dataframe 
dat.7 <- cbind(dat.7, infill_kal7alltest.df)


#----------------------------------------------------------


############
# Evaluate #
############

# Assess
ggplot() +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_avg_test), linetype="dashed", colour="red", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_avg_test), size=5.4, fill="red", colour="black", shape=22) +

  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_linterp_test), linetype="dashed", colour="dark green", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_linterp_test), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_spinterp_test), linetype="dashed", colour="green", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_spinterp_test), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_stinterp_test), linetype="dashed", colour="light green", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_stinterp_test), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_kal_test), linetype="dashed", colour="orange", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_kal_test), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us), size=1.2, colour="gray70") +
  geom_point(data=dat.7, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray70", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0700 (test)", x=as.Date("2020-09-20"), y=195, size=10) +
  scale_y_continuous(limits=c(0,200), breaks=seq(0,200,by=25)) +
  scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.text.x=element_text(angle=45, hjust=1),
    axis.title=element_text(size=28, face="bold"),
    panel.grid.major=element_line(colour="light gray"),
    panel.grid.minor=element_blank())












####################################################################################################################################################

####################################################################################################################################################

####################################################################################################################################################



#                                                           NADINA vs. STELLAKO


# Comparing Nadina and Stellako sonars (with offset) can help inform which infilling method might be best


####################################################################################################################################################

#                                                               CLEANING

nad.raw <- read.csv("nadina2020_dailyreport.csv")
stel.raw <- read.csv("stellako2020_dailyreport.csv")

nad.sonar <- nad.raw %>% 
  rename(date=DATE,
    water_temp_C = Water.Temp...deg..C.,
    water_gauge_m = Water.Gauge..m.,
    n_files = Total.Files.Counted, 
    sox_us = Daily.Net.Upstream..expanded.) %>% 
  mutate(date = lubridate::dmy(date)) %>%
  print()

stel.sonar <- stel.raw %>% 
  rename(date=DATE,
    water_temp_C = Water.Temp...deg..C.,
    water_gauge_m = Water.Gauge..m.,
    n_files = Total.Files.Counted, 
    sox_us = Daily.Net.Upstream..expanded.) %>% 
  mutate(date = lubridate::dmy(date)) %>%
  print()




####################
# DETERMINE OFFSET #
####################

# Lining up overall profiles to determine travel time -- should be 5-6 days but just confirm


# plot
ggplot() +
  geom_point(data=nad.sonar, aes(x=date-6, y=sox_us), colour="aquamarine") +
  geom_line(data=nad.sonar, aes(x=date-6, y=sox_us), colour="aquamarine") +
  geom_point(data=stel.sonar, aes(x=date, y=sox_us), colour="coral") +
  geom_line(data=stel.sonar, aes(x=date, y=sox_us), colour="coral") +
  scale_x_date(breaks="3 day") +
  theme_bw()


# Plot shows travel time is between 5-6 days. The tail end seems to be closer to 6 days while the initial peak is around 5 days. Because this is
# informing infilling around Sept 10-11, will use 6 days. 



###################
# STELLAKO SUBSET #
###################

# Select the dates of Stellako that correspond to the missing Nadina hours on Sept 10 and 11 (Nadina-6)
# That would correspond to Stellako on Sept 4 and 5

stel.dat.raw <- read.csv("stellako Sonar_tool_2020_DATAENTRY.csv")

# CLEAN
stel.dat <- stel.dat.raw %>% 
  rename(bank=Bank,
    observer=Observer,
    date=Date,
    count_hr=Count.Hour,
    hr_portion=Portion.of.hour,
    time_mins=Time.counted_min,
    sox_us=Sox_us,
    sox_ds=Sox_ds,
    ch_us=CH_us,
    ch_ds=CH_ds,
    obs_count=Obs.Count..,
    comments=Comments) %>% 
  mutate(date = lubridate::dmy(date)) %>%
  mutate(ydate = lubridate::yday(date)) %>% 
  print()

ggplot(stel.dat, aes(x=date, y=sox_us)) +
  geom_point() +
  geom_line() +
  theme_bw()











