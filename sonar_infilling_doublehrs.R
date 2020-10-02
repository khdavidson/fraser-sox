# SONAR infilling - double hours missing 
# Sept 28 2020

####################################################################################################################################################

library(tidyverse)
library(broom)         # for multiple linear models through groupings e.g., do()  
library(imputeTS)
library(grid)          # for grid.draw()

setwd("~/Documents/ANALYSIS/data/Sonar")

nad.dat.raw <- read.csv("Nadina Sonar_tool_2020_KDinfill_DATAENTRY.csv")



####################################################################################################################################################

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

#                                                           DOUBLE HR ISSUES 

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

#                                                             0300 FILES


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

#####
# 1a. 8 day window overall mean 
#####

# Calculate mean over 8-day window
mean(dat.3.sub$sox_us, na.rm=T)    # 12.75

# Manully input 8-day mean into missing values of full dataframe for comparison later
dat.3$sox_us_infill_fullmean <- ifelse(is.na(dat.3$sox_us), 12.75, dat.3$sox_us)



#####
# 1b. 4 day window lead-up and tail-out
#####

# Calculate lead-up 4-day mean
mean(dat.3.sub[dat.3.sub$date < as.Date("2020-09-10"),]$sox_us, na.rm=T)    # 17.5

# Calculate tail-out 4-day mean
mean(dat.3.sub[dat.3.sub$date > as.Date("2020-09-11"),]$sox_us, na.rm=T)    # 8

# Manually input each 4-day mean into missing values of full dataframe for comparison later
dat.3$sox_us_infill_partmean <- ifelse(is.na(dat.3$sox_us) & dat.3$date==as.Date("2020-09-10"), 17.5,
                                  ifelse(is.na(dat.3$sox_us) & dat.3$date==as.Date("2020-09-11"), 8, dat.3$sox_us))


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
    

# Assess
ggplot() +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_fullmean), linetype="dashed", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_partmean), linetype="dashed", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="red", colour="black",shape=23) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_linterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_spinterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_stinterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.3, aes(x=date, y=sox_us_infill_kal), linetype="dashed", size=1.1) +
  geom_point(data=dat.3, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.3, aes(x=date, y=sox_us), size=1.2) +
  geom_point(data=dat.3, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray60", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0300", x=as.Date("2020-09-22"), y=145, size=10, face="bold") +
  scale_y_continuous(limits=c(0,150)) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.title=element_text(size=28, face="bold"))



####################################################################################################################################################

#                                                             0400 FILES

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

#####
# 1a. 8 day window overall mean 
#####

# Calculate mean over 8-day window
mean(dat.4.sub$sox_us, na.rm=T)    # 12.75

# Manully input 8-day mean into missing values of full dataframe for comparison later
dat.4$sox_us_infill_fullmean <- ifelse(is.na(dat.4$sox_us), 12.75, dat.4$sox_us)



#####
# 1b. 4 day window lead-up and tail-out
#####

# Calculate lead-up 4-day mean
mean(dat.4.sub[dat.4.sub$date < as.Date("2020-09-10"),]$sox_us, na.rm=T)    # 17.5

# Calculate tail-out 4-day mean
mean(dat.4.sub[dat.4.sub$date > as.Date("2020-09-11"),]$sox_us, na.rm=T)    # 8

# Manually input each 4-day mean into missing values of full dataframe for comparison later
dat.4$sox_us_infill_partmean <- ifelse(is.na(dat.4$sox_us) & dat.4$date==as.Date("2020-09-10"), 17.5,
                                  ifelse(is.na(dat.4$sox_us) & dat.4$date==as.Date("2020-09-11"), 8, dat.4$sox_us))



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
    

# Assess
ggplot() +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_fullmean), linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_partmean), linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="red", colour="black",shape=23) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_linterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_spinterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
    
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_stinterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us_infill_kal), linetype="dashed", size=1.1) +
  geom_point(data=dat.4, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.4, aes(x=date, y=sox_us), size=1.2) +
  geom_point(data=dat.4, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray60", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0400", x=as.Date("2020-09-22"), y=125, size=10, face="bold") +
  scale_y_continuous(limits=c(0,130)) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.title=element_text(size=28, face="bold"))






####################################################################################################################################################

#                                                             0500 FILES


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

#####
# 1a. 8 day window overall mean 
#####

# Calculate mean over 8-day window
mean(dat.5.sub$sox_us, na.rm=T)    # 12.75

# Manully input 8-day mean into missing values of full dataframe for comparison later
dat.5$sox_us_infill_fullmean <- ifelse(is.na(dat.5$sox_us), 12.75, dat.5$sox_us)



#####
# 1b. 5 day window lead-up and tail-out
#####

# Calculate lead-up 5-day mean
mean(dat.5.sub[dat.5.sub$date < as.Date("2020-09-10"),]$sox_us, na.rm=T)    # 17.5

# Calculate tail-out 5-day mean
mean(dat.5.sub[dat.5.sub$date > as.Date("2020-09-11"),]$sox_us, na.rm=T)    # 8

# Manually input each 5-day mean into missing values of full dataframe for comparison later
dat.5$sox_us_infill_partmean <- ifelse(is.na(dat.5$sox_us) & dat.5$date==as.Date("2020-09-10"), 17.5,
                                  ifelse(is.na(dat.5$sox_us) & dat.5$date==as.Date("2020-09-11"), 8, dat.5$sox_us))


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
    

# Assess
ggplot() +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_fullmean), linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_partmean), linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="red", colour="black",shape=23) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_linterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
    
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_spinterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_stinterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.5, aes(x=date, y=sox_us_infill_kal), linetype="dashed", size=1.1) +
  geom_point(data=dat.5, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.5, aes(x=date, y=sox_us), size=1.2) +
  geom_point(data=dat.5, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray60", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0500", x=as.Date("2020-09-22"), y=190, size=10, face="bold") +
  scale_y_continuous(limits=c(0,200)) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.title=element_text(size=28, face="bold"))





####################################################################################################################################################

#                                                             0600 FILES


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
  filter(count_hr=="6", date > "2020-09-06" & date < "2020-09-16", obs_count=="1") %>%
  print()


#----------------------------------------------------------


################### 
# METHOD 1: MEANS #
###################

#####
# 1a. 8 day window overall mean 
#####

# Calculate mean over 8-day window
mean(dat.6.sub$sox_us, na.rm=T)    # 12.76

# Manully input 8-day mean into missing values of full dataframe for comparison later
dat.6$sox_us_infill_fullmean <- ifelse(is.na(dat.6$sox_us), 12.76, dat.6$sox_us)



#####
# 1b. 4 day window lead-up and tail-out
#####

# Calculate lead-up 6-day mean
mean(dat.6.sub[dat.6.sub$date < as.Date("2020-09-10"),]$sox_us, na.rm=T)    # 17.6

# Calculate tail-out 6-day mean
mean(dat.6.sub[dat.6.sub$date > as.Date("2020-09-11"),]$sox_us, na.rm=T)    # 8

# Manually input each 6-day mean into missing values of full dataframe for comparison later
dat.6$sox_us_infill_partmean <- ifelse(is.na(dat.6$sox_us) & dat.6$date==as.Date("2020-09-10"), 17.6,
                                  ifelse(is.na(dat.6$sox_us) & dat.6$date==as.Date("2020-09-11"), 8, dat.6$sox_us))


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
    

# Assess
ggplot() +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_fullmean), linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_partmean), linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="red", colour="black",shape=23) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_linterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_spinterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_stinterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.6, aes(x=date, y=sox_us_infill_kal), linetype="dashed", size=1.1) +
  geom_point(data=dat.6, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.6, aes(x=date, y=sox_us), size=1.2) +
  geom_point(data=dat.6, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray60", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0600", x=as.Date("2020-09-22"), y=290, size=10, face="bold") +
  #scale_y_continuous(limits=c(0,150)) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.title=element_text(size=28, face="bold"))




####################################################################################################################################################

#                                                             0700 FILES


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
  filter(count_hr=="7", date > "2020-09-07" & date < "2020-09-17", obs_count=="1") %>%
  print()


#----------------------------------------------------------


################### 
# METHOD 1: MEANS #
###################

#####
# 1a. 8 day window overall mean 
#####

# Calculate mean over 8-day window
mean(dat.7.sub$sox_us, na.rm=T)    # 12.77

# Manully input 8-day mean into missing values of full dataframe for comparison later
dat.7$sox_us_infill_fullmean <- ifelse(is.na(dat.7$sox_us), 12.77, dat.7$sox_us)



#####
# 1b. 4 day window lead-up and tail-out
#####

# Calculate lead-up 7-day mean
mean(dat.7.sub[dat.7.sub$date < as.Date("2020-09-10"),]$sox_us, na.rm=T)    # 17.7

# Calculate tail-out 7-day mean
mean(dat.7.sub[dat.7.sub$date > as.Date("2020-09-11"),]$sox_us, na.rm=T)    # 8

# Manually input each 7-day mean into missing values of full dataframe for comparison later
dat.7$sox_us_infill_partmean <- ifelse(is.na(dat.7$sox_us) & dat.7$date==as.Date("2020-09-10"), 17.7,
                                  ifelse(is.na(dat.7$sox_us) & dat.7$date==as.Date("2020-09-11"), 8, dat.7$sox_us))


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
    

# Assess
ggplot() +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_fullmean), linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_partmean), linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="red", colour="black",shape=23) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_linterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_spinterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_stinterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="green", colour="black", shape=24) +

  geom_line(data=dat.7, aes(x=date, y=sox_us_infill_kal), linetype="dashed", size=1.1) +
  geom_point(data=dat.7, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.7, aes(x=date, y=sox_us), size=1.2) +
  geom_point(data=dat.7, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray60", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0700", x=as.Date("2020-09-22"), y=195, size=10, face="bold") +
  scale_y_continuous(limits=c(0,200)) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.title=element_text(size=28, face="bold"))






####################################################################################################################################################

#                                                             0800 FILES


#----------------------------------------------------------


#######################
# CREATE DATA SUBSETS #
#######################

# No lead up 0s
dat.8 <- nad.dat %>%
  filter(count_hr=="8", date > as.Date("2020-08-28"), obs_count=="1") %>% 
  print()

# 8-day window (for means) 
dat.8.sub <- nad.dat %>% 
  filter(count_hr=="8", date > "2020-09-08" & date < "2020-09-18", obs_count=="1") %>%
  print()


#----------------------------------------------------------


################### 
# METHOD 1: MEANS #
###################

#####
# 1a. 8 day window overall mean 
#####

# Calculate mean over 8-day window
mean(dat.8.sub$sox_us, na.rm=T)    # 12.88

# Manully input 8-day mean into missing values of full dataframe for comparison later
dat.8$sox_us_infill_fullmean <- ifelse(is.na(dat.8$sox_us), 12.88, dat.8$sox_us)



#####
# 1b. 8 day window lead-up and tail-out
#####

# Calculate lead-up 8-day mean
mean(dat.8.sub[dat.8.sub$date < as.Date("2020-09-10"),]$sox_us, na.rm=T)    # 18.8

# Calculate tail-out 8-day mean
mean(dat.8.sub[dat.8.sub$date > as.Date("2020-09-11"),]$sox_us, na.rm=T)    # 8

# Manually input each 8-day mean into missing values of full dataframe for comparison later
dat.8$sox_us_infill_partmean <- ifelse(is.na(dat.8$sox_us) & dat.8$date==as.Date("2020-09-10"), 18.8,
                                  ifelse(is.na(dat.8$sox_us) & dat.8$date==as.Date("2020-09-11"), 8, dat.8$sox_us))


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
    

# Assess
ggplot() +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_fullmean), linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_fullmean), size=5.3, fill="blue", colour="black", shape=22) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_partmean), linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_partmean), size=5.3, fill="red", colour="black",shape=23) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_linterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_linterp), size=5.3, fill="dark green", colour="black", shape=24) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_spinterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_spinterp), size=5.3, fill="green", colour="black", shape=24) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_stinterp), linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_stinterp), size=5.3, fill="light green", colour="black", shape=24) +

  geom_line(data=dat.8, aes(x=date, y=sox_us_infill_kal), linetype="dashed", size=1.1) +
  geom_point(data=dat.8, aes(x=date, y=sox_us_infill_kal), size=5.3, fill="orange", colour="black", shape=25) +
  
  geom_line(data=dat.8, aes(x=date, y=sox_us), size=1.2) +
  geom_point(data=dat.8, aes(x=date, y=sox_us), size=6.5, colour="black", fill="gray60", stroke=1.1, shape=21) +
  
  labs(x="Date", y="Number of U/S sockeye") +
  annotate(geom="text", label="0800", x=as.Date("2020-09-22"), y=590, size=10, face="bold") +
  scale_y_continuous(limits=c(0,600)) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=24),
    axis.title=element_text(size=28, face="bold"))




#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------


################
# ALL TOGETHER #
################

grid.newpage()
grid.draw(rbind(ggplotGrob(p3), ggplotGrob(p4), ggplotGrob(p5), ggplotGrob(p6), ggplotGrob(p7), ggplotGrob(p8), size="last"))

grid.arrange(p3, p4, p5, p6, p7, p8, ncol = 3, nrow = 2)








