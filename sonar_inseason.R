
# ALLO! 

# Here is some code to replicate the "Inseason_reporting" tab in Excel sonar tools
# changes are committed to khdavidson/fraser-sox/sonar_inseason repo

###############################
# STEPS PRECEDING THIS SCRIPT #
###############################

  # Obviously there were some steps taken before this. 
  # First, export the 'Data entry' and 'Environmental Data' tabs (as-is) as independent .csv files. It is important you make no formatting changes, as the following code is based on cleaning and re-formatting those current formats.
  # Save them wherever you want to reference from - the exact location doesn't matter. 

  # The following code will replicate the 'Inseason_reporting' tab, and maybe eventually also produce some pretty graphs (TBD...)

##########
# SET UP #
##########

# load libraries to use
library(dplyr)
library(tidyr)
library(ggplot2)
library(XLConnect)
library(xlsx)
library(openxlsx)

# set working directory where spreadsheet is
setwd("C:/DFO-MPO/Data")

##############
# Count data #
##############

# read in COUNT data 
counts <- read.xlsx("Stellako Sonar_tool_2019.xlsm", sheet = 7, startRow=5, cols=c(1:12), colNames=T, detectDates=T)
                                                                                            
# reformat COUNT dataframe 
counts <- counts %>%                                                  # rename columns to be more R friendly
  rename(bank = Bank,
         observer = Observer,
         date = Date,
         count_hr_24 = Count.Hour,
         hr_block = Portion.of.hour,
         time_length_min = Time.counted_min,
         sox_us = Sox_us,
         sox_ds = Sox_ds,
         ch_us = CH_us,
         ch_ds = CH_ds,
         count_number = `Obs.Count.#`,
         comments = Comments) %>%
  mutate_at(vars(c(4)), funs(as.numeric))                             # reformat some integers to be numeric

# re-arrange COUNT data for easy visualizaton, remove the one training count, add net column 
counts <- counts %>%
  arrange(date, count_hr_24) %>%                                                             # ordered by date, and then count hour (1-24)
  filter(count_number != "NA") %>%
  mutate(sox_us_net = sox_us-sox_ds) %>%
  mutate(date_hr = paste(paste(gsub("-", "-", date)), count_hr_24, sep="-"))























##### 

# summarize by mean, sd, cv
counts.cv <- counts %>% 
  group_by(bank, date, count_hr_24) %>% 
  summarize(net_mean = mean(sox_us_net), net_sd = ifelse(sox_us_net=0,0,sd(sox_us_net)), cv = ifelse(net_sd>0, abs(net_sd/net_mean), 0)) %>% 
  print()


######################
# Environmental data #
######################

# read in ENV data  
enviro <- read.xlsx("Stellako Sonar_2018.xlsx", sheet = 2, startRow=3, cols=c(1:13), colNames=T, detectDates=T)

# reformat ENV dataframe 
enviro <- enviro %>%                                                                           # rename columns to be more R friendly
  rename(date = `Date.(dd/mm/yy)`,
         observer_1 = Observer1, 
         observer_2 = Column1,
         tod = Column2,
         gauge_m = `Gauge.(m)`,
         bankfull_p = `%Bankfull`,
         brightness = Brightness,
         cloud_p = `%Cloudy`,
         precip_type = PrecType,
         precip_int = PrecInt,
         fish_vis = FishVis,
         water_temp = WTemp,
         water_clarity = WClarity) %>%
  filter(date != "", date != "**sonar pulled 0820 Oct 2, 2018") %>%
  mutate(date = lubridate::ymd(date)) %>%                                                    
  mutate_at(vars(c(5, 12)), funs(as.numeric))
  














#######################
# PRELIM CALCULATIONS #
#######################





########################
# CREATE SUMMARY TABLE #
########################

# summarize by date, expanded based on number of files counted 
t0 <- counts.avg %>% 
  group_by(date) %>% 
  summarize(total_us = sum(avg_net, na.rm=T), n_files = n()) %>%
  mutate_at(vars(c(3)), funs(as.numeric)) %>%
  mutate(daily_net_exp = as.numeric(ifelse(n_files=="24", total_us*3,
                                ifelse(n_files=="12", total_us*6,
                                       ifelse(n_files=="9", total_us*9, ""))))) %>%
  mutate_at(4, funs(round(.,0))) %>%
  print(t0)

# join with environmental data - recall enviro dataframe is 'e.raw'
t1 <- left_join(t0, enviro, by ='date')

# omit unecessary columns and re-order remaining columns
t1 <- t1 %>%
  select(-c(5:7, 9:14, 16))%>%
  select(date, water_temp, gauge_m, n_files, daily_net_exp) 






#####################################################################################################################

########################
# DATA VERIFICATION/QC #
########################

# interobserver agreement - % agreement 
counts.ioa.p <- counts %>%
  spread(count_number, sox_us_net) %>% 
  rename(count_1 = `1`,
    count_2 = `2`,
    count_3 = `3`,
    count_4 = `4`) %>%
  mutate(group.mean = (count_1+count_2+count_3+count_4)/(n(count_1,count_2,count_3,count_4, na.rm=T)))

counts.ioa.p$group.mean <- (count_1+count_)


  











