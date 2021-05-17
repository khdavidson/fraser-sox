
# ALLO! 

# Code to replicate the "Inseason_reporting" tab in Excel sonar tools


################################################################################################################################################################

# load packages to use
library(tidyverse)
#library(xlsx)       # Not working lately
library(openxlsx)

# set working directory where Excel sonar data entry file is
setwd("~/ANALYSIS/data/Sonar")                                                                #**** NEEDS TO BE CUSTOMIZED FOR EACH USER

# read in data 
counts.raw <- read.xlsx("Chilko Sonar Tool 2019- no infill.xlsm", sheet=7, startRow=5, cols=c(1:12), colNames=T, detectDates=T)
env.raw <- read.xlsx("Chilko Sonar Tool 2019- no infill.xlsm", sheet=2, startRow=3, cols=c(1:13), colNames=T, detectDates=T)


################################################################################################################################################################


#                                                                           CLEAN


#---------- COUNT data 
counts <- counts.raw %>%                                                                      # rename columns to be more R friendly
  rename(bank = Bank,
         observer = Observer,
         date = Date,
         count_hr = Count.Hour,
         hr_bin = Portion.of.hour,
         file_length_min = Time.counted_min,
         sox_us = Sox_us,
         sox_ds = Sox_ds,
         ch_us = CH_us,
         ch_ds = CH_ds,
         count_n = `Obs.Count.#`,
         comments = Comments) %>%
  mutate_at("count_hr", as.numeric) %>%                                                       # reformat some integers to be numeric
  #filter(!is.na(count_n)) %>%                                                                # remove rows where count_n is an NA 
  mutate(sox_us_net = sox_us-sox_ds) %>%                                                      # calculate net upstream
  mutate(group = ifelse(count_hr%in%c(2,4,8,10,14,16,20,22), "A2", 
                        ifelse(count_hr%in%c(3,9,15,21), "A3", 
                               ifelse(count_hr%in%c(0,6,12,18), "A23", "A")))) %>%
  arrange(date, hr_bin, bank) %>%                                                             # ordered by date, and then count hour (1-24)
  print()


#---------- ENVIRONMENTAL data
env <- env.raw %>%                                                                            # rename columns to be more R friendly
  rename(date = `Date.(dd/mm/yy)`,
         observer_1 = Observer1, 
         observer_2 = Observer2,
         time = Time,
         gauge_m = `Gauge.(m)`,
         bankfull = `%Bankfull`,
         brightness = Brightness,
         cloud_cover = `%Cloudy`,
         precip_type = PrecType,
         precip_int = PrecInt,
         fish_vis = FishVis,
         water_temp = WTemp,
         water_clarity = WClarity) %>%
  print()




################################################################################################################################################################



#                                                                GENERATING THE IN-SEASON REPORT


#---------- COUNT data
# Function to calculate daily abundance based on all files, every 2nd file and every 3rd file counted
count.summary.fx <- function(group_name, expansion_factor){                                                    # Tell function argument names to take later
  counts %>%                                                                                                   # call dataframe to manipulate
    filter(hr_bin=="0-20 min", count_n==1, grepl(group_name, group)) %>%                                       # filter to select only 0-20min files, primary counts, and detect a pattern (using "grepl") based on the argument group_name (which we supply as a function argument manually) within the variable named "group"
    group_by(date) %>%                                                                                         # group by date
    summarize(daily_total=sum(sox_us_net), "Number of files counted (every {group_name})" := n()) %>%          # sum up the net upstream sockeye each day in a new column called "daily_total", and then count the number of files each day in a new column called "Number of files counted (every {group_name})", where "{group_name}" will be replaced by a custom name we give as a function argument
    mutate("Daily net upstream (expanded*{expansion_factor})" := daily_total*expansion_factor) %>%             # create a new column called "Daily net upstream (expanded*{expansion_factor})" where "{group_name}" will be replaced by a custom name we give as a function argument. This will expand the daily sockeye counts by a custom expansion factor we give as a function argument called "expansion_factor"
    select(-daily_total)                                                                                       # Remove the original unexpanded daily net total to replicate exactly the Excel in-season report
}                                                         

# Create a pipe to use the function to calculate daily passage and expand it, and then join it together in one table
# This replicates taking every file and multiplying by 3, every 2nd file and multiplying by 6, and every 3rd file and multiplying by 9. 
counts.daily <- count.summary.fx("A", 3) %>%                                                                   # call the function "count.summary.fx" we created above. Give it the first custom arguments of "A" and "3". "A" detects all entries with an "A" in the "group" variable, while "3" multiples the daily net upstream sockeye movement by a factor of 3. Store the results of this entire pipe in "counts.daily"
  full_join(count.summary.fx("2", 6), by="date") %>%                                                           # call the function "count.summary.fx" we created above. Give it the first custom arguments of "2" and "6". "2" detects all entries with a "2" in the "group" variable, while "6" multiples the daily net upstream sockeye movement by a factor of 6. Join it ("full_join") to the results of the function call above. 
  full_join(count.summary.fx("3", 9), by="date") %>%                                                           # call the function "count.summary.fx" we created above. Give it the first custom arguments of "3" and "6". "3" detects all entries with a "3" in the "group" variable, while "9" multiples the daily net upstream sockeye movement by a factor of 9. Join it ("full_join") to the results of the function calls above. 
  print()                                                                                                      # show the results to make sure it worked 


#---------- ENVIRO data
# Extract daily water gauge and temperature data
env.daily <- env %>%                                                                                           # call the "env" dataframe and create a pipe storing everything in "env.daily"
  group_by(date) %>%                                                                                           # group by date 
  summarize("Water temperature" = water_temp, "Water gauge (m)" = gauge_m) %>%                                 #
  print()


#---------- THE WHOLE SHABANG
inseason.report <- full_join(env.daily, counts.daily, by="date")                                               # Join the expanded count data and the environmental data together by individual "date" to replicate the Excel in-season sonar report 


















