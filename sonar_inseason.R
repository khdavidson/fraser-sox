# HELLO!
# Here is some code to replicate the "Inseason_reporting" tab in Excel sonar tools

###############################
# STEPS PRECEDING THIS SCRIPT #
###############################

  # Obviously there were some steps taken before this. 
  # First, export the 'Data entry' tab as-is, as a .csv. It is important you make no formatting changes, as the following code is based on cleaning and re-formatting that current format.
  # Save it wherever you want to reference it from, the exact location doesn't matter. 

  # The following code will replicate the 'Inseason_reporting' tab, and maybe eventually also produce some pretty graphs (TBD...)

##########
# SET UP #
##########

# Load libraries to use
library(dplyr)
library(tidyr)
library(ggplot2)

# Set working directory where .csv data file is stored. This will vary by computer.
setwd("~/Data/Sonar")

# Read in data 
headers <- read.csv("Stellako Sonar_2018.csv", skip=4, header=F, nrows=1, as.is=T)    # Extract 5th row which will be our headers and remove other garbage header info
raw = read.csv("Stellako Sonar_2018.csv", skip = 5, header = F)                       # Extract just data
colnames(raw)= headers                                                                # Apply headers (extracted above) to dataframe. This method preserves row numbering and doesn't retain garbage info                                # apply character top row as column headers 

# Reformat dataframe 
raw <- raw[,-c(13:18)]                                                                # Removes extra NA columns
raw <- raw %>%                                                                        # Rename columns to be more R friendly
  rename(bank = Bank,
         observer = Observer,
         date = Date,
         count_hr_24 = `Count Hour`,
         hr_block = `Portion of hour`,
         time_length_min = `Time counted_min`,
         sox_us = Sox_us,
         sox_ds = Sox_ds,
         ch_us = CH_us,
         ch_ds = CH_ds,
         count_number = `Obs Count #`,
         comments = Comments) %>%
  mutate(date = lubridate::dmy(date)) %>%                                                 # Reformat date
  mutate_at(vars(c(4, 9, 10)), funs(as.numeric))                                          # Reformat some integers to be numeric

# Re-arrange for easy visualizaton 
raw <- raw %>%
  arrange(date, count_hr_24)














