# Nadleh 2021 (& Stellako) final report 
# Sept 2021 

###########################################################################################################################################

# libraries 
library(tidyverse)
library(readxl)     

setwd("~/ANALYSIS/data")

catch.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="nightly_catch")
enviro.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="environmentals")
lf.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="length_frequency")
bio.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="biosampling")


###########################################################################################################################################

#                                                             CLEAN 

catch <- catch.raw %>% 
  mutate_at(vars(n_marked_released:n_chinook_smolts), as.numeric) %>%
  print()

enviro <- enviro.raw %>%
  mutate_at(vars(water_gauge_m:water_temp_C), as.numeric) %>%
  print()

bio <- bio.raw %>%
  mutate_at(vars(length_mm:weight_g), as.numeric) %>%
  print()

# Expand length-frequency table to be R friendly
lf <- lf.raw[rep(1:nrow(lf.raw), lf.raw[["count"]]),]


###########################################################################################################################################


#                                              MIGRATION DYNAMICS: NIGHTLY AND ANNUALLY































