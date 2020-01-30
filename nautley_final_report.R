# NAUTLEY 2019 FINAL REPORT CODE
# 29-Jan-2020
# All DNA, scales, length and weight data to be run by DFO are now here.
# More data may come from E-Watch: DNA and physiological samples. 

# libraries and wd
library(tidyverse)
library(XLConnect)
library(xlsx)
library(openxlsx)

setwd("~/ANALYSIS/Data")


####################################################################################################################################################

                                                        ######################
                                                        # BIOSAMPLE ANALYSIS #
                                                        ######################

# read data
dat <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheetIndex=3, detectDates=T)          # very slow! 

data <- dat %>% 
  mutate_at(vars(c(10:26)), funs(as.character)) %>% 
  mutate_at(vars(c(10:26)), funs(as.numeric)) %>% 
  print()

########################
# DNA MIGRATION TIMING #
########################

gsid <- data %>% 
  filter(prob1 <= 0.8) %>% 
  print()


























