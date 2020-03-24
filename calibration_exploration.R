# calibration data exploration 
# davidson - 23mar2020

library(vegan)
library(tidyverse)

# read data 
setwd("~/ANALYSIS/Data")
data_raw <- read.csv("Calibration_v3.csv")


#####################
# FILTER/CLEAN DATA #
#####################

# rename. only using data from 1998 onward. Arbitrary cutoff date discussed with P. Welch 
data <- data_raw %>% 
  rename(watershed_group = Watershed_Group,
    run_group = Run_Timing_Group, 
    cu = Conservation_Unit,
    stream_name = Gazetted_Stream_Name,
    year = Year,
    stability = Volatile_Stable,
    size = Size,
    water_clarity = Water_Clarity,
    substrate_colour = Substrate_Colour,
    canopy_cover = Canopy_Covery,
    lwd = Large_Woody_Debris,
    vis_method = Visual_Method,
    peak_live = Peak_Live,
    cuml_dead = Cum_Dead,
    vis_est = `Peak_Live_.Cum_dead`,
    hpe_method = High_Precision_Estimate_Method,
    hpe = High_precision_estimate,
    index = Index) %>% #index calculated as hpe/vis_est
  filter(year >= 1998) %>% 
  print()

## NOTE: No estimates have been removed or investigated for their quality. It is assumed that all surveys included in the "Calibration_v3.csv"
## file have been verified and are appropriate for this investigative analysis. However, prior to a "good copy" analysis, all of these should be
## individually verified (or confirmed by a knowing party) as being high quality. I can't verify this as these data were compiled prior to my
## involvement. 


#######################
# SUMMARY/EXPLORATION #
#######################

# See what watershed group has the biggest sample size, which will help inform where to start for trials 

wshed_grp <- data %>% 
  group_by(watershed_group) %>% 
  summarize(n = n()) %>% 
  print()

#### sample sizes as follows (as of Mar 23, 2020 which includes data up to 2018): 
#  watershed_group          n
#1 Early South Thompson     9
#2 Early Stuart            28     # MOST
#3 Harrison-Lillooet       13     # 3rd most, but fair bit less than 2nd most 
#4 Late South Thompson     12
#5 Late Stuart             11
#6 Lower Fraser             6
#7 Nechako                 24     # 2nd most
#8 North Thompson           1
#9 Quesnel                  4

# therefore Early Stuart and Nechako will be focus from hereon for trial analyses. Just arbitrarily chosen, but they have similar # data points.
focal_grps <- c("Nechako", "Early Stuart")

focal <- data %>% 
  filter(watershed_group %in% focal_grps) %>%
  print() # recall this is already filtered by dates >= 1998

#################################################################################################################################################

# ideas moving forward
# - cluster analyses based on environmental covariates may show qualitative groupings
# - glmm to model: index in a given stream (or watershed group?) ~ env covariates 





l1 <- lm(data$hpe ~ data$vis_est)
r1 <- resid(l1)
plot(r1)








