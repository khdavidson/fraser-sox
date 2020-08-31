# sockeye escapement database 
# recent to june 2020
# script initiated 26-aug-2020

# this script includes any exploratory analyses conducted using the DFO sockeye escapement database to inform field planning

###################################################################################################################################################

library(tidyverse)

setwd("~/Documents/ANALYSIS/data")

data_raw <- read.csv("DFO Sockeye Escapement All Years (June 2020).csv")

###################################################################################################################################################

#                                                             DATA CLEANING 

data <- data_raw %>% 
  rename(year = Year,
    watershed_group = Watershed.Group.Name,
    stream = Stock.Name.stream.,
    timing_group = Timing.Group,
    cu = CU.Name,
    peak_spawn = spnpeak,
    arrival_date = arrival,
    total_pop = Total,
    adults = Adults,
    spawn_success = spawn.,
    estimate_method = Estimate.Method,
    survey_start = Survey.Start,
    survey_end = Survey.End,
    num_inspections = Number.Inspections,
    estimate_type = est_type) %>%
  print()

###################################################################################################################################################

#                                                               NADINA ESCAPEMENT 

# Investigating Nadina escapement over time to inform the 'moving target' fecundity sampling method at the Nadina channel, collaboratively developed
# between StAD and SEP 

# select just Nadina from 'recent' years. before 1991 the run was split into the suspected early (extirpated) and late runs
# then group by year to add the river and channel populations into one collective run 
nad <- data %>% 
  filter(grepl('Nadina', stream), year >= 1991) %>% 
  group_by(year) %>% 
  summarize(total_pop=sum(total_pop), total_jack=sum(jacks), total_males=sum(males), total_females=sum(females), total_eff_fem=sum(eff_fem), 
    avg_spawn_success=mean(spawn_success)) %>%
  print()

ggplot(nad, aes(x=year, y=total_pop)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks=seq(0,80000,15000)) +
  scale_x_continuous(breaks=seq(1991,2020,1)) +
  theme_bw()







