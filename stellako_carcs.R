# stellako recoveries 



setwd("~/ANALYSIS/Data")
library(tidyverse)

data <- read.csv("Master Roving Analysis Tool decker modifications to peak spawn_count_data.csv")


stellako <- data %>% 
  rename(year=Year,
    run_timing_group=Run.Timing,
    watershed_group = Watershed.Group,
    stream_shore=Stream.Shore,
    survey=`Survey..`,
    date=Date,
    survey_type=Survey.Type, 
    area=Area,
    water_temp=Water.temperature,
    stream_alias=Stream.Alias,
    live_count1=Live.Count.1,
    live_count2=Live.Count.2,
    oe=`Obs..Eff.`,
    hold_perc=X.Hold,
    spawn_perc=X.Spawning,
    spawnout_perc=X.SpawnedOut,
    male_carc=Carcass.Male,
    fem_carc=Carcass.Female,
    jack_carc=Carcass.Jack,
    unsex_carc_g=`Ground.Carcass.Unsexed`,
    unsex_carc_a1=`Aerial.Carcass.Unsexed.Observer.1`,
    unsex_carc_a2=`Aerial.Carcass.Unsexed.Obsterver.2`,
    unsex_car=`Carcass.Unsexed..interpret..blank..as.zero.carcasses.`) %>%
  filter(stream_shore == "Stellako River", area != "Above Fence", area != "Below Fence", area != "Stellako Totals", area != "Didson Weir", 
    area != "Fence", !grepl("Aerial", area), area != "Outlet Area") %>% 
  mutate_at(vars(c(20,21,33)), funs(as.numeric))

stellako$date <- as.Date(stellako$date, format = "%d-%B-%Y")

summary <- stellako %>% 
  mutate(carc_sum = male_carc+fem_carc+jack_carc) %>% 
  group_by(area) %>% 
  summarize(sum=sum(carc_sum, na.tm=T)) %>%
  print()
  

ggplot(stellako, aes(x=date, y=))












