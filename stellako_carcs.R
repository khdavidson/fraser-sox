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
    unsex_carc_a2=`Aerial.Carcass.Unsexed.Observer.2`,
    unsex_car=`Carcass.Unsexed..interpret..blank..as.zero.carcasses.`) %>%
  filter(stream_shore == "Stellako River", area != "Above Fence", area != "Below Fence", area != "Stellako Totals", area != "Didson Weir", 
    area != "Fence", !grepl("Aerial", area), area != "Outlet Area") %>% 
  mutate_at(vars(c(20,21,33)), funs(as.numeric)) %>% 
  select(-c(Blank1, Blank2:Run.TimingStream.Shore)) %>%
  mutate(male_carc = ifelse(is.na(male_carc), 0, male_carc)) %>% 
  mutate(fem_carc = ifelse(is.na(fem_carc), 0, fem_carc)) %>% 
  mutate(jack_carc = ifelse(is.na(jack_carc), 0, jack_carc)) %>% 
  mutate(carc_sum = male_carc+fem_carc+jack_carc) %>% 
  select(-c(Male.Carcass.NR:Female.Carcass.100.))

# fix dates separately easier
stellako$date <- as.Date(stellako$date, format = "%d-%B-%Y")
stellako$jdate <- paste(as.numeric(format(stellako$date, "%j")))
stellako$monthday <- format(as.Date(stellako$date, format="%Y-%m-%d"),"%b-%d")

stellako$area <- factor(stellako$area, levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), ordered=T)

# summarize totals by area 
summary <- stellako %>% 
  group_by(area) %>% 
  summarize(sum=sum(carc_sum), n_surveys = n()) %>%
  mutate(carcs_per_survey = sum/n_surveys) %>% 
  print()
  

ggplot(summary, aes(x=area, y=sum, colour=area, fill=area)) +
  geom_bar(stat="identity") 












