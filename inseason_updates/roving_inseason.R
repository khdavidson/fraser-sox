# Roving in-season update code 
# Aug 2021 

# This code replaces the macros within the Roving Tool to generate more reproducible and less breakable in-season updates. 
# Anywhere there are << >> symbols, it indicates something YOU need to change to allow the code to run on your specific computer. 
# See the accompanying help file for detailed instructions. 

# Load libraries to use
library(tidyverse)     # for everything
library(readxl)        # for read_excel()
library(openxlsx)      # for createWorkbook()

# Set working directory
setwd("~/ANALYSIS/data")      # << UPDATE THIS WITH YOUR WORKING DIRECTORY ON YOUR COMPUTER >>

# Load the count ('count_entry') and environmental ('Environment_entry') data tabs 
counts.raw <- read_excel("Early Stuart Roving_Daily_Report_2021.xlsm", sheet="Count_entry")                  
enviro.raw <- read_excel("Early Stuart Roving_Daily_Report_2021.xlsm", sheet="Environment_entry")


#################################################################################################################################################

#                                                             CLEANING

# Clean the count_entry tab for use in R
counts <- counts.raw %>% 
  rename(year=Year,
         stream_name=Stream_ID,
         area=Area,
         stream_shore=`Stream/Shore`,
         run_timing=`Run Timing`,
         watershed_group=`Watershed Group`,
         alias=`This column automatically provides historical/alias name if applicable`,
         area_description=`This column automatically provides a description of the counting area to confirm your choice of \"Area\"  in the previous column`,
         survey_no=`Survey #`,
         date=`Date  (i.e. 20-Oct-14)`,
         survey_type=`Survey Type`,
         calibration_code=`Calibration  code  leave \"blank\" =no calibration                  1= escapement and calibration                     2= calibration only`,
         live_count_obs1=`Live Count Observer 1`,
         live_count_obs2=`Live Count Observer 2`,
         oe=`Observer Efficiency`,
         holding=`% Holding`,
         spawning=`% Spawning`,
         spawned_out=`% Spawned out`,
         behaviour_check=`This column checks if sum of columns( P,Q,R ) = 100%`,
         carc_male=`Carcass Male`,
         carc_fem=`Carcass Female`,
         carc_male_nr=`Male Carcass NR`,
         carc_male_0=`Male Carcass 0%`,
         carc_male_100=`Male Carcass 100%`,
         carc_fem_nr=`Female Carcass NR`,
         carc_fem_0=`Female Carcass 0%`,
         carc_fem_50=`Female Carcass 50%`,
         carc_fem_100=`Female Carcass 100%`,
         carc_jack=`Carcass Jack`,
         carc_ground_unsex=`Ground Carcass Unsexed`,
         carc_aerial_unsex_obs1=`Aerial Carcass Unsexed Observer 1`,
         carc_aerial_unsex_obs2=`Aerial Carcass Unsexed Observer 2`,
         carc_total_unsex=`Total Carcass Unsexed (formula cell, leave blank)`,
         carc_female_check1=`This column checks if sum of columns( Y, Z, AA, AB ) = Carcass Female`,
         carc_female_check2=`This column checks if sum of columns( U,V,W,X ) = Carcass Female`,
         live_tags=`# Live Tags`,
         carc_tags=`# Carcass tags`,
         psc_male=`# PSC Sample Males`,
         psc_female=`# PSC Sample  Females`,
         psc_jack=`# PSC Sample  Jacks`,
         dna_samples=`# DNA Samples`,
         live_chinook=`Live Count Chinook`,
         live_coho=`Live Count Coho`,
         live_kokanee=`Live Count Kokanee`,
         carc_coho=`Carcass Count Coho`,
         carc_chinook=`Carcass Count Chinook`,
         comments=`Comments (specific to one Area within a Stream/Shore) (OK for Comments to exceed width of this column)`) %>%
  select(year:comments) %>%
  mutate_at("oe", as.numeric) %>%
  print()


# Clean the Environment_entry tab for use in R
enviro <- enviro.raw %>% 
  rename(year=Year,
         stream_name=StreamID,
         stream_shore=`Stream/Shore`,
         run_timing=`Run Timing`,
         watershed_group=`Watershed Group`,
         alias=`This column automatically provides alias name if applicable`,
         survey_no=`Survey #`,
         survey_type=`Survey type`,
         date=`Date  (e.g. 20-Oct-14)`,
         obs1=`Observer 1`,
         obs2=`Observer 2`,
         peak_spawn_start=`Start PeakSpawn (e.g. 20-Oct-14)`,
         peak_spawn_end=`End PeakSpawn`,
         start_time=`Start Time   (24 hr: hh:mm)`,
         end_time=`End Time`,
         bankfull=`%Bankfull`,
         brightness=Brightness,
         cloud_cover=`%Cloudy`,
         precip_type=`Precip Type`,
         precip_intensity=`Precip Intensity`,
         fish_vis=`Fish Vis`,
         water_temp=`WTemp.`,
         water_clarity=`WClarity`,
         water_gauge_m=`Gauge (m)`,
         comments1=`Comments (relevant to the Stream/Shore as a whole) (OK for Comments to exceed width of column)`,
         comments2=`Additional Comments`) %>% 
  select(year:comments2) %>%
  print()
  

#################################################################################################################################################

#                                                                    DAILY REPORT 

daily.report <- counts %>% 
  group_by(year, run_timing, stream_shore, survey_type) %>%
  summarize(n_surveys=max(survey_no), 
            male_carcs=sum(carc_male, na.rm=T), female_carcs=sum(carc_fem, na.rm=T), jack_carcs=sum(carc_jack, na.rm=T),
            ground_unsex=sum(carc_ground_unsex, na.rm=T), aerial_unsex=sum(carc_aerial_unsex_obs1, na.rm=T)+sum(carc_aerial_unsex_obs2, na.rm=T),
            fem_nr=sum(carc_fem_nr, na.rm=T), fem_0=sum(carc_fem_0, na.rm=T), fem_50=sum(carc_fem_50, na.rm=T), fem_100=sum(carc_fem_100, na.rm=T), male_nr=sum(carc_male_nr, na.rm=T),
            male_psc=sum(psc_male, na.rm=T), female_psc=sum(psc_female, na.rm=T), jack_psc=sum(psc_jack, na.rm=T),
            n_tags=sum(live_tags, na.rm=T)+sum(carc_tags, na.rm=T), n_dna=sum(dna_samples, na.rm=T)) %>%
  mutate(total_unsex=ground_unsex+aerial_unsex,
         total_carc=total_unsex+male_carcs+female_carcs+jack_carcs) %>%
  select(year:aerial_unsex, total_unsex, total_carc, fem_nr:n_dna) %>%
  print()


#################################################################################################################################################

#                                                               BI-WEEKLY REPORT 

# Count aspects of the bi-weekly report
count.biweek <- counts %>% 
  filter(area!="Mouth") %>%
  group_by(year, run_timing, stream_shore, survey_type, survey_no) %>%
  summarize(date=unique(date),
            live_count1=sum(live_count_obs1, na.rm=T), live_count2=sum(live_count_obs2, na.rm=T), 
            male_carcs=sum(carc_male, na.rm=T), female_carcs=sum(carc_fem, na.rm=T), jack_carcs=sum(carc_jack, na.rm=T),
            ground_unsex=sum(carc_ground_unsex, na.rm=T), aerial_unsex1=sum(carc_aerial_unsex_obs1, na.rm=T), aerial_unsex2=sum(carc_aerial_unsex_obs2, na.rm=T),
            fem_nr=sum(carc_fem_nr, na.rm=T), fem_0=sum(carc_fem_0, na.rm=T), fem_50=sum(carc_fem_50, na.rm=T), fem_100=sum(carc_fem_100, na.rm=T),
            oe_avg=mean(oe, na.rm=T), oe_min=min(oe, na.rm=T), oe_max=max(oe, na.rm=T)) %>%
  mutate(average_live_count = round(((live_count1+live_count2)/2),0),
         total_carcasses = male_carcs+female_carcs+jack_carcs+ground_unsex+aerial_unsex1+aerial_unsex2,
         total_unsex = ground_unsex+aerial_unsex1+aerial_unsex2) %>%
  select(year:survey_no, date:live_count2, average_live_count, total_carcasses, male_carcs:aerial_unsex2, total_unsex, fem_nr:oe_max)


# Environmental aspects of the bi-weekly report 
enviro.biweek <- enviro %>% 
  group_by(year, run_timing, stream_shore, survey_type, survey_no) %>%
  summarize(water_temp=water_temp, spawn_start=peak_spawn_start, spawn_end=peak_spawn_end) %>%
  print()


# Join 
biweekly.report <- count.biweek %>%
  left_join(., enviro.biweek, by=c("year", "run_timing", "stream_shore", "survey_type", "survey_no")) %>%
  select(year:fem_100, water_temp, oe_avg:oe_max, spawn_start, spawn_end)


#################################################################################################################################################

#                                                             EXPORT TO AN EXCEL FILE

# Create a blank Excel workbook
wb <- createWorkbook()

# Populate the blank workbook with tabs (i.e., worksheets) for the daily and bi-weekly reports
addWorksheet(wb, "daily_report")
addWorksheet(wb, "biweekly_report")

# Write the summary data generated above to each of those blank worksheets 
writeData(wb, "daily_report", daily.report, startRow = 1, startCol = 1)
writeData(wb, "biweekly_report", test, startRow = 1, startCol = 1)

# Export it to an .xlsx file for viewing/further use 
saveWorkbook(wb, file = "EStu_roving_inseason_2021.xlsx", overwrite = TRUE)        # << CHANGE FILE NAME FOR EACH PROJECT >>
 












