# In-season roving sex ratio examination 
# Aug 2021


################################################################################################################################################

library(tidyverse)
library(readxl)

setwd("C:/DFO-MPO/#DFO-MPO_INSEASON_FILES")

estu.counts.raw <- read_excel("Early Stuart Roving_Daily_Report_2021 (edited Aug_12).xlsm", sheet="Count_entry")


################################################################################################################################################

#                                                         CLEANING

estu.counts <- estu.counts.raw %>% 
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


################################################################################################################################################


# sex ratio over time by stream 
sr <- estu.counts %>% 
  filter(survey_type=="Ground", !is.na(stream_shore), 
         !stream_shore%in%c("Middle River","Baptiste Creek","Hudson Bay Creek","Blanchet North Creek","Hooker Creek","Leo Creek", "Macdougall Creek", "Sinta Creek", "Tliti Creek")) %>%
  group_by(stream_shore, date) %>% 
  summarize(males=sum(carc_male, na.rm=T), females=sum(carc_fem, na.rm=T)) %>%
  mutate(male_sr = round(males/(males+females),2), female_sr = round(females/(males+females),2)) %>%
  mutate(male_sr = ifelse(male_sr=="NaN", NA, male_sr),
         female_sr = ifelse(female_sr=="NaN", NA, female_sr)) %>%
  print()

# Plot of sex ratio
ggplot() +
  geom_point(data=sr%>%filter(male_sr>0), aes(x=date, y=male_sr), shape=21, size=2, fill="blue") +
  geom_point(data=sr%>%filter(female_sr>0), aes(x=date, y=female_sr), shape=21, size=2, fill="magenta") +
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.25)) +
  facet_wrap(.~stream_shore)
  
# Plot of # carcasses 
ggplot() +
  geom_point(data=sr%>%filter(males>0), aes(x=date, y=males), shape=21, size=2, fill="blue") +
  geom_point(data=sr%>%filter(females>0), aes(x=date, y=females), shape=21, size=2, fill="magenta") +
  #scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.25)) +
  facet_wrap(.~stream_shore, scales="free_y")




















