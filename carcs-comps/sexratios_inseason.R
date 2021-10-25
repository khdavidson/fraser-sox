# In-season Roving tool FOR OPERATIONAL BIOLOGISTS
# Aug 2021


################################################################################################################################################

library(tidyverse)
library(readxl)
library(egg)

setwd("C:/DFO-MPO/#DFO-MPO_INSEASON_FILES")

lstu.counts.raw <- read_excel("Late Stuart Roving_Daily_Report_2021 (2).xlsm", sheet="Count_entry")
chilko.counts.raw <- read_excel("Chilko Roving_Daily_Report_2021.xlsm", sheet="Count_entry")

################################################################################################################################################

#                                                         CLEANING

counts <- chilko.counts.raw %>% 
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
  mutate(carc_male = ifelse(is.na(carc_male),0,carc_male),
         carc_fem = ifelse(is.na(carc_fem),0,carc_fem),
         carc_jack = ifelse(is.na(carc_jack),0,carc_jack)) %>%
  select(year:comments) %>%
  mutate_at("oe", as.numeric) %>%
  print()


################################################################################################################################################


# sex ratio over time by stream 
sr <- counts %>% 
  filter(survey_type=="Ground", !is.na(stream_shore)) %>% #, 
         #!stream_shore%in%c("Middle River","Baptiste Creek","Hudson Bay Creek","Blanchet North Creek","Hooker Creek","Leo Creek", "Macdougall Creek", "Sinta Creek", "Tliti Creek")) %>%
  group_by(stream_shore, date) %>% 
  summarize(males=sum(carc_male, na.rm=T), females=sum(carc_fem, na.rm=T), 
            live_count1=sum(live_count_obs1, na.rm=T), live_count2=sum(live_count_obs2, na.rm=T)) %>%
  mutate(male_sr = round(males/(males+females),2), female_sr = round(females/(males+females),2),
         female_sr = ifelse(female_sr=="NaN", NA, female_sr),
         live_count = round((live_count1+live_count2)/2, 0)) %>%
  group_by(stream_shore) %>%
  mutate(cuml_males=cumsum(males), cuml_females=cumsum(females),
         cuml_male_sr=round(cuml_males/(cuml_males+cuml_females),2), cuml_female_sr=round(cuml_females/(cuml_males+cuml_females),2)) %>%
  print()

# Plot of sex ratio over time 
ggplot() +
  geom_hline(yintercept = 0.5, colour="red") +
  geom_point(data=sr%>%filter(male_sr>0), aes(x=date, y=male_sr), shape=21, size=3, stroke=1, fill="blue", colour="blue", alpha=0.7) +
  geom_point(data=sr%>%filter(female_sr>0), aes(x=date, y=female_sr), shape=21, size=3, stroke=1, fill="hot pink", colour="hot pink", alpha=0.7) +
  geom_line(data=sr, aes(x=date, y=cuml_male_sr), colour="blue", size=1) +
  geom_line(data=sr, aes(x=date, y=cuml_female_sr), colour="hot pink", size=1) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.25)) +
  facet_wrap(.~stream_shore) +
  theme_bw()
  
# Plot of # carcasses 
ggplot() +
  geom_point(data=sr%>%filter(male_sr>0), aes(x=date, y=males), shape=21, size=3, stroke=1, fill="blue", colour="blue", alpha=0.7) +
  geom_point(data=sr%>%filter(female_sr>0), aes(x=date, y=females), shape=21, size=3, stroke=1, fill="hot pink", colour="hot pink", alpha=0.7) +
  #scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.25)) +
  facet_wrap(.~stream_shore, scales="free_y") +
  theme_bw()

# Plot of avg live count 
ggplot() +
  geom_bar(data=sr, aes(x=date, y=live_count), stat="identity", fill="forest green", colour="forest green", size=1, alpha=0.7) +
  #scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.25)) +
  facet_wrap(.~stream_shore, scales="free_y") +
  theme_bw()


################################################################################################################################################





