# Fecundity work 

# libraries
library(tidyverse)
library(XLConnect)
library(xlsx)
library(openxlsx)
library(stringr)
library(withr)

# read
setwd("~/ANALYSIS/Data")
fdat <- read.xlsx("Sockeye Fecundity Database_Feb2019.xlsx", sheet=1, detectDates=T)

# clean 
fec <- fdat %>%
  select(1:16) %>%
  rename(run_timing = Timing.Group,
    watershed_group = Watershed.Group.Name,
    stock = `Stock.Name(stream)`,
    year = Year,
    fish_no = FishNo,
    age = Age,
    length_std = Std.Length,
    weight_total = Total.Weight,
    weight_sub = Sub.Weight,
    count_sub = Sub.Count,
    count_total = Total.Count,
    misc_eggs = Misc.Eggs,
    dish_dw = `Dish.Dry.Weight.Grams`,
    dish_egg_dw = `Dish&Eggs.Dry.Weight.Grams`,
    prespawn = `Pre-spawn.(Y/N)`,
    comments = Comments) %>% 
  mutate(weight_sub = ifelse(weight_sub == "N/A", NA, weight_sub)) %>%
  mutate(count_sub = ifelse(count_sub == "N/A", NA, count_sub)) %>%
  mutate(age = ifelse(age == "N/A", NA, age)) %>%
  mutate_at(vars(c(6,9:10)), funs(as.numeric)) %>%
  mutate(age = age/10) %>%                                                 # divded by 10 to make easier to split at decimal place
  mutate(prespawn = ifelse(prespawn=="N/A", NA, prespawn)) %>%
  mutate(ufid = paste(year, seq(1:length(year)), sep="-")) %>%
  separate(age, c("age_total", "age_fwater")) %>%
  mutate(age_total = ifelse(age_total==0, age_fwater, age_total)) %>%
  mutate_at(vars(c(6:7)), funs(as.numeric)) %>% 
  mutate(age_fwater = ifelse(age_fwater>3, NA, age_fwater)) %>%
  mutate(length_std = ifelse(length_std==0.0, NA, length_std)) %>%
  print()



################
# CALCULATIONS #
################

# prelim calculations 
fec <- fec %>% 
  mutate(count_total = ifelse(count_total==0, NA, count_total)) %>%
  mutate(egg_dw = dish_egg_dw - dish_dw) %>% 
  mutate(fecundity_est = (count_sub/weight_sub)*weight_total)%>% 
  mutate(fecundity_emp = ifelse(!is.na(count_total), count_total, NA)) %>%
  mutate(fecundity_check = ifelse(is.na(fecundity_emp), NA, abs(fecundity_emp-fecundity_est))) %>% 
  mutate(fecundity = ifelse(!is.na(fecundity_emp), fecundity_emp, fecundity_est)) %>%
  mutate(egg_dw = egg_dw/10) %>%                                     # egg dry weight is a total weight for 10 eggs
  print()

      # outliers where (fecundity_emp - fecundity_est) > 50
      outliers <- fec %>% 
        filter(fecundity_check > 50) %>% 
        print()
      
      # 471 cases where >50 egg discrepancy


####################
# DATA EXPLORATION #
####################

# by age over time ~ prespawn
ages <- c(3, 4, 5)
p1 <- fec %>% 
  filter(!is.na(age_total), !is.na(prespawn), prespawn != "Not Recorded", age_total %in% ages) %>%
  mutate_at(vars(c(6)), funs(as.factor)) %>%
  #group_by(year, age_total, prespawn) %>%
  #summarize(mean=mean(fecundity, na.rm=T), sd=sd(fecundity, na.rm=T), mean_length=mean(length_std, na.rm=T), sd_length=sd(length_std, na.rm=T)) %>% 
  print()

ggplot(p1) +
  geom_point(aes(x=year, y=fecundity, fill=age_total, colour=age_total)) +
  geom_smooth(aes(x=year, y=fecundity, colour=age_total, fill=age_total), method="lm", se=T, size=1, alpha=0.15) +
  scale_x_continuous(breaks=seq(1960,2020,by=15)) +
  theme_bw() +
  facet_wrap(~prespawn, nrow=2, ncol=1)

ggplot(p1) +
  geom_point(aes(x=year, y=length_std, fill=age_total, colour=age_total)) +
  geom_smooth(aes(x=year, y=length_std, colour=age_total, fill=age_total), method="lm", se=T, size=1, alpha=0.15) +
  scale_x_continuous(breaks=seq(1960,2020,by=15)) +
  theme_bw() +
  facet_wrap(~prespawn, nrow=2, ncol=1)

# by over time ~ prespawn -> 5 year olds def drive this trend (DC work)
p1 <- fec %>% 
  mutate_at(vars(c(6)), funs(as.factor)) %>%
  filter(!is.na(age_total), !is.na(prespawn), prespawn != "Not Recorded") %>%
  group_by(year, prespawn) %>%
  summarize(mean=mean(fecundity,na.rm=T), sd=sd(fecundity, na.rm=T)) %>% 
  print()

ggplot(p1, aes(x=year, y=mean)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(1960,2020,by=15)) +
  theme_bw() +
  facet_wrap(~prespawn, nrow=2, ncol=1)




# TO PICK UP NEXT TIME: 
# determine distribution of data 
# consider glmm: fecundity ~ age + stock + year + prespawn + size + age*stock + age*year + stock*year (?)
  # need to test for correlations between these (e.g., size and stock)
# consider with lenght ~ ... 





  







