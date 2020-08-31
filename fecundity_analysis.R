# Fecundity work - transcribing into R work done in 2017 by D Couture and S. Decker
# transcribed 2019/2020 by K Davidson


###################################################################################################################################################

library(tidyverse)
library(XLConnect)
library(xlsx)
library(openxlsx)
library(stringr)
library(withr)

setwd("~/Documents/ANALYSIS/data")

fdat <- read.xlsx("Sockeye Fecundity Database_Feb2019.xlsx", sheet=1, detectDates=T)

###################################################################################################################################################

#                                                               CLEANING CODE


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
  mutate(ufid = paste(year, seq(1:length(year)), sep="-")) %>%                                                               # create unique fish identifier (ufid)
  mutate(weight_sub = ifelse(weight_sub == "N/A", NA, weight_sub)) %>%                                                       # change "N/A" text to true NA
  mutate(count_sub = ifelse(count_sub == "N/A", NA, count_sub)) %>%                                                          # change "N/A" text to true NA
  mutate(age = ifelse(age == "N/A", NA, age)) %>%                                                                            # change "N/A" text to true NA
  mutate(prespawn = ifelse(prespawn=="N/A", NA, prespawn)) %>%                                                               # change "N/A" text to true NA
  mutate_at(c("age", "weight_sub", "count_sub"), as.numeric) %>%                                                             # change variable format to numeric
  mutate(age = ifelse(age>9, age/10, age)) %>%                                                                               # if age is double digits, divded by 10 to make easier to split at decimal place
  separate(age, c("age_total", "age_fwater")) %>%                                                                            # split age into adult (total) and freshwater ages
  mutate_at(c("age_total", "age_fwater"), as.numeric) %>%                                                                    # change variable format to numeric
  mutate(length_std = ifelse(length_std==0.0, NA, length_std)) %>%                                                           # if standard length was 0.0 (not possible), make it NA
  mutate(count_total = ifelse(count_total==0, NA, count_total)) %>%                                                          # these aren't true zeroes, they were poorly-entered data (not counted so should be NA)
  select(ufid, run_timing:comments) %>%                                                                                      # re-arrange columns
  print()


#------Explanation on age separations:   
# Some ages were entered using subscript notation (e.g., "42"), and some were just entered as adult ages (e.g., "4"). 
# Ages above 9 (i.e., 31, 32, 41, 42, 52, etc.) were divided by 10 to insert a decimal place between the subscript ages (e.g., "42" became "4.2"). 
# This allows for easy splitting at the decimal. Ages that were entered as 3, 4, 5, etc. became "3.0", "4.0" etc.
# Ages were then split at the decimal into total age ("4") and freshwater age ("2").
# However, for fish that had no freshwater age recorded (i.e., were "3.0", "4.0", etc.) instead of splitting into "3" and "0", R defaulted to "3"
# and "NA". That is OK because the freshwater age is unknown, so NA is more appropriate than 0.

# **** HOWEVER! If any true 3sub0 or 4sub0 fish are ever entered into this database, THE ABOVE CODE from lines 47-48 must be revamped as the true
# **** 0 freshwater age will be lost! (As of July 2020 there were no age-0 fish)
#-------

# Also note there are a lot of fish with POH or fork lengths entered in the comments. There is no other fecundity data associated with them though
# so have not extracted that information for now, but may want to in the future. 

###################################################################################################################################################

#                                                             CALCULATING FECUNDITY


# Fecundity is measured in two ways: 
# 1. A sub-sample of skeins from each site, each year are fully counted (each egg counted) and weighed (= "EMPIRICAL FECUNDITY")
# 2. For the rest, a sub-sample of eggs are counted and weighed, and a mathemetical calculation is applied to expand to estimate fecundity(= "ESTIMATED FECUNDITY")



# prelim calculations 
fec <- fec %>% 
  mutate(egg_dw = dish_egg_dw - dish_dw) %>%                                                             # (dish and egg dry weight)-(dish weight) = egg dry weight
  mutate(fecundity_est = (count_sub/weight_sub)*weight_total)%>%                                         # estimated fecundity: ((# eggs in sub-sample/weight of sub-sample)*total skein weight)
  mutate(fecundity_emp = ifelse(!is.na(count_total), count_total+misc_eggs, NA)) %>%                     # take total count + lost eggs (misc_eggs) as empirical fecundity
  mutate(fecundity = ifelse(!is.na(fecundity_emp), fecundity_emp, fecundity_est)) %>%                    # final fecundity, either empirical or estimated
  mutate(fecundity_check = ifelse(is.na(fecundity_emp), NA, abs(fecundity_emp-fecundity_est))) %>%       # for skeins where both a sub-sample and total counts were taken for ground-truthing, a check to calculate the difference 
  mutate(egg_dw_ind = egg_dw/10) %>%                                                                     # egg dry weight is a total weight for 10 eggs
  print()

# outliers where (fecundity_emp - fecundity_est) > 50
outliers <- fec %>% 
  filter(fecundity_check > 50) %>% 
  print()
# 533 cases where >50 egg discrepancy

write.csv(fec, "fecundity_r_working.csv", row.names=F)

###################################################################################################################################################

#                                                                      DATA EXPLORATION 


#----------------- Fecundity ~ Estimated or Empirical  --> no difference 
e.vs.e <- fec %>% 
  filter(!is.na(fecundity_check)) %>% 
  gather("method", "fecundity_val", 20:21) %>%
  group_by(method) %>%
  summarize(avg = mean(fecundity_val), sd = sd(fecundity_val), n=n()) %>%
  print()

ggplot(e.vs.e, aes(x=method, y=avg)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd), width=0.2)


#----------------- Fecundity and size ~ age  
# --> 5 yr olds most fecund (likely due to small n for 6 and 7 year olds--uncertain data)
# --> 5, 6 and 7 yr olds largest, likely sig b/w 5 and 3 YO but not 4 and 5
age <- fec %>% 
  filter(!is.na(age_total)) %>% 
  group_by(age_total) %>% 
  summarize(fec_avg = mean(fecundity, na.rm=T), sd_fec = sd(fecundity, na.rm=T), std_avg=mean(length_std, na.rm=T), std_sd=sd(length_std, na.rm=T)) %>%
  print()

ggplot(age, aes(x=age_total, y=fec_avg)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=fec_avg-sd_fec, ymax=fec_avg+sd_fec), width=0.2)

ggplot(age, aes(x=age_total, y=std_avg)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=std_avg-std_sd, ymax=std_avg+std_sd), width=0.2)


#----------------- Fecundity ~ Pre-spawn or sacrifice  --> no difference in fecundity between pre-spawn and sacrifice fish
p_vs_s <- fec %>% 
  filter(prespawn != "Not Recorded", !is.na(prespawn)) %>%
  group_by(prespawn) %>%
  summarize(fec_avg = mean(fecundity, na.rm=T), sd_fec = sd(fecundity, na.rm=T)) %>% 
  print()

ggplot(p_vs_s, aes(x=prespawn, y=fec_avg)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=fec_avg-sd_fec, ymax=fec_avg+sd_fec), width=0.2)


#----------------- How many samples per system per year do we have? 
samples <- fec %>% 
  group_by(stock,year) %>%
  summarize(n=n()) %>%
  arrange(n) %>%
  print()

# Over time, Bivouac, Paula, Narrows and Felix creek have <10 samples total


#----------------- Fecundity ~ population  --> sig effect, but is it because of age, or latitude? 

# Number of unique populations 
length(unique(samples$stock))

# Remove populations with less than 10 fecundity samples overall
stocks_lessthan_10 <- c("Bivouac Creek", "Paula Creek", "Narrows Creek", "Felix Creek")

FvP_dat <- fec %>% 
  filter(!(stock %in% stocks_lessthan_10)) %>% 
  print()

FvP_sum <- FvP_dat %>% 
  group_by(stock) %>%
  summarize(fec_avg=mean(fecundity, na.rm=T), fec_sd=sd(fecundity, na.rm=T)) %>% 
  print()

ggplot(FvP_sum, aes(x=reorder(stock, -fec_avg), y=fec_avg)) +
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=fec_avg-fec_sd, ymax=fec_avg+fec_sd), width=0) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# Test 
FvP_lm <- lm(fecundity ~ stock, data=FvP_dat)
resid_FvP <- resid(FvP_lm)
hist(resid_FvP)
plot(resid_FvP)
qqnorm(resid_FvP)
qqline(resid_FvP)

summary(aov(fecundity ~ stock, data=FvP_dat))

# Re-do for only 4 year olds 
FvP4_dat <- fec %>% 
  filter(!(stock %in% stocks_lessthan_10), age_total==4) %>%
  print()

FvP4_lm <- lm(fecundity ~ stock, data=FvP4_dat)
resid_FvP4 <- resid(FvP4_lm)
hist(resid_FvP4)
plot(resid_FvP4)
qqnorm(resid_FvP4)
qqline(resid_FvP4)

summary(aov(fecundity ~ stock, data=FvP4_dat))


# Re-do for only 4 year olds and by watershed group (closest to latitude proxy we have right now, also kind of encompasses run-timing)
unique(FvP4_dat$watershed_group)

FvW4_lm <- lm(fecundity ~ watershed_group, data=FvP4_dat)
resid_FvW4 <- resid(FvW4_lm)
hist(resid_FvW4)
plot(resid_FvW4)
qqnorm(resid_FvW4)
qqline(resid_FvW4)

summary(aov(fecundity ~ watershed_group, data=FvP4_dat))
TukeyHSD(aov(fecundity ~ watershed_group, data=FvP4_dat))


#----------------- Size ~ population  

SvP_sum <- fec %>% 
  group_by(stock) %>%
  summarize(size_avg=mean(length_std, na.rm=T), size_sd=sd(length_std, na.rm=T)) %>% 
  print()

ggplot(SvP_sum, aes(x=reorder(stock, -size_avg), y=size_avg)) +
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=size_avg-size_sd, ymax=size_avg+size_sd), width=0) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

summary(aov(length_std ~ stock, data=fec))
TukeyHSD(aov(length_std ~ stock, data=fec))





###################################################################################################################################################

#                                                           FECUNDITY AND SIZE OVER TIME

# Only do for main ages 
ages <- c(3, 4, 5)

fec_temp <- fec %>%
  mutate_at(vars(c(7)), funs(as.factor)) %>%
  filter(age_total %in% ages) %>% 
  #group_by(year, age_total) %>% 
  #summarize(fec_avg = mean(fecundity, na.rm=T), sd_fec = sd(fecundity, na.rm=T)) %>% 
  print()

#----------------- Fecundity ~ time
summary(lm(fec_temp[fec_temp$age_total=="5",]$fecundity ~ fec_temp[fec_temp$age_total=="5",]$year))
summary(lm(fec_temp[fec_temp$age_total=="4",]$fecundity ~ fec_temp[fec_temp$age_total=="4",]$year))

ggplot(fec_temp) +
  geom_point(aes(x=year, y=fecundity, fill=age_total, colour=age_total), shape=21, size=4, alpha=0.5) +
  geom_smooth(aes(x=year, y=fecundity, colour=age_total, fill=age_total), method="lm", se=T, size=1, alpha=0.15) +
  scale_x_continuous(breaks=seq(1960,2020,by=15)) +
  theme_bw() 
# I would remove the one 3yo point from 1986 as it does not seem to be reliable. 

View(fec_temp %>% group_by(year, age_total) %>% summarize(mean=mean(fecundity, na.rm=T), sd=sd(fecundity, na.rm=T)))

#----------------- Length ~ time
ggplot(fec_temp) +
  geom_point(aes(x=year, y=length_std, fill=age_total, colour=age_total), shape=21, size=4, alpha=0.5) +
  geom_smooth(aes(x=year, y=length_std, colour=age_total, fill=age_total), method="lm", se=T, size=1, alpha=0.15) +
  scale_x_continuous(breaks=seq(1960,2020,by=15)) +
  theme_bw() 

View(fec_temp %>% group_by(year, age_total) %>% summarize(mean=mean(length_std, na.rm=T), sd=sd(length_std, na.rm=T)))

summary(lm(fec_temp[fec_temp$age_total=="5",]$length_std ~ fec_temp[fec_temp$age_total=="5",]$year))
summary(lm(fec_temp[fec_temp$age_total=="4",]$length_std ~ fec_temp[fec_temp$age_total=="4",]$year))

# As expected, the linear trends of fecundity~time and size~time are very similar, because fecundity~size is a strong relationship. How strong: 


#----------------- Fecundity ~ length
ggplot(fec_temp) +
  geom_point(aes(x=length_std, y=fecundity, fill=age_total, colour=age_total), shape=21, size=3, alpha=0.4) +
  #geom_point(aes(x=length_std, y=log(fecundity), fill=age_total, colour=age_total), shape=21, size=3, alpha=0.4) +  # LOG(Y) COMPARISON
  scale_fill_manual(values = c("3"="red", "4"="gray", "5"="blue")) +
  scale_colour_manual(values = c("3"="red", "4"="gray", "5"="blue")) +
  theme_bw() 


####################################################################################################################################################

#                                                        FECUNDITY ~ SIZE + AGE DEEPER DIVE

#########
# STATS #
#########

# The relationship is likely best fit by an exponential relationship (as is known), but examine normality etc anyway
lm_fecsize <- lm(fec_temp$fecundity ~ fec_temp$length_std)
r_fs <- resid(lm_fecsize)
hist(r_fs)
plot(r_fs)
qqnorm(r_fs)
qqline(r_fs)
# Data very peaked - non-normal as expected 


# Assess log-linear model vs. exponential model 
summary(lm(log(fecundity) ~ length_std, data=fec_temp))
rlm <- resid(lm(log(fecundity) ~ length_std, data=fec_temp))
hist(rlm)
plot(rlm)
qqnorm(rlm)
qqline(rlm)

exp <- lm(fecundity ~ exp(length_std), data=fec_temp)


# Basic plot w/ cubic function
cubicline=function(coeff=1,x.vals=c(0:100),...){
    lines(x.vals,coeff*x.vals^3,...)
}

plot(fecundity ~ length_std, data=fec_temp) 
cubicline(coeff=0.022,x.vals=seq(40,75,5),col="red") 


###################################################################################################################################################

#                                                           BUILD MASTER SPREADSHEET 

## Now we have done some data exploration, we know: 
# There are very few 6 and 7 year olds in the dataset. These should likely be removed - the key ages are 3, 4 and 5 year olds
# There is one 3 year old fecundity from 1986 that seems unreliable - it could be removed
# Remove the systems with fewer than 10 samples (each year ?? )
ages_of_interest <-c("3", "4", "5")

fec_master <- fec %>% 
  filter(age_total %in% ages_of_interest, !(stock %in% stocks_lessthan_10)) %>%
  print()
  









# TO PICK UP NEXT TIME: 
# determine distribution of data 
# consider glmm: fecundity ~ age + stock + year + length + age*stock + age*length
  # need to test for correlations between these (e.g., size and stock)





  







