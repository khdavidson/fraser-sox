# Nautley tagging sim ## 

setwd("~/ANALYSIS/Data")
library(tidyverse)
library(lubridate)
library(XLConnect)
library(xlsx)
library(openxlsx)
library(pwr)
library(WebPower)
library(lsr)
library(effsize)

#####################
# LOAD & CLEAN DATA #
#####################

#####
# 1999 & 2000 Data
#####
naut9900 <- read.csv("1999-2000 Nautley data.csv")

# reformat data 
nad.hist <- naut9900 %>%
  unite(date, date, year, sep="-") %>%
  mutate(year = paste(str_sub(date, -4))) %>%
  mutate(date = lubridate::dmy(date)) %>% 
  print

nad.hist$start_time <- factor(nad.hist$start_time, levels=c("20:00", "21:00", "22:00", "22:30", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00"), ordered=T)
nad.hist$end_time <- factor(nad.hist$end_time, levels=c("21:00", "22:00", "22:30", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00", "5:00"), ordered=T)

#####
# 2018 Data 
#####
nad.18 <- read.csv("2019 Nautley_dailydata.csv")

# time blocks of interest
time_of_interest <- c("20:00", "20:15", "20:20", "20:30", "20:40", "20:45", "21:00", "21:30", "22:00", "22:10", "22:20", "22:30", 
                      "23:00", "23:30", "23:40", "0:00", "0:30", "1:00", "1:10", "1:30", "1:45", "2:00", "2:15", "2:25", "2:30")

# clean data, filter, order
nad.18 <- nad.18 %>% 
  mutate(date = lubridate::dmy(date)) %>%
  mutate(start_date = lubridate::dmy(start_date)) %>%
  mutate(end_date = lubridate::dmy(end_date)) %>%
  filter(trap_type == "small RST") %>%
  filter(start_time %in% time_of_interest) %>%
  print()

nad.18$start_time <- factor(nad.18$start_time, 
  levels = c("20:00", "20:15", "20:20", "20:30", "20:40", "20:45", "21:00", "21:30", "22:00", "22:10", "22:20", "22:30", "23:00", "23:30", 
    "23:40", "0:00", "0:30", "1:00", "1:10", "1:30", "1:45", "2:00", "2:15", "2:25", "2:30"), ordered=T)
nad.18$end_time <- factor(nad.18$end_time, 
  levels = c("20:00", "22:00", "22:15", "22:30", "23:00", "23:30", "0:00", "0:30", "1:00", "1:30", "1:45", "2:00", "2:15", "2:30",
    "3:00", "8:30", "9:00"), ordered=T)

################################################################################################################################################

#                                                                 HISTORICAL DATA

#######################
# RAW CATCH OVER TIME #
#######################

# For 1999 & 2000
daily <- nad.hist %>% 
  group_by(date, year) %>% 
  summarize(total = sum(total_sox)) %>% 
  print()

# For 2019
daily18 <- nad.18 %>% 
  group_by(date) %>% 
  summarize(total = sum(sox_smolts)) %>% 
  mutate(year = 2019) %>%
  select(date, year, total) %>%
  print()

# Join
daily2 <- rbind(as.data.frame(daily), as.data.frame(daily18))

# Plot
ggplot(daily2, aes(x=date, y=total, group=year)) +
  geom_line(size=0.9, aes(colour=year)) +
  geom_point(pch=21, size=3, fill="white", aes(colour=year), stroke=1.5) + 
  scale_x_date(labels = function(x) format(x, "%d-%b")) +
  facet_grid(rows=vars(year))


##################################################
# PROPORTION OF RUN EACH HOUR, FOR EACH DAY-YEAR #
##################################################

# For 1999 & 2000
hr.propn <- nad.hist %>% 
  group_by(year, date, end_time) %>% 
  summarize(n = sum(total_sox)) %>%
  mutate(daily_total = sum(n)) %>%
  mutate(propn = (n/daily_total)*100) %>%
  print()

avg.hr.propn <- hr.propn %>% 
  group_by(year, end_time) %>% 
  summarize(avg_propn = mean(propn), sd_propn = sd(propn)) %>% 
  mutate(sd_propn = ifelse(sd_propn == "NaN", 0, sd_propn)) %>%
  print()

# For 2018
hr.propn18 <- nad.18 %>% 
  group_by(date, end_time) %>% 
  summarize(n = sum(sox_smolts)) %>%
  mutate(daily_total = sum(n)) %>%
  mutate(propn = (n/daily_total)*100) %>%
  filter(date >= as.Date("2019-04-26")) %>%
  print()

avg.hr.propn.18 <- hr.propn18 %>% 
  group_by(end_time) %>% 
  summarize(avg_propn = mean(propn), sd_propn = sd(propn)) %>% 
  mutate(sd_propn = ifelse(sd_propn == "NaN", 0, sd_propn)) %>%
  mutate(year = 2018) %>%
  select(year, end_time, avg_propn, sd_propn) %>%
  print()

avg.propns <- rbind(as.data.frame(avg.hr.propn), as.data.frame(avg.hr.propn.18))
avg.propns$end_time <- factor(avg.propns$end_time, 
  levels = c("20:00","21:00", "22:00", "22:15", "22:30", "23:00", "23:30", "0:00", "0:30", "1:00", "1:30", "1:45", "2:00", "2:15", "2:30",
    "3:00","4:00","5:00", "8:30", "9:00"), ordered=T)
avg.propns <- avg.propns %>% 
  filter(end_time != "8:30")

# PLOT
ggplot(avg.propns, aes(x=end_time, y=avg_propn, group=year)) +
  geom_ribbon(aes(ymin=avg.propns$avg_propn-avg.propns$sd_propn, ymax=avg.propns$avg_propn+avg.propns$sd_propn), linetype=2, alpha=0.15) +
  geom_line(size=0.9, aes(colour=year)) +
  geom_point(pch=21, size=3, fill="white", aes(colour=year), stroke=1.5) + 
  facet_grid(rows=vars(year))

###############################################################################################################################################

#                                                         DNA SIMULATIONS 

# Part of what helps to know how much we can tag is to know how much we need to take for DNA as this is key. 

setwd("~/ANALYSIS/Data")

# read data 
ind.dat <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheet = 3, detectDates = T)

# Select only DNA - n=175 sent in first round DNA
dna <- ind.dat %>% 
  filter(dna_select_bin == "1") %>% 
  print()

###################
# SUBSET BY STOCK #
###################

bystock <- dna %>% 
  group_by(region1) %>% 
  summarize(n = n()) %>% 
  print()

# Sample every 2nd fish
bystock.2 <- dna %>% 
  group_by(date) %>% 
  slice(which(row_number() %% 2 == 1)) %>% 
  print()
sample_n()

################################################################################################################################################

#                                                                 POWER ANALYSIS

# Power of 0.80 is considered appropriate 

###############
# LENGTH DATA #
###############

#####
# PRELIM DATA LOOKS 
#####

# Length x weight by stock
lxw <- ind.dat %>%
  filter(!is.na(length_mm), !is.na(weight_g)) %>% 
  filter(age == "1", region1 != 0, !is.na(region1)) %>%                                  # just did for 1 year old fish because the only 2 year olds we have (n=3) create outliers
  mutate_at(vars(c(9)), funs(as.factor)) %>%
  print()

# tests of normality - this tells me I could use an anova power analysis 
lm1 <- lm(lxw$length_mm ~ lxw$date + lxw$region1)
  aov1 <- aov(lxw$length_mm ~ lxw$date + lxw$region1)
plot(lm1)
r1 <- resid(lm1)
hist(r1)
qqnorm(r1)
qqline(r1)   # Outliers (lines 404, 934, 1063) all seem to be fairly small for that time. Otherwise normal. 

lm2 <- lm(lxw$length_mm ~ lxw$weight_g)
  aov1 <- aov(lxw$length_mm ~ lxw$date + lxw$region1)
plot(lm2)
r2 <- resid(lm2)
hist(r2)
qqnorm(r2)
qqline(r2)   # Outliers (lines 404, 279, 1067) 404 seems small, 279 seems fat, 1067 seems generally fat. Otherwise normal. 

lm3 <- lm(lxw$length_mm ~ lxw$region1)
plot(lm3)
r3 <- resid(lm3)
hist(r3)
qqnorm(r3)
qqline(r3)


             
                                                                    # POWER

# Useful stats:
group_us <- dna %>% 
  filter(region1 != 0) %>%
  group_by(region1) %>%
  summarize(mean = mean(length_mm), sd=sd(length_mm)) %>% 
  print()
pooled_s <- dna %>% 
  filter(region1 != 0) %>% 
  summarize(sd = sd(length_mm)) %>% 
  print()


##########
# LENGTH #
##########

#####
# MODEL 1: length ~ date + stock
#####
lm1 <- lm(lxw$length_mm ~ lxw$date + lxw$region1)

# Power: Regression
  # r2 full = 0.66  ,  because i'm interested in both predictors, R2 reduced=0
  # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.66-0)/(1-0.66) = 1.94
wp.regression(n=NULL, p1=2, p2=0, f2=1.94, power=0.8)             # n = 9 fish
wp.regression(n=NULL, p1=2, p2=0, f2=0.02, power=0.8)             # Cohen small effect size 
wp.regression(n=NULL, p1=2, p2=0, f2=0.15, power=0.8)             # Cohen medium effect size
wp.regression(n=NULL, p1=2, p2=0, f2=0.35, power=0.8)             # Cohen large effect size
# Power: Correlation
  # r2 = 0.66
wp.correlation(n=NULL, r=0.66, alpha=0.05, power=0.8)             # n = 15 fish
pwr.r.test(n=NULL, r=0.66, sig.level=0.05, power=0.8)             # n = 15 fish


#####
# MODEL 2: length ~ stock
#####
lm.r1 <- lm(lxw$length_mm ~ lxw$region1)    

# Power: Regression - not really relevant to this model
  # r2 full = 0.54  ,  because i'm interested in both predictors, R2 reduced=0
  # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.53-0)/(1-0.53) = 1.94
wp.regression(n=NULL, p1=2, p2=0, f2=1.12, power=0.8)                                # n = 12 fish
# Power: Correlation - not really relevant to this model
  # r2 = 0.66
wp.correlation(n=NULL, r=0.53, alpha=0.05, power=0.8)                                # n = 25 fish
pwr.r.test(n = NULL, r = 0.53, sig.level = 0.05, power = 0.8)                        # n = 25 fish.
# Power: t-test
  cohensD(lxw$length_mm ~ lxw$region1, method="pooled")
pwr.t.test(n=NULL, d=2.14, sig.level=0.05, power=0.8, type = "two.sample")           # n = 5 fish in each group

#####
# MODEL 3: length ~ date
#####
lm.r2 <- lm(lxw$length_mm ~ lxw$date)    

# Power: Regression
  # r2 full = 0.37  ,  because i'm interested in both predictors, R2 reduced=0
  # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.37-0)/(1-0.37) = 0.59
wp.regression(n=NULL, p1=2, p2=0, f2=0.59, power=0.8)                               # n = 20 fish
# Power: Correlation
  # r2 = 0.37
wp.correlation(n=NULL, r=0.37, alpha=0.05, power=0.8)                               # n = 55 fish
pwr.r.test(n=NULL, r=0.37, sig.level=0.05, power=0.8)                               # n = 55 fish


##########
# WEIGHT #
##########

#####
# MODEL 1: Weight ~ date + stock
#####
lm2 <- lm(lxw$weight_g ~ lxw$date + lxw$region1)

# Power: Regression
  # r2 full = 0.73  ,  because i'm interested in both predictors, R2 reduced=0
  # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.73-0)/(1-0.73) = 2.7
wp.regression(n=NULL, p1=2, p2=0, f2=2.7, power=0.8)                           # n = 8 fish
wp.regression(n=NULL, p1=2, p2=0, f2=0.02, power=0.8)                          # Cohen small effect size 
wp.regression(n=NULL, p1=2, p2=0, f2=0.15, power=0.8)                          # Cohen medium effect size
wp.regression(n=NULL, p1=2, p2=0, f2=0.35, power=0.8)                          # Cohen large effect size
# Power: Correlation
  # r2 = 0.73
wp.correlation(n=NULL, r=0.73, alpha=0.05, power=0.8)                          # n = 12 fish
pwr.r.test(n=NULL, r=0.73, sig.level=0.05, power=0.8)                          # n = 12 fish


#####
# MODEL 2: Weight ~ stock
#####
lm.wr1 <- lm(lxw$weight_g ~ lxw$region1)    

# Power: Regression
  # r2 full = 0.54  ,  because i'm interested in both predictors, R2 reduced=0
  # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.54-0)/(1-0.54) = 1.17
wp.regression(n=NULL, p1=2, p2=0, f2=1.17, power=0.8)                          # n = 12 fish
# Power: Correlation
  # r2 = 0.54
wp.correlation(n=NULL, r=0.54, alpha=0.05, power=0.8)                          # n = 24 fish
pwr.r.test(n=NULL, r=0.54, sig.level=0.05, power=0.8)                          # n = 24 fish
# Power: ANOVA
  cohens_f(aov(lxw$weight_g ~ lxw$region1 + lxw$date))     # f = 1.44 and 0.87 for region and date, respectively 
  # pi = ni/N (0.51 and 0.49)
  # ni = number of obs in group i (86 and 82)
  # N = total number of obs (168)
  # ui = mean group i (115 and 96.9 mm)
  # u = grand mean (106.5 mm)
  # s2 = error variance within groups. sds 7.6 and 9.5. se=sd/sqrt(n)=7.6/sqrt(86)=0.82. variance=se^2*n=57.62  //  se=sd/sqrt(n)=9.5/sqrt(82)=1.04. variance=se^2*n=88.56
  # f = sqrt((sum  pi*(ui - u)^2)/s2)
    # nadina: (0.51*(115 - 106.5)^2)/27.62 = 1.33
    # stellako: (0.49*(96.9-106.5)^2)/88.56 = 0.51
    # f = sqrt(2.92+3.58) = 2.54
pwr.anova.test(k=2, n=NULL, f=1.44, sig.level=0.05, power=0.8)                # n = 4 per group 
pwr.anova.test(k=2, n=NULL, f=0.87, sig.level=0.05, power=0.8)                # n = 7 per group 
wp.anova(f=1.44, k=2, n=NULL, alpha=0.05, power=0.8)                          # n = 7 fish overall 
wp.anova(f=0.86, k=2, n=NULL, alpha=0.05, power=0.8)                          # n = 13 fish overall
# Power: t-test 
  # d = |u1 - u2|/s2 , ui = group means, s2 = common error variance. 
    # se=12.6/sqrt(168)=0.972 , var=(0.972)^2*168 = 158.72
    # d = (115-96.9)/158.72=0.11
  cohensD(lxw$weight_g ~ lxw$region1, method="pooled")      # d = 2.15
pwr.t.test(n=NULL, d=2.15, sig.level=0.05, power=0.8)                         # n = 5 per group

  
#####
# MODEL 3: Weight ~ date
#####
lm.wr2 <- lm(lxw$weight_g ~ lxw$date)    

# Power: Regression
  #r2 full = 0.48  ,  because i'm interested in both predictors, R2 reduced=0
  # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.48-0)/(1-0.48) = 0.92
wp.regression(n=NULL, p1=1, p2=0, f2=0.92, power=0.8)                              # n = 11 fish
# Power: Correlation
  # R2 = 0.48
wp.correlation(n=NULL, r=0.48, alpha=0.05, power=0.8)                              # n = 31 fish
pwr.r.test(n=NULL, r=0.48, sig.level=0.05, power=0.8)                              # n = 31 fish
  

#####
# MODEL 3: weight ~ length
#####
lm.wl <- lm(lxw$weight_g ~ lxw$length_mm)    

# Power: Regression
  #r2 full = 0.95  ,  because i'm interested in both predictors, R2 reduced=0
  # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.95-0)/(1-0.95) = 19
  cohens_f(lm.wl)     # f = 4.4
wp.regression(n=NULL, p1=1, p2=0, f2=2.4, power=0.8)                               # n = 7 fish can't run over f2=2.3
# Power: Correlation
  # R2 = 0.95
wp.correlation(n=NULL, r=0.95, alpha=0.05, power=0.8)                              # n = 5 fish
pwr.r.test(n=NULL, r=0.95, sig.level=0.05, power=0.8)                              # n = 6 fish



# COLLECTIVELY, THIS SUGGESTS WE NEED A MINIMUM OF 55 FISH TO DETECT THE EXISTING LENGTH ~ DATE CORRELATION OF R=0.37. 
  # While fewer fish could be needed based on some of these results, this seems to be the limiting factor (weakest relationship). 
  # Our sampling strategy will be paired so that means need at least 55 samples (paired L and W) over time 
    # Minimum for regression is 30 so that works well. But need 55 per STOCK ideally. So need 110 over time (based on this sample size)


##################################### SAMPLING SIMULATION 

# What happens if we sampled every 3rd day - would the relationships continue? 










#                                                         TAGGING SIMULATIONS

#######################################
# TAG 50% HOURLY ABUNDANCE, UP TO 300 #
#######################################

# For 1999 & 2000 
nad.hist <- nad.hist %>%
  mutate(sox_50 = total_sox*0.5) %>% 
  mutate(sox_50 = ifelse(sox_50>300, 300, sox_50))

# For 2018
nad.18 <- nad.18 %>%
  mutate(sox_50 = sox_smolts*0.50) %>%
  mutate(sox_50m = ifelse(sox_50>300, 300, sox_50)) %>%
  print()













  # total tags by year
  y_tags <- nad.hist %>%
    group_by(year) %>%
    summarize(sum = sum(sox_50)) %>%
    print()
  
  # total tags by day
  d_tags <- nad.hist %>%
    group_by(date) %>%
    summarize(sum = sum(sox_50)) %>%
    print()

# hourly abundance 
ggplot(nad.hist, aes(x=hour, y=total_sox, group=date, colour=year)) + 
  geom_point(aes(colour=year), size=2.5, fill="white", pch=21, stroke=1.3) +
  geom_line(size=1) +
  facet_grid(~year)



#############
# 2018 data #
#############

# tags by date
d.tag.18 <- nad.18 %>% 
  group_by(date) %>% 
  summarize(sum=sum(sox_50)) %>% 
  print

# plot 
ggplot(nad.18, aes(x=start_time, y=sox_smolts, group=date)) +
  geom_line()