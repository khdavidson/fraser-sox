# Nautley tagging sim ## 

setwd("~/Documents/ANALYSIS/data")

library(tidyverse)
library(lubridate)
library(XLConnect)
library(xlsx)
library(openxlsx)
library(pwr)
library(WebPower)
library(lsr)
library(effsize)
library(pwr2)
library(paramtest)
library(withr)

# read data 
ind.dat <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheet = 3, detectDates = T)
catch <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheet = 2, detectDates = T)


###############################################################################################################################################

#                                                         DNA SIMULATIONS 

# Part of what helps to know how much we can tag is to know how much we need to take for DNA as this is key. 

# how many DNA samples were taken all together each day? Excluding E-Watch fish
dailydna <- ind.dat %>% 
  filter(!is.na(whatman_sheet), is.na(ewatch_fid)) %>%
  group_by(date) %>% 
  summarize(n=n()) %>%
  print()

dailycatch <- catch %>% 
  group_by(date) %>% 
  summarize(sum=sum(sox_smolts)) %>% 
  print()

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


#                                                                 POWER ANALYSIS



                                                        #####################################
                                                        # STEP 1: DISTRIBUTION OF RESIDUALS #
                                                        #####################################

# Length x weight by stock - just 1 year old fish because the only age-2s we have (n=3) create outliers (not representative)
lxw <- ind.dat %>%
  filter(!is.na(length_mm), !is.na(weight_g)) %>% 
  filter(age == "1", region1 != 0, !is.na(region1)) %>%                                 
  mutate_at(vars(c(9)), funs(as.factor)) %>%
  print()

# LENGTH normality tests - this tells me I could use an anova power analysis 
lm1 <- lm(lxw$length_mm ~ lxw$date + lxw$region1)
plot(lm1)
r1 <- resid(lm1)
hist(r1)
qqnorm(r1)
qqline(r1)   # Outliers (lines 404, 934, 1063) all seem to be fairly small for that time. Otherwise normal. 

lm2 <- lm(lxw$length_mm ~ lxw$weight_g)
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

# -----

# WEIGHT normality tests - this tells me I could use an anova power analysis 
wm1 <- lm(lxw$weight_g ~ lxw$date + lxw$region1)
plot(wm1)
wr1 <- resid(wm1)
hist(wr1)
qqnorm(wr1)
qqline(wr1) 

wm2 <- lm(lxw$weight_g ~ lxw$region1)
plot(wm2)
wr2 <- resid(wm2)
hist(wr2)
qqnorm(wr2)
qqline(wr2)
             
                                                        ########################################
                                                        # STEP 2: POWER ANALYSIS FOR EACH TYPE #
                                                        ########################################

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
  summary(lm(lxw$length_mm ~ lxw$date + lxw$region1))
  summary(aov(lxw$length_mm ~ lxw$date + lxw$region1))                    # no significant interaction effect
# Power: Regression
    # r2 full = 0.66  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.66-0)/(1-0.66) = 1.94
  wp.regression(n=NULL, p1=2, p2=0, f2=1.94, power=0.8)             # n = 9 fish
  wp.regression(n=NULL, p1=2, p2=1, f2=0.35, power=0.8)             # how much does adding date affect? n = 26 fish
  wp.regression(n=NULL, p1=2, p2=1, f2=0.85, power=0.8)             # how much does adding GSID affect? n = 13 fish
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
  summary(lm(lxw$length_mm ~ lxw$region1))  
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
  summary(lm(lxw$length_mm ~ lxw$date))    
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
  summary(lm(lxw$weight_g ~ lxw$date + lxw$region1))
  summary(aov(lxw$weight_g ~ lxw$date + lxw$region1))
# Power: Regression
    # r2 full = 0.73  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.73-0)/(1-0.73) = 2.7
  wp.regression(n=NULL, p1=2, p2=0, f2=2.7, power=0.8)                           # n = 8 fish
  wp.regression(n=NULL, p1=2, p2=1, f2=0.73, power=0.8)                          # how much does adding date affect? n = 14 fish
  wp.regression(n=NULL, p1=2, p2=1, f2=0.93, power=0.8)                          # how much does adding GSID affect? n = 12 fish
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
  summary(lm(lxw$weight_g ~ lxw$region1))
# Power: Regression - not super relevant for this comparison
    # r2 full = 0.54  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.54-0)/(1-0.54) = 1.17
  wp.regression(n=NULL, p1=2, p2=0, f2=1.17, power=0.8)                          # n = 12 fish
# Power: Correlation - not super relevant for this comparison
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
  summary(lm(lxw$weight_g ~ lxw$date))
# Power: Regression
    #r2 full = 0.48  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.48-0)/(1-0.48) = 0.92
  wp.regression(n=NULL, p1=1, p2=0, f2=0.92, power=0.8)                              # n = 11 fish
# Power: Correlation
    # R2 = 0.48
  wp.correlation(n=NULL, r=0.48, alpha=0.05, power=0.8)                              # n = 31 fish
  pwr.r.test(n=NULL, r=0.48, sig.level=0.05, power=0.8)                              # n = 31 fish
  

#####
# MODEL 4: weight ~ length           * this comparison can use the larger dataset (not limited by stock ID), but still age-1 only 
#####
lw <- ind.dat %>%
  filter(!is.na(length_mm), !is.na(weight_g)) %>% 
  filter(age == "1") %>%                                 
  print()
  
  summary(lm(lw$weight_g ~ lw$length_mm))    
# Power: Regression
    #r2 full = 0.94  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.94-0)/(1-0.94) = 15.7
    cohens_f(lm(lw$weight_g ~ lw$length_mm))     # f = 4.1
  wp.regression(n=NULL, p1=1, p2=0, f2=2.3, power=0.8)                               # n = 7 fish can't run over f2=2.3
# Power: Correlation
    # R2 = 0.95
  wp.correlation(n=NULL, r=0.94, alpha=0.05, power=0.8)                              # n = 5 fish
  pwr.r.test(n=NULL, r=0.94, sig.level=0.05, power=0.8)                              # n = 6 fish



# COLLECTIVELY, THIS SUGGESTS WE NEED A MINIMUM OF 55 FISH TO DETECT THE EXISTING LENGTH ~ DATE CORRELATION OF R=0.37. 
  # While fewer fish could be needed based on some of these results, this seems to be the limiting factor (weakest relationship). 
  # Our sampling strategy will be paired so that means need at least 55 samples (paired L and W) over time 
    # Minimum for regression is 30 so that works well. But need 55 per STOCK ideally. So need 110 over time (based on this sample size)


                                                  #####################################################
                                                  # STEP 3: POWER ANALYSIS UNDER SUB-SAMPLING REGIMES #
                                                  #####################################################

#########################
# Sampled every 3rd day # 
#########################

dates_3 <- as.Date(cut(lxw$date, breaks="3 days"))
sub3 <- lxw %>% 
  filter(date %in% dates_3) %>% 
  print()

#####
# MODEL 1: Length ~ date + stock
#####
  summary(lm(sub3$length_mm ~ sub3$date + sub3$region1))
  summary(aov(sub3$length_mm ~ sub3$date + sub3$region1))
# Power: Regression
    # r2 full = 0.64  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.64-0)/(1-0.64) = 1.8
  wp.regression(n=NULL, p1=2, p2=0, f2=1.8, power=0.8)                           # n = 10 fish
  wp.regression(n=NULL, p1=2, p2=0, f2=0.02, power=0.8)                          # Cohen small effect size 
  wp.regression(n=NULL, p1=2, p2=0, f2=0.15, power=0.8)                          # Cohen medium effect size
  wp.regression(n=NULL, p1=2, p2=0, f2=0.35, power=0.8)                          # Cohen large effect size
# Power: Correlation
    # r2 = 0.73
  wp.correlation(n=NULL, r=0.64, alpha=0.05, power=0.8)                          # n = 16 fish
  pwr.r.test(n=NULL, r=0.64, sig.level=0.05, power=0.8)                          # n = 16 fish
# Power: 2-way ANOVA 
    # Where a=region,  b=date
    # r2 = 0.64
    cohens_f(lm(sub3$length_mm ~ sub3$date + sub3$region1))    # region f=0.85, date f=1.07, 
  pwr.2way(a=2, b=60, alpha=0.05, size.A=NULL, size.B=30, f.A=0.85, f.B=1.07, power.A=0.8, power.B=0.8)

#####
# MODEL 3: length ~ date
#####
  summary(lm(sub3$length_mm ~ sub3$date)) 
  summary(aov(sub3$length_mm ~ sub3$date + sub3$region1))
# Power: Regression
    # r2 full = 0.38  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.38-0)/(1-0.38) = 0.61
  wp.regression(n=NULL, p1=2, p2=0, f2=0.61, power=0.8)                               # n = 20 fish
# Power: Correlation
    # r2 = 0.29
  wp.correlation(n=NULL, r=0.38, alpha=0.05, power=0.8)                               # n = 52 fish
  pwr.r.test(n=NULL, r=0.38, sig.level=0.05, power=0.8)                               # n = 52 fish

#-----  
  
#####
# MODEL 1: Weight ~ date + stock
#####
  summary(lm(sub3$weight_g ~ sub3$date + sub3$region1))
    cohens_f(lm(sub3$weight_g ~ sub3$date + sub3$region1))    # date f = 1.30, region f = 0.87
  summary(aov(sub3$weight_g ~ sub3$date + sub3$region1))
# Power: Regression
    # r2 full = 0.70  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.70-0)/(1-0.70) = 2.3
  wp.regression(n=NULL, p1=2, p2=0, f2=2.3, power=0.8)                           # n = 9 fish
  wp.regression(n=NULL, p1=2, p2=0, f2=0.02, power=0.8)                          # Cohen small effect size 
  wp.regression(n=NULL, p1=2, p2=0, f2=0.15, power=0.8)                          # Cohen medium effect size
  wp.regression(n=NULL, p1=2, p2=0, f2=0.35, power=0.8)                          # Cohen large effect size
# Power: Correlation
    # r2 = 0.73
  wp.correlation(n=NULL, r=0.70, alpha=0.05, power=0.8)                          # n = 13 fish
  pwr.r.test(n=NULL, r=0.70, sig.level=0.05, power=0.8)                          # n = 13 fish  
  
#####
# MODEL 3: Weight ~ date
#####
  summary(lm(sub3$weight_g ~ sub3$date))
  summary(aov(sub3$weight_g ~ sub3$date))
# Power: Regression
    #r2 full = 0.48  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.48-0)/(1-0.48) = 0.92
  wp.regression(n=NULL, p1=1, p2=0, f2=0.92, power=0.8)                              # n = 11 fish
# Power: Correlation
    # R2 = 0.48
  wp.correlation(n=NULL, r=0.48, alpha=0.05, power=0.8)                              # n = 31 fish
  pwr.r.test(n=NULL, r=0.48, sig.level=0.05, power=0.8)                              # n = 31 fish

  
  
  
#########################
# Sampled every 5th day # 
#########################

dates_5 <- as.Date(cut(lxw$date, breaks="5 days"))
sub5 <- lxw %>% 
  filter(date %in% dates_5) %>% 
  print()

#####
# MODEL 1: Length ~ date + stock
#####
  summary(lm(sub5$length_mm ~ sub5$date + sub5$region1))
    cohens_f(lm(sub5$length_mm ~ sub5$date + sub5$region1))    # date f = 1.30, region f = 0.87
  summary(aov(sub5$length_mm ~ sub5$date + sub5$region1))
# Power: Regression
    # r2 full = 0.66  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.66-0)/(1-0.66) = 1.94
  wp.regression(n=NULL, p1=2, p2=0, f2=1.94, power=0.8)                          # n = 9 fish
  wp.regression(n=NULL, p1=2, p2=0, f2=0.02, power=0.8)                          # Cohen small effect size 
  wp.regression(n=NULL, p1=2, p2=0, f2=0.15, power=0.8)                          # Cohen medium effect size 68 fish
  wp.regression(n=NULL, p1=2, p2=0, f2=0.35, power=0.8)                          # Cohen large effect size
# Power: Correlation
    # r2 = 0.73
  wp.correlation(n=NULL, r=0.66, alpha=0.05, power=0.8)                          # n = 15 fish
  pwr.r.test(n=NULL, r=0.66, sig.level=0.05, power=0.8)                          # n = 15 fish  

#####
# MODEL 3: length ~ date
#####
  summary(lm(sub5$length_mm ~ sub5$date))
  summary(aov(sub5$length_mm ~ sub5$date))
# Power: Regression
    # r2 full = 0.49  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.49-0)/(1-0.49) = 0.96
  wp.regression(n=NULL, p1=2, p2=0, f2=0.96, power=0.8)                               # n = 14 fish
# Power: Correlation
    # r2 = 0.29
  wp.correlation(n=NULL, r=0.49, alpha=0.05, power=0.8)                               # n = 30 fish
  pwr.r.test(n=NULL, r=0.49, sig.level=0.05, power=0.8)                               # n = 30 fish

#-----  
  
#####
# MODEL 1: Weight ~ date + stock
#####
  summary(lm(sub5$weight_g ~ sub5$date + sub5$region1))
    cohens_f(lm(sub5$weight_g ~ sub5$date + sub5$region1))    # date f = 1.30, region f = 0.87
  summary(aov(sub5$weight_g ~ sub5$date + sub5$region1))
# Power: Regression
    # r2 full = 0.73  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.73-0)/(1-0.73) = 2.9
  wp.regression(n=NULL, p1=2, p2=0, f2=2.9, power=0.8)                           # n = 8 fish
  wp.regression(n=NULL, p1=2, p2=0, f2=0.02, power=0.8)                          # Cohen small effect size 
  wp.regression(n=NULL, p1=2, p2=0, f2=0.15, power=0.8)                          # Cohen medium effect size 68 fish
  wp.regression(n=NULL, p1=2, p2=0, f2=0.35, power=0.8)                          # Cohen large effect size
# Power: Correlation
    # r2 = 0.73
  wp.correlation(n=NULL, r=0.73, alpha=0.05, power=0.8)                          # n = 12 fish
  pwr.r.test(n=NULL, r=0.73, sig.level=0.05, power=0.8)                          # n = 12 fish  

#####
# MODEL 3: Weight ~ date
#####
  summary(lm(sub5$weight_g ~ sub5$date))    
# Power: Regression
    #r2 full = 0.55  ,  because i'm interested in both predictors, R2 reduced=0
    # Effect size f2=(R2 full - R2 reduced)/(1-R2 full) = f2=(0.55-0)/(1-0.55) = 1.2
  wp.regression(n=NULL, p1=1, p2=0, f2=1.2, power=0.8)                              # n = 9 fish
# Power: Correlation
    # R2 = 0.48
  wp.correlation(n=NULL, r=0.55, alpha=0.05, power=0.8)                              # n = 23 fish
  pwr.r.test(n=NULL, r=0.55, sig.level=0.05, power=0.8)                              # n = 23 fish


ggplot(sub5, aes(x=date, y=length_mm, colour=region1)) +
  geom_point() +
  geom_smooth(aes(colour=region1, fill=region1), method="lm", se=T, size=1.2, alpha=0.15) 


                                                      ####################
                                                      # STEP 3: SIMULATE #
                                                      ####################

lm_test <- function(simNum, N, b1, b2, b0=0, x1m=0, x1sd=1, x2m=0, x2sd=1) {
    x1 <- rnorm(N, x1m, x1sd)
    x2 <- rnorm(N, x2m, x2sd)
    yvar <- sqrt(1 - b1^2 - b2^2)  # residual variance
    y <- rnorm(N, b0 + b1*x1 + b2*x2, yvar)
    model <- lm(y ~ x1 + x2)
    
    # pull output from model (two main effects and interaction)
    est_x1 <- coef(summary(model))['x1', 'Estimate']
    p_x1 <- coef(summary(model))['x1', 'Pr(>|t|)']
    sig_x1 <- p_x1 < .05
    est_x2 <- coef(summary(model))['x2', 'Estimate']
    p_x2 <- coef(summary(model))['x2', 'Pr(>|t|)']
    sig_x2 <- p_x2 < .05

    return(c(est_x1=est_x1, p_x1=p_x1, sig_x1=sig_x1, est_x2=est_x2, p_x2=p_x2,
        sig_x2=sig_x2))
}

# we vary N at 200 and 300; we are also setting coefficient of x predicting
# y to be approx. .15 across all simulations
power_lm <- grid_search(lm_test, params=list(N=seq(10, 100, by=1)), n.iter=1000, output='data.frame', b1=0.6, b2=0, 
    parallel='snow', ncpus=4)
res<-results(power_lm) %>%
    group_by(N.test) %>%
    summarise(power_x1=mean(sig_x1), power_x2=mean(sig_x2))

ggplot(res) +
  geom_line(aes(x=N.test,y=power_x1), colour="blue") +
  geom_line(aes(x=N.test,y=power_x2), colour="red") 




################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

                                                            ############################
                                                            # ABUNDANCE / TAGGING SIMS #
                                                            ############################

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
# 2019 Data 
#####
nad.19raw <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheet=2, detectDates = T)

# time blocks of interest
time_of_interest <- c("20:00", "20:15", "20:20", "20:30", "20:40", "20:45", "21:00", "21:30", "22:00", "22:10", "22:20", "22:30", 
                      "23:00", "23:30", "23:40", "00:00", "00:30", "01:00", "01:10", "01:30", "01:45", "02:00", "02:15", "02:25", "02:30")

# clean data, filter, order
nad.19 <- nad.19raw %>% 
  filter(trap_type == "small RST") %>%
  filter(start_time %in% time_of_interest) %>%
  print()

nad.19$start_time <- factor(nad.19$start_time, 
  levels = c("20:00", "20:15", "20:20", "20:30", "20:40", "20:45", "21:00", "21:30", "22:00", "22:10", "22:20", "22:30", "23:00", "23:30", 
    "23:40", "00:00", "00:30", "01:00", "01:10", "01:30", "01:45", "02:00", "02:15", "02:25", "02:30"), ordered=T)
nad.19$end_time <- factor(nad.19$end_time, 
  levels = c("20:00", "22:00", "22:15", "22:30", "23:00", "23:30", "00:00", "00:30", "01:00", "01:30", "01:45", "02:00", "02:15", "02:30",
    "03:00", "08:30", "09:00"), ordered=T)

################################################################################################################################################

#                                                                 HISTORICAL DATA

#######################
# RAW CATCH OVER TIME #
#######################

# For 1999 & 2000
daily <- nad.hist %>% 
  group_by(date, year) %>% 
  summarize(total = sum(total_sox)) %>% 
  mutate(yday=lubridate::yday(date)) %>%
  print()

# For 2019
daily19 <- nad.19 %>% 
  group_by(date) %>% 
  summarize(total = sum(sox_smolts)) %>% 
  mutate(year = 2019) %>%
    mutate(yday=lubridate::yday(date)) %>%
  print()

# Join
daily2 <- rbind(as.data.frame(daily), as.data.frame(daily19))

# Plot
ggplot(daily2, aes(x=as.Date(yday, origin="2000-01-01"), y=total, group=year)) +
  geom_line(aes(colour=year), size=0.9) +
  geom_point(aes(fill=year, colour=year), stroke=1.5, pch=21, size=3, alpha=0.7) + 
  scale_x_date(date_labels="%b %d", date_breaks="5 day") +
  labs(x="", y="Raw catch") +
  facet_wrap(.~year, scales = "free_y", nrow=3) +
  theme_bw() +
  theme(legend.position=c(0.1,0.9),
    legend.title=element_blank(),
    legend.text=element_text(size=13),
    legend.spacing.y = unit(0, "mm"),
    legend.background = element_rect(colour="black"),
    axis.text = element_text(colour="black", size=13), 
    axis.title = element_text(face="bold", size=15),
    strip.text = element_text(size=13))

##################################################
# PROPORTION OF RUN EACH HOUR, FOR EACH DAY-YEAR #
##################################################

# For 1999 & 2000
# total number of fish caught each "hour" 
hr.propn9900 <- nad.hist %>% 
  group_by(year, date, end_time) %>% 
  summarize(n = sum(total_sox)) %>%
  mutate(daily_total = sum(n)) %>%
  mutate(propn = (n/daily_total)*100) %>%

  group_by(year, end_time) %>% 
  summarize(avg_propn = mean(propn), sd_propn = sd(propn)) %>% 
  mutate(sd_propn = ifelse(is.na(sd_propn), 0, sd_propn)) %>%
  print()

# For 2019
hr.propn19 <- nad.19 %>% 
  group_by(date, end_time) %>% 
  summarize(n = sum(sox_smolts)) %>%
  mutate(daily_total = sum(n)) %>%
  mutate(propn = (n/daily_total)*100) %>%
  filter(date > as.Date("2019-04-26")) %>%   # this was done because prior to Apr 26 trap checks were not recorded hourly, just bulk by day
  #filter(date != "2019-04-26" | end_time != "20:00") %>%    

  group_by(end_time) %>% 
  summarize(avg_propn = mean(propn), sd_propn = sd(propn)) %>% 
  mutate(sd_propn = ifelse(is.na(sd_propn), 0, sd_propn)) %>%
  mutate(year = 2019) %>%
  print()

avg.propns <- rbind(as.data.frame(hr.propn9900), as.data.frame(hr.propn19))
avg.propns$end_time <- with_options(c(scipen = 999), str_pad(avg.propns$end_time, 5, pad = "0"))

avg.propns$end_time <- factor(avg.propns$end_time, 
  levels = c("20:00","21:00", "22:00", "22:15", "22:30", "23:00", "23:30", "00:00", "00:30", "01:00", "01:30", "01:45", "02:00", "02:15", "02:30",
    "03:00","04:00","05:00", "08:30", "09:00"), ordered=T)
avg.propns <- avg.propns %>% 
  filter(end_time != "08:30")

# PLOT
ggplot(avg.propns, aes(x=end_time, y=avg_propn, group=year)) +
  geom_ribbon(aes(ymin=avg_propn-sd_propn, ymax=avg_propn+sd_propn), linetype=2, alpha=0.15) +
  geom_line(aes(colour=year), size=0.9) +
  geom_point(aes(fill=year, colour=year), stroke=1.5, pch=21, size=3, alpha=0.7) + 
  labs(x="Time", y="Hourly proportion") +
  facet_wrap(.~year, scales = "free_y", nrow=3) +
  theme_bw() +
  theme(legend.position=c(0.9,0.9),
    legend.text = element_text(size=13),
    legend.title=element_blank(),
    legend.spacing.y = unit(0, "mm"),
    legend.background = element_rect(colour="black"), 
    axis.text = element_text(colour="black", size=13), 
    axis.title = element_text(face="bold", size=15),
    strip.text = element_text(size=13))




#                                                         TAGGING SIMULATIONS

###################################
# TAG HOURLY ABUNDANCE, UP TO 300 #
###################################

# For 1999 & 2000 
tags.hist <- nad.hist %>%
  mutate(sox_tag = paste(total_sox)) %>% 
  mutate(sox_tag = ifelse(total_sox>300, 300, total_sox)) %>%
  mutate_at(vars(c(6)), funs(as.numeric)) %>%
  print()

  # total tags by year
  y_tags <- tags.hist %>%
    group_by(year) %>%
    summarize(sum = sum(sox_tag)) %>%
    print()
  
  # total tags by day
  d_tags <- tags.hist %>%
    group_by(date) %>%
    summarize(sum = sum(sox_tag)) %>%
    print()

# For 2018
tags.18 <- nad.18 %>%
  mutate(sox_tag = paste(sox_smolts)) %>%
  mutate(sox_tag = ifelse(sox_tag>300, 300, sox_tag)) %>%
  mutate_at(vars(c(17)), funs(as.numeric)) %>%
  print()

  # total tags by day
  d_tags18 <- tags.18 %>%
    group_by(date) %>%
    summarize(sum = sum(sox_tag)) %>%
    print()

  # total tags by year
  y_tags18 <- tags.18 %>%
    summarize(applied = sum(sox_tag)) %>%
    mutate(recovered = applied*.02) %>%
    print()



  
  
  
  
  
  