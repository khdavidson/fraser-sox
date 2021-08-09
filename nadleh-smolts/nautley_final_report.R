# NAUTLEY 2019 FINAL REPORT CODE
# 29-Jan-2020
# All DNA, scales, length and weight data to be run by DFO are now here.
# More data may come from E-Watch: DNA and physiology 

# libraries and wd
library(tidyverse)
library(readxl)
library(withr)
library(scales)
library(strucchange)

setwd("~/ANALYSIS/Data")


# read data used for all sections below
dat <- read_excel("nadleh_ANALYTICAL_database_2019.xlsx", sheet=3)          # might be very slow! sometimes 'sheet' may have to be changed to 'sheetIndex' depending on the order packages are loaded

# quick re-code so that figures display real names not numbers. Based on metadata table in sheet1 of the Excel file 
dat <- dat %>% 
  mutate(NEWregion1 = ifelse(NEWregion1==4, "Nadina", ifelse(NEWregion1 ==12, "Stellako", NEWregion1))) %>%
  mutate_at("weight_g", as.numeric) %>% 
  mutate(cf = (weight_g/length_mm^3)*100000) %>%
  print()

# note from B. Butler June 2020: the RST was moved once, 

####################################################################################################################################################

                                                            ##################
                                                            # SAMPLE SUMMARY #  
                                                            ##################

# How many fish were sampled for a full 'suite' of samples (length, weight, DNA, scales)?
# removing entires where: 
## whatman sheet = NA (i.e., no DNA taken)
## psc book # = NA (i.e., no scales taken)
## weight = NA (i.e., no weight recorded)
## length = NA (i.e., no length recorded)
dat %>% 
  filter(!is.na(whatman_sheet) & !is.na(psc_book_no) & !is.na(weight_g) & !is.na(length_mm)) %>%
  summarize(n=n()) %>%
  print()



####################################################################################################################################################

                                                              ################
                                                              # DNA ANALYSIS #
                                                              ################

############
# BAD GSID #
############
# need to determine: total # sampled, total # sent for gsid, total # not good, and of not good, the # that did not amplify

# total # sampled 
dat %>% 
  mutate(whatman_sheet = ifelse(is.na(whatman_sheet), "unsampled", "sampled")) %>%
  group_by(whatman_sheet) %>% 
  summarize(n=n()) 
# 1183 smolts were sampled for DNA (2889 unsampled)

# total # sent away for GSID analysis
dat %>%
  filter(!is.na(whatman_sheet)) %>%
  mutate(dna_select_bin = ifelse(is.na(dna_select_bin), "unsent", "sent")) %>%
  group_by(dna_select_bin) %>%
  summarize(n=n()) 
# 441 were sent away for analysis (742 available but un-analyzed)

# total # not good // total # not amplified 
dat %>%
  mutate(dna_select_bin = ifelse(is.na(dna_select_bin), "unsent", "sent")) %>%
  filter(dna_select_bin == "sent") %>%
  filter(NEWprob1<0.8 | prob1<0.8 | grepl('did not amplify', dna_comment)) %>%    
  group_by(grepl('did not amplify', dna_comment)) %>%           # returns true/false. TRUE=did not amplify
  summarize(n=n())
# of those sent away, 66 did not amplify and 33 were below the 80% probability threshold 

# 441-(66+33) = 345 genetically identified smolts


########################
# DNA data exploration #
########################

# BAD samples - stock probability < 0.8
badgsid <- dat %>% 
  filter(prob1 < 0.8 | NEWprob1 < 0.8) %>% 
  print()

  # Over time to see if any temporal trend  
  p80ot <- badgsid %>% 
    group_by(date) %>% 
    summarize(n=n()) %>% 
    print()
  
  ggplot(p80ot, aes(x=date, y=n)) +
    geom_bar(stat="identity")
  
  
# GOOD samples going forward
gsid <- dat %>% 
  filter(prob1 >= 0.8 | NEWprob1 >= 0.8) %>%                               # Using K. Flynn (DFO Molecular Genetics Lab) recommendation of p >= 0.8
  print()

  # NO CLEAR TEMPORAL TREND. 33 samples omitted having less than p = 0.80 

# % Nadina and Stellako all together
dat %>% 
  filter(!is.na(NEWregion1)) %>% 
  group_by(NEWregion1) %>% 
  summarize(n=n()) %>% 
  mutate(total=sum(n)) %>% 
  mutate(propn = n/total) %>% 
  print()

 
##########################
# DNA - Migration Timing #
##########################

stocks_of_interest <- c("Nadina", "Stellako")

stock_ot_r1 <- gsid %>% 
  filter(NEWregion1 %in% stocks_of_interest) %>%
  mutate_at(vars(c(10:11,13:14,18,20,22,24)), funs(as.character)) %>% 
  mutate_at(vars(c(10:11,13:14,18,20,22,24)), funs(as.factor)) %>% 
  group_by(date, date_group, NEWregion1) %>% 
  summarize(daily = n()) %>% 
  group_by(date, date_group) %>% 
  mutate(propn = daily/sum(daily)) %>% 
  print()

# total % nadina/stellako
gsid %>% 
  group_by(NEWregion1) %>% 
  summarize(n=n()) %>% 
  print()

# Optimize loess smooth span trial - not included in report, exploratory exercise 
# define function that returns the SSE - from http://r-statistics.co/Loess-Regression-With-R.html
calcSSE <- function(x){
  loessMod <- try(loess(propn ~ date, data=stock_ot_r1, span=x), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(class(res)!="try-error"){
    if((sum(res, na.rm=T) > 0)){
      sse <- sum(res^2)  
    }
  }else{
    sse <- 99999
  }
  return(sse)
}

# Run optim to find span that gives min SSE, starting at 0.5 - from http://r-statistics.co/Loess-Regression-With-R.html
optim(par=c(0.5), calcSSE, method="SANN")
# $par: -11.47
# $value: 99999        <- minimum SSE maxed out, didn't converge (?), too much var probably given optimum span is -11 (out of 0:1 bounds)
# $counts:
#  function gradient 
#    10000       NA 
# $convergence: 0
# $message: NULL


# plot samples per day
ggplot(stock_ot_r1, aes(x=date, y=daily, group=NEWregion1, colour=NEWregion1)) + 
  geom_point() +
  geom_smooth(aes(colour=NEWregion1, fill=NEWregion1), se=F, size=1, alpha=0.15) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  labs(x="Date", y="Number of fish", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    legend.text = element_text(size=8), 
    legend.title = element_text(size=10),
    legend.position="none")

# proportion - WHITE
ggplot(stock_ot_r1, aes(x=date, y=propn, group=NEWregion1, colour=NEWregion1, fill=NEWregion1)) + 
  #geom_hline(yintercept = 0.5, size=1, linetype="dashed", colour="red", alpha=0.8) +
  geom_smooth(se=F, size=1.5, method="loess", span=0.25) +      # note having SE bars make no sense here - there is no variation in daily %s because just 1 value per group
  geom_point(shape=21, size=4, stroke=1.5, colour="black") +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_colour_manual(values=c("#44bd44", "blue")) +
  scale_fill_manual(values=c("#4cd24c", "blue")) +
  labs(x="Date", y="Proportion", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=24, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=21, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(1.5, "mm"),
    panel.grid.major = element_blank(),#line(colour="gray80", size=0.8),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=1.1),
    legend.text = element_text(size=21), 
    legend.title = element_blank(),
    legend.position = c(0.16,0.88),
    legend.background = element_rect(colour="black", size=0.8),
    legend.key.size = unit(7, "mm"),
    legend.margin = margin(t=-2,b=4,l=5,r=8))

# proportion - BLACK
ggplot(stock_ot_r1, aes(x=date, y=propn, group=NEWregion1)) +
  geom_point(aes(colour=NEWregion1, fill=NEWregion1), size=5.5, shape=21, alpha=0.6) +
  geom_smooth(aes(colour=NEWregion1), se=F, size=2) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_fill_manual(values=c("#00ffc1", "#ff9a00")) +
  scale_colour_manual(values=c("#00ffc1", "#ff9a00")) +
  labs(x="Date", y="Proportion", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=30, face = "bold", colour="white"),
    axis.title.x = element_text(margin=margin(t=10,l=0,r=0,b=0)),
    axis.title.y = element_text(margin=margin(t=0,l=0,r=10,b=0)),
    axis.text = element_text(size=25, colour="white"),
    axis.text.x = element_text(angle=45, hjust=1, colour="white"),
    axis.ticks = element_line(colour="white"),
    plot.background = element_rect(fill="black", colour="black"),
    panel.border = element_rect(colour="white"),
    panel.background = element_rect(fill="black"),
    panel.grid = element_line(colour = "black"), 
    legend.background = element_rect(fill="black", colour="white"),
    legend.key = element_rect(fill="black"),
    legend.box.margin = margin(0.1,0.1,0.1,0.1),
    legend.text = element_text(size=18, colour="white"), 
    legend.title = element_blank(),
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(0, 'cm'),
    legend.position = c(0.115,0.89))#,
    #legend.position = "none")

    # proportion NADINA - BLACK
    ggplot(subset(stock_ot_r1 %>% filter(NEWregion1 == "Nadina")), aes(x=date, y=propn, group=NEWregion1)) +
      geom_point(aes(colour=NEWregion1, fill=NEWregion1), size=5.5, shape=21, alpha=0.6) +
      geom_smooth(aes(colour=NEWregion1), se=F, size=2) +
      scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
      scale_fill_manual(values=c("#00ffc1", "#ff9a00")) +
      scale_colour_manual(values=c("#00ffc1", "#ff9a00")) +
      labs(x="Date", y="Proportion", fill="Region", colour="Region") +
      theme_bw() +
      theme(axis.title = element_text(size=30, face = "bold", colour="white"),
       axis.title.x = element_text(margin=margin(t=10,l=0,r=0,b=0)),
        axis.title.y = element_text(margin=margin(t=0,l=0,r=10,b=0)),
        axis.text = element_text(size=25, colour="white"),
        axis.text.x = element_text(angle=45, hjust=1, colour="white"),
        axis.ticks = element_line(colour="white"),
        plot.background = element_rect(fill="black", colour="black"),
        panel.border = element_rect(colour="white"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_line(colour = "gray30"), 
        legend.position = "none")





###########################
# DNA - Length and weight #
###########################

stocks_of_interest <- c("Nadina", "Stellako")

stock_lw_r1 <- gsid %>% 
  filter(NEWregion1 %in% stocks_of_interest, age==1) %>%
  mutate_at(vars(c(10:11,13:14,18,20,22,24)), funs(as.character)) %>% 
  mutate_at(vars(c(10:11,13:14,18,20,22,24)), funs(as.factor)) %>% 
  print()

# LENGTH anova
ml <- lm(stock_lw_r1$length_mm ~ stock_lw_r1$NEWregion1)
mlt <- lm(stock_lw_r1$length_mm ~ stock_lw_r1$date)
summary(mlt)
rl<-resid(ml)
hist(rl)
plot(rl)
qqnorm(rl)
qqline(rl)
summary(aov(stock_lw_r1$length_mm ~ stock_lw_r1$NEWregion1))

# WEIGHT anova
mw <- lm(stock_lw_r1$weight_g ~ stock_lw_r1$NEWregion1)
mwt <- lm(stock_lw_r1$weight_g ~ stock_lw_r1$date)
summary(mwt)
rw<-resid(mw)
hist(rw)
plot(rw)
qqnorm(rw)
qqline(rw)
summary(aov(stock_lw_r1$weight_g ~ stock_lw_r1$NEWregion1))

# CF MODELS
# normal pooled cf linear models
mcf <- lm(stock_lw_r1$cf ~ stock_lw_r1$NEWregion1)
rcf<-resid(mcf)
hist(rcf)
plot(rcf)
qqnorm(rcf)
qqline(rcf)
summary(aov(stock_lw_r1$cf ~ stock_lw_r1$NEWregion1))    # surprising that it's significant given variation and very close values (see barplot below)

# barplot CF by stock
cf_sum <- stock_lw_r1 %>% 
  group_by(NEWregion1) %>% 
  summarize(mean=mean(cf), sd=sd(cf)) %>% 
  print()

ggplot(cf_sum, aes(x=NEWregion1,y=mean)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), stat="identity", width=0.1)


# weird that cf is sig dif between nadina and stellako. i think early season may be driving this, want to try with arbitrary split before-after
# may 9

# INVESTIGATION - MAY 9 (arbitrary split)
# UP TO May 9 (incl)
cf_pre_m9 <- stock_lw_r1 %>% 
  filter(date <= "2019-05-09") %>% 
  #group_by(NEWregion1) %>% 
  #summarize(mean=mean(cf), sd=sd(cf)) %>% 
  print()

mcf_pm9 <- lm(cf_pre_m9$cf ~ cf_pre_m9$NEWregion1)
rcf_pm9<-resid(mcf_pm9)
hist(rcf_pm9)
plot(rcf_pm9)
qqnorm(rcf_pm9)
qqline(rcf_pm9)
summary(aov(cf_pre_m9$cf ~ cf_pre_m9$NEWregion1))

# AFTER May 9
cf_aft_m9 <- stock_lw_r1 %>% 
  filter(date > "2019-05-09") %>% 
  #group_by(NEWregion1) %>% 
  #summarize(mean=mean(cf), sd=sd(cf)) %>% 
  print()

mcf_am9 <- lm(cf_aft_m9$cf ~ cf_aft_m9$NEWregion1)
mcf_am9<-resid(mcf_am9)
hist(mcf_am9)
plot(mcf_am9)
qqnorm(mcf_am9)
qqline(mcf_am9)
summary(aov(cf_aft_m9$cf ~ cf_aft_m9$NEWregion1))

# CLEAR THERE IS DIFFERENCES OVER TIME OR SOME PARTICULAR VALUES DRIVING THIS, SO USE MORE SOPHISTICATED BREAPOINT METHOD TO DETECT WHERE THIS
# OCCURRS FOR BOTH STOCKS OVER TIME

# BREAPOINT EXPLORATION: 
# breakpoints
cf.ts <- ts(stock_lw_r1$cf)   # make as a time series for all individuals (pooled stocks)
fs.cf <- Fstats(cf.ts~1)      # Get sequence of f-statistics for all possible break points within the middle 70% of cf.ts
summary(fs.cf)
plot(fs.cf)
lines(breakpoints(fs.cf))

bp.cf <- breakpoints(cf.ts~1) # BIC indicates 2 breakpoints
summary(bp.cf)

## fit null hypothesis model and breakpoint models to check with AICc
cand.models <- list( )
cand.models[[1]] <- lm(cf.ts ~ 1)
cand.models[[2]] <- lm(cf.ts ~ breakfactor(bp.cf, breaks = 1))
cand.models[[3]] <- lm(cf.ts ~ breakfactor(bp.cf, breaks = 2))
cand.models[[4]] <- lm(cf.ts ~ breakfactor(bp.cf, breaks = 3))
cand.models[[5]] <- lm(cf.ts ~ breakfactor(bp.cf, breaks = 4))
cand.models[[6]] <- lm(cf.ts ~ breakfactor(bp.cf, breaks = 5))
modnames <- paste("mod", 1:length(cand.models), sep = " ")

##generate AICc table
aictab(cand.set = cand.models, modnames = modnames, sort = TRUE)
##round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = cand.models, modnames = modnames, sort = TRUE), digits = 4, LL = TRUE)   # 3 breakpoints is the best based on AIC
# visualize
plot(cf.ts.nad)
lines(ts(fitted(cand.models[[2]])))
lines(ts(fitted(cand.models[[3]])))
lines(ts(fitted(cand.models[[4]])))
lines(ts(fitted(cand.models[[5]])))

# extract breakpoint dates
bp.cf <- breakpoints(cf.ts~1) # BIC indicates 2 breakpoints
summary(bp.cf)
bp.cf$breakpoints
ci.cf <- confint(bp.cf)
plot(bp.cf)
plot(cf.ts)
lines(bp.cf)
lines(ci.cf)
# AIC 3 breakpoints at obs: 129 (May 04), 180 (May 07), 236 (May 12)
# BIC 2 breakpoints at obs: 148 (May 05), 236 (May 12) -- GOING WITH BIC, AIC seems like it could be over-fitting as it prefers more breakpoints
### tried this above analysis split by stock. Nadina wanted 4 breaks, Stellako wanted 2 breaks. Ultimately I want to find common breaks to break up
### anova comparisons so it makes sense doing the breakpoint analysis as the pooled sample, not split by stock. 

 
# ONE-WAY ANOVAS BASED ON BIC BREAKPOINTS: UP TO MAY 4 (incl), MAY 5-12 (incl), MAY 13+
# 1. Start to May 4 (inclusive)
may4 <- stock_lw_r1 %>% 
  filter(date <= "2019-05-04") %>% 
  print()

mcf_pm4 <- lm(may4$cf ~ may4$NEWregion1)
rcf_pm4<-resid(mcf_pm4)
hist(rcf_pm4)
plot(rcf_pm4)
qqnorm(rcf_pm4)
qqline(rcf_pm4)
summary(aov(may4$cf ~ may4$NEWregion1))


# 2. May 5-12 (inclusive)
may512 <- stock_lw_r1 %>% 
  filter(date >= "2019-05-05" & date <= "2019-05-12") %>% 
  print()

mcf_pm512 <- lm(may512$cf ~ may512$NEWregion1)
rcf_pm512<-resid(mcf_pm512)
hist(rcf_pm512)
plot(rcf_pm512)
qqnorm(rcf_pm512)
qqline(rcf_pm512)
summary(aov(may512$cf ~ may512$NEWregion1))
TukeyHSD(aov(may512$cf ~ may512$NEWregion1))


# 3. May 13+ (inclusive)
may13 <- stock_lw_r1 %>% 
  filter(date >= "2019-05-13") %>% 
  print()

mcf_pm13 <- lm(may13$cf ~ may13$NEWregion1)
rcf_pm13<-resid(mcf_pm13)
hist(rcf_pm13)
plot(rcf_pm13)
qqnorm(rcf_pm13)
qqline(rcf_pm13)
summary(aov(may13$cf ~ may13$NEWregion1))


# TWO-WAY ANOVA WITH NADINA/STELLAKO AND 3 DATE GROUPS 
# Exporatory - Doesn't make sense - Ignore
stock_lw_r1 <- stock_lw_r1 %>%
  mutate(cf_date_group = ifelse(date < as.Date("2019-05-05"), 1, ifelse(date > as.Date("2019-05-12"),3,2))) %>% 
  mutate_at(vars(c(41)), funs(as.factor)) %>%
  print()

aov.2 <- aov(stock_lw_r1$cf ~ stock_lw_r1$NEWregion1 + stock_lw_r1$cf_date_group)
summary(aov.2)
TukeyHSD(aov.2)



# White plots
# Length - white
ggplot(stock_lw_r1, aes(x=date, y=length_mm, group=NEWregion1, colour=NEWregion1, fill=NEWregion1)) + 
  geom_point(shape=21, size=3, stroke=1.7) +
  geom_smooth(method = "lm", se=T, size=1.5) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_y_continuous(limits=c(80,140)) +
  scale_colour_manual(values=alpha(c("#00BF00", "blue"), 0.7)) +
  scale_fill_manual(values=c("#4cd24c", "#6666ff")) +
  labs(x="Date", y="Length (mm)", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=24, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=21, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(1.5, "mm"),
    panel.grid.major = element_line(colour="gray80", size=0.8),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=1.1),
    legend.text = element_text(size=21), 
    legend.title = element_blank(),
    legend.position = c(0.11,0.88),
    legend.background = element_rect(colour="black", size=0.8),
    legend.key.size = unit(7, "mm"),
    legend.margin = margin(t=-2,b=4,l=5,r=8))

# length daily average 
mean_lw <- stock_lw_r1 %>%
  filter(age==1) %>%
  group_by(date, NEWregion1) %>% 
  summarize(n=n(), meanl=mean(length_mm), sel=sd(length_mm)/sqrt(length(length_mm)), meanw=mean(weight_g), sew=sd(weight_g)/sqrt(length(weight_g))) %>% 
  print()

ggplot(mean_lw, aes(x=date, y=meanl)) + 
  geom_errorbar(aes(ymin=meanl-sel, ymax=meanl+sel, colour=NEWregion1), width=0, size=1.5, alpha=0.7) +
  geom_point(aes(fill=NEWregion1, size=n), shape=21, stroke=1.5, colour="black") +  #POINTS SCALED BY SAMPLE SIZE HERE- NOT THE SAME AS IN THE REPORT
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_size(range=c(2,10), guide="none") +                          # REMOVE TO GET REPORT VERSION
  scale_colour_manual(values=c("#44bd44", "blue"), guide="none") +   # REMOVE GUIDE="NONE" TO GET REPORT VERSION
  scale_fill_manual(values=c("#4cd24c", "blue")) +
  labs(x="", y="Length (mm)", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=24, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=21, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(1.5, "mm"),
    panel.grid.major = element_blank(),#line(colour="gray80", size=0.8),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=1.1),
    legend.text = element_text(size=21), 
    legend.title = element_blank(),
    legend.position = c(0.09,0.86),
    legend.background = element_rect(colour="black", size=0.8),
    legend.key.size = unit(7, "mm"),
    legend.margin = margin(t=-2,b=4,l=5,r=8)) +
  guides(fill = guide_legend(override.aes = list(size=4)))         # REMOVE TO GET REPORT VERSION
# scaling points by sample size is hard because the legend point size and errorbar line size change sizes. Didn't do for the report in the end. 


# Weight - white
ggplot(stock_lw_r1, aes(x=date, y=weight_g, group=NEWregion1, colour=NEWregion1, fill=NEWregion1)) + 
  geom_point(shape=21, size=3, stroke=1.7) +
  geom_smooth(method="lm", se=T, size=1.5) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_colour_manual(values=alpha(c("#00BF00", "blue"), 0.7)) +
  scale_fill_manual(values=c("#4cd24c", "#6666ff")) +
  labs(x="Date", y="Weight (g)", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=24, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=21, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(1.5, "mm"),
    panel.grid.major = element_line(colour="gray80", size=0.8),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=1.1),
    #legend.text = element_text(size=21), 
    #legend.title = element_blank(),
    legend.position = "none") #c(0.11,0.88),
    #legend.background = element_rect(colour="black", size=0.8),
    #legend.key.size = unit(7, "mm"),
    #legend.margin = margin(t=-2,b=4,l=5,r=8))

# weight daily average
mean_lw <- stock_lw_r1 %>%
  filter(age==1) %>% 
  group_by(date, NEWregion1) %>% 
  summarize(meanl=mean(length_mm), sel=sd(length_mm)/sqrt(length(length_mm)), meanw=mean(weight_g), sew=sd(weight_g)/sqrt(length(weight_g))) %>% 
  print()

ggplot(mean_lw, aes(x=date, y=meanw, group=NEWregion1, colour=NEWregion1, fill=NEWregion1)) +
  geom_errorbar(aes(ymin=meanw-sew, ymax=meanw+sew), width=0, size=1.5, alpha=0.7) +
  geom_point(shape=21, size=4, stroke=1.5, colour="black") +
  scale_y_continuous(limits=c(5,20)) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_colour_manual(values=c("#44bd44", "blue")) +
  scale_fill_manual(values=c("#4cd24c", "blue")) +
  labs(x="", y="Weight (g)", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=24, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=21, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(1.5, "mm"),
    panel.grid.major = element_blank(),#line(colour="gray80", size=0.8),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=1.1),
    #legend.text = element_text(size=21), 
    #legend.title = element_blank(),
    legend.position = "none") #c(0.11,0.88),
    #legend.background = element_rect(colour="black", size=0.8),
    #legend.key.size = unit(7, "mm"),
    #legend.margin = margin(t=-2,b=4,l=5,r=8))

# Weight~length - white
ggplot(stock_lw_r1, aes(x=length_mm, y=weight_g, group=NEWregion1, colour=NEWregion1)) + 
  geom_point() +
  geom_smooth(aes(colour=NEWregion1, fill=NEWregion1), method="lm", se=T, size=1, alpha=0.15) +
  labs(x="Length (mm)", y="Weight (g)", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    legend.text = element_text(size=8), 
    legend.title = element_text(size=10),
    legend.position="none")

# cf - white
ggplot(stock_lw_r1, aes(x=date, y=cf, group=NEWregion1, colour=NEWregion1, fill=NEWregion1)) + 
  geom_point(shape=21, size=3, stroke=1.7) +
  geom_smooth(method = "lm", se=T, size=1.5) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  #scale_y_continuous(limits=c(80,140)) +
  scale_colour_manual(values=alpha(c("#00BF00", "blue"), 0.7)) +
  scale_fill_manual(values=c("#4cd24c", "#6666ff")) +
  labs(x="Date", y="Condition factor", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=24, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=21, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(1.5, "mm"),
    panel.grid.major = element_line(colour="gray80", size=0.8),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=1.1),
    #legend.text = element_text(size=21), 
    #legend.title = element_blank(),
    legend.position = "none") #c(0.11,0.88),
    #legend.background = element_rect(colour="black", size=0.8),
    #legend.key.size = unit(7, "mm"),
    #legend.margin = margin(t=-2,b=4,l=5,r=8))

# cf daily average 
cf_daily_mean <- stock_lw_r1 %>% 
  filter(age==1) %>%
  group_by(date, NEWregion1) %>% 
  summarize(mean=mean(cf), sd=sd(cf), se=sd(cf)/sqrt(length(cf))) %>% 
  print()

ggplot(cf_daily_mean, aes(x=date, y=mean, group=NEWregion1, colour=NEWregion1, fill=NEWregion1)) +
  geom_vline(xintercept = c(as.Date("2019-05-05"), as.Date("2019-05-12")), linetype="dashed", size=0.5) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0, size=1.5, alpha=0.7) +
  geom_point(shape=21, size=4, stroke=1.5, colour="black") +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_colour_manual(values=c("#44bd44", "blue")) +
  scale_fill_manual(values=c("#4cd24c", "blue")) +
  labs(x="", y="Condition factor", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=24, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=21, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(1.5, "mm"),
    panel.grid.major = element_blank(),#line(colour="gray80", size=0.8),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=1.1),
    #legend.text = element_text(size=21), 
    #legend.title = element_blank(),
    legend.position = "none") #c(0.11,0.88),
    #legend.background = element_rect(colour="black", size=0.8),
    #legend.key.size = unit(7, "mm"),
    #legend.margin = margin(t=-2,b=4,l=5,r=8))



# Black plots (powerpoint)
# Length - BLACK
ggplot(stock_lw_r1, aes(x=date, y=length_mm, group=NEWregion1)) + 
  geom_point(aes(colour=NEWregion1, fill=NEWregion1), size=8.5, shape=21, alpha=0.5, stroke=1.1) +
  geom_smooth(aes(colour=NEWregion1), method="lm", se=T, size=4.5, alpha=0.3, fill="gray60") +
  scale_fill_manual(values=c("#00ffc1", "#ff9a00")) +
  scale_colour_manual(values=c("#00ffc1", "#ff9a00")) +
  scale_x_date(date_breaks = "4 days", date_labels = "%b %d") +
  scale_y_continuous(limits=c(80,140)) +
  labs(x="Date", y="Length (mm)", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=46, face = "bold", colour="white"),
    axis.title.x = element_text(margin=margin(t=10,l=0,r=0,b=0)),
    axis.title.y = element_text(margin=margin(t=0,l=0,r=7,b=0)),
    axis.text = element_text(size=40, colour="white"),
    axis.text.x = element_text(angle=45, hjust=1, colour="white"),
    axis.ticks = element_line(colour="white"),
    plot.background = element_rect(fill="black", colour="black"),
    panel.border = element_rect(colour="white", size=1.7),
    panel.background = element_rect(fill="black"),
    panel.grid = element_line(colour = "black"), 
    legend.background = element_rect(fill="black"),
    legend.key = element_rect(fill="black"),
    legend.box.margin = margin(0.1,0.1,0.1,0.1),
    legend.text = element_text(size=18, colour="white"), 
    legend.title = element_blank(),
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(0, 'cm'),
    legend.position = "none")

    # Length - BLACK - Nadina
    ggplot(subset(stock_lw_r1 %>% filter(NEWregion1 == "Nadina")), aes(x=date, y=length_mm, group=NEWregion1)) + 
      geom_point(aes(colour=NEWregion1, fill=NEWregion1), size=8.5, shape=21, alpha=0.5, stroke=1.1) +
      geom_smooth(aes(colour=NEWregion1, fill=NEWregion1), method="lm", se=F, size=4.5, alpha=0.3) +
      scale_fill_manual(values=c("#00ffc1", "#ff9a00")) +
      scale_colour_manual(values=c("#00ffc1", "#ff9a00")) +
      scale_x_date(date_breaks = "4 days", date_labels = "%b %d") +
      scale_y_continuous(limits=c(80,140)) +
      labs(x="Date", y="Length (mm)", fill="Region", colour="Region") +
      theme_bw() +
      theme(axis.title = element_text(size=46, face = "bold", colour="white"),
        axis.title.x = element_text(margin=margin(t=10,l=0,r=0,b=0)),
        axis.title.y = element_text(margin=margin(t=0,l=0,r=7,b=0)),
        axis.text = element_text(size=40, colour="white"),
        axis.text.x = element_text(angle=45, hjust=1, colour="white"),
        axis.ticks = element_line(colour="white"),
        plot.background = element_rect(fill="black", colour="black"),
        panel.border = element_rect(colour="white", size=1.7),
        panel.background = element_rect(fill="black"),
        panel.grid = element_line(colour = "gray50"), 
        legend.background = element_rect(fill="black"),
        legend.key = element_rect(fill="black"),
        legend.box.margin = margin(0.1,0.1,0.1,0.1),
        legend.text = element_text(size=18, colour="white"), 
        legend.title = element_blank(),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0, 'cm'),
        legend.position = "none")

# Weight - black
ggplot(stock_lw_r1, aes(x=date, y=weight_g, group=NEWregion1, colour=NEWregion1)) + 
  geom_point(aes(colour=NEWregion1, fill=NEWregion1), size=8.5, shape=21, alpha=0.5, stroke=1.1) +
  geom_smooth(aes(colour=NEWregion1, fill=NEWregion1), method="lm", se=F, size=4.5, alpha=0.3) +
  scale_x_date(date_breaks = "4 days", date_labels = "%b %d") +
  scale_y_continuous(limits=c(5,20)) +
  scale_fill_manual(values=c("#00ffc1", "#ff9a00")) +
  scale_colour_manual(values=c("#00ffc1", "#ff9a00")) +
  labs(x="Date", y="Weight (g)", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=46, face = "bold", colour="white"),
    axis.title.x = element_text(margin=margin(t=10,l=0,r=0,b=0)),
    axis.title.y = element_text(margin=margin(t=0,l=0,r=7,b=0)),
    axis.text = element_text(size=40, colour="white"),
    axis.text.x = element_text(angle=45, hjust=1, colour="white"),
    axis.ticks = element_line(colour="white"),
    plot.background = element_rect(fill="black", colour="black"),
    panel.border = element_rect(colour="white", size=1.7),
    panel.background = element_rect(fill="black"),
    panel.grid = element_line(colour = "gray50"), 
    legend.background = element_rect(fill="black"),
    legend.key = element_rect(fill="black"),
    legend.box.margin = margin(0.1,0.1,0.1,0.1),
    legend.text = element_text(size=18, colour="white"), 
    legend.title = element_blank(),
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(0, 'cm'),
    legend.position = "none")

    # Weight - black
    ggplot(subset(stock_lw_r1 %>% filter(NEWregion1 == "Nadina")), aes(x=date, y=weight_g, group=NEWregion1, colour=NEWregion1)) + 
      geom_point(aes(colour=NEWregion1, fill=NEWregion1), size=8.5, shape=21, alpha=0.5, stroke=1.1) +
      geom_smooth(aes(colour=NEWregion1, fill=NEWregion1), method="lm", se=F, size=4.5, alpha=0.3) +
      scale_x_date(date_breaks = "4 days", date_labels = "%b %d") +
      scale_y_continuous(limits=c(5,20)) +
      scale_fill_manual(values=c("#00ffc1", "#ff9a00")) +
      scale_colour_manual(values=c("#00ffc1", "#ff9a00")) +
      labs(x="Date", y="Weight (g)", fill="Region", colour="Region") +
      theme_bw() +
      theme(axis.title = element_text(size=46, face = "bold", colour="white"),
        axis.title.x = element_text(margin=margin(t=10,l=0,r=0,b=0)),
        axis.title.y = element_text(margin=margin(t=0,l=0,r=7,b=0)),
        axis.text = element_text(size=40, colour="white"),
        axis.text.x = element_text(angle=45, hjust=1, colour="white"),
        axis.ticks = element_line(colour="white"),
        plot.background = element_rect(fill="black", colour="black"),
        panel.border = element_rect(colour="white", size=1.7),
        panel.background = element_rect(fill="black"),
        panel.grid = element_line(colour = "gray50"), 
        legend.background = element_rect(fill="black"),
        legend.key = element_rect(fill="black"),
        legend.box.margin = margin(0.1,0.1,0.1,0.1),
        legend.text = element_text(size=18, colour="white"), 
        legend.title = element_blank(),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0, 'cm'),
        legend.position = "none")


    

####################################################################################################################################################

                                                              ################
                                                              # AGE ANALYSIS #
                                                              ################        
    

# read data - same as top just for quick ref
dat <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheet=3, detectDates=T)          # might be very slow! sometimes 'sheet' may have to be changed to 'sheetIndex' depending on the order packages are loaded
dat <- dat %>% 
  mutate(NEWregion1 = ifelse(NEWregion1==4, "Nadina", ifelse(NEWregion1 ==12, "Stellako", NEWregion1))) %>%
  print()

# how many readable scale samples (i.e., remove all NA) and what % were age-0, age-1 and age-2?
age <- dat %>% 
  filter(!is.na(age)) %>% 
  group_by(age) %>% 
  summarize(n=n()) %>%
  mutate(sum = sum(n)) %>% 
  mutate(propn = n/sum) %>%
  mutate(age = ifelse(age==0, "Age-0", ifelse(age==1, "Age-1", "Age-2"))) %>%
  print()

age$age <- factor(age$age, levels=c("Age-2", "Age-1", "Age-0"), ordered=T)

# lollipop plot by age - black
ggplot(age, aes(x=`age`, y=propn)) + 
  geom_segment(aes(x=age, xend=age, y=0, yend=propn), size=1.5, colour="#fff7a4") +
  geom_point(stat="identity", size=7, shape=21, fill="#fff7a4", colour="#fff7a4", stroke=1.5) + 
  scale_y_continuous(limits=c(0,1.05)) +
  labs(x="", y="Proportion")+
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_text(size=46, face = "bold", colour="white"),
    axis.title.x = element_text(margin=margin(t=10,l=0,r=0,b=0)),
    axis.title.y = element_text(margin=margin(t=0,l=0,r=7,b=0)),
    axis.text = element_text(size=40, colour="white"),
    axis.text.x = element_text(colour="white"),
    axis.ticks = element_line(colour="white"),
    plot.background = element_rect(fill="black", colour="black"),
    panel.border = element_rect(colour="white", size=1.7),
    panel.background = element_rect(fill="black"),
    panel.grid = element_line(colour = "gray50"), 
    legend.background = element_rect(fill="black"),
    legend.key = element_rect(fill="black"),
    legend.box.margin = margin(0.1,0.1,0.1,0.1),
    legend.text = element_text(size=18, colour="white"), 
    legend.title = element_blank(),
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(0, 'cm'),
    legend.position = "none")

    
# by stock
stocks_of_interest = c("Nadina", "Stellako")

age.cu <- dat %>% 
  filter(!is.na(age), NEWregion1 %in% stocks_of_interest) %>% 
  group_by(NEWregion1, age) %>% 
  summarize(n=n()) %>%
  mutate(sum = sum(n)) %>% 
  mutate(propn = n/sum) %>%
  print()

# lollipop plot by age split by stock
ggplot(subset(age.cu %>% filter(NEWregion1=="Nadina")), aes(x=age, y=n)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=age, xend=age, y=0, yend=n))  
ggplot(subset(age.cu %>% filter(NEWregion1=="Stellako")), aes(x=age, y=n)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=age, xend=age, y=0, yend=n)) 
    
    

####################################################################################################################################################

                                                              ########################################
                                                              # LENGTH, WEIGHT, AGE summary by STOCK #
                                                              ########################################

   
# overall average length, weight, CF of all ageable and DNA identified individuals  
# toggle including just filter(!is.na(age)) and group_by(age) for overall age breakdown not limited DNA identified individuals 
View(dat %>%
  filter(!is.na(NEWregion1), !is.na(age)) %>%
  #filter(!is.na(age)) %>%
  group_by(NEWregion1, age) %>%
  #group_by(age) %>%
  summarize(n = n(), meanL = mean(length_mm, na.rm=T), seL = sd(length_mm, na.rm=T)/sqrt(length(length_mm)), 
    meanW = mean(weight_g, na.rm=T), seW = sd(weight_g, na.rm=T)/sqrt(length(weight_g)),
    meanCF = mean(cf, na.rm=T), seCF = sd(cf, na.rm=T)/sqrt(length(cf))))
    
####################################################################################################################################################

                                                        ####################
                                                        # MIGRATION TIMING #
                                                        ####################

  # After talking with Scott, we need to further standardize catch by the known hours of migration. Including all hours of fishing can bias the
  # catch towards lower numbers because you include hours where no fish are migrating. Based on knowledge from the crew on when peak migration
  # is, we decided on the hours of 9pm to 3am (21:00 - 03:00), 6 hours total. This means that some catches will be excluded because their fishing 
  # windows don't fall in this time period, so you are essentially dividing by 0. 

##############
# CATCH DATA #
##############

# read catch data
catch <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheet=2, detectDates=T)          # might be very slow! sometimes 'sheet' may have to be changed to 'sheetIndex' depending on the order packages are loaded

# filter by RST and night sampling
catch <- catch %>% 
  filter(trap_type == "small RST") %>% 
  filter(start_time != "11:30", start_time != "11:00") %>% 
  print()

# summarize to apply "cpue"
cpue <- catch %>% 
  group_by(start_date, start_time, end_time) %>% 
  summarize(sum=sum(sox_smolts)) %>%
  mutate(hrs_in_window = 6) %>%
  print()
# NOTE: THIS PIPE LOOP ABOVE CAN'T REALLY BE CHANGED BECAUSE THE 'hrs_in_window" changes on the next code block refer to specific column and row #s

# manually change hrs_in_window for shorter times - these were determined visually looking at start and end times as they aren't always consistent
cpue[46,5] <- 5.5
cpue[47,5] <- 5.5
cpue[48,5] <- 5.5
cpue[49,5] <- 5.5

cpue[55,5] <- 5.5
cpue[56,5] <- 5.5
cpue[57,5] <- 5.5
cpue[58,5] <- 5.5
cpue[59,5] <- 5.5

cpue[66,5] <- 5.5
cpue[67,5] <- 5.5
cpue[68,5] <- 5.5
cpue[69,5] <- 5.5
cpue[70,5] <- 5.5
  
cpue[71,5] <- 5.5
cpue[72,5] <- 5.5
cpue[73,5] <- 5.5
cpue[74,5] <- 5.5
cpue[75,5] <- 5.5
  
cpue[81,5] <- 5.5
cpue[82,5] <- 5.5
cpue[83,5] <- 5.5
cpue[84,5] <- 5.5
cpue[85,5] <- 5.5
  
cpue[111,5] <- 5.5
cpue[112,5] <- 5.5
cpue[113,5] <- 5.5
cpue[114,5] <- 5.5
  
cpue[115,5] <- 5.5
cpue[116,5] <- 5.5
cpue[117,5] <- 5.5
cpue[118,5] <- 5.5
  
cpue[124,5] <- 5.5
cpue[125,5] <- 5.5
cpue[126,5] <- 5.5
cpue[127,5] <- 5.5
  
cpue[133,5] <- 5.5
cpue[134,5] <- 5.5
cpue[135,5] <- 5.5
cpue[136,5] <- 5.5
  
cpue[137,5] <- 5.5
cpue[138,5] <- 5.5
cpue[139,5] <- 5.5
cpue[140,5] <- 5.5
  
cpue[145,5] <- 5.5
cpue[146,5] <- 5.5
cpue[147,5] <- 5.5
cpue[148,5] <- 5.5
cpue[149,5] <- 5.5
cpue[150,5] <- 5.5
cpue[151,5] <- 5.5
cpue[152,5] <- 5.5
cpue[153,5] <- 5.5
cpue[154,5] <- 5.5
cpue[155,5] <- 5.5

cpue2 <- cpue %>% 
  group_by(start_date) %>% 
  summarize(sum = sum(sum)) %>%
  mutate(hrs_in_window = ifelse(start_date == "2019-05-02" | start_date == "2019-05-04" |start_date ==  "2019-05-06" | start_date == "2019-05-07"
    | start_date == "2019-05-09" | start_date == "2019-05-15" | start_date == "2019-05-16" | start_date == "2019-05-18" 
    | start_date == "2019-05-20" | start_date == "2019-05-21" | start_date == "2019-05-23" | start_date == "2019-05-24" 
    | start_date == "2019-05-25", 5.5, 6)) %>%
  mutate(cpue = sum/hrs_in_window) %>%
  print()

##################
# DISCHARGE DATA #
##################

# read in & clean discharge data 
discharge <- read.csv("NAUT_DISCH_08JB003_QR_Dec-19-2019_12_44_31AM.csv")

discharge2 <- discharge %>% 
  rename(date = `Date..PST.`,
         param = Parameter,
         discharge_m3s = `Value..m3.s.`) %>% 
  separate(date, c("date", "time"), sep=" ") %>%
  print()

# reformat time series
discharge2$time <- with_options(c(scipen = 999), str_pad(discharge2$time, 5, pad = "0"))
discharge2$date <- lubridate::mdy(discharge2$date)
discharge2$datetime <- as.POSIXct(paste(discharge2$date, discharge2$time), tz="")

# summary discharge data
discharge2 <- discharge2 %>% 
  group_by(date) %>% 
  summarize(mean_dis=mean(discharge_m3s), min_dis=min(discharge_m3s), max_dis=max(discharge_m3s)) %>%
  print()

ggplot(discharge2)

########
# plot #
########

# CPUE and discharge - white report
no_fishing <- data.frame(xstart = as.Date('2019-04-21'), xend = as.Date('2019-04-25'))

# CPUE + discharge figure - legend item changed to 'catch' for report figure a and b, but here represents CPUE
ggplot() +
  geom_rect(data = no_fishing, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill="gray85") +
  geom_ribbon(data=discharge2, aes(x=date, ymin=min_dis*12, ymax=max_dis*12), fill="#8bc2fd") +
  geom_line(data=discharge2, aes(x=date, y=mean_dis*12, colour="Discharge"), size=1.2) +                          ##1785fc blue
  geom_bar(data=cpue2, aes(x=start_date, y=cpue, colour="Catch", fill="Catch"), size=0.9, width=1.1, stat="identity", colour="#e27f14", alpha=0.8) +      ##fc992e fill  #e27f14 outline 
  scale_y_continuous(breaks = seq(0,1300,250),
                     sec.axis = sec_axis(~./12, name = expression(bold("Discharge"~m^3/s)), labels=seq(0,100,25), breaks=seq(0,100,25))) +
  scale_x_date(limits=as.Date(c("2019-04-13", "2019-05-27")), breaks="4 day", labels = date_format("%b %d")) +
  scale_fill_manual("", values = c("Catch" = "#fc992e")) + 
  scale_colour_manual("", values = c("Discharge"="#1785fc")) + 
  labs(x="Date", y="Catch (smolts/hour)") +
  theme_bw() +
  theme(axis.title = element_text(size=24, face="bold"),
    axis.title.y = element_text(margin = margin(t=0, b=0, l=0, r=6)),
    axis.title.y.right = element_text(margin=margin(t=0, b=0, l=7, r=0), face="bold"),
    axis.text = element_text(size=21, colour="black"),
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(1.5, "mm"),
    legend.text = element_text(size=21), 
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(t = 0, r = 1, b = 1, l = 1, unit = "mm"),
    legend.title = element_blank(),
    legend.position = c(0.85,0.87),
    #legend.background = element_rect(colour="black"),
    panel.border = element_rect(size=1.1),
    panel.grid.major = element_blank(),#line(colour="gray85", size=0.5),
    panel.grid.minor = element_blank())

# raw catch + discharge figure
ggplot() +
  geom_rect(data=no_fishing, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill="gray85") +
  geom_ribbon(data=discharge2, aes(x=date, ymin=min_dis*75, ymax=max_dis*75), fill="#8bc2fd") +
  geom_line(data=discharge2, aes(x=date, y=mean_dis*75, colour="Discharge"), size=1.2) +                          ##1785fc blue
  geom_bar(data=cpue2, aes(x=start_date, y=sum, colour="Raw catch", fill="Raw catch"), size=0.9, width=1.1, stat="identity", colour="#e27f14", alpha=0.8) +      ##fc992e fill  #e27f14 outline 
  scale_y_continuous(limits=c(0,8000), breaks = seq(0,8000,1500),
                     sec.axis = sec_axis(~./75, name = expression(bold("Discharge"~m^3/s)), labels=seq(0,100,25), breaks=seq(0,100,25))) +
  scale_x_date(limits=as.Date(c("2019-04-13", "2019-05-27")), breaks="4 day", labels = date_format("%b %d")) +
  scale_fill_manual("", values = c("Raw catch" = "#fc992e")) + 
  scale_colour_manual("", values = c("Discharge"="#1785fc")) + 
  labs(x="Date", y="Raw catch") +
  theme_bw() +
  theme(axis.title = element_text(size=24, face="bold"),
    axis.title.y = element_text(margin = margin(t=0, b=0, l=0, r=6)),
    axis.title.y.right = element_text(margin=margin(t=0, b=0, l=7, r=0), face="bold"),
    axis.text = element_text(size=21, colour="black"),
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(1.5, "mm"),
    #legend.text = element_text(size=21), 
    #legend.spacing = unit(0, "cm"),
    #legend.margin = margin(t = 0, r = 1, b = 1, l = 1, unit = "mm"),
    #legend.title = element_blank(),
    legend.position = "none", #c(0.85,0.87),
    panel.border = element_rect(size=1.1),
    panel.grid.major = element_blank(),#line(colour="gray85", size=0.5),
    panel.grid.minor = element_blank())

# CPUE ONLY - black ppt
ggplot(data=cpue2, aes(x=start_date, y=cpue)) +
  #geom_bar(stat="identity", fill="#fff7a4", colour="#fff7a4", alpha=0.85, width=1.15) + 
  scale_y_continuous(breaks = seq(0,1300,300))+
  scale_x_date(limits=as.Date(c("2019-04-13", "2019-05-27")), breaks="4 day", labels = date_format("%b %d")) +
  labs(x="Date", y="CPUE") +
      theme_bw() +
      theme(axis.title = element_text(size=30, face = "bold", colour="white"),
        axis.title.x = element_text(margin=margin(t=10,l=0,r=0,b=0)),
        axis.title.y = element_text(margin=margin(t=0,l=0,r=5,b=0)),
        axis.text = element_text(size=25, colour="white"),
        axis.text.x = element_text(angle=45, hjust=1, colour="white"),
        axis.ticks = element_line(colour="white"),
        plot.background = element_rect(fill="black", colour="black"),
        plot.margin=unit(c(0.2,3.43,0.2,0.2), "cm"),
        panel.border = element_rect(colour="white"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_line(colour = "gray30"), 
        legend.background = element_rect(fill="black", colour="white"),
        legend.key = element_rect(fill="black"),
        legend.box.margin = margin(0.1,0.1,0.1,0.1),
        legend.text = element_text(size=18, colour="white"), 
        legend.title = element_blank(),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0, 'cm'),
        legend.position = c(0.2,0.2))

# CPUE + discharge - black ppt
ggplot() +
  geom_ribbon(data=discharge2, aes(x=date, ymin=min_dis*10, ymax=max_dis*10), fill="gray80", alpha=0.7) +
  geom_line(data=discharge2, aes(x=date, y=mean_dis*10), size=1.5, colour="white") +
  geom_bar(data=cpue2, aes(x=start_date, y=cpue), stat="identity", fill="#fff7a4", colour="#fff7a4", alpha=0.85, width=1.15) + 
  scale_y_continuous(breaks = seq(0,1300,300),
                     sec.axis = sec_axis(~./10, name = expression(bold("Discharge"~m^3/s)), breaks=seq(0,150,25))) +
  scale_x_date(limits=as.Date(c("2019-04-13", "2019-05-27")), breaks="4 day", labels = date_format("%b %d")) +
  labs(x="Date", y="CPUE") +
      theme_bw() +
      theme(axis.title = element_text(size=30, face = "bold", colour="white"),
        axis.title.x = element_text(margin=margin(t=10,l=0,r=0,b=0)),
        axis.title.y = element_text(margin=margin(t=0,l=0,r=5,b=0)),
        axis.text = element_text(size=25, colour="white"),
        axis.text.x = element_text(angle=45, hjust=1, colour="white"),
        axis.ticks = element_line(colour="white"),
        plot.background = element_rect(fill="black", colour="black"),
        panel.border = element_rect(colour="white"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_line(colour = "gray30"), 
        legend.background = element_rect(fill="black", colour="white"),
        legend.key = element_rect(fill="black"),
        legend.box.margin = margin(0.1,0.1,0.1,0.1),
        legend.text = element_text(size=18, colour="white"), 
        legend.title = element_blank(),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0, 'cm'),
        legend.position = c(0.2,0.2))



# img specs::export pdf us letter landscape

