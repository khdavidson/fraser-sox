# NAUTLEY 2019 FINAL REPORT CODE
# 29-Jan-2020
# All DNA, scales, length and weight data to be run by DFO are now here.
# More data may come from E-Watch: DNA and physiological samples. 

# libraries and wd
library(tidyverse)
library(XLConnect)
library(xlsx)
library(openxlsx)

setwd("~/ANALYSIS/Data")


####################################################################################################################################################

                                                        ######################
                                                        # BIOSAMPLE ANALYSIS #
                                                        ######################

# read data
dat <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheet=3, detectDates=T)          # might be very slow! sometimes 'sheet' may have to be changed to 'sheetIndex' depending on the order packages are loaded

########################
# DNA data exploration #
########################

# BAD samples - stock probability < 0.8
badgsid <- dat %>% 
  filter(prob1 < 0.8 | NEWprob1 < 0.8) %>% 
  print()

  # Over time to see if any temporal trend  
  p0.8ot <- gsid %>% 
    group_by(date) %>% 
    summarize(n=n()) %>% 
    print()
  
  ggplot(p0.8ot, aes(x=date, y=n)) +
    geom_bar(stat="identity")
  
  
# GOOD samples going forward
gsid <- dat %>% 
  filter(prob1 >= 0.8 | NEWprob1 >= 0.8) %>%                               # Using K. Flynn's recommendation of p >= 0.8
  print()

  # NO CLEAR TEMPORAL TREND. 33 samples omitted having less than p = 0.80 


##########################
# DNA - Migration Timing #
##########################

stocks_of_interest <- c(4, 12)

stock_ot_r1 <- gsid %>% 
  filter(NEWregion1 %in% stocks_of_interest) %>%
  mutate_at(vars(c(10:11,13:14,18,20,22,24)), funs(as.character)) %>% 
  mutate_at(vars(c(10:11,13:14,18,20,22,24)), funs(as.factor)) %>% 
  group_by(date, date_group, NEWregion1) %>% 
  summarize(daily = n()) %>% 
  group_by(date, date_group) %>% 
  mutate(propn = daily/sum(daily)) %>% 
  print()

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

ggplot(stock_ot_r1, aes(x=date, y=propn, group=NEWregion1, colour=NEWregion1)) + 
  geom_point() +
  geom_smooth(aes(colour=NEWregion1, fill=NEWregion1), se=F, size=1, alpha=0.15) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  labs(x="Date", y="Proportion", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    legend.text = element_text(size=8), 
    legend.title = element_text(size=10))


###########################
# DNA - Length and weight #
###########################

stocks_of_interest <- c(4, 12)

stock_lw_r1 <- gsid %>% 
  filter(NEWregion1 %in% stocks_of_interest) %>%
  mutate_at(vars(c(10:11,13:14,18,20,22,24)), funs(as.character)) %>% 
  mutate_at(vars(c(10:11,13:14,18,20,22,24)), funs(as.factor)) %>% 
  print()

ggplot(stock_lw_r1, aes(x=date, y=length_mm, group=NEWregion1, colour=NEWregion1)) + 
  geom_point() +
  geom_smooth(aes(colour=NEWregion1, fill=NEWregion1), method="lm", se=T, size=1, alpha=0.15) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  labs(x="Date", y="Length (mm)", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    legend.text = element_text(size=8), 
    legend.title = element_text(size=10),
    legend.position="none")

ggplot(stock_lw_r1, aes(x=date, y=weight_g, group=NEWregion1, colour=NEWregion1)) + 
  geom_point() +
  geom_smooth(aes(colour=NEWregion1, fill=NEWregion1), method="lm", se=T, size=1, alpha=0.15) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  labs(x="Date", y="Weight (g)", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    legend.text = element_text(size=8), 
    legend.title = element_text(size=10),
    legend.position="none")

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














