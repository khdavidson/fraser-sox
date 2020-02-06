# nadina carcass recoveries over time by area 

library(tidyverse)
library(lubridate)
library(strptime)

setwd("~/ANALYSIS/Data")

# read
data <- read.csv("nadina_recoveries_spatiotemporal.csv")
data$date <- as.Date(data$date)

# clean
data <- data %>% 
  gather("sex", "count", 7:8) %>% 
  mutate_at(vars(c(8)), funs(as.numeric)) %>%
  print()


###########
# BY AREA #
###########

area <- data %>% 
  filter(!is.na(count)) %>%
  group_by(area2, sex) %>% 
  summarize(total = sum(count, na.rm=T), mean=mean(count, na.rm=T), sd=sd(count, na.rm=T)) %>%
  group_by(area2) %>% 
  mutate(propn = total/sum(total)) %>%
  print()

areaT <- area %>% 
  group_by(area2) %>% 
  summarize(sum=sum(total)) %>% 
  mutate(T_propn = sum/sum(sum)) %>%
  print()

# proportion
ggplot() +
  geom_line(data=areaT, aes(x=area2, y=T_propn), size=1.2) +
  geom_bar(data=area, aes(x=area2, y=propn, group=sex, fill=sex), stat="identity", colour="black", alpha=0.6, position=position_dodge(width=0.9))
  #geom_line(data=area, aes(x=area2, y=propn, group=sex, colour=sex)) +
  ylim(0,1)


###########
# BY YEAR #
###########

year <- data %>% 
  filter(!is.na(count)) %>% 
  group_by(year, sex) %>% 
  summarize(total = sum(count, na.rm=T), mean=mean(count, na.rm=T), sd=sd(count, na.rm=T)) %>%
  mutate(propn = total/sum(total)) %>%
  print()
    
# proportion
ggplot() +
  geom_line(data=year, aes(x=year, y=propn, group=sex, colour=sex), size=1.2)
  ylim(0,1)

  
####################
# BY YEAR AND AREA #
####################

yearea <- data %>% 
  group_by(year, area2, sex) %>% 
  filter(!is.na(count), !is.na(area2)) %>% 
  summarize(total=sum(count, na.rm=T)) %>% 
  group_by(year) %>% 
  mutate(propn=total/sum(total)) %>%
  ungroup() %>%
  mutate_at(vars(c(1:2)), funs(as.factor)) %>%
  print()

ggplot(yarea, aes(x=year, y=propn, group=area2, colour=area2)) +
  geom_line(size=1.2) +
  geom_point(size=2.7, fill="white", pch=21, stroke=1.2) +
  facet_grid(~sex)


##################
# OT WITHIN YEAR #
##################

data$monthday <- format(as.Date(data$date, format="%Y-%m-%d"),"%b-%d")


ggplot(data, aes(x=monthday, y=count, group=sex, colour=sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~area2)
















  
  
  
