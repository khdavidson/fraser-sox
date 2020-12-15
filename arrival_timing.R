# daily passage database 
# Dec 2020

# this script takes the summary database of all high-precision STAD programs that offer daily abundance estimates (fences, sonars) from 2000 onward.
# some caveats to these data:
# - there are no SEP channel data included
# - only programs with RELIABLE daily estimates are included. E.g., Stellako is not included due to a) changing program start dates and b) uncertainty 
#   over the daily % Stellako/Nadina each year (DNA is not always taken each year)
# - these are NOT intended to be used to generate escapement estimates/spawner abundances. much post-processing analysis is done to allocate sockeye
#   to lake spawning, tributaries, channels, etc. These data should NEVER be summed.

setwd("~/Documents/ANALYSIS/data")

library(tidyverse)
library(xlsx)
library(openxlsx)

options(scipen = 9999)

dat.raw <- read.xlsx("daily_counts.xlsx", sheet="all_stocks")

####################################################################################################################################################

# HI SUPER IMPORTANT: 

# this code needs to be updated each year depending on the focal year of interest and which systems have useable programs (e.g., Quesnel only
# gets a sonar in its dominant year(s)). 

# because of this IT IS NOT REPRODUCIBLE IN IT'S CURRENT FORMAT

####################################################################################################################################################

#                                                             CLEANED

data <- dat.raw %>% 
  mutate(date = as.Date(date, origin="1899-12-30")) %>%
  mutate(yday = lubridate::yday(date)) %>% 
  print()

####################################################################################################################################################

#                                                              10% of run

# 10% arrival dates for all populations available 
p10_dates <- data %>% 
  group_by(stock, year) %>% 
  mutate(cuml_daily_abundance = cumsum(daily_abundance)) %>%
  mutate(daily_perc=(cuml_daily_abundance/sum(daily_abundance))*100) %>% 
  filter(daily_perc <= 10.0) %>%
  group_by(stock, year, data_quality) %>% 
  summarize(p10_date = max(date)) %>%
  mutate(p10_date = as.POSIXct(p10_date, format="%d-%b-%Y")) %>%
  mutate(p10_mday = paste(format(p10_date, "%d-%b"))) %>%
  mutate(p10_yday = lubridate::yday(p10_date)) %>%
  print()

# filter down to relevant 2020 year populations 
forplot <- p10_dates %>% 
  mutate(group = ifelse(year==2020, "2020", ifelse(year==2019, "2019", ifelse(year==2011, "2011", "Historical")))) %>% 
  mutate_at(vars(c(1)), funs(as.factor)) %>%
  filter(stock %in% c("Upper Chilliwack", "Cultus", "Scotch Creek", "Birkenhead", "Chilko", "Nadina")) %>%
  mutate(data_quality = ifelse(year!="2020", NA, data_quality)) %>%
  print()
forplot$stock <- factor(forplot$stock, levels=c("Upper Chilliwack", "Cultus", "Scotch Creek", "Birkenhead", "Chilko", "Nadina"), ordered=T)

# plot 
ggplot(forplot, aes(x=stock, y=as.Date(p10_yday, origin = as.Date("1970-01-01")))) +
  geom_jitter(aes(fill=group, size=group, colour=group), stat="identity", width=0, height=0.4, stroke=1.5, shape=21) +
  scale_fill_manual(name=NULL, breaks=c("2020", "2019", "2011", "Historical"), values=c("#00b8ff", "gray60", "gray80", "gray80")) +
  scale_colour_manual(name=NULL, breaks=c("2020", "2019", "2011", "Historical"), values=c("black", "gray20", "gray20", "gray70")) +
  scale_size_manual(name=NULL, breaks=c("2020", "2019", "2011", "Historical"), values=c(6.5, 5, 5, 4.5)) +
  scale_y_date(date_labels = "%b %d", date_breaks="4 day") +
  geom_text(aes(label=data_quality), size=4, nudge_x=0.22, nudge_y=2, angle=15, check_overlap=T, colour="red") +
  labs(x="", y="") +
  theme_bw() +
  theme(text=element_text(colour="black", size=27, face="bold"),
    panel.grid = element_line(colour="gray85"),
    axis.text.x = element_text(angle=30, hjust=1),
    legend.position = c(0.16,0.50),
    #legend.title = element_blank(), 
    legend.background = element_rect(colour="black"),
    legend.margin=margin(t=-0.1, r=0.15, b=0.1, l=0.1, unit="cm")) +
  coord_flip()















