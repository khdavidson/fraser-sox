# south thompson historical juvenile sampling 

# read in
setwd("~/ANALYSIS/Data")
data <- read.csv("South Thompson Smolt data.csv")

library(tidyverse)


# re-format, clean
data2 <- data %>% 
  mutate_at(vars(c(1)), funs(as.factor)) %>% 
  mutate(Date = lubridate::dmy(Date)) %>% 
  mutate(doy = strftime(Date, format = "%j")) %>% 
  mutate_at(vars(c(43)), funs(as.numeric)) %>%
  mutate_at(vars(c(18)), funs(as.numeric)) %>%
  print()

summary <- data2 %>% 
  group_by(Year, doy) %>% 
  summarize(mean_ss = mean(Sockeye.Smolt), sd=sd(Sockeye.Smolt), set=max(Set.Number)) %>% 
  filter(!is.na(Year)) %>%
  print()

summary2 <- data2 %>% 
  filter(!is.na(Year)) %>%
  group_by(Year) %>% 
  summarize(n_sets = n(), sum_fish = sum(Sockeye.Smolt)) %>%
  mutate(CPUE = sum_fish/n_sets) %>%
  print()
  
group_by(Year) %>% 
  summarize(mean=sum/n()) %>% 
  print()

# plot over time
ggplot(summary, aes(x=as.Date(doy, origin = "2012-01-01"), y=mean_ss, group=Year, fill=Year, label=set)) +
  geom_ribbon(aes(x=as.Date(doy, origin = "2012-01-01"), ymin=abs(mean_ss-sd), ymax=mean_ss+sd), alpha=0.25) +
  geom_line(size=1.5, aes(colour=Year)) +
  geom_point(shape=21, size=4, stroke=1, colour="black") +
  labs(x="Date", y="Smolts per set") +
  geom_text(vjust=-1.5) +
  theme_bw() 









