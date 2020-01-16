# Nautley tagging sim ## 

setwd("~/Data")
library(tidyverse)

# 1999 nautley data
naut99 <- read.csv("1999 Nautley.csv")



# tagging sim 
naut99 <- naut99 %>% 
  unite(date, date, year, sep="-")

naut99 <- naut99 %>%
  mutate(sox_50 = total_sox*0.5) %>% 
  print()

naut99$hour <- factor(naut99$hour, levels=c("20:00", "21:00", "22:00", "23:00", "0:00", "1:00", "2:00", "3:00", "4:00"), ordered=T)

ggplot(naut99, aes(x=hour, group=date)) + 
  geom_line(aes(y=total_sox)) + 
  geom_line(aes(y=sox_50))








naut99 %>% group_by(date) %>% summarize(sum=sum(total_sox))
