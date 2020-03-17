# chilko temporal long-term 
## daily smolt outmigration by year (1 and 2 year olds combined), last update by S. Decker Feb 21, 2019

library(tidyverse)
library(lubridate)

# read
setwd("~/ANALYSIS/Data")
data <- read.csv("Chilko summary_daily.csv", na.strings=c("","NA"))
data[2:33] <- sapply(data[2:33], as.numeric)

# re-organize 
data2 <- data %>% 
  gather("year", "smolt_count", 2:33) %>% 
  rename(monthday = X) %>%
  mutate(year = substring(year, 2)) %>%
  mutate(date = paste(monthday, year, sep="-")) %>%
  mutate(date = lubridate::dmy(date)) %>%
  mutate(doy = strftime(date, format = "%j")) %>% 
  mutate(smolt_count = ifelse(is.na(smolt_count), 0, smolt_count)) %>%
  mutate_at(vars(c(5)), funs(as.numeric)) %>%
  group_by(year) %>% 
  mutate(cuml_smolt = cumsum(smolt_count)) %>%
  mutate(cuml_propn = cuml_smolt/sum(smolt_count)) %>% 
  print()


# plot all years
ggplot(data2, aes(x=doy, y=smolt_count, group=year, colour=year, fill=year)) +
  geom_point(shape=21, colour="black", stroke=1.3, size=3) +
  geom_line(size=1.2) +
  theme_bw() +
  theme(text = element_text(size=25))

ggplot(data2, aes(as.Date(doy, origin = "2020-01-01"), y=cuml_smolt, group=year, colour=year, fill=year)) +
  geom_point(shape=21, colour="black", stroke=1.3, size=3) +
  geom_line(size=1.2) +
  #scale_y_continuous() +
  scale_x_date(date_labels = "%b-%d", date_breaks="5 days") +
  labs(x="Date", y="Cumulative smolt count") +
  theme_bw() +
  theme(text = element_text(size=25))


# recent since 2000 
recent <- data2 %>% 
  filter(year > 2000) %>% 
  print()

ggplot(recent, aes(as.Date(doy, origin = "2020-01-01"), y=cuml_smolt, group=year, colour=year, fill=year)) +
  geom_point(shape=21, colour="black", stroke=1.3, size=3) +
  geom_line(size=1.2) +
  scale_x_date(date_labels = "%b-%d", date_breaks="5 days") +
  labs(x="Date", y="Cumulative smolt count") +
  theme_bw()+
  theme(text = element_text(size=25))


#########
# Apply quartiles and 10% filters
propns <- data2 %>% 
  mutate(quartiles = ifelse(cuml_propn <= 0.25, "25%", 
    ifelse(cuml_propn <= 0.50, "50%", 
      ifelse(cuml_propn <= 0.75, "75%", "100%")))) %>% 
  mutate(p10 = ifelse(cuml_propn <= 0.10, "10%", NA)) %>% 
  print()

# Filter by date when 10% pass by chilko
p10 <- propns %>% 
  filter(p10=="10%") %>% 
  group_by(year) %>% 
  summarize(monthday_p10 = max(date)) %>% 
  mutate_at(vars(c(2)), funs(as.character)) %>%
  mutate(monthday_p10 = substring(monthday_p10, 6)) %>%
  print()
write.csv(p10, "chilko_10p.csv", row.names=F)

# Filter by date when 25% pass by chilko
p25 <- propns %>% 
  filter(quartiles=="25%") %>% 
  group_by(year) %>% 
  summarize(date_p25 = max(date)) %>% 
  mutate_at(vars(c(2)), funs(as.character)) %>%
  mutate(date_p25 = substring(date_p25, 6)) %>%
  print()
write.csv(p25, "chilko_25p.csv", row.names=F)





