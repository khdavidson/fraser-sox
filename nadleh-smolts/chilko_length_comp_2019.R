# quick length comparisons of Chilko, Cultus and Nautley in 2019
# for Michelle Lloyd (RM) to show to TNG Fisheries Forum 
# apr 29 2020

setwd("~/ANALYSIS/Data")
library(tidyverse)

# read in
data <- read.csv("chilko_length_comparisons.csv")

# summarize
summary <- data %>%
  group_by(project) %>% 
  summarize(mean = mean(length_mm), sd=sd(length_mm)) %>% 
  print()

# plot 
ggplot(summary, aes(x=project, y=mean, fill=project)) +
  geom_bar(stat="identity", colour="black", size=1) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.1, size=1) +
  scale_y_continuous(limits=c(0,125), breaks=seq(0,125, by=25)) +
  labs(x="", y="Length (mm)") +
  theme_bw() +
  theme(axis.title = element_text(size=24, face = "bold"),
    axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
    axis.text = element_text(size=21, colour="black"),
    #axis.text.x = element_text(angle=45, hjust=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(1.5, "mm"),
    panel.grid.major = element_blank(),#line(colour="gray80", size=0.8),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(size=1.1),
    legend.text = element_text(size=21), 
    legend.title = element_blank(),
    legend.position = c(0.1,0.9),
    legend.background = element_rect(colour="black", size=0.8),
    legend.key.size = unit(7, "mm"),
    legend.margin = margin(t=-2,b=4,l=5,r=8))

