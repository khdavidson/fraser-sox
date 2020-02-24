# NAUTLEY 2019 FINAL REPORT CODE
# 29-Jan-2020
# All DNA, scales, length and weight data to be run by DFO are now here.
# More data may come from E-Watch: DNA and physiological samples. 

# libraries and wd
library(tidyverse)
library(XLConnect)
library(xlsx)
library(openxlsx)
library(withr)
library(scales)

setwd("~/ANALYSIS/Data")


####################################################################################################################################################

                                                              ################
                                                              # DNA ANALYSIS #
                                                              ################

# read data
dat <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheet=3, detectDates=T)          # might be very slow! sometimes 'sheet' may have to be changed to 'sheetIndex' depending on the order packages are loaded

dat <- dat %>% 
  mutate(NEWregion1 = ifelse(NEWregion1==4, "Nadina", ifelse(NEWregion1 ==12, "Stellako", NEWregion1))) %>%
  print()

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
  filter(NEWregion1 %in% stocks_of_interest) %>%
  mutate_at(vars(c(10:11,13:14,18,20,22,24)), funs(as.character)) %>% 
  mutate_at(vars(c(10:11,13:14,18,20,22,24)), funs(as.factor)) %>% 
  print()

# Length - white
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

# Weight - white
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

    # Length NADINA - BLACK
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

    # Weight NADINA - black
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
    

# read data
dat <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheet=3, detectDates=T)          # might be very slow! sometimes 'sheet' may have to be changed to 'sheetIndex' depending on the order packages are loaded

dat <- dat %>% 
  mutate(NEWregion1 = ifelse(NEWregion1==4, "Nadina", ifelse(NEWregion1 ==12, "Stellako", NEWregion1))) %>%
  print()

age <- dat %>% 
  filter(!is.na(age)) %>% 
  group_by(age) %>% 
  summarize(n=n()) %>%
  mutate(sum = sum(n)) %>% 
  mutate(propn = n/sum) %>%
  mutate(age = ifelse(age==0, "Age-0", ifelse(age==1, "Age-1", "Age-2"))) %>%
  print()

age$age <- factor(age$age, levels=c("Age-2", "Age-1", "Age-0"), ordered=T)
    
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
    
ggplot(subset(age.cu %>% filter(NEWregion1=="Nadina")), aes(x=age, y=n)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=age, xend=age, y=0, yend=n))  
ggplot(subset(age.cu %>% filter(NEWregion1=="Stellako")), aes(x=age, y=n)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=age, xend=age, y=0, yend=n)) 
    
    
    
    
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

  # manually change hrs_in_window for shorter times 
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

# read in discharge data 
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

########
# plot #
########

# CPUE - white
ggplot() +
  geom_ribbon(data=discharge2, aes(x=date, ymin=min_dis*10, ymax=max_dis*10), fill="gray60") +
  geom_line(data=discharge2, aes(x=date, y=mean_dis*10), size=1.2, colour="black") +
  geom_bar(data=cpue2, aes(x=start_date, y=cpue), stat="identity", fill="gray80", colour="black") + 
  scale_y_continuous(breaks = seq(0,1300,250),
                     sec.axis = sec_axis(~./10, name = expression(bold("Discharge"~m^3/s)), breaks=seq(0,150,25))) +
  scale_x_date(limits=as.Date(c("2019-04-13", "2019-05-27")), breaks="3 day", labels = date_format("%b %d")) +
  labs(x="Date", y="CPUE \n(smolts/hour in peak period)") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face="bold"),
    axis.title.y = element_text(margin = margin(t=0, b=0, l=0, r=6)),
    axis.title.y.right = element_text(margin=margin(t=0, b=0, l=7, r=0), face="bold"),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    legend.text = element_text(size=14), 
    legend.title = element_text(size=15))

# CPUE ONLY - black
ggplot(data=cpue2, aes(x=start_date, y=cpue)) +
  geom_bar(stat="identity", fill="#fff7a4", colour="#fff7a4", alpha=0.85, width=1.15) + 
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

# CPUE + discharge - black
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




