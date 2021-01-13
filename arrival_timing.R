# Daily passage exploratory analyses 

# this script takes the summary database of all high-precision STAD programs that offer daily abundance estimates (fences, sonars) from 2000 onward.
# some caveats to these data:
# - there are no SEP channel data included
# - Stellako data are questionable because a) changing program start dates may/may not capture Nadina, and b) uncertainty over the daily % 
#   Stellako/Nadina each year (DNA is not always taken each year so the opportunity to split daily passage by stock composition is not always known)
# - these are NOT intended to be used to generate escapement estimates/spawner abundances. much post-processing analysis is done to allocate sockeye
#   to lake spawning, tributaries, channels, etc. These data should NEVER be summed to estimate total escapement.

setwd("~/Documents/ANALYSIS/data")

library(tidyverse)
library(egg)
library(xlsx)
library(openxlsx)

options(scipen = 9999)
set.seed(1234)

dat.raw <- read.xlsx("daily_counts.xlsx", sheet="all_stocks")
escdb.raw <- read.xlsx("SKAll-Forecast (June 2020).xlsx", sheet="SKAll")

# ALLOO! SUPER IMPORTANT: 
# this code needs to be updated each year depending on the focal year of interest and which systems have useable programs (e.g., Quesnel only
# gets a sonar in its dominant year(s)).
# because of this IT IS NOT REPRODUCIBLE IN IT'S CURRENT FORMAT

# Organization key:

    ################
    # MAIN HEADING #
    ################
    
    #-------- sub-heading
    
    # minor heading/comment


###################################################################################################################################################

#                                                                         CLEANED

######################
# DAILY PASSAGE DATA #
######################
daily.data <- dat.raw %>% 
  mutate(date = as.Date(date, origin="1899-12-30")) %>%
  mutate(yday = lubridate::yday(date)) %>% 
  mutate(group = ifelse(year==2020, "2020", ifelse(year==2019, "2019", "Historical"))) %>% 
  print()


############################################
# ESCAPEMENT DATABASE - PEAK OF SPAWN DATA #
############################################
pos.raw <- escdb.raw %>% 
  rename(year=Year,
    watershed_group=Watershed.Group.Name,
    stock=`Stock.Name(stream)`,
    timing_group=Timing.Group,
    cu=CU.Name,
    peak_spawn=spnpeak,
    total=Total,
    forecast_group=Forecast) %>%
  filter(year>2000, !is.na(peak_spawn), !grepl("Early", peak_spawn), !grepl("Late", peak_spawn), !grepl("Mid", peak_spawn)) %>% 
  print()

# fixing dates - there are all sorts of entry typos, extra commas, etc. so this cleans up the dates so they are all the same formats
dt <- data.frame(str_split_fixed(pos.raw$peak_spawn, "[-]|\\s", 4))
names(dt) <- c("st_month","st_day","end_t1","end_t2")
dt$yr <- pos.raw$year

dt1 <- data.frame(sapply(dt, function(x)gsub("[.]", "", x)))          # removing dot(.) and replacing with ""
dt1$start_date <- as.Date(paste0(dt1$st_day, dt1$st_month, dt1$yr), format="%d%b%Y")
dt1$end_date <- ifelse(dt1$end_t2=="", paste0(dt1$end_t1, dt1$st_month), paste0(dt1$end_t2, dt1$end_t1))
dt1$end_date <- as.Date(paste0(dt1$end_date, dt1$yr), format="%d%b%Y")

dates <- dt1 %>% 
  select(start_date, end_date) %>% 
  print() 

# check before joining - these need to match 
nrow(dates)
nrow(pos.raw)


#-------- Join 
pos <- cbind(pos.raw, dates)


#-------- Clean joined frame 
pos <- pos %>% 
  select(year, watershed_group, stock, timing_group, cu, total, eff_fem, est_type, forecast_group, start_date, end_date) %>%
  mutate(start_yday = lubridate::yday(start_date),
    end_yday = lubridate::yday(end_date)) %>%
  mutate(forecast_group = ifelse(forecast_group==1, "Early Stuart",
    ifelse(forecast_group==2, "Late Stuart",
      ifelse(forecast_group==3, "Stellako",
        ifelse(forecast_group==4, "Bowron", 
          ifelse(forecast_group==5, "Raft",
            ifelse(forecast_group==6, "Quesnel",
              ifelse(forecast_group==7, "Chilko",
                ifelse(forecast_group==8, "Seymour",
                  ifelse(forecast_group==9, "Late Shuswap", 
                    ifelse(forecast_group==10, "Birkenhead",
                      ifelse(forecast_group==11, "Cultus",
                        ifelse(forecast_group==12, "Portage",
                          ifelse(forecast_group==13, "Weaver Creek",
                            ifelse(forecast_group==14, "Fennel Creek",
                              ifelse(forecast_group==15, "Scotch Creek",
                                ifelse(forecast_group==16, "Gates",
                                  ifelse(forecast_group==17, "Nadina",
                                    ifelse(forecast_group==18, "Upper Pitt River",
                                      ifelse(forecast_group==19, "Harrison",
                                        ifelse(forecast_group==20, "Fraser Pink",
                                          ifelse(forecast_group==21, "South Thompson",
                                            ifelse(forecast_group==22, "Taseko",
                                              ifelse(forecast_group==23, "Chilliwack",
                                                ifelse(forecast_group==24, "Nahatlatch",
                                                  ifelse(forecast_group==25, "North Thompson",
                                                    ifelse(forecast_group==26, "North Thompson Misc",
                                                      ifelse(forecast_group==27, "Widgeon",
                                                        ifelse(forecast_group==28, "Harrison (D/S)", forecast_group))))))))))))))))))))))))))))) %>%
  mutate(group = ifelse(year==2020, "2020", ifelse(year==2019, "2019", ifelse(is.na(year), "Historical", "Historical")))) %>%
  filter(!is.na(forecast_group)) %>%
  mutate(timing_group = ifelse(timing_group=="T1", "", ifelse(timing_group=="T2", "Early Summer", ifelse(timing_group=="T3", "Summer", "Late")))) %>%
  mutate_at(vars(c(4)), funs(as.factor)) %>%
  print()

pos$group <- factor(pos$group, levels=c("Historical", "2019", "2020"), ordered=T)
pos$timing_group <- factor(pos$timing_group, levels = c("", "Early Summer", "Summer", "Late"), ordered=T)


###################################################################################################################################################
###################################################################################################################################################


#                                                                   RUN CURVES w/ 10%

#########################
# CLEAN AND EXTRACT 10% #
#########################
# Extract 10% dates for all systems and years in the spreadsheet
# exclude Stellako as it isn't appropriate for this type of analysis
dates_10p <- daily.data %>% 
  filter(stock!="Stellako-Nadina") %>%
  group_by(stock, year) %>% 
  mutate(cuml_daily_abundance=cumsum(daily_abundance)) %>%
  mutate(cuml_daily_perc=(cuml_daily_abundance/sum(daily_abundance))*100) %>% 
  filter(cuml_daily_perc>=10.0) %>%
  group_by(stock, year) %>%       # data quality could be removed depending on if this is done w/ in-season/prelim data or all near final
  filter(date==min(date)) %>%
  mutate(date=as.POSIXct(date, format="%d-%b-%Y")) %>%
  mutate(p10_mday=paste(format(date, "%d-%b"))) %>%
  print()

# *year specific* create grouping variable and extract the stocks of interest for the focal year
# in this case, want to plot 2019 AND 2020 together 
arrival_10p_2019_20 <- dates_10p %>% 
  mutate_at(vars(c(1)), funs(as.factor)) %>%
  filter(stock %in% c("Cultus", "Scotch Creek", "Birkenhead", "(Horsefly River)", "(Mitchell River)", "Quesnel", "Chilko", "Nadina", 
    "Upper Chilliwack")) %>%
  select(stock, year, date, yday, group) %>%
  mutate(date = as.Date(date)) %>%
  mutate(p10_flag = "10% date") %>%
  print()

# export the 10% dates 
write.csv(arrival_10p_2019_20, "arrival_10p_dates.csv")

# join for plotting
forplot <- left_join(data, arrival_10p_2019_20, by=c("stock", "year", "date", "yday", "group"))


#########
# PLOTS #
#########
#-------- FIG 1. Jittered dot plot of 10% dates over time
arrival_10p_2019_20$stock <- factor(arrival_10p_2019_20$stock, levels=c("Upper Chilliwack", "Cultus", "Scotch Creek", "Birkenhead", 
  "(Horsefly River)", "(Mitchell River)", "Quesnel",  "Chilko", "Nadina"), ordered=T)

ggplot(arrival_10p_2019_20, aes(x=stock, y=as.Date(yday, origin = as.Date("1970-01-01")))) +
  geom_jitter(aes(fill=group, size=group, colour=group), stat="identity", width=0, height=0.4, stroke=1.5, shape=21) +
  scale_fill_manual(name=NULL, breaks=c("2020", "2019", "Historical"), values=c("#00b8ff", "gray60", "gray80")) +
  scale_colour_manual(name=NULL, breaks=c("2020", "2019", "Historical"), values=c("black", "gray20", "gray70")) +
  scale_size_manual(name=NULL, breaks=c("2020", "2019", "Historical"), values=c(6.5, 5, 4.5)) +
  scale_y_date(date_labels = "%b %d", date_breaks="4 day") +
  #geom_text(aes(label=data_quality), size=4, nudge_x=0.22, nudge_y=2, angle=15, check_overlap=T, colour="red") +   
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


#-------- FIG 2. Run timing curves with focal 10% points
forplot$group <- factor(forplot$group, levels=c("Historical", "2019", "2020"), ordered=T)
forplot <- forplot %>% 
  filter(stock != "Mitchell River", stock != "Horsefly River") %>% 
  print()

# Fig 2a. Single stock - manually change stock=="" call
ggplot(data=forplot) +
  geom_line(data=subset(forplot %>% filter(stock=="Cultus")), 
    aes(x=as.Date(yday, origin = as.Date("1970-01-01")), y=daily_abundance, group=year, colour=group, alpha=group), size=1) +
  geom_point(data=subset(forplot %>% filter(stock=="Cultus", p10_flag=="10% date")), 
    aes(x=as.Date(yday, origin = as.Date("1970-01-01")), y=daily_abundance, group=year, colour=group, fill=group, size=group, alpha=group), 
    stroke=1.5, shape=21) +
  scale_colour_manual(name="Cultus", breaks=c("2020", "2019", "Historical"), values=c("#00b8ff", "gray40", "gray60")) +
  scale_fill_manual(name="Cultus", breaks=c("2020", "2019", "Historical"), values=c("#00b8ff", "gray40", "gray60")) +
  scale_alpha_manual(name="Cultus", breaks=c("2020", "2019", "Historical"), values=c(0.9, 0.9, 0.5)) +
  scale_size_manual(name="Cultus", breaks=c("2020", "2019", "Historical"), values=c(6.5, 5, 4.5)) +
  scale_x_date(date_labels = "%b %d", date_breaks="7 day") +
  scale_y_continuous(breaks=seq(0,1300,by=400)) +
  labs(x="", y="Daily abundance") +
  theme_bw() +
  theme(text = element_text(colour="black", size=27),
    panel.grid = element_line(colour="gray85"),
    axis.title.y = element_text(face="bold"),
    axis.text = element_text(colour="black"),
    axis.text.x = element_text(angle=30, hjust=1),
    legend.position = c(0.16,0.85),
    legend.background = element_rect(colour="black"),
    legend.margin=margin(t=0, r=0.15, b=0.1, l=0.1, unit="cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.title = element_text(face="bold")) 

# Fig 2b. Faceted stocks
ggplot(data=forplot) +
  geom_line(data=forplot, 
    aes(x=as.Date(yday, origin = as.Date("1970-01-01")), y=daily_abundance, group=year, colour=group, alpha=group), size=0.7) +
  geom_point(data=subset(forplot %>% filter(p10_flag=="10% date")), 
    aes(x=as.Date(yday, origin = as.Date("1970-01-01")), y=daily_abundance, group=year, colour=group, fill=group, size=group, alpha=group), 
    stroke=1.2, shape=21) +
  scale_colour_manual(name=NULL, breaks=c("2020", "2019", "Historical"), values=c("#00b8ff", "gray40", "gray60")) +
  scale_fill_manual(name=NULL, breaks=c("2020", "2019", "Historical"), values=c("#00b8ff", "gray40", "gray60")) +
  scale_alpha_manual(name=NULL, breaks=c("2020", "2019", "Historical"), values=c(0.9, 0.9, 0.5)) +
  scale_size_manual(name=NULL, breaks=c("2020", "2019", "Historical"), values=c(4, 3, 2)) +
  scale_x_date(date_labels = "%b %d", date_breaks="14 day") +
  #scale_y_continuous(breaks=seq(0,1300,by=400)) +
  labs(x="", y="Daily abundance") +
  theme_bw() +
  theme(text = element_text(colour="black", size=17),
    panel.grid = element_blank(),
    axis.title.y = element_text(face="bold"),
    axis.text = element_text(colour="black"),
    axis.text.x = element_text(angle=30, hjust=1),
    legend.position = c(0.7,0.1),
    legend.background = element_rect(colour="black"),
    legend.margin=margin(t=0, r=0.15, b=0.1, l=0.1, unit="cm"),
    legend.spacing.y = unit(0.2, "cm")) +
  facet_wrap(~stock, scales="free")


####################################################################################################################################################
####################################################################################################################################################


#                                                           PEAK OF SPAWN RANGES 

# this script takes the summary database of peak of spawn
# some caveats to these data:
# - variable data quality exists based on effort (survey frequency and duration) so these should be taken with a grain of salt! 


#-------- PLOT
ggplot(data=pos, aes(x=as.Date(start_yday, origin = as.Date("1970-01-01")), xend=as.Date(end_yday, origin = as.Date("1970-01-01")), 
  y=reorder(forecast_group, desc(forecast_group)), 
  yend=reorder(forecast_group, desc(forecast_group)), colour=group, alpha=group, size=group)) +
  geom_segment(stat="identity") +
  scale_colour_manual(name="Peak of spawn", breaks=c("2020", "2019", "Historical"), values=c("#00b8ff", "#00b8ff", "gray60")) +
  scale_alpha_manual(name="Peak of spawn", breaks=c("2020", "2019", "Historical"), values=c(0.9, 0.6, 0.35)) +
  scale_size_manual(name="Peak of spawn", breaks=c("2020", "2019", "Historical"), values=c(4, 4.5, 3)) +
  scale_x_date(date_labels = "%b %d", date_breaks="10 day") +
  labs(x="", y="") +
  facet_wrap(~timing_group, strip.position = "left", scales = "free_y",  nrow=4) +
  theme_bw() +
  theme(text = element_text(colour="black", size=15),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour="gray85"),
    panel.spacing = unit(0, "lines"),
    axis.title.y = element_text(face="bold"),
    axis.text = element_text(colour="black"),
    axis.text.x = element_text(angle=30, hjust=1),
    legend.position = c(0.90, 0.81),
    legend.background = element_rect(colour="black"),
    legend.margin=margin(t=0.1, r=0.25, b=0.1, l=0.1, unit="cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.title = element_text(face="bold", size=13),
    legend.text = element_text(size=13),
    strip.placement = "outside",                     
    strip.background = element_rect(fill = "white", colour="white"),
    strip.text.y = element_text(angle=90, colour="black", face="bold", size=13)) 


####################################################################################################################################################
####################################################################################################################################################


#                                                   NADINA MIGRATION 2020 vs. HISTORICAL

# Scott request to share with PSC (Steve Latham, Eric Taylor) 
# Jan 11, 2021

#########
# CLEAN #
#########
sn.df <- daily.data %>% 
  filter(stock%in%c("Nadina", "Stellako-Nadina")) %>% 
  mutate_at(vars(c(1,2)), funs(as.factor)) %>%
  mutate(analysis=ifelse(grepl("extrapolated", comments), "extrapolated", "observed")) %>%
  print()

sn.df$analysis <- factor(sn.df$analysis, levels=c("observed", "extrapolated"), ordered=T)

#-------- Stellako program start dates - to see what years had early starts and would have captured Nadina
stel.start <- sn.df %>% 
  filter(stock=="Stellako-Nadina") %>% 
  group_by(year) %>% 
  summarize(min(date)) %>%
  print()


########
# PLOT #
########
#-------- Nadina & Stellako facet plot
n<-ggplot(data=sn.df %>% filter(stock=="Nadina", analysis!="extrapolated"), 
        aes(x=as.Date(yday, origin="1970-01-01"), y=daily_abundance, group=year, colour=year)) +
  geom_line(size=1.2) +
  scale_x_date(date_labels="%b %d", breaks="10 day", limits = c(as.Date(210, origin="1970-01-01"), as.Date(285, origin="1970-01-01"))) +
  scale_y_continuous(breaks=seq(0,10000, by=2000)) +
  scale_colour_manual(breaks=c(2016,2018,2019,2020), values=c("#1A7CFF", "#bd00ff", "#EA8824", "#24EA25")) +
  labs(x="", y="Daily abundance", colour="NADINA") +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=10),
    axis.title=element_text(face="bold", size=12),
    panel.grid = element_blank(),
    legend.position=c(0.85,0.80),
    legend.background = element_rect(colour="black"),
    legend.margin=margin(t=0.1, r=0.25, b=0.1, l=0.25, unit="cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.title = element_text(face="bold", size=12),
    legend.text = element_text(size=10))

s<-ggplot(sn.df %>% filter(stock=="Stellako-Nadina", !grepl("late install", comments), analysis!="extrapolated"), 
    aes(x=as.Date(yday, origin="1970-01-01"), y=daily_abundance, group=year, colour=year)) +
  geom_line(size=1.2) +
  scale_x_date(date_labels="%b %d", breaks="10 day", limits=c(as.Date(210, origin="1970-01-01"), as.Date(285, origin="1970-01-01"))) +
  scale_y_continuous(breaks=seq(0,15000, by=2500)) +
  scale_colour_manual(breaks=c(2018,2019,2020), values=c("#bd00ff", "#EA8824", "#24EA25")) +
  labs(x="", y="Daily abundance", colour="STELLAKO") +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=10),
    axis.title=element_text(face="bold", size=12),
    panel.grid = element_blank(),
    legend.position=c(0.85,0.80),
    legend.background = element_rect(colour="black"),
    legend.margin=margin(t=0.1, r=0.25, b=0.1, l=0.25, unit="cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.title = element_text(face="bold", size=12),
    legend.text = element_text(size=10))

ggarrange(s, n, nrow=2)







