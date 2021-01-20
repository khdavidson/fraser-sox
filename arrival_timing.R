# Daily passage, arrival, and peak of spawn exploratory analyses 

# this script takes the summary databases available from STAD programs 
# some caveats to these data:
# - there are no SEP channel data included
# - Stellako data are questionable because a) changing program start dates may/may not capture Nadina, and b) uncertainty over the daily % 
#   Stellako/Nadina each year (DNA is not always taken each year so the opportunity to split daily passage by stock composition is not always known)
# - daily passage data are NOT intended to be used to generate escapement estimates/spawner abundances. much post-processing analysis is done to allocate sockeye
#   to lake spawning, tributaries, channels, etc. These data should NEVER be summed to estimate total escapement.
# - daily passage code needs to be updated each year depending on the focal year of interest and which systems have useable programs 
#   (e.g., Quesnel only gets a sonar in its dominant year(s)).
# - forecast groups should be double-checked; taseko in particular might not be represented by forecast grouping value for all yrs (might just be
#   a subset of years)


setwd("~/Documents/ANALYSIS/data")

library(tidyverse)
library(egg)
library(ggridges)
library(xlsx)
library(openxlsx)

options(scipen = 9999)
set.seed(1234)

dat.raw <- read.xlsx("daily_counts.xlsx", sheet="all_stocks")
escdb.raw <- read.xlsx("SKAll-Forecast (June 2020).xlsx", sheet="SKAll")
roving.raw <- read.xlsx("Master Roving Analysis Spreadsheet.xlsx", sheet="count_data", detectDates=T)



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
  filter(!is.na(peak_spawn), !grepl("Early", peak_spawn), !grepl("Late", peak_spawn), !grepl("Mid", peak_spawn)) %>% 
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
  select(year, watershed_group, stock, timing_group, cu, total, eff_fem, est_type, forecast_group, start_date, end_date, est_type) %>%
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
  mutate(era = ifelse(year<2000, "Pre-2000", "2000-present")) %>%
  print()

pos$group <- factor(pos$group, levels=c("Historical", "2019", "2020"), ordered=T)
pos$timing_group <- factor(pos$timing_group, levels = c("", "Early Summer", "Summer", "Late"), ordered=T)


###############
# ROVING DATA #
###############

roving.data <- roving.raw %>% 
  rename(year=Year,
    timing_group=Run.Timing,
    watershed_group=Watershed.Group,
    system=`Stream/Shore`,
    survey=`Survey.#`,
    date=Date,
    survey_type=Survey.Type,
    area=Area,
    area_descr=Area.Description,
    live_obs1=Live.Count.1,
    live_obs2=Live.Count.2,
    oe=`Obs..Eff.`,
    hold_perc=`%Hold`,
    spawn_perc=`%Spawning`,
    spawnout_perc=`%SpawnedOut`,
    carc_male=Carcass.Male,
    carc_fem=Carcass.Female,
    male_nr=Male.Carcass.NR,
    fem_nr=Female.Carcass.NR,
    fem_0=`Female.Carcass.0%`,
    fem_50=`Female.Carcass.50%`,
    fem_100=`Female.Carcass.100%`,
    carc_jack=Carcass.Jack,
    carc_unsex_ground=Ground.Carcass.Unsexed,
    carc_unsex_aerial1=Aerial.Carcass.Unsexed.Observer.1,
    carc_unsex_aerial2=Aerial.Carcass.Unsexed.Observer.2,
    carc_unsex=`Carcass.Unsexed.(interpret."blank".as.zero.carcasses)`) %>% 
  mutate(date=as.Date(date)) %>% 
  print()



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


#-------- PLOT - ****2020 not showing yet because it is not in the escapement database***
ggplot(data=pos, aes(x=as.Date(start_yday, origin = as.Date("1970-01-01")), xend=as.Date(end_yday, origin = as.Date("1970-01-01")), 
  y=reorder(forecast_group, desc(forecast_group)), 
  yend=reorder(forecast_group, desc(forecast_group)), colour=group, alpha=group, size=group)) +
  geom_segment(stat="identity") +
  scale_colour_manual(name="Peak of spawn", breaks=c("2020", "2019", "Historical"), values=c("purple", "#00b8ff", "gray60")) +
  scale_alpha_manual(name="Peak of spawn", breaks=c("2020", "2019", "Historical"), values=c(0.9, 0.6, 0.35)) +
  scale_size_manual(name="Peak of spawn", breaks=c("2020", "2019", "Historical"), values=c(4, 4.5, 3)) +
  scale_x_date(date_labels = "%b %d", date_breaks="10 day") +
  labs(x="", y="") +
  facet_wrap(~timing_group, strip.position = "left", scales = "free_y",  nrow=4) +
  theme_bw() +
  theme(text = element_text(colour="black", size=15),
    panel.grid.major.x = element_line(colour="gray85"),
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


####################################################################################################################################################
####################################################################################################################################################

#                                                            TASEKO EXPLORATION

# SD request Jan 14 2021
# using both roving and escapement databases


###################
# ROVING ANALYSIS #
###################
taseko_rov <- roving.data %>% 
  filter(grepl("Yohetta", system) | grepl("Taseko", system) | grepl("Lastman", system)) %>% 
  select(year:survey_type, area, area_descr, live_obs1:spawnout_perc, carc_male, carc_fem, carc_jack:carc_unsex_aerial2) %>%
  mutate(total_gcarc=carc_male+carc_fem+carc_jack+carc_unsex_ground,
    total_acarc=carc_unsex_aerial1+carc_unsex_aerial2,
    live_avg=(live_obs1+live_obs2)/2) %>%
  select(-c(carc_male, carc_fem, carc_jack, carc_unsex_ground, carc_unsex_aerial1, carc_unsex_aerial2, live_obs1, live_obs2)) %>%
  mutate(total_carcs=ifelse(!is.na(total_acarc), total_acarc, total_gcarc)) %>%
  mutate(system_coarse=ifelse(grepl("Yohetta", system), "Yohetta", system)) %>%
  select(-c(total_gcarc, total_acarc)) %>%
  group_by(year, system_coarse, survey, date) %>%
  summarize(total_carcs=sum(total_carcs, na.rm=T), total_live=sum(live_avg, na.rm=T)) %>%
  mutate_at("year", as.factor) %>%
  mutate(yday=lubridate::yday(date)) %>%
  pivot_longer(cols=c(total_carcs, total_live), names_to="count_type") %>%
  print()


#-------- Plots
#t<-
ggplot() +
  geom_bar(data=taseko_rov%>%filter(system_coarse=="Taseko Lake", value>0), 
    aes(x=as.Date(yday, origin="1970-01-01"), y=value, group=interaction(count_type,year), fill=year), 
    position="stack", stat="identity", colour="black", width=1, size=0.6) +
  scale_x_date(date_labels="%b %d", date_breaks="3 day", limits=c(as.Date(245, origin="1970-01-01"), as.Date(276, origin="1970-01-01"))) +
  scale_y_continuous(limits=c(0,140), breaks=seq(0,140,by=20)) +
  #scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest"))
  scale_fill_manual(breaks=c(2004,2005,2006,2010,2011,2018), values = c("#e1783f","#f3f42e","#01ff52","#01c5ff","#c170d0","gray70"),
    labels=c("2004","2005","2006","2010","2011","2018 (live only)")) +
  labs(x="", y="Count", fill="Year of survey(s)", caption="Figure 1. Taseko carcass (colours) and live (gray) counts recorded on each survey (each bar) from 2003-2018. \nNo record for a year means no survey or no sockeye (live or dead) recorded. Data source: 'taseko_roving'.") +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=12),
    axis.title=element_text(face="bold", size=14),
    panel.grid.major = element_line(colour="gray80"),
    panel.grid.minor = element_blank(),
    legend.position=c(0.85,0.75),
    legend.background = element_rect(colour="black"),
    legend.margin=margin(t=0.1, r=0.25, b=0.1, l=0.25, unit="cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.title = element_text(face="bold", size=13),
    legend.text = element_text(size=12),
    plot.caption = element_text(size=11, color="black", face="italic", hjust=0))

y<-ggplot() +
  geom_bar(data=taseko_rov%>%filter(system_coarse=="Yohetta", value>0), 
    aes(x=as.Date(yday, origin="1970-01-01"), y=value, group=interaction(count_type,year), fill=interaction(count_type,year)), 
    stat="identity", colour="black", width=1, size=0.6) +
  scale_x_date(date_labels="%b %d", date_breaks="3 day", limits=c(as.Date(245, origin="1970-01-01"), as.Date(276, origin="1970-01-01"))) +
  scale_fill_manual(breaks=c("total_carcs.2010","total_carcs.2013","total_live.2013"), values=c("#01ff52","#7001ff","gray70"),
    labels=c("2010","2013","2013 (live)")) +
  labs(x="", y="Count", fill="Yohetta") +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=12),
    axis.title=element_text(face="bold", size=14),
    panel.grid.major = element_line(colour="gray80"),
    legend.position=c(0.85,0.75),
    legend.background = element_rect(colour="black"),
    legend.margin=margin(t=0.1, r=0.25, b=0.1, l=0.25, unit="cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.title = element_text(face="bold", size=13),
    legend.text = element_text(size=12))

ggarrange(t, y, nrow=2)



#######################
# ESCAPEMENT ANALYSIS #
#######################
taseko_pos <- pos %>%
  filter(forecast_group=="Taseko") %>%
  print()

taseko.xtra <- escdb.raw %>% 
  filter(`Stock.Name(stream)`=="Taseko Lake", grepl("Early", spnpeak) | grepl("Mid", spnpeak) | grepl("Early", arrival)) %>% 
  rename(year=Year,
    watershed_group=Watershed.Group.Name,
    stock=`Stock.Name(stream)`,
    timing_group=Timing.Group,
    cu=CU.Name,
    peak_spawn=spnpeak,
    total=Total,
    forecast_group=Forecast) %>%
  mutate(start_date = as.Date(c("15-Sep-1976","15-Sep-1980","1-Sep-1998", "1-Sep-2007", "1-Aug-2016"), format="%d-%b-%Y")) %>%
  mutate(end_date = as.Date(c("22-Sep-1976","22-Sep-1980","8-Sep-1998", "2-Sep-2007", "2-Aug-2016"), format="%d-%b-%Y")) %>% 
  select(year, watershed_group, stock, timing_group, cu, total, eff_fem, est_type, forecast_group, start_date, end_date, est_type) %>%
  mutate(start_yday = lubridate::yday(start_date), end_yday = lubridate::yday(end_date)) %>%
  mutate(forecast_group = ifelse(forecast_group==22, "Taseko", forecast_group)) %>%
  mutate(group = ifelse(year==2020, "2020", ifelse(year==2019, "2019", ifelse(is.na(year), "Historical", "Historical")))) %>%
  mutate(timing_group = ifelse(timing_group=="T1", "", ifelse(timing_group=="T2", "Early Summer", ifelse(timing_group=="T3", "Summer", "Late")))) %>%
  mutate_at(vars(c(4)), funs(as.factor)) %>%
  mutate(era = ifelse(year<2000, "Pre-2000", "2000-present")) %>%
  print()

taseko_pos <- rbind(taseko_pos, taseko.xtra)
taseko_pos <- taseko_pos %>%
  mutate(data_type=ifelse(year%in%c(2007,2016),"arrival",NA)) %>%
  print()


ggplot(taseko_pos, 
  aes(x=as.Date(start_yday, origin = as.Date("1970-01-01")), xend=as.Date(end_yday, origin = as.Date("1970-01-01")), 
      y=reorder(year, desc(year)), yend=reorder(year, desc(year)), colour=est_type, size=total)) +
  geom_segment(stat="identity") +
  geom_text(aes(label=data_type), size=4, nudge_x=2.5, nudge_y=0, angle=0, check_overlap=T, colour="red") +
  scale_colour_manual(breaks=c("Type-2: True Abundance, medium resolution","Type-3: Relative Abundance, high resolution","Type-4: Relative Abundance, medium resolution"), 
    values=c("#4ba0e3", "#ff9f10", "#ce3746")) +
  scale_size_continuous(breaks=seq(65,31667,by=15000), range=c(3,10)) +
  scale_x_date(date_labels = "%b %d", date_breaks="5 day") +
  labs(x="", y="", colour="Estimate type", size="Escapement", caption="Figure 2. Estimated peak of spawn date ranges scaled by escapement size and coloured by escapement program data quality (2007 and 2016 \nare arrival dates only). Type-2 data typically collected from high-precision methods (mark-recapture, SONAR, fence). Type-3 data typically \ncollected from visual surveys or uncertain high-precision programs. Type-4 data from visual surveys with uncertainty, presence/absence, \ncarcass expansions, etc. Data source: 'taseko_escapement'.") +
  theme_bw() +
  theme(text = element_text(colour="black", size=12),
    panel.grid.major.x = element_line(colour="gray80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour="gray80"),
    panel.spacing = unit(0, "lines"),
    axis.title.y = element_text(face="bold"),
    axis.text = element_text(colour="black", size=12),
    #axis.text.x = element_text(angle=0, hjust=1),
    legend.position = c(0.2, 0.75),
    legend.background = element_rect(colour="black"),
    legend.margin=margin(t=0.1, r=0.4, b=0.1, l=0.1, unit="cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.title = element_text(face="bold", size=13),
    legend.text = element_text(size=12),
    plot.caption = element_text(size=11, color="gray20", face="italic", hjust=0)) +
  guides(colour = guide_legend(override.aes = list(size = 2)),
    size = guide_legend(override.aes = list(colour = "gray60"))) 


################################
# EXPORTING SUPPLEMENTARY DATA #
################################

# Exporting raw data uncleaned: pos.raw and roving.raw
# but with just taseko 

taseko.pos.raw <- escdb.raw %>% 
  filter(grepl("Taseko", `Stock.Name(stream)`)) %>% 
  print()

taseko.roving.raw <- roving.raw %>% 
  filter(grepl("Taseko", `Stream/Shore`)) %>% 
  print()

write.xlsx(x=list("taseko_escapement"=taseko.pos.raw, "taseko_roving"=taseko.roving.raw), file="taseko_jan2020_RSA.xlsx", row.names=F)













