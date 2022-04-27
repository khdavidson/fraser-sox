# NADLEH and STELLAKO 2021
# DNA pull requests for MGL and scale priorities for PSC





#                                                 NOTE THIS SCRIPT DIVDED INTO 3 SECTIONS:

# Initial DNA investigation and pull, lines ~15-214
# PSC scale priorities, lines ~218-335
# Second DNA investigation and pull, lines ~343 onward.


##################################################################################################################################################

library(tidyverse)
library(readxl)

setwd("~/ANALYSIS/data")

# Biosample data 
nad.bio.raw <- read_excel("nadleh_data_entry_2021_verifiedSH.xlsx", sheet="biosampling", na="NA")
stel.bio.raw <- read_excel("stellako_data_entry_2021_verifiedSH.xlsx", sheet="biosampling", na="NA")
# Catch data
nad.catch.raw <- read_excel("nadleh_data_entry_2021_verifiedSH.xlsx", sheet="nightly_catch", na="NA")
stel.catch.raw <- read_excel("stellako_data_entry_2021_verifiedSH.xlsx", sheet="nightly_catch", na="NA")

##################################################################################################################################################

#                                                            JOIN AND CLEAN etc.

# Join first for easy cleaning 
bio.raw <- rbind(nad.bio.raw, stel.bio.raw)
catch.raw <- rbind(nad.catch.raw, stel.catch.raw)

# Clean 
bio.dat <- bio.raw %>% 
  mutate_at(c("date_opened", "date_closed"), as.Date)

catch.dat <- catch.raw %>% 
  mutate_at(c("date_opened", "date_closed", "date_true"), as.Date)


#--------- PULL DNA FISH
dna.dat <- bio.dat %>%
  filter(!is.na(whatman_sheet) & !is.na(whatman_cell), 
         !grepl("hinook", comments),
         !grepl("don't run", comments),
         !grepl("dead", comments),
         !grepl("issed length", comments),
         !grepl("no fin clip", comments)) %>%  
  print()


##################################################################################################################################################

#                                                         RAW ABUNDANCE PATTERNS 

daily.catch <- catch.dat %>% 
  group_by(site, date_closed) %>% 
  summarize(total = sum(total_unmarked, na.rm=T)+sum(total_recaps, na.rm=T)) %>% 
  print()


##################################################################################################################################################

#                                                             DATA EXPLORATION

#-------- TABLES
# Nadleh 
nadleh.dna <- dna.dat %>% 
  filter(site=="Nadleh") %>% 
  group_by(date_closed) %>% 
  summarize(n=n()) %>%
  mutate(n_analyzed = ifelse(date_closed==as.Date("2021-04-25") | date_closed>=as.Date("2021-04-28") & date_closed<=as.Date("2021-05-16"), 10, 
                             ifelse(date_closed==as.Date("2021-04-26") | date_closed==as.Date("2021-04-27"), 20, NA))) %>%
  print() 

sum(nadleh.dna$n_analyzed, na.rm=T)

# Analysis plan for Nadleh: send n=10 samples each day for the main peaks, plus inflection points for continuous. 
# The tails are less important right now as they are the stragglers, but may be useful based on Stellako DNA results.


# Stella
stella.dna <- dna.dat %>% 
  filter(site=="Stellako") %>% 
  group_by(date_closed) %>% 
  summarize(n=n()) %>%
  mutate(cutoff = ifelse(date_closed>=as.Date("2021-05-04") & date_closed<=as.Date("2021-05-17") & n>=20, 20, 
                         ifelse(date_closed>=as.Date("2021-05-04") & date_closed<=as.Date("2021-05-17") & n<20, n,
                                ifelse(n>10, 10, n)))) %>%
  print()

sum(stella.dna$cutoff)

# Analysis plan for Stellako: In the major peak period (see date range above), 30 per day unless <30 samples were collected, in that case will
# run everything from that period. From the tails, n=10. Tails are more important here to see if there are any Stellako fish, especially early
# as it might line up with Nadleh migration. 



#-------- PLOTS
# Nadleh
ggplot() +
  geom_bar(data=dna.dat%>%filter(site=="Nadleh")%>%group_by(site, date_closed)%>%summarize(n=n()), 
           aes(x=date_closed, y=n), stat="identity", position="dodge", colour="orange", fill="orange", alpha=0.7, width=0.8) +
  geom_point(data=daily.catch%>%filter(site=="Nadleh"), aes(x=date_closed, y=total/100), size=2, alpha=0.6) +
  geom_line(data=daily.catch%>%filter(site=="Nadleh"), aes(x=date_closed, y=total/100), size=1, alpha=0.6) +
  geom_bar(data=nadleh.dna, aes(x=date_closed, y=n_analyzed), stat="identity", position="dodge", fill="green", alpha=0.5) +
  scale_y_continuous(breaks=seq(0,55,by=5), name="DNA samples", sec.axis = sec_axis(~.*100, name="Catch", breaks=seq(0,6000,by=1000))) +
  scale_x_date(date_breaks="3 day", date_labels="%B %d") +
  theme_bw()

# Stella 
ggplot(data=dna.dat%>%filter(site=="Stellako")%>%group_by(site, date_closed)%>%summarize(n=n()), aes(x=date_closed, y=n)) +
  geom_bar(stat="identity", position="dodge", fill="aquamarine") +
  geom_point(data=daily.catch%>%filter(site=="Stellako"), aes(x=date_closed, y=total/10), size=2, alpha=0.6) +
  geom_line(data=daily.catch%>%filter(site=="Stellako"), aes(x=date_closed, y=total/10), size=1, alpha=0.6) +
  geom_bar(data=stella.dna, aes(x=date_closed, y=cutoff), stat="identity", position="dodge", fill="purple", alpha=0.5) +
  scale_y_continuous(breaks=seq(0,70,by=5), name="DNA samples", sec.axis = sec_axis(~.*10, name="Catch", breaks=seq(0,700,by=100))) +
  scale_x_date(date_breaks="3 day", date_labels="%B %d") +
  theme_bw()




################################################################################################################################################

#                                                             DNA SAMPLE #1 PULL

#------- NADLEH DNA PULL
# Date ranges to pull from: 
# 2021-04-25 to 2021-04-28 inclusive (n=10 per day)
# 2021-04-30 to 2021-05-05 (n=10 per day)
# 2021-05-07 to 2021-05-16 (n=10 per day)
set.seed(123)
nadleh.rando10 <- dna.dat %>% 
  filter(site=="Nadleh", 
         date_closed==as.Date("2021-04-25") | date_closed>=as.Date("2021-04-28"), date_closed<=as.Date("2021-05-16")) %>% 
  group_by(date_closed) %>%
  slice_sample(n=10) %>%
  print()

set.seed(123)
nadleh.rando20 <- dna.dat %>% 
  filter(site=="Nadleh", 
         date_closed==as.Date("2021-04-26") | date_closed==as.Date("2021-04-27")) %>% 
  group_by(date_closed) %>%
  slice_sample(n=20) %>%
  print()

nadleh.2yo <- dna.dat %>% 
  filter(grepl("2 year old", comments)) %>% 
  print()

nadleh.first.pull <- rbind(nadleh.rando10, nadleh.rando20, nadleh.2yo)


#------- STELLA DNA PULL
# Date ranges to pull from: 
# Tails: Start to 2021-05-04 inclusive and 2021-05-18 to end (n=10 per day)
# 2021-05-04 to 2021-05-17 except for 2021-05-06 (n=20 per day)
# 2021-05-06 (n=all)
set.seed(123)
stella.rando <- dna.dat %>% 
  filter(site=="Stellako",
         date_closed>=as.Date("2021-05-04") & date_closed<=as.Date("2021-05-05") | 
         date_closed>=as.Date("2021-05-07") & date_closed<=as.Date("2021-05-17")) %>%
  group_by(date_closed) %>%
  slice_sample(n=20) %>%
  print()

set.seed(123)
stella.tails <- dna.dat %>% 
  filter(site=="Stellako", date_closed<as.Date("2021-05-04") | date_closed>=as.Date("2021-05-18")) %>%
  group_by(date_closed) %>%
  slice_sample(n=10) %>%
  print()

stella.extra <- dna.dat %>% 
  filter(site=="Stellako", date_closed==as.Date("2021-05-06") | grepl("kokanee", comments)) %>%
  print()

stella.first.pull <- rbind(stella.rando, stella.tails, stella.extra)


#------- FINAL DATABASE OF PULL #1
fullpull.1 <- rbind(stella.first.pull, nadleh.first.pull)

write.csv(fullpull.1, "northern_smolt_dna_pull1_june2021.csv", row.names=F)

# Nadleh
ggplot() +
  geom_bar(data=fullpull.1%>%filter(site=="Nadleh")%>%group_by(date_closed)%>%summarize(n=n()), 
           aes(x=date_closed, y=n), stat="identity", position="dodge", colour="orange", fill="orange", alpha=0.7, width=0.8) +
  geom_point(data=daily.catch%>%filter(site=="Nadleh"), aes(x=date_closed, y=total/100), size=2, alpha=0.6) +
  geom_line(data=daily.catch%>%filter(site=="Nadleh"), aes(x=date_closed, y=total/100), size=1, alpha=0.6) +
  geom_bar(data=nadleh.dna, aes(x=date_closed, y=n_analyzed), stat="identity", position="dodge", fill="green", alpha=0.5) +
  scale_y_continuous(breaks=seq(0,55,by=5), name="DNA samples", sec.axis = sec_axis(~.*100, name="Catch", breaks=seq(0,6000,by=1000))) +
  scale_x_date(date_breaks="3 day", date_labels="%B %d") +
  theme_bw()

# Stella 
ggplot() +
  geom_bar(data=fullpull.1%>%filter(site=="Stellako")%>%group_by(date_closed)%>%summarize(n=n()), 
           aes(x=date_closed, y=n), stat="identity", position="dodge", fill="aquamarine") +
  geom_point(data=daily.catch%>%filter(site=="Stellako"), aes(x=date_closed, y=total/10), size=2, alpha=0.6) +
  geom_line(data=daily.catch%>%filter(site=="Stellako"), aes(x=date_closed, y=total/10), size=1, alpha=0.6) +
  geom_bar(data=stella.dna, aes(x=date_closed, y=cutoff), stat="identity", position="dodge", fill="purple", alpha=0.5) +
  scale_y_continuous(breaks=seq(0,70,by=5), name="DNA samples", sec.axis = sec_axis(~.*10, name="Catch", breaks=seq(0,700,by=100))) +
  scale_x_date(date_breaks="3 day", date_labels="%B %d") +
  theme_bw()


##################################################################################################################################################

##################################################################################################################################################

##################################################################################################################################################




#                                                         SCALE SAMPLE PRIORITIZATION FOR PSC

# The PSC has indicated they will analyze all samples sent, but to expedite the process they have asked us to provide priorities if desired. 

#--------- PULL ALL SCALE FISH
scale.pull <- bio.dat %>%
  filter(!is.na(PSC_book) & !is.na(PSC_cell), !grepl("hinook", comments), 
         !grepl("don't run", comments), !grepl("mixed up cell for scales", comments),
         !grepl("scales both in cell #12", comments),
         !grepl("no scale", comments),
         !grepl("double scales", comments),
         !grepl("scales in 60", comments)) %>%   # edited after sending list to PSC 
  print()

# Export to share with PSC
write.csv(scale.pull, "northern_smolt_scale_samples_june2021.csv", row.names=F)


##################################################################################################################################################

#                                                               DATA EXPLORATION


#-------- NADLEH SCALES 
ggplot(scale.pull%>%filter(site=="Nadleh")%>%group_by(length_mm)%>%summarize(n=n()), aes(x=length_mm, y=n)) + 
  geom_bar(stat="identity", colour="black", fill="hot pink", width=1, alpha=0.6) + 
  #geom_density(data=scale.pull, aes(x=length_mm), color="darkblue", fill="lightblue", alpha=0.3, size=0.8) +
  scale_x_continuous(breaks=seq(0,250,by=5)) +
  theme_bw()

ggplot(scale.pull%>%filter(site=="Nadleh"), aes(x=length_mm)) + 
  geom_histogram(aes(y=..density..), colour="gray50", fill="gray50", alpha=0.5, bins=60, size=0.7)+
  geom_density(color="blue", fill="blue", alpha=0.2, size=1) +
  scale_x_continuous(breaks=seq(0,250,by=10)) +
  theme_bw()

# RANDOM SELECTIONS: 
# 95-105 mm
set.seed(1)
rando95_105 <- scale.pull %>% 
  filter(site=="Nadleh", length_mm>=95 & length_mm<=105) %>%
  group_by(length_mm) %>%
  slice_sample(n=5) %>%
  print()

# 110-115
set.seed(2)
rando110_115n <- scale.pull %>% 
  filter(site=="Nadleh", length_mm>=110 & length_mm<=115) %>%
  group_by(length_mm) %>%
  slice_sample(n=5) %>%
  print()

# >150
all150n <- scale.pull %>% 
  filter(site=="Nadleh", length_mm>150) %>%
  print()

# JOIN 
scale.priorities.n <- rbind(rando95_105, rando110_115n, all150n)



#-------- STELLA SCALES 
ggplot(scale.pull%>%filter(site=="Stellako")%>%group_by(length_mm)%>%summarize(n=n()), aes(x=length_mm, y=n)) + 
  geom_bar(stat="identity", colour="black", fill="hot pink", width=1, alpha=0.6) + 
  #geom_density(data=scale.pull, aes(x=length_mm), color="darkblue", fill="lightblue", alpha=0.3, size=0.8) +
  scale_x_continuous(breaks=seq(0,250,by=5)) +
  theme_bw()

ggplot(scale.pull%>%filter(site=="Stellako"), aes(x=length_mm)) + 
  geom_histogram(aes(y=..density..), colour="gray50", fill="gray50", alpha=0.5, bins=60, size=0.7)+
  geom_density(color="blue", fill="blue", alpha=0.2, size=1) +
  scale_x_continuous(breaks=seq(0,250,by=10)) +
  theme_bw()

# RANDOM SELECTIONS:
# 100-105 mm
set.seed(1)
rando100_105 <- scale.pull %>% 
  filter(site=="Stellako", length_mm>=100 & length_mm<=105) %>%
  group_by(length_mm) %>%
  slice_sample(n=5) %>%
  print()

# 110-115
set.seed(2)
rando110_115s <- scale.pull %>% 
  filter(site=="Stellako", length_mm>=110 & length_mm<=115) %>%
  group_by(length_mm) %>%
  slice_sample(n=5) %>%
  print()

# >150
all150s <- scale.pull %>% 
  filter(site=="Stellako", length_mm>150) %>%
  print()

# JOIN 
scale.priorities.s <- rbind(rando100_105, rando110_115s, all150s)



#-------- FULL PRIORITY LIST
scale.priorities <- rbind(scale.priorities.n, scale.priorities.s)

# DO NOT OVERWRITE THIS FILE AS IT IS NOW BEING USED IN nautley_database.R : 
# write.csv(scale.priorities, "northern_smolt_scale_PRIORITIES_june2021.csv", row.names=F)



##################################################################################################################################################

##################################################################################################################################################

##################################################################################################################################################



#                                                    SECOND ROUND OF DNA SAMPLE SUBMISSION
#                                                                   Dec 2021 

setwd("~/ANALYSIS/data")
library(tidyverse)
library(readxl)
library(openxlsx)

bio.dat.raw <- read_excel("Northern_smolt_database_2019-2021.xlsx", sheet="biosampling")
catch.dat.raw <- read_excel("Northern_smolt_database_2019-2021.xlsx", sheet="nightly_catch")
pullv1 <- read_excel("northern_smolt_dna_pull1_june2021_v1.xlsx", sheet="northern_smolt_dna_pull1_june20")  # this is the file i initially sent to MGL for analysis
pullv2 <- read_excel("northern_smolt_dna_pull1_june2021_v2.xlsx", sheet="northern_smolt_dna_pull1_june20")  # this is the revised file i sent for analysis but that they didn't end up analyzing

##################################################################################################################################################

#                                                                 CLEAN

pullv1 <- pullv1 %>% 
  filter(site != "Stellako") %>%
  mutate(whatman_uid = ifelse(whatman_uid=="NA", paste(c(whatman_sheet,whatman_cell), sep="-"), whatman_uid))

pullv2 <- pullv2 %>% 
  filter(site != "Stellako")

bio.dat.21 <- bio.dat.raw %>% 
  filter(year=="2021", site=="Nadleh", species=="Sockeye") %>%
  print()

catch.dat.21 <- catch.dat.raw %>%
  filter(year=="2021", site=="Nadleh", !grepl("Release location", location))


##################################################################################################################################################

#                                                               CURRENT GSI DATA SUMMARIES

bio.dat.21%>%filter(dna_select_bin==1)%>%summarize(n=n())
# n=200 total analyzed so far

bio.dat.21%>%filter(dna_select_bin==1, !is.na(weight_g))%>%summarize(n=n())
# n=32 w/ weight so far
bio.dat.21%>%filter(dna_select_bin==1, !is.na(weight_g))%>%group_by(b1_reg1)%>%summarize(n=n())
# n=18 nadina, n=14 stellako so far

#----

ggplot(data=bio.dat.21%>%group_by(dna_select_bin,length_mm)%>%summarize(n=n()), aes(x=length_mm, y=n, fill=as.factor(dna_select_bin))) +
  geom_bar(stat="identity")

ggplot() +
  geom_vline(xintercept = as.Date("2021-05-04"), colour="red",size=2) +
  geom_bar(data=bio.dat.21%>%group_by(dna_select_bin,date_closed)%>%summarize(n=n()), 
           aes(x=as.Date(date_closed), y=n, fill=as.factor(dna_select_bin)), stat="identity", alpha=0.65) +
  geom_line(data=catch.dat.21%>%group_by(date_closed)%>%summarize(n=sum(total_unmarked)/50), aes(x=as.Date(date_closed),y=n), size=1)+
  geom_point(data=catch.dat.21%>%group_by(date_closed)%>%summarize(n=sum(total_unmarked)/50), aes(x=as.Date(date_closed),y=n), size=3)+
  scale_x_date(date_breaks="2 day", date_labels = "%b %d") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust=1))
# equal #s each day 

ggplot(data=bio.dat.21 %>%
  filter(length_mm>130) %>%
  group_by(dna_select_bin, date_closed) %>%
  summarize(n=n()), 
    aes(x=date_closed,y=n,fill=as.factor(dna_select_bin) )) +
  geom_bar(stat="identity")
# n=4 over 130mm May 4-16 window analyzed 


##################################################################################################################################################

#                                                         SECOND SELECTION TO SEND

# Extract the samples not analyzed yet to pull from (just Nadleh 2021)
pull.from <- bio.dat.21 %>%
  mutate_at("date_closed", as.Date) %>%
  filter(dna_select_bin=="0", !is.na(whatman_uid), site=="Nadleh", year=="2021") %>%
  print()


#------- ROUND 2 SELECTIONS
#---- First randomly select 10 samples per day from days where no samples were analyzed in first round
dates.zeroes <- c(as.Date("2021-04-22"), "2021-04-23", "2021-04-24", "2021-04-29", "2021-05-06", "2021-05-17", "2021-05-18", "2021-05-19", "2021-05-20",
           "2021-05-21", "2021-05-22")

set.seed(1234)
pull.zeros <- pull.from %>%
  filter(date_closed %in% c(as.Date(dates.zeroes))) %>%
  group_by(date_closed) %>%
  sample_n(10) %>%
  mutate(dna_select_bin = 2)


#---- Next randomly select additional samples for days already analyzed but needing more
# Late and smaller peaks
dates.peaks <- c(as.Date("2021-05-01"), "2021-05-04", "2021-05-08", "2021-05-09", "2021-05-11", "2021-05-13")

set.seed(1234)
pull.peaks1 <- pull.from %>%
  filter(date_closed %in% c(as.Date(dates.peaks))) %>%
  group_by(date_closed) %>% 
  sample_n(10)%>%
  mutate(dna_select_bin = 2)

# Main peak Apr 26-27
date.big.peaks <- c(as.Date("2021-04-26"), "2021-04-27")

set.seed(1234)
pull.peaks2 <- pull.from %>%
  filter(date_closed %in% c(as.Date(date.big.peaks))) %>%
  group_by(date_closed) %>% 
  sample_n(20)%>%
  mutate(dna_select_bin = 2)

# Transition period around May 4 based on round #1
date.trans.peaks <- c(as.Date("2021-05-02"), "2021-05-03", "2021-05-05")

set.seed(1234)
pull.peaks3 <- pull.from %>%
  filter(date_closed %in% c(as.Date(date.trans.peaks))) %>%
  group_by(date_closed) %>% 
  sample_n(10)%>%
  mutate(dna_select_bin = 2)


#---- JOIN 1
pull.zeropeak <- full_join(pull.zeros, pull.peaks1) %>%
  full_join(., pull.peaks2) %>%
  full_join(., pull.peaks3)

# Identify second-round picks in database based on the whatman_uids
fullpull2.1 <- bio.dat.21 %>%
  mutate(dna_select_bin = ifelse(whatman_uid%in%pull.zeropeak$whatman_uid, 2, dna_select_bin)) %>%
  print()


#---- Finally target select additional samples for any remaining very large fish (>130mm)
pull.fatty <- fullpull2.1 %>%
  mutate_at("date_closed", as.Date) %>%
  filter(dna_select_bin=="0", !is.na(whatman_uid), site=="Nadleh", year=="2021") %>%
  filter(length_mm>130) %>%
  mutate(dna_select_bin = 2) %>%
  print()


#------- JOIN final:
pull.zeropeakfat <- full_join(pull.zeropeak, pull.fatty)
nrow(pull.zeropeakfat)

# Identify second-round picks in database based on the whatman_uids
fullpull2.2 <- bio.dat.21 %>%
  mutate(dna_select_bin = ifelse(whatman_uid%in%pull.zeropeakfat$whatman_uid, 2, dna_select_bin)) %>%
  mutate_at(c("date_opened", "date_closed"), as.Date) %>%
  print()


fullpull2.2 %>%
  filter(site=="Nadleh",  year=="2021", dna_select_bin!="0") %>% 
  group_by(dna_select_bin) %>%
  summarize(n=n())


#------- EXAMINE! 
fullpull2.2$dna_select_bin <- factor(fullpull2.2$dna_select_bin, levels=c(0,2,1), ordered=T)

ggplot() +
  geom_line(data=catch.dat.21%>%group_by(date_closed)%>%summarize(n=sum(total_unmarked)/50), aes(x=as.Date(date_closed),y=n), size=0.6, alpha=0.7)+
  geom_point(data=catch.dat.21%>%group_by(date_closed)%>%summarize(n=sum(total_unmarked)/50), aes(x=as.Date(date_closed),y=n), size=1.5, alpha=0.7) +
  
  geom_vline(xintercept = as.Date("2021-05-04"), colour="red",size=1) +
  geom_bar(data=fullpull2.2%>%group_by(dna_select_bin,date_closed)%>%summarize(n=n()), 
           aes(x=as.Date(date_closed), y=n, fill=as.factor(dna_select_bin), alpha=as.factor(dna_select_bin)), stat="identity") +
  #geom_bar(data=fullpull2.2%>%filter(dna_select_bin%in%c("1","2"))%>%group_by(date_closed)%>%summarize(n=n()), 
  #         aes(x=as.Date(date_closed), y=n), stat="identity", fill="green") +
  scale_alpha_manual(breaks=c(0,2,1), values=c(0.1, 0.7, 0.7)) +
  scale_x_date(date_breaks="2 day", date_labels = "%b %d") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust=1))

# some days have odd extra fish here and there because it is targeting fat fish as well.


#------- EXPORT
to.export <- fullpull2.2 %>% 
  filter(dna_select_bin==2) %>%
  select(year:age, dna_select_bin,comments)

dna_req_2 <- createWorkbook()
addWorksheet(dna_req_2, "Sheet1")
writeData(dna_req_2, sheet="Sheet1", x=to.export)
saveWorkbook(dna_req_2, "northern_smolt_dna_pull2_jan2022.xlsx", overwrite = T)














