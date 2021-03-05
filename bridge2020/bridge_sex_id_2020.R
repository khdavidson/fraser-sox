# genetic vs. phenotypic sex ID 
# bridge river samples 
# collected Aug 2020
# data analysis 

setwd("~/Documents/ANALYSIS/data")

library(tidyverse)
library(xlsx)
library(openxlsx)
library(lubridate)

fence.raw <- read.xlsx("LBR_Fish fence_2020_sockeye tissue samples.xlsx", sheet="Sheet1", detectDates = T)
dna.raw <- read.xlsx("LBR_sockeye-L_Bridge_R_bb_resp_2020_2020-11-30.xlsx", sheet="Genetic_Sex")
gsid.raw <- read.xlsx("LBR-fence-clean.xlsx", sheet="LBR-gsid-clean")

###################################################################################################################################################

#                                                                 CLEAN

#------ fence visual sex id data
fence <- fence.raw %>%
  rename(date=Date,
    check_time=Trap.check,
    spp=Species,
    cs_fish_no=`Fish.#`,
    sex_fence=Sex,
    fl_cm=`Fork.Length.(cm)`,
    dna=`DNA.(Y/N)`,
    comments=Comments) %>%
  mutate(date=as.Date(date, origin = "1899-12-30")) %>%
  mutate_at("sex_fence", as.factor) %>%
  mutate(yday=lubridate::yday(date)) %>%
  filter(spp=="Sockeye") %>%
  select(yday, cs_fish_no, fl_cm, sex_fence) %>%
  mutate(ufid=seq(1:nrow(fence.raw))) %>%
  print()

#write.csv(fence, "LBR-fence-clean.csv", row.names=F)   already written

#------ genetic sex ID data
dna <- dna.raw %>%
  rename(stock=Stock,
    yday=JulDate,
    ufid=`Fish#`,
    vial_no=`Vial#`,
    sex_sdy=`Genetic_Sex.(sdY)`,
    tray=Tray,
    marker=Marker) %>%
  mutate_at("sex_sdy", as.factor) %>%
  select(yday, ufid, sex_sdy) %>%
  print()


#------ genetic stock ID data
gsid <- gsid.raw %>% 
  mutate_at(c("gsid_level", "region1_name", "region2_name", "region3_name", "region4_name", "region5_name"), as.factor) %>%
  print()

 
###################################################################################################################################################

#                                                                 JOIN AND ASSESS

# pair up
sex_db <- left_join(fence, dna, by=c("yday", "ufid"))

# assess
sex_db <- sex_db %>%
  mutate(sex_fence = ifelse(sex_fence=="M", "Male", ifelse(sex_fence=="F", "Female", "Unknown"))) %>%
  mutate(sex_sdy = ifelse(grepl("Male", sex_sdy), "Male", ifelse(grepl("Female", sex_sdy), "Female", "Unknown"))) %>%
  mutate(flag = ifelse(sex_fence == sex_sdy, "ok", "FLAG")) %>%
  mutate_at("flag", as.factor) %>%
  print()

# summarize: % wrongly ID'd
sex_sum <- sex_db %>%
  filter(sex_sdy != "Unknown") %>%
  group_by(flag) %>%
  summarize(n_instance = length(flag)) %>%
  mutate(propn_instance = n_instance/sum(n_instance)) %>%
  print()

# investigate directionality of fence sex ID 
sex_dir <- sex_db %>% 
  filter(flag=="FLAG", sex_sdy != "Unknown") %>%
  group_by(sex_fence) %>%
  summarize(n_sex = length(sex_fence)) %>%
  print()

ggplot(sex_dir, aes(x=sex_fence, y=n_sex)) +
  geom_bar(stat="identity", colour="black", fill="gray70", size=1.3) +
  labs(x="Phenotypic sex assigned at fence", y="Number of sex ID disagreements", 
    caption="Figure. Number of cases from the Bridge River fence where the sockeye phenotypic sex did not match the genetic sex ID (sdY).") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
    axis.title=element_text(face="bold"),
    plot.caption = element_text(size=11, color="gray20", face="italic", hjust=0))


##########
# LENGTH #
##########

# summary of avg +/- sd FLs for males and females flagged and ok
sex_lgth_sum <- sex_db %>%
  filter(sex_sdy != "Unknown") %>%
  group_by(flag, sex_fence, sex_sdy) %>%
  summarize(mean_fl = mean(fl_cm), sd_fl=sd(fl_cm)) %>%
  print()

#------- STATS
# remove unknown genetic sex and create grouping var
sex_lgth <- sex_db %>%
  filter(sex_sdy != "Unknown") %>%
  mutate(fence_group = paste(sex_fence, flag, sep="-")) %>%
  print()

# calculate sample sizes of each group
sample_sizes <- sex_lgth %>%
  group_by(fence_group) %>%
  summarize(n = n()) %>%
  print()

lm <- lm(sex_lgth$fl_cm ~ sex_lgth$flag)
r<-resid(lm)
plot(r)
hist(r)
qqnorm(r)
qqline(r)

aov <- aov(sex_lgth$fl_cm ~ sex_lgth$fence_group)
summary(aov)
TukeyHSD(aov)


#------- PLOTS
ggplot(sex_lgth, aes(x=fence_group, y=fl_cm, fill=sex_fence)) +
  geom_boxplot(size=0.5) +
  labs(x="Visual sex ID agreement/disagreement", y="Fork length (cm)", fill="Sex",
    caption="Figure. Fork length (cm) of sockeye visually identified as female (pink) and male (green) at the Bridge River fence grouped by agreement ('ok') \nor disagreement ('FLAG') with their genetic sex ID. For example, 'Female-FLAG' indicates sockeye visually called females that later screened as genetically male. \nOne fish of unknown phenotypic sex was later assigned as a male. Sample sizes given in gray for each group.") +
  annotate("text", x=c(1,2,3,4,5), y=c(55,54,55,59,41), label=c(24,45,3,40,1), colour="gray40", size=4) +
  scale_y_continuous(breaks=seq(40,70,by=10), limits=c(40,70)) +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
    axis.title=element_text(face="bold"),
    plot.caption = element_text(size=10, color="gray20", face="italic", hjust=0),
    legend.position = c(0.9,0.8),
    legend.background = element_rect(colour="black"))
 

 
###################################################################################################################################################
###################################################################################################################################################
###################################################################################################################################################


#                                                            STOCK PROPORTIONS

# on yday 235 there is one fish missing from the genetic sample that was recorded in the sex/length data, i.e., sex-length data has n=7 fish on 
# day 235 while there are only n=6 genetic samples. It appears fish number 73 was missing but not sure details of this as all fish are recorded
# as having dna taken. *** UPDATE fish 73 had "number of missing loci exceeded" for the individual ID run therefore obviously lab issues.

# using 80% GSID cut-off recommended by K. Flynn @ MGL Jan 2020

gsid$region1_name <- factor(gsid$region1_name, 
  levels=c("Early_Stuart(Fr)", "Takla-Trembleur - Early Stuart", "Early Stuart", "Driftwood-Nar", "Paula-Felix",
    "Early_Summer(Fr)", "Bowron - Early Summer", "Bowron", 
      "Nadina-Francois - Early Summer", "Nadina", 
      "Chilko - Early Summer", "Chilko_south", "Chilko",
      "Anderson-Seton - Early Summer", "Gates_Cr", "Gates",
      "Shuswap - Early Summer", "Misc. Early Summer - Early Shuswap", "Anstey_R",
      "Summer(Fr)_", "Francois-Fraser - Summer", "Stellako",
      "Quesnel - Summer", "Quesnel", "M_Horsefly"), ordered=T)

lines <- c("Early_Stuart(Fr)", "Early_Summer(Fr)", "Summer(Fr)_")

#------ visualize prior to filtering 
ggplot(gsid %>% filter(!is.na(region1_name)) %>% arrange(ufid), aes(x=ufid, y=region1_name)) + 
  geom_hline(yintercept = "Early_Stuart(Fr)", colour="black") +
  geom_hline(yintercept = "Early_Summer(Fr)", colour="black") +
  geom_hline(yintercept = "Summer(Fr)_", colour="black") +
  geom_point(shape=21, fill="gray70", colour="gray20", alpha=0.7, stroke=1.1, size=1.3) +
  #scale_size_continuous(range=c(0.2,4)) +
  scale_x_continuous(limits = c(1,116), breaks=seq(1,116,by=4)) +
  scale_y_discrete(labels=c("Early_Stuart(Fr)"=expression(bold(`Early_Stuart(Fr)`)), "Early_Summer(Fr)"=expression(bold(`Early_Summer(Fr)`)),
                            "Summer(Fr)_"=expression(bold(`Summer(Fr)_`)), parse=TRUE)) +
  labs(x="UFID", y="First Regional Assignment") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"))



#------ filter by 80% stock id cutoff
gsid.ass <- gsid %>% 
  filter(region1_prob >= 0.8) %>%
  print()

# examine those less than 0.8
gsid.nass <- gsid %>% 
  filter(region1_prob < 0.8) %>%
  print()
### there are some here with quite high assignment rates (~78%), and also some where the cumulative 











