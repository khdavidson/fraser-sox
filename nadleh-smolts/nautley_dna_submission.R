

# Nautley DNA sample request #2 and Progress Report Code. Some figures are shared, the reason for both analyses in one script. 
# KD
# Dec 2019

library(tidyverse)
library(XLConnect)
library(xlsx)
library(openxlsx)
library(janitor)
library(gridExtra)
library(ggridges)
library(scales)
library(ggpubr)
library(padr)
library(stringr)
library(withr)
library(padr)
library(GmAMisc)

# set wd
setwd("~/Documents/ANALYSIS/data")

# read in individual data 
nad.df <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheet = 3, colNames=T, detectDates=T)

# ordered factor 
nad.df$length_class <- factor(nad.df$length_class, levels=c("<80", "80-89", "90-99", "100-109", "110-119", "120-130", ">130", ordered=T))



##################################################################################################################################################

#                                                        Information to inform SAMPLE PULL #2



########################
# TOTAL SAMPLE SUMMARY #
########################

# what % of samples taken have been run? 
sub1 <- nad.df %>% 
  filter(whatman_sheet != "NA") %>%
  group_by(dna_select_bin) %>% 
  summarize(n=n()) %>% 
  print()

# what % have been successfully run?
samp_propn <- nad.df %>%
  filter(whatman_sheet != "NA", !grepl("did not amplify", dna_comment), region1 != "0") %>%
  group_by(dna_select_bin, region1) %>% 
  summarize(DNA_samp = n()) %>%
  mutate(propn = DNA_samp/sum(DNA_samp)) %>%
  print()

# range of did not amplify samples 
no.amp <- nad.df %>%
  filter(grepl("did not amplify", dna_comment)) %>%
  group_by(date, length_class) %>%
  summarize(n = n()) %>%
  group_by(date) %>%
  mutate(perc=n/sum(n)) %>%
  print()

# plot this for visual
ggplot(no.amp, aes(x=date, y=length_class, height=n)) +
  geom_density_ridges(stat="identity", scale=1)

ggplot(no.amp, aes(x=date, y=n, fill=length_class)) +
  geom_bar(stat = "identity", colour="black") +
  theme_bw()+
  labs(x="Date", y="Count", fill="Length class") +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.text = element_text(size=15, colour="black"),
    legend.text = element_text(size=13), 
    legend.title = element_text(size=18))



##########################
# SAMPLE SUMMARY TO DATE #
##########################

# all samples taken over time 
dna.date <- nad.df %>% 
  filter(whatman_sheet!="NA", !grepl("did not amplify", dna_comment)) %>% 
  group_by(dna_select_bin, date_group) %>%
  summarize(n = n()) %>%
  group_by(date_group) %>%
  mutate(perc = n/sum(n)) %>%
  print()

# all samples taken - by length
dna.lgth <- nad.df %>% 
  filter(whatman_sheet!="NA", !grepl("did not amplify", dna_comment)) %>% 
  group_by(dna_select_bin, length_class) %>%
  summarize(n = n()) %>%
  group_by(length_class) %>%
  mutate(perc = n/sum(n)) %>%
  print()

dna.lgth$length_class <- factor(dna.lgth$length_class, levels=c("<80", "80-89", "90-99", "100-109", "110-119", "120-130", ">130", ordered=T))


# all samples taken - by weight
dna.wgt <- nad.df %>% 
  filter(whatman_sheet!="NA", !grepl("did not amplify", dna_comment), dna_select_bin=="1") %>% 
  print()


# plot to see together

  # plots by number of samples 
  n.d <- ggplot(dna.date, aes(x=date_group, y=n, fill=dna_select_bin)) +
    geom_bar(aes(fill=dna_select_bin), stat="identity", colour="black") +
    theme_bw() +
    labs(x="Date", y="Count", fill="Submission status") +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=0.93, hjust=1),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))
  
  n.l <- ggplot(dna.lgth, aes(x=length_class, y=n, fill=dna_select_bin)) +
    geom_bar(aes(fill=dna_select_bin), stat="identity", colour="black") +
    theme_bw() +
    labs(x="Length class (mm)", y="Count", fill="Submission status") +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=0.6),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))
  
  # plots by proportion of samples 
  p.d <- ggplot(dna.date, aes(x=date_group, y=perc, fill=dna_select_bin)) +
    geom_bar(aes(fill=dna_select_bin), stat="identity", colour="black") +
    theme_bw() +
    labs(x="Date", y="Percent of total", fill="Submission status") +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=0.93, hjust=1),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))
  
  p.l <- ggplot(dna.lgth, aes(x=length_class, y=perc, fill=dna_select_bin)) +
    geom_bar(aes(fill=dna_select_bin), stat="identity", colour="black") +
    theme_bw() +
    labs(x="Length class (mm)", y="Percent of total", fill="Submission status") +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=0.6),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=13))
  
  p.w <- ggplot(dna.wgt, aes(x=date, y=perc, fill=dna_select_bin)) +
    geom_bar(stat="identity", colour="black") +
    theme_bw()
  
  # plot number and proprotion in one mega graph
  ggarrange(n.d, n.l, p.d, p.l, ncol=2, nrow=2, common.legend = TRUE, legend="right")

  
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################

#                                                           SUBMISSION 1 SAMPLE BREAKDOWN 


#############################
# Random resampling: LENGTH #
#############################


#-------- ISOLATING SUBSAMPLE FISH BY DIFFERENT SAMPLING METHODS

###CORE LF FISH: these are all the length frequency fish that weren't selected for extra sampling so they should be most representative  
pop <- nad.df %>% 
  filter(!is.na(length_mm), is.na(whatman_sheet)) %>% 
  print()
pop$length_class <- factor(pop$length_class, levels=c("<80", "80-89", "90-99", "100-109", "110-119", "120-130", ">130"), ordered=T)

  
###DNA SUBSAMPLED FISH: Select the DNA sampled fish and summarize the number of fish sampled...
pull1 <- nad.df %>% 
  filter(dna_select_bin == "1") %>% 
  print()

# ...by date and length 
pull1.dl <- pull1 %>% 
  group_by(date, length_class) %>% 
  summarize(n=n()) %>%
  print()
pull1.dl$length_class <- factor(pull1.dl$length_class, levels=c("<80", "80-89", "90-99", "100-109", "110-119", "120-130", ">130"), ordered=T)


###ALL FISH: Select all fish measured for length (LF only + DNA sub sampling)
length <- nad.df %>% 
  filter(!is.na(length_mm)) %>% 
  group_by(date,length_class) %>% 
  summarize(n=n()) %>% 
  ungroup() %>%
  add_row(date=as.Date("2020-01-01"), length_class=">130", n=NA) %>%    # added dummy variable for missing ">130mm" length class for plot below 
  print()
length$length_class <- factor(length$length_class, levels=c("<80", "80-89", "90-99", "100-109", "110-119", "120-130", ">130"), ordered=T)


#-------- PLOTS

# Length class "abundance" (i.e., n obs) over time
ggplot(length, aes(x=date, y=length_class, height=n)) +
  geom_density_ridges(stat="identity", scale=1) 

# Comparing ALL fish measured for length (white), L-F fish (gray), DNA-lengthed fish (blue) grouped by length class 
ggplot() +
  geom_bar(data=length%>%group_by(length_class)%>%summarize(sum=sum(n)), aes(x=length_class, y=sum/10), stat="identity", fill="white", colour="black") +
  geom_bar(data=pop%>%group_by(length_class)%>%summarize(n=n()), aes(x=length_class, y=n/10), stat="identity", fill="gray80", width=0.8, alpha=0.7) +
  geom_bar(data=pull1.dl%>%group_by(length_class)%>%summarize(n=n()), aes(x=length_class, y=n), fill="blue", stat="identity", alpha=0.3, width=0.8) +
  scale_y_continuous(breaks=seq(0,150,by=25)) +
  labs(x="", y="Number of samples (gray and white scaled down 1 \norder of magnitude for visualization)") +
  theme_bw()

# Comparing ALL fish measured for length (white) grouped by length class to the DNA-lengthed fish grouped by length-class (gray) OVER TIME
ggplot() +
  geom_density_ridges(data=length%>%filter(date!="2020-01-01"), aes(x=date, y=length_class, height=n), stat="identity", scale=1, fill="white", alpha=0.8) +
  geom_density_ridges(data=pop%>%group_by(date, length_class)%>%summarize(n=n()), aes(x=date, y=length_class, height=n), stat="identity", scale=1, fill="gray80", alpha=0.7) +
  geom_density_ridges(data=pull1.dl, aes(x=date, y=length_class, height=n), colour="blue", fill="blue", stat="identity", scale=1, alpha=0.3) +
  scale_x_date(date_breaks="5 day", date_labels="%b %d") +
  theme_bw()

# so far it appears that using the DNA sub-sampling method does not mirror the "true" distribution well 


#-------- Linear models (for interest)
lm.dl <- lm(pull1.dl$n ~ pull1.dl$date + pull1.dl$length_class)
summary(lm.dl)
r.dl<-resid(lm.dl)
hist(r.dl)
plot(r.dl)
plot(lm.dl)                                             # Obs 16 and 68 are individual points, they determine their own predicted value (same as observed value) therefore leverage=1

lm.d <- lm(pull1.dl$n ~ pull1.dl$date)
summary(lm.d)
r.d<-resid(lm.d)
hist(r.d)
plot(r.d)
plot(lm.d)

lm.l <- lm(pull1.dl$n ~ pull1.dl$length_class)
summary(lm.l)
r.l<-resid(lm.l)
hist(r.l)
plot(r.l)
plot(lm.l)                                              # Obs 16 and 68 are individual points, they determine their own predicted value (same as observed value) therefore leverage=1









#-------- BOOTSTRAPPING
  
# random bootstrap resampling of the length-frequency only "population" of fish 
set.seed(12345)
boot.l <- prop.table(table(replicate(10000, sample(pop$length_mm, size=100, replace=TRUE))))
  boot.l <- as.data.frame(boot.l)
  ggplot(boot.l, aes(x=Var1,y=Freq))+
    geom_bar(stat="identity")
  
# random boostrap resampling of the DNA length-class sub-sample fish   
boot.lc <- prop.table(table(replicate(10000, sample(pop$length_class, size=100, replace=TRUE))))
  boot.lc <- as.data.frame(boot.lc)
  boot.lc$Var1<-factor(boot.lc$Var1, levels=c("<80", "80-89", "90-99", "100-109", "110-119", "120-130", ">130", ordered=T))
  ggplot(boot.lc, aes(x=Var1,y=Freq))+
    geom_bar(stat="identity")


## **** there shnould be comparison plots here - NEXT DAY!! ********
GGPLOT




# --------------------- split for each stock: NADINA 
pullN <- nad.df %>% 
    mutate_at(vars(c(17)), funs(as.factor)) %>%
    filter(dna_select_bin == "1" & region1=="4") %>% 
    print()
  
# by date 
pullN.d <- pullN %>% 
  group_by(date) %>% 
  summarize(n=n()) %>%
  print()

ggplot(pullN.d, aes(x=date, y=n)) +
  geom_bar(stat="identity")

# by length 
pullN.l <- pullN %>% 
  group_by(length_class) %>% 
  summarize(n=n()) %>%
  print()

    length <- nad.df %>% 
      filter(!is.na(length_mm)) %>% 
      group_by(date,length_class) %>% 
      summarize(n=n())

    ggplot(length, aes(x=date, y=length_class, height=n)) +
      geom_density_ridges(stat="identity", scale=1) 

    
ggplot() +
  geom_bar(data=length, aes(x=length_class, y=n/10), stat="identity", fill="white", width=0.8, alpha=0.7) +
  geom_bar(data=pullN.l, aes(x=length_class, y=n), stat="identity", colour="black", alpha=0.5) 

# by date and length 
pullN.dl <- pullN %>% 
  group_by(date, length_class) %>% 
  summarize(n=n()) %>%
  print()

ggplot(pullN.dl, aes(x=date, y=length_class, height=n)) +
  geom_density_ridges(stat="identity", scale=1) 


# --------------------- split for each stock: STELLAKO 
pullS <- nad.df %>% 
    mutate_at(vars(c(17)), funs(as.factor)) %>%
    filter(dna_select_bin == "1" & region1=="12") %>% 
    print()
  
# by date 
pullS.d <- pullS %>% 
  group_by(date) %>% 
  summarize(n=n()) %>%
  print()

ggplot(pullS.d, aes(x=date, y=n)) +
  geom_bar(stat="identity")

# by length 
pullS.l <- pullS %>% 
  group_by(length_class) %>% 
  summarize(n=n()) %>%
  print()

    length <- nad.df %>% 
      filter(!is.na(length_mm)) %>% 
      group_by(date,length_class) %>% 
      summarize(n=n())

    ggplot(length, aes(x=date, y=length_class, height=n)) +
      geom_density_ridges(stat="identity", scale=1) 

    
ggplot() +
  geom_bar(data=length, aes(x=length_class, y=n/10), stat="identity", fill="white", width=0.8, alpha=0.7) +
  geom_bar(data=pullS.l, aes(x=length_class, y=n), stat="identity", colour="black", alpha=0.5) 

# by date and length 
pullS.dl <- pullS %>% 
  group_by(date, length_class) %>% 
  summarize(n=n()) %>%
  print()

ggplot(pullS.dl, aes(x=date, y=length_class, height=n)) +
  geom_density_ridges(stat="identity", scale=1) 
ggplot(pullS.dl, aes(x=date, y=length_class)) +
  geom_density_ridges2()









#############################
# Random resampling: WEIGHT #
#############################

# create a weight integer for easy plotting 
nad.df <- nad.df %>% 
  mutate(weight_round = round(weight_g,digits=0)) %>% 
  print()

# natural breaks to assign weight classes
wgt <- nad.df %>% 
  filter(!is.na(weight_g)) %>% 
  print()

res <- plotJenks(wgt[wgt$weight_round<30,]$weight_round, n=7)
breaks2 <- classIntervals(wgt$weight_class, n=7, style="jenks")         # breaks at: 7, 10, 14, 18, 24, 45
breaks3 <- classIntervals(wgt$weight_class, n=7, style="kmeans")        # breaks at: 6.5, 9.5, 13.5, 17.5, 21.5, 62

nad.df <- nad.df %>% 
  mutate(weight_round = round(weight_g,digits=0)) %>% 
  mutate(weight_class = ifelse(weight_round <= 7, "<= 7g",
    ifelse(weight_round > 7 & weight_round <= 10, "7.1-10g" ,
      ifelse(weight_round > 10 & weight_round <= 14, "10.1-14g", 
        ifelse(weight_round > 14 & weight_round <= 18, "14.1-18g", 
          ifelse(weight_round > 18 & weight_round <= 24, "18.1-24g", 
            ifelse(weight_round > 24 & weight_round <= 45, "24.1-45g", ">45g"))))))) %>% 
  print()

# bootstrap
set.seed(12345)
boot.w <- prop.table(table(replicate(10000, sample(nad.df$weight_class, size=100, replace=TRUE))))
  boot.w <- as.data.frame(boot.w)
  boot.w$Var1 <- factor(boot.w$Var1, levels = c("<= 7g", "7.1-10g", "10.1-14g", "14.1-18g", "18.1-24g", "24.1-45g", ">45g", ordered=T))  
    
  ggplot(boot.w, aes(x=Var1,y=Freq))+
    geom_bar(stat="identity")

boot.w2 <- prop.table(table(replicate(10000, sample(nad.df$weight_round, size=100, replace=TRUE))))
  boot.w2 <- as.data.frame(boot.w2)
  boot.w$Var1 <- factor(boot.w$Var1, levels = c("<= 7g", "7.1-10g", "10.1-14g", "14.1-18g", "18.1-24g", "24.1-45g", ">45g", ordered=T))  

  ggplot(boot.w2, aes(x=Var1,y=Freq))+
    geom_bar(stat="identity")

# all fiss
wc <- nad.df %>% 
  filter(!is.na(weight_g)) %>% 
  group_by(date, weight_class) %>% 
  summarize(n=n()) %>% 
  print()

wc$weight_class <- factor(wc$weight_class, levels = c("<= 7g", "7.1-10g", "10.1-14g", "14.1-18g", "18.1-24g", "24.1-45g", ">45g", ordered=T))
nad.df$weight_class <- factor(nad.df$weight_class, levels = c("<= 7g", "7.1-10g", "10.1-14g", "14.1-18g", "18.1-24g", "24.1-45g", ">45g", ordered=T))

ggplot(data=subset(nad.df %>% filter(!is.na(weight_class))), aes(x=date, y=weight_class)) +
  geom_density_ridges(scale=1) 
ggplot(wc, aes(x=date, y=weight_class, height=n)) +
  geom_density_ridges2(stat="identity", scale=1) 



# ------ split for each stock: NADINA 
pullN <- nad.df %>% 
    mutate_at(vars(c(17)), funs(as.factor)) %>%
    filter(region1=="4") %>% 
    print()
  
# by date 
pullN.d <- pullN %>% 
  group_by(date) %>% 
  summarize(n=n()) %>%
  print()

ggplot(pullN.d, aes(x=date, y=n)) +
  geom_bar(stat="identity")

# by weight 
pullN.w <- pullN %>% 
  group_by(weight_class) %>% 
  summarize(n=n()) %>%
  print()

    weight <- nad.df %>% 
      filter(!is.na(weight_g)) %>% 
      group_by(weight_class) %>% 
      summarize(n=n()) %>% 
      print()

    ggplot(pullN.w, aes(x=date, y=weight_class, height=n)) +
      geom_density_ridges(stat="identity", scale=1) 
    ggplot(wc, aes(x=date, y=weight_class, height=n)) +
      geom_density_ridges2(stat="identity", scale=1)
    
ggplot() +
  geom_bar(data=weight, aes(x=weight_class, y=n/2), stat="identity", colour="gray90", fill="white", width=1, alpha=0.7) +
  geom_bar(data=pullN.w, aes(x=weight_class, y=n), stat="identity", colour="black", width=1, alpha=0.4) 

# by date and weight 
pullN.dw <- pullN %>% 
  group_by(date, weight_class) %>% 
  summarize(n=n()) %>%
  print()

ggplot(pullN.dw, aes(x=date, y=weight_class, height=n)) +
  geom_density_ridges(stat="identity", scale=1) 
ggplot(pullN.dl, aes(x=date, y=length_class)) +
  geom_density_ridges2()


# ------ split for each stock: STELLAKO 
pullS <- nad.df %>% 
    mutate_at(vars(c(17)), funs(as.factor)) %>%
    filter(dna_select_bin == "1" & region1=="12") %>% 
    print()
  
# by date 
pullS.d <- pullS %>% 
  group_by(date) %>% 
  summarize(n=n()) %>%
  print()

ggplot(pullS.d, aes(x=date, y=n)) +
  geom_bar(stat="identity")

# by length 
pullS.l <- pullS %>% 
  group_by(weight_class) %>% 
  summarize(n=n()) %>%
  print()

    weight <- nad.df %>% 
      filter(!is.na(weight_g)) %>% 
      group_by(weight_class) %>% 
      summarize(n=n())

    ggplot(weight, aes(x=date, y=weight_class, height=n)) +
      geom_density_ridges2(stat="identity", scale=1) 
    ggplot(weight, aes(x=date, y=weight_class)) +
      geom_density_ridges2()
    
ggplot() +
  geom_bar(data=weight, aes(x=weight_class, y=n/2), stat="identity", colour="gray90", fill="white", width=1, alpha=0.7) +
  geom_bar(data=pullS.l, aes(x=weight_class, y=n),  stat="identity", width=1, colour="black", alpha=0.4) 

# by date and length 
pullS.dl <- pullS %>% 
  group_by(date, length_class) %>% 
  summarize(n=n()) %>%
  print()

ggplot(pullS.dl, aes(x=date, y=length_class, height=n)) +
  geom_density_ridges(stat="identity", scale=1) 
ggplot(pullS.dl, aes(x=date, y=length_class)) +
  geom_density_ridges2()






#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################


#                                                           UNSUBMITTED SAMPLE BREAKDOWN 


# what is the date range of unsubmitted samples? 
unsamp.date <- nad.df %>% 
  filter(whatman_sheet != "NA", dna_select_bin == "0") %>%
  group_by(date_group) %>% 
  summarize(n=n()) %>% 
  print()

# what is the length range of unsubmitted samples?
unsamp.lgth <- nad.df %>% 
  filter(whatman_sheet != "NA", dna_select_bin == "0") %>% 
  group_by(length_class) %>% 
  summarize(n=n()) %>% 
  print()

unsamp.lgth$length_class <- factor(unsamp.lgth$length_class, levels=c("<80", "80-89", "90-99", "100-109", "110-119", "120-130", ">130", ordered=T))

# plot

  # by date
  u.d <- ggplot(unsamp.date, aes(x=date_group, y=n)) +
    geom_bar(stat="identity", colour="black", fill="gray60") +
    theme_bw() +
    labs(x="Date", y="Count", fill="Submission status") +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=0.93, hjust=1),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))
  
  # by length
  u.l <- ggplot(unsamp.lgth, aes(x=length_class, y=n)) +
    geom_bar(stat="identity", colour="black", fill="gray60") +
    theme_bw() +
    labs(x="Length class (mm)", y="Count", fill="Submission status") +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=0.6),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))

  ggarrange(u.d, u.l, ncol=2, common.legend = TRUE, legend="bottom")

  

#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################


#                                                       NADINA VS STELLAKO COMPARISONS


##############
# BIOMETRICS #
##############

# reduced dataset for run samples
svn.b <- nad.df %>% 
  filter(whatman_sheet!="NA", !grepl("did not amplify", dna_comment), region1 %in% c(4,12)) %>% 
  mutate_at(vars(c(18)), funs(as.character)) %>%
  mutate(region1 = ifelse(region1=="4", "Nadina", "Stellako")) %>%
  print()

#fitlm = lm(length ~ date + region, data = svn)
#svn$predlm = predict(fitlm)
#predslm = predict(fitlm, interval = "confidence")
#svn = cbind(svn, predslm)


# plot 
  # by length
  l<-ggplot(svn.b, aes(x=date, y=length_mm, group=region1)) +
    geom_smooth(aes(colour=region1, fill=region1), method="lm", se=T, size=1.5, alpha=0.15) +
    geom_point(aes(colour=region1, fill=region1),alpha=0.5, size=3) +
    scale_colour_manual(values=c("#0059d1", "#81a926")) +
    scale_fill_manual(values=c("#0059d1", "#81a926")) +
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
  
  # by weight
  w<-ggplot(svn.b, aes(x=date, y=weight_g, group=region1)) +
    geom_smooth(aes(colour=region1, fill=region1), method="lm", se=T, size=1.5, alpha=0.15) +
    geom_point(aes(colour=region1, fill=region1),alpha=0.5, size=3) +
    scale_colour_manual(values=c("#0059d1", "#81a926")) +
    scale_fill_manual(values=c("#0059d1", "#81a926")) +
    scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
    labs(x="Date", y="Weight (g)", fill="Region", colour="Region") +
    theme_bw() +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.title.y = element_text(margin=margin(t=2,l=0,r=6,b=0)),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, hjust=1),
      legend.background = element_rect(colour="black"),
      legend.position = c(0.1,0.85),
      legend.text = element_text(size=13), 
      legend.title = element_text(size=15)) 
  
  # width vs length
  lw<-ggplot(svn.b, aes(x=length_mm, y=weight_g, group=region1)) +
    geom_point(aes(colour=region1, fill=region1),alpha=0.5, size=2) +
    geom_smooth(aes(colour=region1, fill=region1), method="lm", se=T, alpha=0.15) +
    scale_colour_manual(values=c("#0059d1", "#81a926")) +
    scale_fill_manual(values=c("#0059d1", "#81a926")) +
    labs(x="Length (mm)", y="Weight (g)", fill="Region", colour="Region") +
    theme_bw() +
    theme(axis.title = element_text(size=11, face = "bold"),
      axis.text = element_text(size=9, colour="black"),
      #axis.text.x = element_text(angle=45, vjust=0.3),
      legend.text = element_text(size=8), 
      legend.title = element_text(size=10)) 
  
  ggarrange(l, w, lw, ncol=2, nrow=2, common.legend = TRUE, legend="right")
  ggarrange(l, w, ncol=2, nrow=1)

###############
# RUN TIMING  #
###############

# daily and cumulative run timing
svn.a <- nad.df %>% 
  filter(whatman_sheet!="NA", !grepl("did not amplify", dna_comment), region1 %in% c(4,12)) %>% 
  mutate_at(vars(c(18)), funs(as.character)) %>%
  mutate(region1 = ifelse(region1=="4", "Nadina", "Stellako")) %>%
  group_by(date, region1) %>%
  summarize(n=n()) %>%
  group_by(region1) %>%
  mutate(cuml_n = cumsum(n)) %>%
  mutate(cuml_p = cuml_n/sum(n)) %>%
  print()

  
# plot
  # by daily run time 
  a<-ggplot(svn.a, aes(x=date, y=n, group=region1, colour=region1)) +
    geom_point(size=4, alpha=0.5) +
    geom_line(size=1.2) +
    scale_colour_manual(values=c("#0059d1", "#81a926")) +
    labs(x="Date", y="Number of smolts", colour="Region") +
    theme_bw() +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=1, hjust=1),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15)) 
  
  # by cumulative run time
  c<-ggplot(svn.a, aes(x=date, y=cuml_p, group=region1, colour=region1)) +
    geom_line(size=1.2) +
  #  geom_point(size=4, alpha=0.5) +
    scale_colour_manual(values=c("#0059d1", "#81a926")) +
    labs(x="Date", y="Cumulative proportion", colour="Region") +
    scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
    theme_bw() +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=1, hjust=1),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15),
      legend.position = c(0.85,0.2),
      legend.background = element_rect(fill="white", colour="black")) 
  
  ggarrange(a, c, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")


#########################
# PROPORTION OF SAMPLES #
#########################

# what proportion of all samples are stellako and nadina?
dna.propn <- nad.df %>% 
  filter(region1 %in% c("4", "12")) %>% 
  group_by(date_group, region1) %>%
  summarize(n=n()) %>% 
  group_by(date_group) %>%
  mutate(propn=n/sum(n)) %>%
  mutate_at(vars(c(2)), funs(as.factor)) %>%
  mutate(region1 = ifelse(region1=="4", "Stellako", "Nadina")) %>%
  print()

# plot
  # by date
    ggplot(dna.propn, aes(x=date_group, y=propn, group=region1, colour=region1)) + 
      labs(x="Date", y="Proportion of samples", colour="Region:") +
      geom_line(stat="identity", size=1.5) +
      scale_colour_manual(values=c("#0059d1", "#81a926")) +
      scale_y_continuous(limits=c(0,1)) +
      theme_bw() +
      theme(axis.title = element_text(size=18, face="bold"),
        axis.title.y = element_text(margin = margin(t=0, b=0, l=0, r=6)),
        axis.text = element_text(size=15, colour="black"),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        legend.text = element_text(size=14), 
        legend.title = element_text(size=15),
        legend.position = c(0.12,0.5),
        legend.background = element_rect(colour="black"))


# same as above but by summarized date so can apply correction factor to overall daily abundance 
dna.propn <- nad.df %>% 
  filter(region1 %in% c("4", "12")) %>% 
  group_by(date, region1) %>%
  summarize(n=n()) %>% 
  group_by(date) %>%
  mutate(propn=n/sum(n)) %>%
  mutate_at(vars(c(2)), funs(as.factor)) %>%
  mutate(region1 = ifelse(region1=="4", "Stellako", "Nadina")) %>%
  print()




#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
 


#                                                       PULL FOR SUBMISSION #2

# Decided can take 200 more samples, will take equal number of samples per day and randomly sample across size classes. 
  # Won't take from time period where samples were too small to amplify (Apr 18-22 inclusive)


# Create df with all unsent samples after Apr 22
nad.pull <- nad.df %>% 
  filter(whatman_sheet != "NA", dna_select_bin == "0", date > as.Date("2019-04-22") & date < as.Date("2019-05-21")) %>% 
  print()

  # there are 25 days to sample from. end date chosen to allow for even equal numbers of samples each day (for simplicity). 

############################
# GENERATE A RANDOM SAMPLE #
############################

# 8 samples per day                                     # LOCKED IN NOW SO CANNOT RE-SAMPLE A NEW RANDOM SAMPLE
#samp <- nad.pull %>%
#  group_by(date) %>%
#  sample_n(8) %>%
#  print() %T>%
#  write.csv("Nautley_random_sample.csv")

samp <- read.csv("Nautley_random_sample_2.csv")


#### FOLLOW UP JAN 6 2020 - Kelsey Flynn noted that 6 samples from May 2 did not actually exist. SD and I decided to pull another 6 randomly
      # from that date. Here is a random sample from May 2 for 6 more samples

# create may 2 database 
samp.may2 <- nad.pull %>%
  filter(date == "2019-05-02", !is.na(psc_book_no), !ufid %in% c("2019-8-95", "2019-8-104")) %>%              # these two samples were already submitted as part of the original 8 random samples so they are already being processed.
  print()

#rando2.2 <- samp.may2 %>% 
#  sample_n(6) %>%
#  print() %T>%
#  write.csv("Nautley_random_sample_may2.csv")

    # samples now locked in and saved in .csv file. Sent to Kelsey Flynn Jan 6 2020.


#### FOLLOW UP JAN 7 2020 - KF says sample 368 was already run, but there is no data for it. rsamp for 1 more sample.
samp.may2.2 <- nad.pull %>%
  filter(date == "2019-05-02", !is.na(psc_book_no), !ufid %in% c("2019-8-95", "2019-8-104", "2019-8-92", "2019-8-107", "2019-8-85", "2019-8-87")) %>%              # these samples were already submitted as part of the original 8 random samples, and then the above random 6 so they are already being processed.
  print()

#rando2.3 <- samp.may2.2 %>% 
#  sample_n(1) %>%
#  print() %T>%
#  write.csv("Nautley_random_sample_may2_2.csv")

#######################
# SAMPLE PULL SUMMARY #
#######################

# submission #2 distribution by LENGTH
samp.lgth <- samp %>% 
  group_by(length.class) %>%
  summarize(n=n()) %>% 
  print()

# ordered factors
samp.lgth$length.class <- factor(samp.lgth$length.class, levels=c("<80", "80-89", "90-99", "100-109", "110-119", "120-130", ">130"), ordered=T)


# plot
ggplot(samp.lgth, aes(x=length.class, y=n)) + 
  geom_bar(stat="identity", fill="turquoise", colour = "black") +
  labs(x="Length class (mm)", y="Number of samples", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    legend.text = element_text(size=14), 
    legend.title = element_text(size=15))


# submission #2 distribution by DATE
samp.date <- samp %>% 
  group_by(group.date) %>%
  summarize(n=n()) %>% 
  mutate(propn=n/sum(n)) %>% 
  print()

# ordered factors - this was for if we wanted to see the % of samples that could be nadina vs stellako based on the cross-over date May 5, but doesn't really make sense and isn't useful
#samp.date$date.cross <- factor(samp.date$date.cross, levels = c("Before May 05", "After May 05"), ordered=T)

# plot
ggplot(samp.date, aes(x=group.date, y=n)) +
  geom_bar(stat="identity", fill="turquoise", colour = "black") +
  labs(x="Date group", y="Number of samples", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    legend.text = element_text(size=14), 
    legend.title = element_text(size=15))


 



########################################
# RANDOM SAMPLE PULL 2 + SAMPLE PULL 1 #       ***** THIS CODE DOES NOT INCLUDE THE 6 NEW SAMPLES FROM MAY 2 ABOVE!!!!! ******
########################################
# How does it look when we combine both pulls?

# extract all samples from pull request 1
pull1 <- nad.df %>% 
  filter(dna_select_bin == "1", !grepl("did not amplify", dna_comment)) %>% 
  print()

# join pull request 1 with random sample pull 2
samp.sim <- full_join(pull1, samp)

# check sample sizes to confirm join was appropriate and no samples were lost 
samp.sim %>% 
  group_by(dna_select_bin) %>% 
  summarize(n=n()) %>% 
  print()


####
# SUMMARIZE AND GRAPH - by DATE 
# If pull #2 is submitted, how does it look over time? 

# summarize by date 
sim.date <- samp.sim %>% 
  mutate(pull.request = ifelse(dna_select_bin=="1", "1", "2")) %>%
  group_by(date, pull.request) %>% 
  summarize(n = n()) %>% 
  mutate(propn=n/sum(n)) %>% 
  print()

# order pull request factors 
sim.date$pull.request <- factor(sim.date$pull.request, levels=c("2", "1"), ordered=T)

    # run timing for overlay line in plot
    run <- nad.df %>% 
      group_by(date) %>% 
      summarize(n=n()) %>% 
      print()
  
# plot 
  # number of samples by date
  sim.d<-ggplot() +
    geom_bar(data=sim.date, aes(x=date, y=n, fill=pull.request), stat="identity", colour = "black") + 
    geom_line(data=run, aes(x=date, y=n/9), colour="red", size=1) +
    labs(x="Date", y="Number of samples", fill="Submission #") +
    scale_fill_manual(values=c("turquoise", "gray85")) +
    scale_y_continuous(sec.axis = sec_axis(~.*9, name = "Number of smolts")) +
    scale_x_date(limits=as.Date(c("2019-04-26", "2019-05-24")), breaks="3 day") +
    theme_bw() +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=1, hjust=1),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))
  
  # proportion by date
  ggplot(sim.date, aes(x=date, y=propn, fill=pull.request)) +
    geom_bar(stat="identity", colour = "black") +
    labs(x="Date group", y="Proportion of samples", fill="Submission #") +
    scale_fill_manual(values=c("turquoise", "gray85")) +
    theme_bw() +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=1, hjust=1),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))



####
# SUMMARIZE AND GRAPH - by LENGTH
# # If pull #2 is submitted, how does it look over lengths? 

# summarize by length
sim.lgth <- samp.sim %>% 
  mutate(pull.request = ifelse(dna_select_bin=="1", "1", "2")) %>%
  group_by(length.class, pull.request) %>% 
  summarize(n = n()) %>% 
  mutate(propn=n/sum(n)) %>% 
  print()

# order pull request factors
sim.lgth$pull.request <- factor(sim.lgth$pull.request, levels=c("2", "1"), ordered=T)


# plot
  # number of samples by length
  sim.l<-ggplot(sim.lgth, aes(x=length.class, y=n, fill=pull.request)) +
    geom_bar(stat="identity", colour = "black") +
    labs(x="Length class (mm)", y="Number of samples", fill="Submission #") +
    scale_fill_manual(values=c("turquoise", "gray85")) +
    theme_bw() +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=1, hjust=1),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))

  
  # MEGA PLOT LENGTH + DATE 
  ggarrange(sim.d, sim.l, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

  
  
  
#################################################################################################################################################
#################################################################################################################################################


                                                              ###############
                                                              # AGE SUMMARY #
                                                              ###############

age <- nad.df %>% 
  filter(age=="1") %>%
  group_by(length_class) %>% 
  summarize(n=n()) %>% 
    mutate(propn=n/sum(n)*100) %>%
  print()




#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################

  
  
                                                        ##############
                                                        # CATCH DATA #
                                                        ##############
  
  
# read in catch data 
catch.df <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheet = 2, colNames=T, detectDates=T)

# CLEANING CODE that went into making the file above.
  # difftime was removed as it wasn't useful
# format start and end times
catch <- catch %>%
  mutate(start_time =  with_options(c(scipen = 999), str_pad(catch$start_time, 5, pad = "0"))) %>% 
  mutate(end_time = with_options(c(scipen = 999), str_pad(catch$end_time, 5, pad = "0"))) %>%
  print()

#format date 
catch$date <- as.Date(catch$date, format = "%d-%b-%y")
catch$start_date <- as.Date(catch$start_date, format = "%d-%b-%y")
catch$end_date <- as.Date(catch$end_date, format = "%d-%b-%y")

# create columns for date-time 
catch$start_datetime <- as.POSIXct(paste(catch$start_date, catch$start_time),tz = "")
catch$end_datetime <- as.POSIXct(paste(catch$end_date, catch$end_time),tz = "")
catch$difftime <- difftime(catch$end_datetime, catch$start_datetime, tz="", units = c("hour"))
catch$difftime <- as.numeric(catch$difftime)


  
#########################
# CPUE by migration TOD #
#########################

  # After talking with Scott, we need to further standardize catch by the known hours of migration. Including all hours of fishing can bias the
  # catch towards lower numbers because you include hours where no fish are migrating. Based on knowledge from the crew on when peak migration
  # is, we decided on the hours of 9pm to 3am (21:00 - 03:00), 6 hours total. This means that some catches will be excluded because their fishing 
  # windows don't fall in this time period, so you are essentially dividing by 0. 
  
# filter by RST, group by day 
cpue2 <- catch %>% 
    filter(trap_type == "small RST")
  
# omit row 21 manually for now because it is 0, falls outside of the range and screws up all the counts 
cpue2 <- cpue2[-c(14),]  
  
# group by date and time   
cpue2 <- cpue2 %>%
  group_by(date) %>%
  summarize(sum=sum(sox_smolts), hrs_in_window = unique(hrs_in_window)) %>% 
  mutate(cpue = sum/hrs_in_window) %>%
  mutate(cpue = ifelse(cpue=="NaN",0,cpue)) %>%
  print()
  
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
  summarize(mean_dis=mean(discharge_m3s)) %>%
  print()

# plot 
  # CPUE2 by date 
  ggplot() +
    geom_line(data=discharge2, aes(x=date, y=mean_dis*10), size=1.2, colour="black") +
    geom_bar(data=cpue2, aes(x=date, y=cpue), stat="identity", fill="gray80", colour="black") + 
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
  

  
  
#################
# CPUE BY STOCK #        TBD!                              
#################
  
   ## Was hoping to use the daily % nadina vs stellako from submission #1 to convert daily total catches to % nadina and % stellako, 
    # but submission #1 left out some days which means the catch can't be easily converted. There isn't a large enough daily sample size to 
    # estimate any kind of decay/increase in proportion over time as it changes quite a bit up and down day-to-day. Once submission 2 is returned
    # this should be solved as we are getting samples from some days that were missed from sub #1. 
  
    # After sub #2, we can convert overall CPUE using the daily % nadina/% stellako sub-sample to estimate abundance of nadina and stellako 
    # smolts outmigrating. 

cpue_stock <- left_join(catch, dna.propn, by=c("date"))

cpue_stock_d <- cpue_stock %>% 
  mutate(propn = ifelse(is.na(propn) & region == "Nadina", 0.22, 0.78)) %>%
  mutate(est_propn = sox_smolts*propn) %>% 
  group_by(date, region) %>%
  summarize(sum=sum(est_propn, na.omit=T)) %>%
  print()

ggplot(cpue_stock, aes(x=date, y=sum, group=region, colour=region)) +
  geom_line(stat="identity", size=1.5) +
  theme_bw() +
  theme(axis.title = element_text(size=18, face="bold"),
    axis.title.y = element_text(margin = margin(t=0, b=0, l=0, r=6)),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    legend.text = element_text(size=14), 
    legend.title = element_text(size=15))















