

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

# set wd
setwd("Z:/Senior Administration/Naudleh smolt program 2019/Kristy")

# read in just count form 
nad.df <- read.xlsx("Nautley Combined data(Current).xlsx", sheet = 6, colNames=T, detectDates=T)

# tidy 
nad.df <- nad.df %>%
  select(-c(7,24:25)) %>%
  rename(sample.key = Sample.key,
         date = `Capture.date.(dd/mmm/yy)`,
         group.date = Grouping.date,
         length = `Smolt.length.(mm)`,
         length.class.code = Length.class.code,
         length.class = Length.class,
         weight = Weight,
         PSC.book = `PSC.book.#`,
         PSC.sample = `PSC.book.sample.#`,
         whatman.sheet = `Whatman.sheet.#.(e.g.,1-35639)`,
         PSC.DNA = `PSC.DNA#`,
         DNA.scales.stat = `DNA.and.scales.(1st.round.select)`,
         scales.only = `Scale.samples.only.(1st.round.select)`,
         comments1 = Comment1,
         comments2 = Comment2, 
         DNA.comment = DNA.Lab.Comment,
         region = Region1,
         scale.lab.date = Scale.Lab.Date,
         age = Age,
         area = Area,
         D.comment = `Dejan's.comment`,
         length.check = Length.check) %>% 
  mutate(date = excel_numeric_to_date(date)) %>% 
  mutate(whatman.sheet = ifelse(whatman.sheet == "N/A", NA, whatman.sheet)) %>%
  mutate(DNA.scales.stat = ifelse(is.na(DNA.scales.stat), 0, DNA.scales.stat)) %>%
  mutate(length.class = factor(length.class, levels= c("<80", "80-89", "90-99", "100-109", "110-119", "120-130", ">130", ordered=T))) %>% 
  mutate_at(vars(c(11)), funs(as.factor)) %>%
  mutate(group.date = sub('^.(.*).$', "\\1", group.date)) %>%
  unite("book_sample", PSC.book:PSC.sample, sep="-", remove=F) %>%
  print()


##################################################################################################################################################

#                                                        Information to inform SAMPLE PULL #2



########################
# TOTAL SAMPLE SUMMARY #
########################

# what % of samples taken have been run? 
sub1 <- nad.df %>% 
  filter(whatman.sheet != "NA") %>%
  group_by(DNA.scales.stat) %>% 
  summarize(n=n()) %>% 
  print()

# what % have been successfully run?
samp_propn <- nad.df %>%
  filter(whatman.sheet != "NA", !grepl("did not amplify", DNA.comment), !sample.key %in% c("367","382")) %>%
  group_by(DNA.scales.stat, region) %>% 
  summarize(DNA_samp = n()) %>%
  mutate(propn = DNA_samp/sum(DNA_samp)) %>%
  print()

# range of did not amplify samples 
no.amp <- nad.df %>%
  filter(grepl("did not amplify", DNA.comment)) %>%
  group_by(date, length.class) %>%
  summarize(n = n()) %>%
  group_by(date) %>%
  mutate(perc=n/sum(n)) %>%
  print()

# plot this for visual
ggplot(no.amp, aes(x=date, y=length.class, height=n)) +
  geom_density_ridges(stat="identity", scale=1)

ggplot(no.amp, aes(x=date, y=n, fill=length.class)) +
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
  filter(whatman.sheet!="NA", !grepl("did not amplify", DNA.comment)) %>% 
  group_by(DNA.scales.stat, group.date) %>%
  summarize(n = n()) %>%
  group_by(group.date) %>%
  mutate(perc = n/sum(n)) %>%
  print()

# all samples taken - by length
dna.lgth <- nad.df %>% 
  filter(whatman.sheet!="NA", !grepl("did not amplify", DNA.comment)) %>% 
  group_by(DNA.scales.stat, length.class) %>%
  summarize(n = n()) %>%
  group_by(length.class) %>%
  mutate(perc = n/sum(n)) %>%
  print()

# all samples taken - by weight
dna.wgt <- nad.df %>% 
  filter(whatman.sheet!="NA", !grepl("did not amplify", DNA.comment), DNA.scales.stat=="1") %>% 
  print()


# plot to see together

  # plots by number of samples 
  n.d <- ggplot(dna.date, aes(x=group.date, y=n, fill=DNA.scales.stat)) +
    geom_bar(aes(fill=DNA.scales.stat), stat="identity", colour="black") +
    theme_bw() +
    labs(x="Date", y="Count", fill="Submission status") +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=0.93, hjust=1),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))
  
  n.l <- ggplot(dna.lgth, aes(x=length.class, y=n, fill=DNA.scales.stat)) +
    geom_bar(aes(fill=DNA.scales.stat), stat="identity", colour="black") +
    theme_bw() +
    labs(x="Length class (mm)", y="Count", fill="Submission status") +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=0.6),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))
  
  # plots by proportion of samples 
  p.d <- ggplot(dna.date, aes(x=group.date, y=perc, fill=DNA.scales.stat)) +
    geom_bar(aes(fill=DNA.scales.stat), stat="identity", colour="black") +
    theme_bw() +
    labs(x="Date", y="Percent of total", fill="Submission status") +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=0.93, hjust=1),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))
  
  p.l <- ggplot(dna.lgth, aes(x=length.class, y=perc, fill=DNA.scales.stat)) +
    geom_bar(aes(fill=DNA.scales.stat), stat="identity", colour="black") +
    theme_bw() +
    labs(x="Length class (mm)", y="Percent of total", fill="Submission status") +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=0.6),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=13))
  
  p.w <- ggplot(dna.wgt, aes(x=date, y=perc, fill=DNA.scales.stat)) +
    geom_bar(stat="identity", colour="black") +
    theme_bw()
  
  # plot number and proprotion in one mega graph
  ggarrange(n.d, n.l, p.d, p.l, ncol=2, nrow=2, common.legend = TRUE, legend="right")



################################
# UNSUBMITTED SAMPLE BREAKDOWN #
################################

# what is the date range of unsubmitted samples? 
unsamp.date <- nad.df %>% 
  filter(whatman.sheet != "NA", DNA.scales.stat == "0") %>%
  group_by(group.date) %>% 
  summarize(n=n()) %>% 
  print()

# what is the length range of unsubmitted samples?
unsamp.lgth <- nad.df %>% 
  filter(whatman.sheet != "NA", DNA.scales.stat == "0") %>% 
  group_by(length.class) %>% 
  summarize(n=n()) %>% 
  print()


# plot

  # by date
  u.d <- ggplot(unsamp.date, aes(x=group.date, y=n)) +
    geom_bar(stat="identity", colour="black", fill="gray60") +
    theme_bw() +
    labs(x="Date", y="Count", fill="Submission status") +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=0.93, hjust=1),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))
  
  # by length
  u.l <- ggplot(unsamp.lgth, aes(x=length.class, y=n)) +
    geom_bar(stat="identity", colour="black", fill="gray60") +
    theme_bw() +
    labs(x="Length class (mm)", y="Count", fill="Submission status") +
    theme(axis.title = element_text(size=18, face = "bold"),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=0.6),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))

  ggarrange(u.d, u.l, ncol=2, common.legend = TRUE, legend="bottom")




#################################   JUST LOOKING IN DEPTH AT DNA SAMPLES SOS FAR: NADINA VS STELLAKO


##############
# BIOMETRICS #
##############

# reduced dataset for run samples
svn.b <- nad.df %>% 
  filter(whatman.sheet!="NA", !grepl("did not amplify", DNA.comment), region %in% c(4,12)) %>% 
  mutate_at(vars(c(18)), funs(as.character)) %>%
  mutate(region = ifelse(region=="4", "Nadina", "Stellako")) %>%
  print()

#fitlm = lm(length ~ date + region, data = svn)
#svn$predlm = predict(fitlm)
#predslm = predict(fitlm, interval = "confidence")
#svn = cbind(svn, predslm)


# plot 
  # by length
  l<-ggplot(svn.b, aes(x=date, y=length, group=region)) +
    geom_smooth(aes(colour=region, fill=region), method="lm", se=T, size=1.5, alpha=0.15) +
    geom_point(aes(colour=region, fill=region),alpha=0.5, size=3) +
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
  
  # by width
  w<-ggplot(svn.b, aes(x=date, y=weight, group=region)) +
    geom_smooth(aes(colour=region, fill=region), method="lm", se=T, size=1.5, alpha=0.15) +
    geom_point(aes(colour=region, fill=region),alpha=0.5, size=3) +
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
  lw<-ggplot(svn.b, aes(x=length, y=weight, group=region)) +
    geom_point(aes(colour=region, fill=region),alpha=0.5, size=2) +
    geom_smooth(aes(colour=region, fill=region), method="lm", se=T, alpha=0.15) +
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
  filter(whatman.sheet!="NA", !grepl("did not amplify", DNA.comment), region %in% c(4,12)) %>% 
  mutate_at(vars(c(18)), funs(as.character)) %>%
  mutate(region = ifelse(region=="4", "Nadina", "Stellako")) %>%
  group_by(date, region) %>%
  summarize(n=n()) %>%
  group_by(region) %>%
  mutate(cuml_n = cumsum(n)) %>%
  mutate(cuml_p = cuml_n/sum(n)) %>%
  print()

  
# plot
  # by daily run time 
  a<-ggplot(svn.a, aes(x=date, y=n, group=region, colour=region)) +
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
  c<-ggplot(svn.a, aes(x=date, y=cuml_p, group=region, colour=region)) +
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
  filter(region %in% c("4", "12")) %>% 
  group_by(group.date, region) %>%
  summarize(n=n()) %>% 
  group_by(group.date) %>%
  mutate(propn=n/sum(n)) %>%
  mutate_at(vars(c(2)), funs(as.factor)) %>%
  mutate(region = ifelse(region=="4", "Stellako", "Nadina")) %>%
  print()

# plot
  # by date
    ggplot(dna.propn, aes(x=group.date, y=propn, group=region, colour=region)) + 
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
  filter(region %in% c("4", "12")) %>% 
  group_by(date, region) %>%
  summarize(n=n()) %>% 
  group_by(date) %>%
  mutate(propn=n/sum(n)) %>%
  mutate_at(vars(c(2)), funs(as.factor)) %>%
  mutate(region = ifelse(region=="4", "Stellako", "Nadina")) %>%
  print()




################################################### PULL FOR SUBMISSION #2

# Decided can take 200 more samples, will take equal number of samples per day and randomly sample across size classes. 
  # Won't take from time period where samples were too small to amplify (Apr 18-22 inclusive)


# Create df with all unsent samples after Apr 22
nad.pull <- nad.df %>% 
  filter(whatman.sheet != "NA", DNA.scales.stat == "0", date > as.Date("2019-04-22") & date < as.Date("2019-05-21")) %>% 
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

samp <- read.csv("Nautley_random_sample.csv", row.names=F)


#### FOLLOW UP JAN 6 2020 - Kelsey Flynn noted that 6 samples from May 2 did not actually exist. SD and I decided to pull another 6 randomly
      # from that date. Here is a random sample from May 2 for 6 more samples

# create may 2 database 
samp.may2 <- nad.pull %>%
  filter(date == "2019-05-02", PSC.DNA != "NA", !sample.key %in% c(356, 365)) %>%              # these two samples were already submitted as part of the original 8 random samples so they are already being processed.
  print()

#rando2.2 <- samp.may2 %>% 
#  sample_n(6) %>%
#  print() %T>%
#  write.csv("Nautley_random_sample_may2.csv")

    # samples now locked in and saved in .csv file. Sent to Kelsey Flynn Jan 6 2020.


#### FOLLOW UP JAN 7 2020 - KF says sample 368 was already run, but there is no data for it. rsamp for 1 more sample.
samp.may2.2 <- nad.pull %>%
  filter(date == "2019-05-02", PSC.DNA != "NA", !sample.key %in% c(356, 365, 370, 374, 353, 368, 346, 348)) %>%              # these samples were already submitted as part of the original 8 random samples, and then the above random 6 so they are already being processed.
  print()

rando2.3 <- samp.may2.2 %>% 
  sample_n(1) %>%
  print() %T>%
  write.csv("Nautley_random_sample_may2_2.csv")

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
  filter(DNA.scales.stat == "1", !grepl("did not amplify", DNA.comment)) %>% 
  print()

# join pull request 1 with random sample pull 2
samp.sim <- full_join(pull1, samp)

# check sample sizes to confirm join was appropriate and no samples were lost 
samp.sim %>% 
  group_by(DNA.scales.stat) %>% 
  summarize(n=n()) %>% 
  print()


####
# SUMMARIZE AND GRAPH - by DATE 
# If pull #2 is submitted, how does it look over time? 

# summarize by date 
sim.date <- samp.sim %>% 
  mutate(pull.request = ifelse(DNA.scales.stat=="1", "1", "2")) %>%
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
  mutate(pull.request = ifelse(DNA.scales.stat=="1", "1", "2")) %>%
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

  
  
  

##################################################################################################################################################

# AGE data

###############
# AGE SUMMARY #
###############

age <- nad.df %>% 
  filter(age=="1") %>%
  group_by(length.class) %>% 
  summarize(n=n()) %>% 
    mutate(propn=n/sum(n)*100) %>%
  print()




##################################################################################################################################################

# Total catch data from Nautley overall catch entry datasheet 

setwd("~/Data")

# read data - exported as a CSV first because of issues reading in using read.xlsx (very slow and sluggish, might be due to weird column names) 
  # also time formats WTF is going on there. 
catch <- read.csv("2019 Nautley.csv")


#################
# DATA CLEANING #
#################
catch <- catch %>% 
  rename(trap_type = `Trap.type.....RST..small.fyke..large.fyke.`,
    date = `Date..dd.mmm.yy.`,
    start_time = `Start.time.....hh.ss.`,
    end_time = `End.time..hh.ss.`,
    RST_tpm = `X.RST.turns.per.min...e.g...7.3...`,
    sox_smolts = `X..Sox.smolts`,
    sox_morts = `X..Sox.morts..incld.in.previous.column.`,
    CH_fry = `X..CH..fry`,
    CH_smolt = `X..CH..smolts`,
    water_temp = `Water.temp..ºC.`,
    gauge_m = `Staff.gauge..m.`,
    comments = Comments) %>%
  print()

# format start and end times
catch <- catch %>%
  mutate_at(vars(c(2:4)), funs(as.character)) %>% 
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


########
# CPUE #
########

# calculate basic CPUE by # smolts/# hours fished - this is because of difference in sampling lengths over the season
total <- catch %>% 
  mutate(sum_std = sox_smolts/difftime) %>%
  group_by(start_date) %>% 
  summarize(cpue=sum(sum_std)) %>%
  print()

# plot 
  # CPUE by date 
  ggplot(total, aes(x=start_date, y=cpue)) +
    geom_bar(stat="identity", fill="gray80", colour="black") + 
    scale_y_continuous(limits=c(0,800)) +
    scale_x_date(limits=as.Date(c("2019-04-13", "2019-05-27")), breaks="4 day", labels = date_format("%b %d")) +
    labs(x="Date", y="CPUE (smolts/hour)") +
    theme_bw() +
    theme(axis.title = element_text(size=18, face="bold"),
      axis.title.y = element_text(margin = margin(t=0, b=0, l=0, r=6)),
      axis.text = element_text(size=15, colour="black"),
      axis.text.x = element_text(angle=45, vjust=1, hjust=1),
      legend.text = element_text(size=14), 
      legend.title = element_text(size=15))


  
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
  
  
#cpue2check <- subset(cpue2, format(start_datetime, '%H') %in% 
  #c('04', '05', '06', '07', '08', "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"))
  
  
  


  

  
  
  
  
  
  
  
  
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















