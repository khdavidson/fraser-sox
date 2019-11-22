# Nautley DNA sample request 

library(tidyverse)
library(xlsx)
library(openxlsx)
library(janitor)
library(gridExtra)
library(ggridges)
library(scales)
library(ggpubr)

# set wd
setwd("Z:/Senior Administration/Naudleh smolt program 2019/Kristy")

# read in just count form 
nad.df <- read.xlsx("nautley Combined data(Current).xlsx", sheet = 6, colNames=T, detectDates=T)

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
  print()

########################
# TOTAL SAMPLE SUMMARY #
########################

# what % of samples taken have been run? 
sub1 <- nad.df %>% 
  filter(whatman.sheet != "NA") %>%
  group_by(DNA.scales.stat) %>% 
  summarize(n=n()) %>% 
  print()

# what % have been successfully run
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

# samples over time 
dna.date <- nad.df %>% 
  filter(whatman.sheet!="NA", !grepl("did not amplify", DNA.comment)) %>% 
  group_by(DNA.scales.stat, group.date) %>%
  summarize(n = n()) %>%
  group_by(group.date) %>%
  mutate(perc = n/sum(n)) %>%
  print()

# samples by length
dna.lgth <- nad.df %>% 
  filter(whatman.sheet!="NA", !grepl("did not amplify", DNA.comment)) %>% 
  group_by(DNA.scales.stat, length.class) %>%
  summarize(n = n()) %>%
  group_by(length.class) %>%
  mutate(perc = n/sum(n)) %>%
  print()

# samples by weight
dna.wgt <- nad.df %>% 
  filter(whatman.sheet!="NA", !grepl("did not amplify", DNA.comment), DNA.scales.stat=="1") %>% 
  print()




###
# GRAPHS
###
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

ggarrange(n.d, n.l, p.d, p.l, ncol=2, nrow=2, common.legend = TRUE, legend="right")


################################
# UNSUBMITTED SAMPLE BREAKDOWN #
################################

unsamp.date <- nad.df %>% 
  filter(whatman.sheet != "NA", DNA.scales.stat == "0") %>%
  group_by(group.date) %>% 
  summarize(n=n()) %>% 
  print()

unsamp.lgth <- nad.df %>% 
  filter(whatman.sheet != "NA", DNA.scales.stat == "0") %>% 
  group_by(length.class) %>% 
  summarize(n=n()) %>% 
  print()

###
# GRAPHS
###
u.d <- ggplot(unsamp.date, aes(x=group.date, y=n)) +
  geom_bar(stat="identity", colour="black", fill="gray60") +
  theme_bw() +
  labs(x="Date", y="Count", fill="Submission status") +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, vjust=0.93, hjust=1),
    legend.text = element_text(size=14), 
    legend.title = element_text(size=15))

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




############


#                                                                 NADINA VS STELLAKO

##############
# BIOMETRICS #
##############

# reduced data 
svn.b <- nad.df %>% 
  filter(whatman.sheet!="NA", !grepl("did not amplify", DNA.comment), region %in% c(4,12)) %>% 
  mutate_at(vars(c(18)), funs(as.character)) %>%
  mutate(region = ifelse(region=="4", "Nadina", "Stellako")) %>%
  print()

#fitlm = lm(length ~ date + region, data = svn)
#svn$predlm = predict(fitlm)
#predslm = predict(fitlm, interval = "confidence")
#svn = cbind(svn, predslm)

# length
l<-ggplot(svn.b, aes(x=date, y=length, group=region)) +
  geom_point(aes(colour=region)) +
  geom_smooth(aes(colour=region, fill=region), method="lm", se=T, alpha=0.15) +
  labs(x="Date", y="Length (mm)", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    legend.text = element_text(size=14), 
    legend.title = element_text(size=15))

# width
w<-ggplot(svn.b, aes(x=date, y=weight, group=region)) +
  geom_point(aes(colour=region)) +
  geom_smooth(aes(colour=region, fill=region), method="lm", se=T, alpha=0.15) +
  labs(x="Date", y="Weight (g)", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, hjust=1),
    legend.text = element_text(size=14), 
    legend.title = element_text(size=15)) 

# length vs width
lw<-ggplot(svn.b, aes(x=length, y=weight, group=region)) +
  geom_point(aes(colour=region)) +
  geom_smooth(aes(colour=region, fill=region), method="lm", se=T, alpha=0.15) +
  labs(x="Length (mm)", y="Weight (g)", fill="Region", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.text = element_text(size=15, colour="black"),
    #axis.text.x = element_text(angle=45, vjust=0.3),
    legend.text = element_text(size=14), 
    legend.title = element_text(size=15)) 

ggarrange(l, w, lw, ncol=2, nrow=2, common.legend = TRUE, legend="right")


###############
# RUN TIMING  #
###############

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

a<-ggplot(svn.a, aes(x=date, y=n, group=region)) +
  geom_point(aes(colour=region), size=4, alpha=0.5) +
  geom_line(aes(colour=region), size=1.2) +
  labs(x="Date", y="Number of smolts", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    legend.text = element_text(size=14), 
    legend.title = element_text(size=15)) 

c<-ggplot(svn.a, aes(x=date, y=cuml_p, group=region)) +
  geom_point(aes(colour=region), size=4, alpha=0.5) +
  geom_line(aes(colour=region), size=1.2) +
  labs(x="Date", y="Cumulative proportion", colour="Region") +
  theme_bw() +
  theme(axis.title = element_text(size=18, face = "bold"),
    axis.text = element_text(size=15, colour="black"),
    axis.text.x = element_text(angle=45, vjust=1, hjust=1),
    legend.text = element_text(size=14), 
    legend.title = element_text(size=15)) 

ggarrange(a, c, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")








