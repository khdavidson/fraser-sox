# Nautley DNA sample request 

library(tidyverse)
library(xlsx)
library(openxlsx)
library(janitor)

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
         DNA.scales = `DNA.and.scales.(1st.round.select)`,
         scales.only = `Scale.samples.only.(1st.round.select)`,
         comments1 = Comment1,
         comments2 = Comment2, 
         DNA.comment = DNA.Lab.Comment,
         scale.lab.date = Scale.Lab.Date,
         age = Age,
         area = Area,
         D.comment = `Dejan's.comment`,
         length.check = Length.check) %>% 
  mutate(date = excel_numeric_to_date(date)) %>% 
  mutate(whatman.sheet = ifelse(whatman.sheet == "N/A", NA, whatman.sheet)) %>%
  print()


# what % of samples taken have been run? 
  filter(whatman.sheet != "NA") %>%
  summarize(key_n = n_distinct(sample.key), whatman_n = n_distinct(whatman.sheet), PSC_n = n_distinct(PSC.DNA)) %>% 
  print()


# view samples over time 
dna.run <- nad.df %>% 
  filter(whatman.sheet!="NA", PSC.DNA != "NA") %>% 
  group_by(date) %>%
  summarize(n = n()) %>%
  print()

ggplot(dna.run, aes(x=date, y=n)) +
  geom_bar(stat="identity")






