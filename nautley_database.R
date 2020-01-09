# nadleh database creation

setwd("~/ANALYSIS/Data")

library(tidyverse)
library(XLConnect)
library(xlsx)
library(openxlsx)
library(janitor)


##########################
# EXPAND FREQUENCY TABLE #
##########################

freq.t <- read.xlsx("2019 Nautley.xlsx", sheet = 4, colNames=T, detectDates=T)

freq.t <- freq.t %>% 
  select(c(1:3)) %>%
  rename(date=Date,
    length_mm = `Length.(mm)`,
    freq=Freq) %>%
  filter(freq != "0") %>% 
  mutate_at(vars(c(1)), funs(as.character)) %>% 
  mutate(date = lubridate::ymd(date)) %>%
  print()

ind.lgth <- freq.t[rep(1:nrow(freq.t), freq.t[["freq"]]), ]
ind.lgth <- as.data.frame(ind.lgth, row.names=1:nrow(ind.lgth))



###################
# INDIVIDUAL DATA #
###################

ind.dat <- read.csv("2019 Nautley_inddata.csv")

ind.dat <- ind.dat %>%
  rename(sample_id = `Sample....from.length...freq.form.`,
    date = `Capture.............date..dd.mmm.yy.`,
    length_mm = `Smolt.length..mm.`,
    weight_g = `Smolt.Weight..g.`,
    psc_book_no = `PSC......book.s.....e.g..1.43.`,
    whatman_sheet = `Whatman..sheet.........e.g..1.35639.`,
    comment1 = Comment,
    comment2 = X,
    comment3 = X.1) %>%
  print()

ind.t <- ind.dat %>% 
  mutate(ewatch_fid = ifelse(grepl('E-Watch',comment1), paste0(comment1), "NA")) %>% 
  mutate(ewatch_fid = ifelse(ewatch_fid!="NA", gsub("[^0-9.]", "",  ewatch_fid), "NA")) %>%
  unite("comment", 7:9, sep=", ") %>%
  mutate(comment = ifelse(comment==", , ", NA, comment)) %>%
  mutate_at(vars(c(2)), funs(as.character)) %>% 
  mutate(date = lubridate::dmy(date)) %>%
  print()
ind.t$comment <- str_remove(string=ind.t$comment, pattern=", , ")

########
# JOIN #
########

ind.db <- full_join(ind.lgth, ind.t, by=c("date", "length_mm"))



#################
# DNA, AGE DATA #
#################

bio.dat <- read.xlsx("Nautley Combined data(Current).xlsx", sheet = 6, colNames=T, detectDates=T)

# tidy 
bio.dat <- bio.dat %>%
  select(-c(5, 24:26, 31)) %>%
  rename(sample_key = Sample.key,
         date = `Capture.date.(dd/mmm/yy)`,
         date_group = Grouping.date,
         length_mm = `Smolt.length.(mm)`,
         length_class = Length.class,
         weight_g = `Smolt.Weight.(g)`,
         psc_book_no = `PSC.book.#`,
         psc_sample_no = `PSC.book.sample.#`,
         whatman_sheet = `Whatman.sheet.#.(e.g.,1-35639)`,
         psc_dna = `PSC.DNA#`,
         dna_sent_bin = `DNA.and.scales.(1st.round.select)`,
         scale_sent_bin = `Scale.samples.only.(1st.round.select)`,
         comment1 = Comment1,
         comment2 = Comment2, 
         dna_comment = DNA.Lab.Comment,
         region1 = Region1,
         prob1 = Prob1,
         region2 = Region2,
         prob2 = Prob2,
         scale_lab_date = Scale.Lab.Date,
         age = Age,
         area = Area,
         dejan_comment = `Dejan's.comment`) %>% 
  mutate(date = excel_numeric_to_date(date)) %>% 
  mutate(whatman.sheet = ifelse(whatman_sheet == "N/A", NA, whatman_sheet)) %>%
  mutate(dna_sent_bin = ifelse(is.na(dna_sent_bin), 0, dna_sent_bin)) %>%
  mutate(scale_sent_bin = ifelse(is.na(scale_sent_bin), 0, scale_sent_bin)) %>%
  mutate(length_class = factor(length_class, levels= c("<80", "80-89", "90-99", "100-109", "110-119", "120-130", ">130", ordered=T))) %>% 
  mutate_at(vars(c(11,12)), funs(as.factor)) %>%
  mutate(date_group = sub('^.(.*).$', "\\1", date_group)) %>%
  mutate(ewatch_fid = ifelse(grepl('E-Watch',comment1), paste0(comment1), NA)) %>% 
  mutate(ewatch_fid = ifelse(!is.na(ewatch_fid), gsub("[^0-9.]", "",  ewatch_fid), ewatch_fid)) %>%
  mutate(comment1 = ifelse(is.na(comment1), "", comment1)) %>% 
  mutate(comment2 = ifelse(is.na(comment2), "", comment2)) %>% 
  unite("comment", 13:14, sep=", ") %>%
  mutate(comment = ifelse(comment ==", ", NA, comment)) %>% 
  print()


# JOIN #        PICK UP HERE - GET JOIN TO WORK

nad.db <- left_join(bio.dat, ind.db, by=c("date", "length_mm", "weight_g", "whatman_sheet", "psc_book_no", "comment", "ewatch_fid"))
















