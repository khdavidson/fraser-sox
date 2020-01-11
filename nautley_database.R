# nadleh database creation

setwd("~/ANALYSIS/Data")

library(tidyverse)
library(XLConnect)
library(xlsx)
library(openxlsx)
library(janitor)
library(scales)
library(ggpubr)
library(padr)
library(stringr)
library(withr)
library(padr)

###################################################################### SHEET 1: metadata #######################################################

metadata <- data.frame(variable = NA, unit = NA, description = NA)                                      # placeholder metadata tab



###################################################################### SHEET 2: hourly total catch #############################################

# read excel
catch <- read.csv("2019 Nautley_dailydata.csv")

# reformat and clean
catch <- catch %>% 
  select(c(1:15)) %>%
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
  mutate_at(vars(c(5:6)), funs(as.character)) %>% 
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

# write to csv
write.csv(catch, "catch.csv", row.names=F)




#################################################################### SHEET 3: individual smolt data ############################################

#####################################################
# Step 1: Compile individual detailed sampling data #
#####################################################

####
# LENGTH AND WEIGHT DATA FROM "2019 Nautley - Individual smolt entry" 
####

ind.dat <- read.csv("2019 Nautley_inddata.csv")                        # exported as csv first because too bulky to use read.xlsx

# reformat and clean
ind.t <- ind.dat %>%
  select(1:10) %>% 
   rename(psc_sample_no = `Sample....from.length...freq.form.`,
    date = `Capture.............date..dd.mmm.yy.`,
    length_mm = `Smolt.length..mm.`,
    weight_g = `Smolt.Weight..g.`,
    psc_book_no = `PSC......book.s.....e.g..1.43.`,
    whatman_sheet = `Whatman..sheet.........e.g..1.35639.`,
    comment1 = Comment) %>%
  mutate(ewatch_fid = ifelse(grepl('E-Watch',comment1), paste0(comment1), "NA")) %>% 
  mutate(ewatch_fid = ifelse(ewatch_fid!="NA", gsub("[^0-9.]", "",  ewatch_fid), "NA")) %>%
  unite("comment", 7:9, sep=", ") %>%
  mutate(comment = ifelse(comment==", , ", NA, comment)) %>%
  mutate_at(vars(c(2,6)), funs(as.character)) %>% 
  mutate(date = lubridate::dmy(date)) %>%
  mutate(sample_id = ifelse(!is.na(psc_book_no), paste(psc_book_no, psc_sample_no, sep="-"), psc_sample_no)) %>%
  mutate(freq = 1) %>%
  mutate(data_source1 = "2019 Nautley - individual smolt entry") %>%
  print()

# remove trailing ",," from comment concatenating
ind.t$comment <- str_remove(string=ind.t$comment, pattern=", , ")



# this is fucked, just give it to jen to sort out.

ind.t

####
# LENGTH FREQUENCY DATA FROM "2019 Nautley - length frequency entry" 
####
freq.t <- read.xlsx("2019 Nautley.xlsx", sheet = 4, colNames=T, detectDates=T)

freq.t <- freq.t %>% 
  select(c(1:3)) %>%
  rename(date=Date,
    length_mm = `Length.(mm)`,
    freq=Freq) %>%
  filter(freq != "0") %>% 
  mutate_at(vars(c(1)), funs(as.character)) %>% 
  mutate(date = lubridate::ymd(date)) %>%
  mutate(data_source1 = "2019 Nautley - length frequency entry") %>%
  print()

freq.lgth <- freq.t[rep(1:nrow(freq.t), freq.t[["freq"]]), ]
freq.lgth <- as.data.frame(freq.lgth, row.names=1:nrow(freq.lgth))

    write.csv(freq.lgth, "freq_lgth.csv", row.names=F)


####
# Join 
####

ind.join <- full_join(freq.lgth, ind.t, by=c("date", "length_mm", "freq", "data_source1"))
write.csv(ind.join, "nad_inds_join.csv", row.names = F)




####
# Clean up new database
####

# create UFID 
ind.join <- ind.join %>% 
  mutate(ufid = ifelse(!is.na(sample_id), paste("2019", sample_id, sep="-"), paste("2019", seq(1:length(is.na(sample_id))), sep="-f"))) %>%         # used "-f" to separate here to distinguish between 2019-frequency # fish and 2019-weird PSC sample with no psc book fish
  select(ufid, sample_id, ewatch_fid, date, 
         length_mm, weight_g, 
         psc_book_no, psc_sample_no, whatman_sheet,
         comment, data_source1) %>%
  mutate_at(vars(c(3,7:8)), funs(as.character)) %>%
  print()

# write to csv - EXPORTED AND CHANGED TO BE EXCEL. IF THIS IS RE-RUN, EXCEL WILL HAVE TO BE REFRESHED TOO. 
 write.csv(ind.join, "nad_inds_join.csv", row.names=F)    

      
      # xlsx file will need to be refreshed if csv is re-run above


##########################
# Step 2: Add scale data #
##########################

scales <- read.xlsx("Nautley Combined data(Current).xlsx", sheet = 7, colNames=T, detectDates=T)

scales <- scales %>% 
  select(1:8) %>% 
  rename(psc_book_no = Book,
    psc_sample_no = `Scale#`,
    sample_id = Match,
    age = Age,
    length_mm = Length,
    weight_g = Weight,
    date = Date,
    area = Area) %>% 
  mutate(psc_book_no = substring(psc_book_no, 6, 10)) %>%
  mutate_at(vars(c(2)), funs(as.character)) %>%
  mutate(sample_id = substring(sample_id, 6, 10)) %>% 
  mutate(date_source2 = "Nautley Combined data(Current).xlsx - Scale data") %>%
  print()

write.csv(scales, "scales.csv", row.names = F)



####
# Join
####

ind.scale.join <- full_join(scales, ind.join, by=c("sample_id", "psc_book_no", "psc_sample_no", "date", "length_mm", "weight_g"))

write.csv(ind.scale.join, "nad_inds_scales_join.csv", row.names=F)



#################################################################################################################################################





















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
  mutate(sample_id=paste(psc_book_no, psc_sample_no, sep="-")) %>%
  mutate(freq = 1) %>%
  mutate(data_source2 = "Nautley Combined data(Current)") %>%
  print()

write.csv(bio.dat, "nad_bio_dat.csv", row.names = F)




########
# JOIN #       
########

nad.db <- full_join(ind.db, bio.dat, by=c("date", "length_mm", "freq",  "psc_sample_no", "weight_g", "psc_book_no", "whatman_sheet",   
                                          "ewatch_fid", "sample_id"))
write.csv(nad.db, "nad_db.csv")

# clean up NA formats
nad.db <- nad.db %>% 
  mutate(weight_g = ifelse(weight_g == "N/A", NA, weight_g)) %>% 
  mutate(whatman_sheet = ifelse(whatman_sheet == "N/A", NA, whatman_sheet)) %>% 
  mutate(ewatch_fid = ifelse(ewatch_fid == "NA", NA, ewatch_fid)) %>% 
  print()

# create UFID 
nad.db <- nad.db %>% 
  mutate(ufid = paste("2019", seq(1:nrow(nad.db)), sep="-")) %>%
  print()

#re-order database - removed following useless columns: sample_key, freq, whatman.sheet
nad.db <- nad.db %>% 
  select(ufid, sample_id, ewatch_fid, date, date_group, 
         length_mm, length_class, weight_g,
         region1, prob1, region2, prob2, age, 
         psc_book_no, psc_sample_no, psc_dna, whatman_sheet,
         dna_sent_bin, scale_sent_bin, 
         comment, dna_comment, dejan_comment, data_source1, data_source2,  
         ES.File.Catch.Date, Sample.Identifier, PSC.Miscellaneous.Date, scale_lab_date, area) %>% 
  print()

write.csv(nad.db, "nadleh_individual_database_2019.csv", row.names=F)


###############
# DAILY CATCH #
###############




# write database
list_of_datasets <- list("metadata" = metadata, "daily_catch" = catch, "individual_smolts" = nad.db)
write.xlsx(list_of_datasets, file = "nadleh_database_2019.xlsx", row.names=F)


######################################################

# DP data request Jan 2020 

RSTcatch <- catch %>% 
  filter(trap_type == "small RST") %>% 
  print()

RSTdaily <- RSTcatch %>% 
  group_by(date) %>% 
  summarize(total_sox = sum(sox_smolts)) %>% 
  print()
write.csv(RSTdaily, "RSTdaily.csv")
metadata <- data.frame(variable = NA, unit = NA, description = NA)
list_of_datasets <- list("metadata" = metadata, "hourly_catch" = RSTcatch, "daily_catch" = RSTdaily, "individual_smolts" = nad.db)

write.xlsx(list_of_datasets, file = "nadleh_database2019_forDP_2.xlsx", row.names=F)




