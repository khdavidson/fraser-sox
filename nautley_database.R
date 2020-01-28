# nadleh database cleaning and formatting

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

  # These data were verified by J. Graf 17-Jan-2020 and the sheets were combined into one Excel file manually by K. Davidson (they came
  # from a mess of Excel files). The following code cleans up a few small final discrepancies and prepares it for analysis and sharing.
    # Code further below is the creation of the data request for Dave Patterson. 

# The sheet breakdown is: 
  # Sheet 1: metadata
  # Sheet 2: verification_notes
  # Sheet 3: daily_catch
  # Sheet 4: individual_data

######################################################## SHEET 2 DAILY CATCH ###################################################################

##################
# LOAD AND CLEAN #
##################

# read data
rawcatch <- read.csv("nautley_database_2019_dailycatch.csv")

# format start and end times
catch <- rawcatch %>%
  mutate(start_time =  with_options(c(scipen = 999), str_pad(rawcatch$start_time, 5, pad = "0"))) %>% 
  mutate(end_time = with_options(c(scipen = 999), str_pad(rawcatch$end_time, 5, pad = "0"))) %>%
  mutate(date = lubridate::dmy(date)) %>%
  mutate(start_date = lubridate::dmy(start_date)) %>%
  mutate(end_date = lubridate::dmy(end_date)) %>%
  mutate_at(vars(c(9)), funs(as.character)) %>% 
  mutate_at(vars(c(9)), funs(as.numeric)) %>% 
  mutate(sox_morts = ifelse(sox_morts=="22+", "22", as.character(sox_morts))) %>% 
  mutate_at(vars(c(10)), funs(as.numeric)) %>%
  mutate_at(vars(c(8)), funs(as.character)) %>%
  mutate(rst_tpm = ifelse(rst_tpm =="", NA, as.character(rst_tpm))) %>%
  select(trap_type, rst_tpm, date, start_date, end_date, start_time, end_time, water_temp, water_gauge,  
    sox_smolts, sox_morts, ch_fry, ch_smolts, comments) %>%
  print()

# create columns for date-time 
catch$start_datetime <- as.POSIXct(paste(catch$start_date, catch$start_time),tz = "")
catch$end_datetime <- as.POSIXct(paste(catch$end_date, catch$end_time),tz = "")
#catch$difftime <- difftime(catch$end_datetime, catch$start_datetime, tz="", units = c("hour"))
#catch$difftime <- as.numeric(catch$difftime)

# write to csv
write.csv(catch, "nautley_2019_dailycatch.csv", row.names=F)




########################################################## SHEET 3 INDIVIDUAL SMOLT DATA ######################################################

##################
# LOAD AND CLEAN #                
##################

# read data
ind.dat <- read.csv("nautley_database_2019_inddata.csv")                        

# reformat 
smolts <- ind.dat %>%
  mutate(date = lubridate::dmy(date)) %>%
  mutate_at(vars(c(7)), funs(as.character)) %>%
  mutate(weight_g = ifelse(weight_g=="N/A" & weight_g=="No weight" & weight_g=="No Weight" & weight_g =="Scale error",
    NA, as.numeric(weight_g))) %>%
  mutate(ewatch_fid = ifelse(grepl('E-Watch', comment1), paste0(comment1), "NA")) %>% 
  mutate(ewatch_fid = ifelse(ewatch_fid!="NA", gsub("[^0-9.]", "",  ewatch_fid), NA)) %>%
  mutate(ewatch_fid = ifelse(psc_sample_no > 115 & is.na(ewatch_fid), psc_sample_no, ewatch_fid)) %>%
  unite("comment", 15:16, sep=", ") %>%
  mutate(comment = ifelse(comment==", ", NA, comment)) %>%
  mutate(comment = gsub('^\\, |\\, $', '', comment)) %>%
  mutate_at(vars(c(14)), funs(as.character)) %>%
  mutate(data_source = ifelse(data_source=="", "Nautley Combined data(Current)Kristy.csv", data_source)) %>%
  mutate(data_source = ifelse(data_source != "Nautley Combined data(Current)Kristy.csv" & 
         data_source != "2019 Nautley - length frequency entry", "2019 Nautley - length frequency entry", data_source)) %>%
  mutate_at(vars(c(10, 16, 18, 30)), funs(as.character)) %>%
  mutate(whatman_sheet = ifelse(whatman_sheet =="N/A", NA, as.character(whatman_sheet))) %>%
  mutate(whatman_sheet = ifelse(whatman_sheet =="", NA, as.character(whatman_sheet))) %>%
  mutate(ewatch_sample_bin = ifelse(is.na(ewatch_sample_bin), 0, ewatch_sample_bin)) %>%
  mutate(sample_id = ifelse(!is.na(psc_book_no), paste(psc_book_no, psc_sample_no, sep="-"), psc_sample_no)) %>%
  mutate(ufid = ifelse(!is.na(sample_id), paste("2019", sample_id, sep="-"), paste("2019", seq(1:length(is.na(sample_id))), sep="-f"))) %>%
  mutate(dna_select_bin = ifelse(!is.na(region1) & !is.na(dna_comment), 1, 0)) %>% 
  mutate(scales_select_bin = ifelse(is.na(age), 0, 1)) %>%
  mutate(jdate = lubridate::yday(date)) %>%
  mutate_at(vars(c(19)), funs(as.character)) %>%
  mutate(lab_identifier = gsub("\\(18\\)", "\\(19\\)", lab_identifier)) %>%                     # changed lab ID to NautleyR(19) from NautleyR(18) so that ID's match up between DNA submissions. 
  select(ufid, sample_id, ewatch_fid, date, date_group, jdate,
    length_mm, length_class, weight_g,
    region1, prob1, region2, prob2, age, 
    psc_book_no, psc_sample_no, whatman_sheet, psc_dna_no,
    dna_select_bin, scales_select_bin, ewatch_sample_bin, lab_identifier,
    comment, dna_comment, dejan_comment, data_source, area) %>%        # omitted: sample key, length_code, es_file_catch_date, length, weight, scale_lab_date, length_check
  print()



#########################
# JOIN WITH DNA BATCH 2 #
#########################

dna.raw <- read.xlsx("NautleyCombined(19)_2020-01-27.xlsx", sheet=5, detectDates=T, startRow=4)
dna.raw <- dna.raw[,-c(15:16)]

dna.combo <- dna.raw %>% 
  filter(Fish != "19.000299999999999") %>% 
  rename(lab_identifier = Fish,
    dna_comment = Comment,
    NEWregion1 = `Region.1`,
    NEWprob1 = `Prob.1`,
    NEWregion2 = `Region.2`,
    NEWprob2 = `Prob.2`,
    region3 = `Region.3`,
    prob3 = `Prob.3`,
    region4 = `Region.4`,
    prob4 = `Prob.4`,
    region5 = `Region.5`,
    prob5 = `Prob.5`, 
    region6 = X13,
    prob6 = X14) %>%
  mutate_at(vars(c(4:5)), funs(as.numeric)) %>%
  mutate(jdate=substring(lab_identifier, 25, 27)) %>%
  mutate(psc_dna_no=substring(lab_identifier, 29, 33)) %>%
  mutate_at(vars(c(15)), funs(as.numeric)) %>% 
  mutate_at(vars(c(16)), funs(as.integer)) %>%
  print()


# Join
smolts.join <- left_join(smolts, dna.combo, by=c("jdate", "psc_dna_no"))

# CLEAN 
smolts.clean <- smolts.join %>% 
  rename(lab_identifier = lab_identifier.x) %>%
  mutate(lab_identifier = ifelse(lab_identifier == "", lab_identifier.y, lab_identifier)) %>% 
  rename(dna_comment = dna_comment.x) %>% 
  mutate(dna_comment = ifelse(dna_comment == "", dna_comment.y, dna_comment)) %>% 
  mutate(dna_select_bin = ifelse(str_detect(lab_identifier, "NautSMOLTS_B2"), 2, dna_select_bin)) %>%
  mutate(dna_select_bin = ifelse(str_detect(dna_comment, "did not amplify") & dna_select_bin==0, 1, dna_select_bin)) %>%
  select(-c("lab_identifier.y", "dna_comment.y")) %>%
  select(ufid, sample_id, ewatch_fid, date, date_group, jdate,
    length_mm, length_class, weight_g, 
    region1, NEWregion1, prob1, NEWprob1, region2, NEWregion2, prob2, NEWprob2,
    region3, prob3, region4, prob4, region5, prob5, region6, prob6, age,
    psc_book_no, psc_sample_no, whatman_sheet, psc_dna_no,
    dna_select_bin, scales_select_bin, ewatch_sample_bin, lab_identifier, 
    comment, dna_comment, dejan_comment, data_source, area) %>%
  print()

# EXPORT
write.csv(smolts.clean, "nautley_2019_smoltdata.csv", row.names=F)   






################################################################################################################################################

###########################
# DATA FOR DAVE PATTERSON #
###########################
# 17-Jan-2020

dpcatch <- catch %>% 
  filter(trap_type=="small RST") %>% 
  group_by(trap_type, date) %>% 
  summarize(total_sox = sum(sox_smolts), total_morts = sum(sox_morts), avg_temp = mean(water_temp), avg_gauge = mean(water_gauge)) %>%
  print()

# write to csv
write.csv(dpcatch, "nautley_2019_dailycatch_DP.csv", row.names=F)




