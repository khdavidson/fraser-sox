


#                                Nechako watershed ('northern') database cleaning and formatting 2019


# 2019 data were verified by J. Graf 17-Jan-2020 and the sheets were combined into one Excel file manually by K. Davidson (they came
# from a mess of Excel files). The following code cleans up a few small final discrepancies and prepares it for analysis and sharing.
# The 2019 data were compiled early on to produce the final report analysis. Note that this has since changed with the addition of 2021 
# data; final report scripts may not run due to changed column names, formats, etc. 

# 2021 data were verified by S. Hobson Jul 2021. Sheets were already combined in 1 Excel file, with the exception of GSI data (not yet
# compiled at this time). The following code cleans up a few small final discrepancies and prepares it for analysis and sharing. No scripts
# were generated from 2021 data (other than in-season report generators) prior to compiling these data. 

# The sheet breakdown in 2019 is: 
# Sheet 1: metadata
# Sheet 2: verification_notes (not included in final)
# Sheet 3: nightly_catch
# Sheet 4: biosampling

# The sheet breakdown in 2021 is: 
# Sheet 1: metadata
# Sheet 2: environmental
# Sheet 3: nightly_catch
# Sheet 4: length_frequency
# Sheet 5: biosampling
# Sheet 6: verification_notes (not included in final)

# GOAL: To combine these formats into 1 flat file for each type of data (enviro, catch, LF, biosample)

##########################################################################################################################################

library(tidyverse)      # for everything
library(readxl)         # for read_excel()
library(openxlsx)       # for createWorkbook() etc.
#library(janitor)
#library(scales)
#library(ggpubr)
#library(padr)
library(stringr)       # for str_pad()
library(withr)         # for with_options()
#library(padr)

setwd("~/ANALYSIS/data/nadleh_raw_files")
options(scipen = 9999999999)

##########################################################################################################################################

# Historical Nadleh data
catch.h.raw <- read.csv("1999-2000 Nautley data.csv")

# 2019 NADLEH data (individual workbooks/data sources)
catch.19.raw <- read_excel("nadleh_ANALYTICAL_database_2019.xlsx", sheet="hourly_catch", na="NA")
ind.smolts.19.raw <- read_excel("nadleh_ANALYTICAL_database_2019.xlsx", sheet="individual_smolts", na="NA")   # Note this isn't the absolute original file, but changes and edits during verification etc were applied here, so this is the most accurate version
#MGLb1.19.raw <- read_excel("Nautley Combined data(Current) - verified JG.xlsx", sheet="Individual smolt data", na="NA")  # as close as it gets now to original batch 1 DNA file
MGLfull.19.raw <- read_excel("NautleyCombined(19)_2020-01-27.xlsx", sheet="Individual Region IDs", skip=3, na="NA")
MGLb2.19.raw <- read_excel("Nautley_Batch2(19)_2020-01-27.xlsx", sheet="Individual Region IDs", skip=3, na="NA")
lf.19.raw <- read_excel("nadleh_ANALYTICAL_database_2019.xlsx", sheet="individual_smolts", na="NA")
Nscale.191.raw <- read_excel("2019SnaurA-DNA-V.xlsx", sheet="RawData", skip=12)
Nscale.192.raw <- read_excel("2019SnaurA1-DNA-V.xlsx", sheet="RawData", skip=12)

# 2021 NADLEH data (workbook with sheets and individual GSI files)
Nenviro.21.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="environmentals", na="NA")
Ncatch.21.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="nightly_catch", na="NA")
Nlf.21.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="length_frequency", na="NA")
Nbio.21.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="biosampling", na="NA")
NMGLb1.21.raw <- read_excel("kokaneee_sockeyeNadlehSm(21)_2021-10-12.xlsx", sheet="Individual IDs", skip=3, na="NA")
NMGLb2.21.raw <- read_excel("kokaneee_sockeyePID20210042_NAUTLEY_R_2021_SMOLTS_2022-04-13.xlsx", sheet="Individual IDs", skip=3, na="NA")
NMGLfull.21.raw <- read_excel("kokaneee_sockeyePID20210042_NADLEH_NAUTLEY_2021_SMOLTS_2022-04-13.xlsx", sheet="Individual IDs", skip=3, na="NA")
Nscale.21.raw <- read_excel("2021SnaurA-DNA-V.xlsx", sheet="RawData", skip = 12)

# 2021 STELLAKO data (workbook with sheets and individual GSI files)
Senviro.21.raw <- read_excel("stellako_data_clean_2021.xlsx", sheet="environmentals", na="NA")
Scatch.21.raw <- read_excel("stellako_data_clean_2021.xlsx", sheet="nightly_catch", na="NA")
Slf.21.raw <- read_excel("stellako_data_clean_2021.xlsx", sheet="length_frequency", na="NA")
Sbio.21.raw <- read_excel("stellako_data_clean_2021.xlsx", sheet="biosampling", na="NA")                        
SMGL.21.raw <- read_excel("kokaneee_sockeyeStellakoSm(21)_2021-10-12.xlsx", sheet="Individual IDs", skip=3, na="NA")
Sscale.21.raw <- read_excel("2021SsterA-DNA-V.xlsx", sheet="RawData", skip=12)


#################################################################################################################################################


#                                                     CREATING THE NIGHTLY CATCH SHEET

# =============== HISTORICAL (99-00) CLEAN/ORGANIZE ===============
catch.h <- catch.h.raw %>%
  rename(date_closed = date,
         time_trap_open = start_time,
         time_trap_closed = end_time,
         total_unmarked = total_sox) %>%
  mutate(site = "Nadleh",
         date_closed = lubridate::dmy(paste0(date_closed, sep="-", year)),
         date_opened = date_closed-1) %>%
  print()


# =============== 2019 CLEAN/ORGANIZE ===============
# Make 2019 format match 2021 as much as possible
catch.19 <- catch.19.raw %>%
  rename(rst_rpms=rst_tpm,
         time_trap_open=start_time,
         time_trap_closed=end_time,
         n_unmarked_sampled = sox_smolts_sampled,
         n_unmarked_dead = sox_morts,
         n_unmarked_lf = n_length_frequency,
         n_chinook_fry = ch_fry,
         n_chinook_smolts = ch_smolts) %>%
  mutate(time_trap_open = with_options(c(scipen = 999), str_pad(catch.19.raw$start_time, 5, pad = "0")),
         time_trap_closed = with_options(c(scipen = 999), str_pad(catch.19.raw$end_time, 5, pad = "0")),
         trap_type = ifelse(trap_type=="small RST", "6' RST", trap_type),
         year="2019", site="Nadleh", crew="BB,AR,AK,JN", location="not recorded in 2019",
         sox_smolts_caught = n_unmarked_sampled + n_unmarked_lf + n_unmarked_spilled + n_unmarked_dead) %>% 
  rename(total_unmarked = sox_smolts_caught) %>%
  select(-c(start_datetime, end_datetime)) %>%
  print()


# =============== 2021 NADLEH CLEAN/ORGANIZE ===============
Ncatch.21 <- Ncatch.21.raw %>% 
  mutate_at(vars(n_marked_released:n_chinook_smolts), as.numeric) %>%
  mutate(trap_type = "8' RST",
         year="2021") %>%
  rename(n_unmarked_lf = n_length_frequency) %>%
  print()


# ===============2021 STELLAKO CLEAN/ORGANIZE ===============
Scatch.21 <- Scatch.21.raw %>% 
  mutate_at(vars(n_marked_released:n_chinook_smolts), as.numeric) %>%
  mutate(trap_type = "6' RST",
         year="2021") %>%
  rename(n_unmarked_lf = n_length_frequency) %>%
  print()


# =============== JOIN ===============
catch.join <- full_join(catch.19, Ncatch.21) %>%
  bind_rows(., Scatch.21) %>%
  select(year, site, trap_type, location, date_opened, date_closed, time_trap_open, time_trap_closed, crew, 
         markers, mark_type, n_marked_released, n_marked_dead, n_recaps_upper_clip, n_recaps_lower_clip, total_recaps,
         n_unmarked_sampled, n_unmarked_lf, n_unmarked_spilled, n_unmarked_dead, total_unmarked, 
         n_chinook_fry, n_chinook_smolts, other_bycatch, comments) %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  mutate_at("year", as.numeric) %>%
  mutate(total_unmarked = ifelse(year=="2021", n_unmarked_sampled+n_unmarked_lf+n_unmarked_spilled+n_unmarked_dead, total_unmarked)) %>%  
  full_join(., catch.h) %>%
  print()

# omitted 'date', 'date_true', columns from 2019 and 2021 spreadsheets (respectively) and open_datetime, close_datetime 



##########################################################################################################################################


#                                              CREATING THE ENVIRONMENTALS SHEET


# =============== 2019 CLEAN/ORGANIZE ===============
# Create an environmentals sheet from 2019 (did not exist originally, it was integrated with catch data)
enviro.19.raw <- catch.19 %>% 
  mutate(rst_rpms = ifelse(rst_rpms=="2.75-0", "2.75", rst_rpms)) %>%
  mutate_at(vars(rst_rpms, water_temp, water_gauge), as.numeric) %>%
  select(trap_type:time_trap_closed, rst_rpms, water_temp:comments) %>%
  print()

enviro.19 <- enviro.19.raw %>%
  group_by(trap_type, date_opened, date_closed, time_trap_open) %>% 
  summarize(rst_rpms=mean(rst_rpms), water_temp_C=mean(water_temp), 
            water_gauge_m=mean(water_gauge), date_opened=unique(date_opened), date_closed=unique(date_closed),
            time=unique(time_trap_open), comments=comments) %>%
  mutate(year = 2019,
         site = "Nadleh",
         trap_type = ifelse(trap_type=="small RST", "6' RST", trap_type),
         crew_initials = "BB,AR,AK,JN")%>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  print()


# =============== 2021 NADLEH CLEAN/ORGANIZE ===============
Nenviro.21 <- Nenviro.21.raw %>%
  mutate_at(vars(rst_rpms, water_gauge_m:water_temp_C), as.numeric) %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  mutate(year = 2021,
         trap_type = "8' RST") %>%
  print()


# =============== 2021 STELLAKO CLEAN/ORGANIZE ===============
Senviro.21 <- Senviro.21.raw %>% 
  mutate_at(vars(rst_rpms, water_gauge_m:water_temp_C), as.numeric) %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  mutate(year = 2021,
         trap_type = "6' RST") %>%
  print()


# =============== Final environmental data ===============
enviro.join <- full_join(Nenviro.21, enviro.19) %>%
  bind_rows(., Senviro.21) %>%
  mutate_at("rst_rpms", as.numeric) %>%
  select(year, site, trap_type, date_opened, date_closed, time, location, crew_initials, rst_rpms, debris_load, debris_type1, 
         debris_type2, water_gauge_m, air_temp_C, water_temp_C, comments) %>%
  print()





##########################################################################################################################################

#                                                 CREATING THE LENGTH-FREQUENCY SHEET



# =============== 2019 CLEAN/ORGANIZE ===============
# Rename and add to 2019 to match 2021 - Note that sampling dates were entered as the day the trap was opened 
lf.19 <- lf.19.raw %>% 
  mutate_at(vars(c(weight_g:prob6)), as.numeric) %>% 
  filter(data_source == "2019 Nautley - length frequency entry") %>%
  select(date_opened, date_closed, length_mm, comment) %>% 
  rename(comments = comment) %>%  
  mutate_at("date_opened", as.Date) %>%
  mutate(year = "2019",
         site = "Nadleh",
         crew = "BB,AR,AK,JN",
         trap_type = "6' RST",
         data_type = "Routine",
         comments = ifelse(comments=="NA", NA, comments)) %>% 
  print()


# =============== 2021 NADLEH CLEAN/ORGANIZE ===============
Nlf.21 <- Nlf.21.raw[rep(1:nrow(Nlf.21.raw), Nlf.21.raw[["count"]]),]
Nlf.21 <- Nlf.21 %>% 
  mutate(year="2021",
         comments = ifelse(comments=="NA", NA, comments)) %>% 
  print()


# =============== 2021 STELLAKO CLEAN/ORGANIZE ===============
Slf.21 <- Slf.21.raw[rep(1:nrow(Slf.21.raw), Slf.21.raw[["count"]]),]
Slf.21 <- Slf.21 %>% 
  mutate(year="2021") %>% 
  print()


# =============== JOIN ===============
lf.join <- full_join(lf.19, Nlf.21) %>% 
  bind_rows(., Slf.21) %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  select(year, site, date_opened, date_closed, crew, trap_type, data_type, length_mm, comments) %>% 
  print()

## removed 'count' as it is not needed anymore - length-frequency tables already expanded 



##########################################################################################################################################

#                                              CREATE BIOSAMPLE SHEET


# =============== 2019 CLEAN/ORGANIZE ===============

# FIELD BIOSAMPLING DATA ---------------
ind.smolts.19 <- ind.smolts.19.raw %>%
  rename(whatman_uid=whatman_sheet,
         psc_uid = sample_id,
         PSC_lab_comments=dejan_comment,
         comments=comment,
         ewatch_uid=ewatch_fid,
         smolt_data_source = data_source,
         DOY_closed = jdate) %>%
  select(-c(NEWregion1, NEWprob1, NEWregion2, NEWprob2, region3:prob6, ewatch_sample_bin, psc_dna_no, area, dna_select_bin, age,
            PSC_lab_comments)) %>%
  mutate(whatman_uid = ifelse(grepl("19[.]0", as.character(whatman_uid)), 
                              as.character(str_pad(whatman_uid, width=9, side="right", pad="0")), 
                              as.character(whatman_uid)),
         whatman_cell = ifelse(grepl("19[.]0", whatman_uid), substring(whatman_uid, 6, 9),
                               ifelse(grepl("-", whatman_uid), sub(".*-", "", whatman_uid), whatman_uid)), 
         whatman_cell = str_remove(whatman_cell, "^0+"),
         ewatch_uid = ifelse(grepl("sacrificed for Dave Patterson", comments), "E-Watch unk ID", ewatch_uid)) %>%
  filter(smolt_data_source != "2019 Nautley - length frequency entry") %>% 
  print()


# MGL FILES: GSI RESULTS ---------------

# Batch 1 subset: note original file was lost, so extracting it from the 2019 combined data for cleaning and easier re-joining
MGLb1.19 <- ind.smolts.19 %>%
  filter(!is.na(lab_identifier) | !is.na(dna_comment), !grepl("NautSMOLTS_B2", lab_identifier)) %>%
  select(lab_identifier, DOY_closed, whatman_uid, whatman_cell, region1, prob1, region2, prob2, dna_comment) %>%
  rename(MGL_identifier = lab_identifier,
         gsi_subset_reg1 = region1,
         gsi_subset_prob1 = prob1,
         gsi_subset_reg2 = region2,
         gsi_subset_prob2 = prob2,
         MGL_comments = dna_comment) %>%
  mutate(dna_data_source = "data entry file - original MGL file lost",
         dna_select_bin = 1,
         MGL_identifier = ifelse(is.na(MGL_identifier) & grepl("-", whatman_uid), 
                                 paste0("NautleyR(19)      RST", sep="   ", DOY_closed, sep="   ", whatman_uid),
                            ifelse(is.na(MGL_identifier) & grepl("19.0", whatman_uid),
                                   paste0("NautleyR(19)      RST", sep="   ", DOY_closed, sep="  ", whatman_cell),
                                   MGL_identifier))) %>%
  print()
  

# Batch 2 subset 
MGLb2.19 <- MGLb2.19.raw %>% 
    filter(Fish != "19.000299999999999") %>% 
    rename(MGL_identifier = Fish,
           MGL_comments = Comment,
           gsi_subset_reg1 = `Region 1`,
           gsi_subset_prob1 = `Prob 1`,
           gsi_subset_reg2 = `Region 2`,
           gsi_subset_prob2 = `Prob 2`,
           gsi_subset_reg3 = `Region 3`,
           gsi_subset_prob3 = `Prob 3`,
           gsi_subset_reg4 = `Region 4`,
           gsi_subset_prob4 = `Prob 4`,
           gsi_subset_reg5 = `Region 5`,
           gsi_subset_prob5 = `Prob 5`,
           gsi_subset_reg6 = `...13`,
           gsi_subset_prob6 = `...14`,
           gsi_subset_reg7=`...15`,
           gsi_subset_prob7=`...16`) %>%
    mutate(whatman_cell = substring(MGL_identifier, 30, 33),
           DOY_closed = substring(MGL_identifier, 25, 27),
           dna_data_source = "Nautley_Batch2(19)_2020-01-27.xlsx",
           dna_select_bin = 2) %>%
    mutate_at(vars(c(gsi_subset_prob1, gsi_subset_reg2, DOY_closed)), as.numeric)
  
MGLb2.19 <- left_join(MGLb2.19, 
  ind.smolts.19 %>% 
    filter(DOY_closed%in%MGLb2.19$DOY_closed & whatman_cell%in%MGLb2.19$whatman_cell ) %>% 
    select(DOY_closed, whatman_uid, whatman_cell)) %>%
  print()



# Full DNA selection: batch 1 and 2
MGLfull.19 <- MGLfull.19.raw %>% 
    filter(Fish != "19.000299999999999") %>% 
    rename(MGL_identifier = Fish,
           MGL_comments = Comment,
           gsi_full_reg1 = `Region 1`,
           gsi_full_prob1 = `Prob 1`,
           gsi_full_reg2 = `Region 2`,
           gsi_full_prob2 = `Prob 2`,
           gsi_full_reg3 = `Region 3`,
           gsi_full_prob3 = `Prob 3`,
           gsi_full_reg4 = `Region 4`,
           gsi_full_prob4 = `Prob 4`,
           gsi_full_reg5 = `Region 5`,
           gsi_full_prob5 = `Prob 5`,
           gsi_full_reg6 = `...13`,
           gsi_full_prob6 = `...14`,
           gsi_full_reg7=`...15`,
           gsi_full_prob7=`...16`) %>%
    mutate(whatman_cell = substring(MGL_identifier, 30, 33),
           DOY_closed = substring(MGL_identifier, 25, 27),
           dna_data_source = "NautleyCombined(19)_2020-01-27.xlsx") %>%
    mutate_at(vars(c(gsi_full_prob1, gsi_full_reg2, DOY_closed)), as.numeric)
  
MGLfull.19 <- left_join(MGLfull.19, 
  ind.smolts.19 %>% 
      filter(DOY_closed%in%MGLfull.19$DOY_closed & whatman_cell%in%MGLfull.19$whatman_cell ) %>% 
      select(DOY_closed, whatman_uid, whatman_cell)) %>%
  print()


# NADLEH SCALE DATA FROM PSC ---------------
Nscale.19 <- rbind(
  Nscale.191.raw %>% 
    select(-c(`Scale Number`, `Sample Code`, `Otolith Age`, contains("(pk)"), `Area 2`:`Coll Period`, Species:`DNA Match`, 
              `Project Name`:`Sampled For`, `DNA Coll. Code`:STD, MEF:`Weight (lbs)`, Sex, Set:`Readable?`, `End Date`)) %>%
    rename(PSC_identifier = `Fish Code`,
           scale_condition = Condition,
           age = `Scale Age`,
           site = `Area 1`,
           year = Year,
           PSC_lab_comments = Notes,
           psc_book_no = `Sample ID#1`,
           psc_sample_no = `Sample ID#2`,
           date_opened = `Start Date`) %>%
    mutate(site = case_when(site=="Nautley River"~"Nadleh"),
           psc_book_no = substring(psc_book_no, 6, 8),
           scale_data_source = "2019SnaurA-DNA-V.xlsx") %>%
    mutate_at("date_opened", as.Date) %>%
    mutate_at("year", as.character) %>%
    select(c(PSC_identifier, scale_condition, age, site, year, PSC_lab_comments, psc_book_no, psc_sample_no, date_opened, scale_data_source)),
  
  Nscale.192.raw %>% 
    select(-c(`Scale Number`, `Sample Code`, `Otolith Age`, contains("(pk)"), `Area 2`:`Coll Period`, Species:`DNA Match`, 
              `Project Name`:`Sampled For`, `DNA Coll. Code`:STD, MEF:`Weight (lbs)`, Sex, Set:`Readable?`, `End Date`)) %>%
    rename(PSC_identifier = `Fish Code`,
           scale_condition = Condition,
           age = `Scale Age`,
           site = `Area 1`,
           year = Year,
           PSC_lab_comments = Notes,
           psc_book_no = `Sample ID#1`,
           psc_sample_no = `Sample ID#2`,
           date_opened = `Start Date`) %>%
    mutate(site = case_when(site=="Nautley River"~"Nadleh"),
           psc_book_no = substring(psc_book_no, 6, 8),
           scale_data_source = "2019SnaurA1-DNA-V.xlsx") %>%
    mutate_at("date_opened", as.Date) %>%
    mutate_at("year", as.character) %>%
    select(c(PSC_identifier, scale_condition, age, site, year, PSC_lab_comments, psc_book_no, psc_sample_no, date_opened, scale_data_source))) %>%
  mutate_at("psc_book_no", as.numeric) %>%
  print()



# Join the MGL files (pseudo Batch 1, Batch 2 subset, and the full combination analysis) 
dna.19 <- full_join(MGLb1.19, MGLb2.19) %>%
  full_join(., MGLfull.19, by=c("whatman_cell", "whatman_uid", "DOY_closed", "MGL_comments", "MGL_identifier")) %>%
  mutate(dna_data_source.y = case_when(is.na(dna_data_source.y) ~ "not included in combination run due to sample processing issue", 
                                       TRUE ~ as.character(dna_data_source.y))) %>%
  unite("dna_data_sources", dna_data_source.x, dna_data_source.y, sep=", ", remove = T) %>%
  select(gsi_subset_reg1:gsi_subset_prob2, gsi_subset_reg3:gsi_subset_prob7, gsi_full_reg1:gsi_full_prob7,
         MGL_identifier, whatman_cell, whatman_uid, DOY_closed, MGL_comments, dna_select_bin, dna_data_sources) %>%
  print()


# Join 2019 GSI results + field biosampling data ---------------
bio.19 <- left_join(ind.smolts.19 %>% select(-c(lab_identifier, region1, prob1, region2, prob2, dna_comment)), 
                    dna.19) %>% 
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  rename(PSC_uid=psc_uid) %>% 
  mutate(across(gsi_subset_reg1:gsi_subset_prob2, ~ ifelse(grepl("missing Loci", MGL_comments), NA ,. )),
         year="2019",
         site="Nadleh",
         samplers="BB,AR,AK/JN") %>%
  left_join(., Nscale.19) %>%
  select(-c(ufid, psc_book_no, psc_sample_no)) %>%
  select(year, site, date_opened, date_closed, date_group, DOY_closed, trap_type, PSC_uid, whatman_uid, whatman_cell, ewatch_uid, samplers,
         length_mm, length_class, weight_g, age, scale_condition,
         gsi_subset_reg1:gsi_subset_prob2, gsi_subset_reg3:gsi_subset_prob7, gsi_full_reg1:gsi_full_prob7,
         scales_select_bin, dna_select_bin, MGL_identifier, PSC_identifier,
         comments, MGL_comments, PSC_lab_comments, smolt_data_source, dna_data_sources, scale_data_source) %>%
  print()



# =============== 2021 NADLEH CLEAN/ORGANIZE ===============

# NADLEH DNA ANALYSIS RESULTS FROM MGL: BATCH 1 ---------------
NMGLb1.21 <- NMGLb1.21.raw %>% 
  filter(Fish != "114-135") %>%
  select(-contains("Stock")) %>%
  rename(MGL_identifier = Fish,
         MGL_comments = Comment,
         gsi_subset_reg1 = `Region 1`,
         gsi_subset_prob1 = `Prob 1`,
         gsi_subset_reg2 = `Region 2`,
         gsi_subset_prob2 = `Prob 2`,
         gsi_subset_reg3 = `Region 3`,
         gsi_subset_prob3 = `Prob 3`,
         gsi_subset_reg4 = `Region 4`,
         gsi_subset_prob4 = `Prob 4`,
         gsi_subset_reg5 = `Region 5`,
         gsi_subset_prob5 = `Prob 5`) %>%
  mutate(whatman_cell = str_sub(MGL_identifier,30,33),
         dna_data_sources="kokaneee_sockeyeNadlehSm(21)_2021-10-12.xlsx",
         dna_select_bin = 1,
         DOY_opened = as.numeric(substring(MGL_identifier, 25, 27))) %>% 
  mutate_at(vars(gsi_subset_reg1, gsi_subset_prob1), as.numeric) %>%
  print()

# NADLEH DNA ANALYSIS RESULTS FROM MGL: BATCH 2 ---------------
NMGLb2.21 <- NMGLb2.21.raw %>% 
  filter(LineInfo != "Sample: 1") %>%
  select(-contains("Stock"), -c(rowID, iSample, Collection, Gear)) %>%
  rename(MGL_identifier = LineInfo,
         MGL_comments = Comment,
         gsi_subset_reg1 = `Region_1`,
         gsi_subset_prob1 = `Prob_1`,
         gsi_subset_reg2 = `Region_2`,
         gsi_subset_prob2 = `Prob_2`,
         gsi_subset_reg3 = `Region_3`,
         gsi_subset_prob3 = `Prob_3`,
         gsi_subset_reg4 = `Region_4`,
         gsi_subset_prob4 = `Prob_4`,
         gsi_subset_reg5 = `Region_5`,
         gsi_subset_prob5 = `Prob_5`,
         DOY_opened = JD,
         whatman_cell = Fish) %>%
  mutate(dna_data_sources="kokaneee_sockeyePID20210042_NAUTLEY_R_2021_SMOLTS_2022-04-13.xlsx",
         dna_select_bin = 2) %>% 
  mutate_at("gsi_subset_reg1", as.numeric) %>%
  mutate_at("whatman_cell", as.character) %>%
  print()

# NADLEH DNA ANALYSIS RESULTS FROM MGL: COMBO ---------------
NMGLfull.21 <- NMGLfull.21.raw %>% 
  filter(LineInfo != "Sample: 1") %>%
  select(-contains("Stock"), -c(rowID, iSample, Collection, Gear)) %>%
  rename(MGL_identifier = LineInfo,
         MGL_comments = Comment,
         gsi_full_reg1 = `Region_1`,
         gsi_full_prob1 = `Prob_1`,
         gsi_full_reg2 = `Region_2`,
         gsi_full_prob2 = `Prob_2`,
         gsi_full_reg3 = `Region_3`,
         gsi_full_prob3 = `Prob_3`,
         gsi_full_reg4 = `Region_4`,
         gsi_full_prob4 = `Prob_4`,
         gsi_full_reg5 = `Region_5`,
         gsi_full_prob5 = `Prob_5`,
         DOY_opened = JD,
         whatman_cell = Fish) %>%
  mutate(dna_data_sources="kokaneee_sockeyePID20210042_NADLEH_NAUTLEY_2021_SMOLTS_2022-04-13.xlsx") %>% 
  mutate_at("gsi_full_reg1", as.numeric) %>%
  mutate_at("whatman_cell", as.character) %>%
  print()

# Join GSI together ---------------
dna.21 <- full_join(NMGLb1.21, NMGLb2.21) %>%
  left_join(., NMGLfull.21, by=c("MGL_identifier", "MGL_comments", "whatman_cell", "DOY_opened")) %>%
  unite("dna_data_sources", dna_data_sources.x, dna_data_sources.y, sep=", ", remove = T) %>%
  print()


# NADLEH SCALE DATA FROM PSC 2021 ---------------
Nscale.21 <- Nscale.21.raw %>% 
  select(-c(`Scale Number`, `Sample Code`, `Otolith Age`, contains("(pk)"), `Area 2`:`Coll Period`, Species:`DNA Match`, 
            `Project Name`:`Sampled For`, `DNA Coll. Code`:STD, MEF:`Weight (lbs)`, Sex, Set:`Readable?`, `End Date`)) %>%
  rename(PSC_identifier = `Fish Code`,
         scale_condition = Condition,
         age = `Scale Age`,
         site = `Area 1`,
         year = Year,
         PSC_lab_comments = Notes,
         PSC_book = `Sample ID#1`,
         PSC_cell = `Sample ID#2`,
         date_opened = `Start Date`) %>%
  mutate(site = case_when(site=="Nautley River"~"Nadleh"),
         PSC_book = substring(PSC_book, 6, 8),
         scale_data_source = "2021SnaurA1-DNA-V.xlsx") %>%
  mutate_at("date_opened", as.Date) %>%
  mutate_at("year", as.character) %>%
  select(c(PSC_identifier, scale_condition, age, site, year, PSC_lab_comments, PSC_book, PSC_cell, date_opened, scale_data_source)) %>%
  mutate(age = ifelse(scale_condition==0 & age==0, NA, age),
         PSC_lab_comments = "Not read as not required for further analysis (KD)") %>%
  print()


# NADLEH FIELD BIOSAMPLING + DNA + SCALE DATA JOIN ---------------
Nbio.21 <- left_join(
  Nbio.21.raw %>%
    rename(ewatch_uid = ewatch_id) %>%
    mutate_at(vars(length_mm:weight_g), as.numeric) %>%
    mutate(trap_type = ifelse(trap_type=="8'", "8' RST", trap_type),
           year = "2021",
           DOY_closed = lubridate::yday(date_closed),
           DOY_opened = lubridate::yday(date_opened)),  
  dna.21) %>%
  left_join(., 
            Nscale.21) %>%
  mutate_at("ewatch_uid", as.character) %>%
  print()

# Note PSC sample N-A-89 didn't join as some samples were confused and not recorded properly. So 1 entry will seem to be missing in the
# exported database but this is OK. 




# =============== 2021 STELLAKO CLEAN/ORGANIZE ===============

# STELLA DNA ANALYSIS RESULTS FROM MGL ---------------
SMGL.21 <- SMGL.21.raw %>% 
  select(-contains("Stock")) %>%  
  filter(Fish != "101-141") %>%
  rename(MGL_identifier = Fish,
         MGL_comments = Comment,
         gsi_full_reg1 = `Region 1`,
         gsi_full_prob1 = `Prob 1`,
         gsi_full_reg2 = `Region 2`,
         gsi_full_prob2 = `Prob 2`,
         gsi_full_reg3 = `Region 3`,
         gsi_full_prob3 = `Prob 3`,
         gsi_full_reg4 = `Region 4`,
         gsi_full_prob4 = `Prob 4`,
         gsi_full_reg5 = `Region 5`,
         gsi_full_prob5 = `Prob 5`) %>%
  filter(MGL_identifier != "101-141") %>%
  mutate(whatman_cell = str_sub(MGL_identifier,30,34),
         whatman_cell = ifelse(str_length(whatman_cell)==5, paste0(0, whatman_cell), whatman_cell),
         dna_data_sources="kokaneee_sockeyeStellakoSm(21)_2021-10-12.xlsx",
         dna_select_bin = 1,
         DOY_opened = as.numeric(substring(MGL_identifier, 25, 27))) %>% 
  mutate_at(vars(gsi_full_reg1, gsi_full_prob1), as.numeric) %>%
  print()


# STELLAKO SCALE DATA FROM PSC 2021 ---------------
Sscale.21 <- Sscale.21.raw %>% 
  select(-c(`Scale Number`, `Sample Code`, `Otolith Age`, contains("(pk)"), `Area 2`:`Coll Period`, Species:`DNA Match`, 
            `Project Name`:`Sampled For`, `DNA Coll. Code`:STD, MEF:`Weight (lbs)`, Sex, Set:`Readable?`, `End Date`)) %>%
  rename(PSC_identifier = `Fish Code`,
         scale_condition = Condition,
         age = `Scale Age`,
         site = `Area 1`,
         year = Year,
         PSC_lab_comments = Notes,
         PSC_book = `Sample ID#1`,
         PSC_cell = `Sample ID#2`,
         date_opened = `Start Date`) %>%
  mutate(site = case_when(site=="Stellako River"~"Stellako"),
         PSC_book = substring(PSC_book, 6, 8),
         scale_data_source = "2021SsterA-DNA-V.xlsx") %>%
  mutate_at("date_opened", as.Date) %>%
  mutate_at("year", as.character) %>%
  select(c(PSC_identifier, scale_condition, age, site, year, PSC_lab_comments, PSC_book, PSC_cell, date_opened, scale_data_source)) %>%
  print()


# STELLA FIELD BIOSAMPLING DATA ---------------
Sbio.21 <- Sbio.21.raw %>%
  rename(ewatch_uid=ewatch_id) %>%
  mutate_at(vars(length_mm:weight_g), as.numeric) %>%
  mutate(trap_type = ifelse(trap_type=="6'", "6' RST", trap_type),
         year="2021",
         ufid = paste("2019", seq(1:nrow(Sbio.21.raw)), sep="-"),
         DOY_closed = lubridate::yday(date_closed)) %>% 
  print()


# Join 2021 Stellako GSI results + field biosampling data ---------------
Sbio.21 <- left_join(Sbio.21, SMGL.21) %>% 
  mutate_at("PSC_cell", as.numeric) %>%
  left_join(., Sscale.21) %>%
  print()

# stellako will be missing  scale samples 2021SsterA0155 because of issues where scales were put inside the envelopes.


# =============== JOIN 2019 + 2021 ===============

# Create vector of IDs for samples slated for priority scale analysis from the PSC for quick creation of a 2021 scales_select_bin variable.
# Note this was made in dna_pulls_2021.R file. 
PSC_scale_priorities_2021 <- read.csv("northern_smolt_scale_PRIORITIES_june2021.csv")$PSC_uid

bio.join <- full_join(Nbio.21, bio.19) %>%
  bind_rows(., Sbio.21) %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  mutate(species = ifelse(grepl("hinook", comments), "Chinook",
                      ifelse(scale_condition==6, "Non-sockeye FLAG", "Sockeye")),
         PSC_uid = ifelse(PSC_uid=="NA-NA", NA, PSC_uid),
         whatman_uid = ifelse(whatman_uid=="NA-NA", NA, whatman_uid),
         length_class = ifelse(length_mm<80, "<80", 
                               ifelse(length_mm>=80 & length_mm <= 89, "80-89",
                                      ifelse(length_mm>=90 & length_mm <= 99, "90-99",
                                             ifelse(length_mm>=100 & length_mm <= 109, "100-109",
                                                    ifelse(length_mm>=110 & length_mm <= 119, "110-119",
                                                           ifelse(length_mm>=120 & length_mm <= 130, "120-130", ">130")))))),
         age = ifelse(scale_condition%in%c(5,7,8,9), NA,
                 ifelse(scale_condition==10, "FLAG", age)),
         PSC_lab_comments = ifelse(is.na(age) & scale_condition!=1, 
                                   "Original age entered as 0 by PSC, changed to NA to avoid age confusion (KD May 2022).", PSC_lab_comments)) %>%
  mutate(scales_select_bin = ifelse(year=="2021" & PSC_uid%in%PSC_scale_priorities_2021, "1", 
                              ifelse(year=="2021" & !PSC_uid%in%PSC_scale_priorities_2021, "2", scales_select_bin)),
         ufidsite = case_when(site=="Nadleh"~"N", site=="Stellako"~"S")) %>%
  group_by(year) %>%
  mutate(ufid = paste(year, ufidsite, row_number(), sep="-")) %>%
  
  select(year, site, species, trap_type, date_opened, date_closed, DOY_opened, DOY_closed, date_group, time_trap_closed, samplers, 
         ufid, PSC_book, PSC_cell, PSC_uid, whatman_sheet, whatman_cell, whatman_uid, ewatch_uid,
         length_mm, length_class, weight_g, age, scale_condition,
         gsi_subset_reg1:gsi_subset_prob5, gsi_subset_reg6:gsi_subset_prob7, gsi_full_reg1:gsi_full_prob5, gsi_full_reg6:gsi_full_prob7,
         dna_select_bin, scales_select_bin, MGL_identifier, PSC_identifier,
         comments, PSC_lab_comments, MGL_comments, smolt_data_source, dna_data_sources, scale_data_source) %>%
  mutate(across(where(is.character), ~na_if(., "NA"))) %>% 
  arrange(year, date_opened) %>%
  print()




##########################################################################################################################################


#                                                             METADATA SHEET


#--------- ENVIRONMENTAL METADATA
enviro.metadata <- tibble(column = names(enviro.join)) %>%
  mutate(units = case_when(column%in%c("site", "location", "debris_type1", "debris_type2", "crew_initials", "comments", "year", "trap_type")~"-",
                           grepl("date", column)~"yyyy-mm-dd", grepl("_C", column)~"celsius", column=="time"~"hh:mm", 
                           column=="rst_rpms"~"rotations per minute", column=="debris_load"~"high/med/low", column=="water_gauge_m"~"meters"),
         description = case_when(column=="site"~"Nautley River (Nadleh Koh) or Stellako River (Stella)",
                                 column=="date_opened"~"Date trap (RST or fyke net) was opened for each fishing interval. Usually spans one night, although some dates in 2019 will span days and/or day+night",
                                 column=="date_closed"~"Date trap (RST or fyke net) was closed for each fishing interval. Usually spans one night, although some dates in 2019 will span days and/or day+night",
                                 column=="date_true"~"True date that corresponds to the time when the measurement was taken",
                                 column=="time"~"Time the environmental measurement(s) taken in 24-hr format where midnight is 00:00",
                                 column=="location"~"Brief location describing where the trap is in the river (e.g., 'river right, off-center of thalweg')",
                                 column=="rst_rpms"~"Rotations per minute for the RST",
                                 column=="debris_load"~"Qualitative estimate of the relative amount of debris in the RST each night",
                                 column=="debris_type1"~"Main type of debris in the RST each night",
                                 column=="debris_type2"~"Secondary type of debris in the RST each night",
                                 column=="water_gauge_m"~"Water height obtained from the real-time Environment Canada hydrological data. These data have NOT been updated post-season with verified EC data",
                                 column=="air_temp_C"~"Air temperature at time of measurement",
                                 column=="water_temp_C"~'Water temperature at time of measurement',
                                 column=="crew_initials"~"Nightly crew initials",
                                 column=="comments"~"Misc comments about conditions, sampling, observations, etc. each night",
                                 column=="year"~"Year of project",
                                 column=="trap_type"~"Rotary screw trap (RST), either 6' or 8' in diameter, or a fyke net")) %>%
  add_row(column="ENVIRONMENTAL METADATA", units=" ", description=" ", .before=1) %>%
  add_row() %>%
  add_row() %>%
  print()


#--------- NIGHTLY CATCH METADATA
catch.metadata <- data.frame(column = names(catch.join)) %>%
  mutate(units = case_when(column%in%c("year", "site", "trap_type", "location", "crew", "markers", "mark_type", "other_bycatch", "comments")~"-",
                    grepl("total", column)~"# of fish", grepl("caught", column)~"# of fish", grepl("date", column)~"yyyy-mm-dd", 
                    grepl("time", column)~"hh:mm", grepl("datetime", column)~"yyyy-mm-dd hh:mm", grepl("n_", column)~"# of fish"),
         description = case_when(column=="year"~"Year of project. Note historical data (1999-2000) have minimal information.",
                                 column=="site"~"Nautley River (Nadleh Koh) or Stellako River (Stella Koh)",
                                 column=="trap_type"~"Rotary screw trap (RST), either 6' or 8' in diameter, or a fyke net.",
                                 column=="location"~"Brief location describing where the trap is in the river (e.g., 'river right, off-center of thalweg')",
                                 column=="date_opened"~"Date trap (RST or fyke net) was opened for each fishing interval/group, where nights with hourly checks are recorded as one open date and one close date (i.e., open date does not necessarily line up to open time). Usually spans one night, although some dates in 2019 will span days and/or day+night",
                                 column=="date_closed"~"Date trap (RST or fyke net) was closed for each fishing interval/group, where nights with hourly checks are recorded as one open date and one close date (i.e., close date does not necessarily line up to close time). Usually spans one night, although some dates in 2019 will span days and/or day+night",
                                 column=="time_trap_open"~"Time trap (RST or fyke net) was open for each fishing interval, usually about 1-hr intervals between trap checks but may be more. In 24-hr format where midnight is 00:00",
                                 column=="time_trap_closed"~"Time trap (RST or fyke net) was closed for each fishing interval, usually about 1-hr intervals between trap checks but may be more. In 24-hr format where midnight is 00:00",
                                 column=="crew"~"Nightly crew initials", 
                                 column=="markers"~"Crew members applying marks for mark-recapture trials, when appropriate",
                                 column=="mark_type"~"Mark type applied to sockeye smolts during mark-recapture trials (if appropriate). Typically it is upper caudal clip ('upper') at Stellako and lower caudal clip ('lower') at Nautley",
                                 column=="n_marked_released"~"Number of sockeye smolts with marks applied for mark-recapture trials that were successfully released upstream alive",
                                 column=="n_marked_dead"~"Number of sockeye smolts that died during the holding period following the mark application and were not released upstream alive. Note these are different than 'n_unmarked_dead' below",
                                # column=="sox_smolts_caught"~"Number of sockeye smolts caught in 2019 each time period (not consistently recorded or broken out by sampled/not-sampled",
                                 column=="n_unmarked_sampled"~"Number of sockeye smolts from the unmarked population that were biosampled for weight, age and/or DNA (these fish were always unmarked smolts and never recaptures from mark-recapture trials)",
                                 column=="n_unmarked_lf"~"Number of unmarked sockeye smolts measured for length-frequency only (recorded on LF sheet). Recaps were measured for length-frequency (and recorded on LF sheet), but not included in unmarked LF totals.",
                                 column=="n_unmarked_spilled"~"Number of sockeye smolts spilled, either from the trap during transfer, during busy nights, etc. These fish were not sampled or measured in any way, only enumerated",
                                 column=="n_unmarked_dead"~"Number of sockeye smolts that died during any part of the fishing/handling process (this is primarily smolts found dead in the RST livebox or mesh, and not fish that died during the mark application/release process)",
                                 column=="total_unmarked"~"Total number of unmarked sockeye encountered: sum of n_unmarked_sampled+n_unmarked_lf+n_unmarked_spilled+n_unmarked_dead.",
                                 column=="n_recaps_upper_clip"~"Number of sockeye smolts recaptured with the upper caudal fin clipped. Assumed that the number entered here are released alive. Comments indicate if any recaptured smolts die during the recapture process.",
                                 column=="n_recaps_lower_clip"~"Number of sockeye smolts recaptured with the lower caudal fin clipped. Assumed that the number entered here are released alive. Comments indicate if any recaptured smolts die during the recapture process.",
                                 column=="total_recaps"~"Total number of marked sockeye smolts recaptured. Generated using Excel formula upon entry.",
                                 column=="n_chinook_fry"~"Number of Chinook fry encountered",
                                 column=="n_chinook_smolts"~"Number of Chinook smolts encountered. Some were also sampled (entered on the biosample page), but not all",
                                 column=="other_bycatch"~"Number and species of other bycatch encountered",
                                 column=="comments"~"Relevant nightly comments")) %>%
  add_row(column="NIGHTLY CATCH METADATA", units=" ", description=" ", .before=1) %>%
  add_row() %>%
  add_row() %>%
  print()


#--------- LENGTH-FREQUENCY METADATA
lf.metadata <- tibble(column = names(lf.join)) %>%
  mutate(units = case_when(column%in%c("site", "crew", "comments", "year", "trap_type", "data_type")~"-",
                           grepl("date", column)~"yyyy-mm-dd", column=="length_mm"~"millimeters"),
         description = case_when(column=="year"~"Year of project", 
                                 column=="site"~"Nautley River (Nadleh Koh) or Stellako River (Stella)",
                                 column=="date_opened"~"Date trap (RST or fyke net) was opened for each fishing interval/group, where nights with hourly checks are recorded as one open date and one close date (i.e., open date does not necessarily line up to open time). Usually spans one night, although some dates in 2019 will span days and/or day+night",
                                 column=="date_closed"~"Date trap (RST or fyke net) was closed for each fishing interval/group, where nights with hourly checks are recorded as one open date and one close date (i.e., open date does not necessarily line up to open time). Usually spans one night, although some dates in 2019 will span days and/or day+night",
                                 column=="crew"~"Nightly crew initials",
                                 column=="comments"~"Misc comments about conditions, sampling, observations, etc. each night",
                                 column=="trap_type"~"Rotary screw trap (RST), either 6' or 8' in diameter, or a fyke net",
                                 column=="data_type"~"Type of length-frequency data colleced, whether it was part of normal length-frequency data collection ('Routine'), from the marked population ('Application'), or from a recapture ('Recapture', and which site the smolt was marked at)",
                                 column=="length_mm"~"Smolt fork length to nearest mm")) %>%
  add_row(column="LENGTH-FREQUENCY METADATA", units=" ", description=" ", .before=1) %>%
  add_row() %>%
  add_row() %>%
  print()


#--------- BIOSAMPLING METADATA
bio.metadata <- tibble(column = names(bio.join)) %>%
  mutate(units = case_when(column%in%c("year", "site", "species", "trap_type", "location", "samplers", "MGL_identifier", "comments", 
                                       "PSC_lab_comments", "MGL_comments", "dna_data_sources", "smolt_data_source", "whatman_cell",
                                       "ewatch_uid", "PSC_identifier", "scale_data_source", "PSC_book", "PSC_cell", "whatman_sheet") ~ "-",
                           grepl("date", column)~"yyyy-mm-dd", grepl("time", column)~"hh:mm", grepl("reg", column)~"-", 
                           grepl("prob", column)~"proportion (*100 for %)", grepl("length", column)~"millimeters", grepl("bin", column)~"-",
                           column=="PSC_uid"~"book#-cell#", column=="whatman_uid"~"sheet#-cell#", column=="ufid"~"year-site-fish#",
                           column=="weight_g"~"grams", column=="age"~"year of life", column=="DOY_closed"~"day of year", 
                           column=="DOY_opened"~"day of year",
                           column=="scale_condition"~"arbitrary categorical number"),
         description = case_when(column=="year"~"Year of project", 
                                 column=="site"~"Nautley River (Nadleh Koh) or Stellako River (Stella)",
                                 column=="species"~"Species of smolt, Sockeye or Chinook",
                                 column=="trap_type"~"Rotary screw trap (RST), either 6' or 8' in diameter, or a fyke net",
                                 column=="samplers"~"Initials of nightly sampling crew",
                                 column=="date_opened"~"Date trap (RST or fyke net) was opened for each fishing interval/group, where nights with hourly checks are recorded as one open date and one close date (i.e., open date does not necessarily line up to open time). Usually spans one night, although some dates in 2019 will span days and/or day+night",
                                 column=="date_closed"~"Date trap (RST or fyke net) was closed for each fishing interval/group, where nights with hourly checks are recorded as one open date and one close date (i.e., open date does not necessarily line up to open time). Usually spans one night, although some dates in 2019 will span days and/or day+night",
                                 column=="DOY_closed"~"Day of year version of date_closed. Note calendar dates are not the same across years due to leap years (365 vs 366 days).",
                                 column=="DOY_opened"~"Day of year version of date_opened. Note calendar dates are not the same across years due to leap years (365 vs 366 days).",
                                 column=="date_group"~"Date grouping from 2019 data. Not sure what this was for - perhaps GSI or smooth plotting. Retained in case important for replication of results at some point",
                                 column=="time_trap_closed"~"Time trap (RST or fyke net) was checked/closed for each fishing interval in 24-hr format where midnight is 00:00. References the time when smolts were netted out of the trap and brought to shore to begin sampling.",
                                 column=="ufid"~"Unique fish ID for use across space/time (PSC/Whatman IDs not guaranteed to be unique over time). Concatenation of year-site abbreviation-unique number within year-site; site abbreviation N=Nadleh, S=Stellako. Fish are uniquely numbered within year-site.",
                                 column=="PSC_book"~"Pacific Salmon Commission (PSC) book ID for scale samples. First term in PSC_uid. Books were identified in 2021 starting with 'N' for Nadleh and 'S' for Stellako (e.g., N-A is Nadleh book A)",
                                 column=="PSC_cell"~"Pacific Salmon Commission (PSC) cell number within a book for scale samples. Second term in PSC_uid.",
                                 column=="PSC_uid"~"Pacific Salmon Commission (PSC) ID for scale samples, concatenation of PSC_book and PSC_cell. Unique ID per year BUT NOT GUARANTEED TO BE UNIQUE OVER CONSEQUTIVE PROGRAM YEARS. Concatenation of PSC book number-cell number",
                                 column=="whatman_sheet"~"Whatman sheet number. First term in whatman_uid.",
                                 column=="whatman_cell"~"Whatman cell within each Whatman sheet. Used to join GSI data to bio data. Second term in whatman_uid.",
                                 column=="whatman_uid"~"Whatman ID for DNA samples, concatenation of whatman_book and whatman_cell. Unique ID per year BUT NOT GUARANTEED TO BE UNIQUE OVER CONSEQUTIVE PROGRAM YEARS. Concatenation of Whatman sheet number-cell number",
                                 column=="ewatch_uid"~"Field IDs given to fish collected for EWatch. Not necessarily the true EWatch ID",
                                 column=="length_mm"~"Smolt fork length to nearest mm",
                                 column=="length_class"~"Smolt fork length classes in 10-mm increments. In 2019 this informed DNA and weight sampling; in 2021 this only informed weight sampling",
                                 column=="weight_g"~"Smolt wet weight in grams",
                                 column=="age"~"Smolt age obtained from scale samples, where age is typically 0, 1 or 2, representing age-0 (new fry), age-1 (smolt that completed 1 year of life and entering 2nd year), and age-2 (smolt completed 2 years of life and entering 3rd year). HOWEVER, note that scales with poor condition also recieve a 0 for age - this is not true age-0. The original scale lab file from 2019 is not available and was copy-pasted in, so 2019 age-0s might be wrong.",
                                 column=="scale_condition"~"Scale condition from PSC lab. Condition 1=good to read. Conditions 7-9 indicate uncertainty (and have age 0 as a result - not true age-0).",
                                 grepl("gsi_subset_reg", column)~"Batch subset genetic stock ID (GSI) regional assignments, where region 1 is the most probable and subsequent regions are the least probable. Samples were run in 2 batches, the 1st batch was priority and 2nd batch was follow-up. GSI results can change depending on the mixture of samples included in the analysis, so samples were run as just batch 1, batch 1+2 combined, and just batch 2, to assess the extent of stock-switching (i.e., reliability of stock ID). See 'dna_select_bin' or 'MGL_identifier' to see what batch samples were part of.",
                                 grepl("gsi_subset_prob", column)~"Batch subset Genetic stock ID (GSI) probability associated with regional assignments, where prob1 is the most probable and subsequent probabilities decrease. Samples were run in 2 batches, the 1st batch was priority and 2nd batch was follow-up. GSI results can change depending on the mixture of samples included in the analysis, so samples were run as just batch 1, batch 1+2 combined, and just batch 2, to assess the extent of stock-switching (i.e., reliability of stock ID). See 'dna_select_bin' or 'MGL_identifier' to see what batch samples were part of.",
                                 grepl("gsi_full_reg", column)~"Batch 1+2 full genetic stock ID (GSI) regional assignments, where region 1 is the most probable and subsequent regions are the least probable. Samples were run in 2 batches, the 1st batch was priority and 2nd batch was follow-up. GSI results can change depending on the mixture of samples included in the analysis, so samples were run as just batch 1, batch 1+2 combined, and just batch 2, to assess the extent of stock-switching (i.e., reliability of stock ID). See 'dna_select_bin' or 'MGL_identifier' to see what batch samples were part of.",
                                 grepl("gsi_full_prob", column)~"Batch 1+2 full genetic stock ID (GSI) probability associated with regional assignments, where prob1 is the most probable and subsequent probabilities decrease. Samples were run in 2 batches, the 1st batch was priority and 2nd batch was follow-up. GSI results can change depending on the mixture of samples included in the analysis, so samples were run as just batch 1, batch 1+2 combined, and just batch 2, to assess the extent of stock-switching (i.e., reliability of stock ID). See 'dna_select_bin' or 'MGL_identifier' to see what batch samples were part of.",
                                 column=="dna_select_bin"~"Categorical code for samples selected for DNA analysis, where 0=not sent, 1=first batch, 2=second batch",
                                 column=="scales_select_bin"~"Categorical code for samples selected for scale analysis, where 0=not sent, 1=first batch/priority, 2=second batch (if applicable)",
                                 column=="MGL_identifier"~"Molecular Genetics Lab (MGL) unique identifier. Needed for reference to GSI files from MGL. Composed of: BatchID(year)  trap_type  day_of_year  whatman_cell",
                                 column=="PSC_identifier"~"PSC scale lab unique identifier. Needed for reference to PSC lab files. Composed of: year    river name abbreviation     consecutive number",
                                 column=="MGL_comments"~"Comments from the MGL from GSI analysis process",
                                 column=="PSC_lab_comments"~"Comments from the PSC lab from scale analysis (Dejan Brkic in 2019)",
                                 column=="comments"~"Misc comments about conditions, sampling, observations, etc. each night",
                                 column=="smolt_data_source"~"Record for 2019 data compilation as many files existed over time",
                                 column=="dna_data_sources"~"Record of GSI analysis file name(s) returned from MGL",
                                 column=="scale_data_source"~"Record of scale analysis file names returned from PSC"
                                 )) %>%
  add_row(column="BIOSAMPLING METADATA", units=" ", description=" ", .before=1) %>%
  print()


#--------- JOIN
metadata <- rbind(enviro.metadata, catch.metadata, lf.metadata, bio.metadata) %>%
  add_row() %>%
  add_row() %>%
  add_row(column = "last export:", units=as.character(Sys.time()), description=" ") 

 

#--------- MGL and PSC code sheets
MGL_region_codes <- tibble(region_code=seq(1:19)) %>% 
  mutate(population_ID = case_when(region_code=="1"~"Early Stuart", region_code=="2"~"Chilliwack", 
                                   region_code=="3"~"Pitt/Alouette/Coquitlam", region_code=="4"~"Nadina/Bowen (suspect typo; Bowron)",
                                   region_code=="5"~"Gates/Nahatlatch", region_code=="6"~"Taseko", region_code=="7"~"North Barriere",
                                   region_code=="8"~"Early South Thompson", region_code=="9"~"Harrison", region_code=="10"~"Widgeon Slough",
                                   region_code=="11"~"Late Stuart", region_code=="12"~"Stellako", region_code=="13"~"Chilko",
                                   region_code=="14"~"Horsefly", region_code=="15"~"Mitchell & other non-Horsefly Quesnel",
                                   region_code=="16"~"Raft/North Thompson", region_code=="17"~"Birkenhead/Big Silver",
                                   region_code=="18"~"Lower Shuswap/Portage", region_code=="19"~"Weaver/Cultus")) %>% 
  print()


PSC_scale_codes <- read_excel("PSC_scale_condition_codes_12-May-2022_forR.xlsx", sheet="Condition Codes") 

##########################################################################################################################################

#                                                       EXPORT TO EXCEL

northern_wb <- createWorkbook()

# create empty sheets
addWorksheet(northern_wb, "metadata")
addWorksheet(northern_wb, "environmental")
addWorksheet(northern_wb, "nightly_catch")
addWorksheet(northern_wb, "length_frequency")
addWorksheet(northern_wb, "biosampling")
addWorksheet(northern_wb, "MGL_GSI_codes")
addWorksheet(northern_wb, "PSC_scale_condition_codes")

# write data to sheets (some with formatting)
writeData(northern_wb, sheet="metadata", x=metadata)
conditionalFormatting(northern_wb, "metadata", cols=1:3, 
                      rows=2, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(northern_wb, "metadata", cols=1:3, 
                      rows=21, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(northern_wb, "metadata", cols=1:3, 
                      rows=49, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(northern_wb, "metadata", cols=1:3, 
                      rows=61, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(northern_wb, "metadata", cols = 1:ncol(metadata), 
                      rows=123, style=createStyle(bgFill="#00e5ff"), type="contains", rule="o")
conditionalFormatting(northern_wb, "metadata", cols=1:ncol(metadata), 
                      rows=37, style=createStyle(bgFill="gray80"), type="contains", rule="o")
conditionalFormatting(northern_wb, "metadata", cols = 1:ncol(metadata), 
                      rows=42, style=createStyle(bgFill="gray80"), type="contains", rule="o")


writeData(northern_wb, sheet="environmental", x=enviro.join)

writeData(northern_wb, sheet="nightly_catch", x=catch.join)
conditionalFormatting(northern_wb, "nightly_catch", cols=16, 
                      rows=0:nrow(catch.join)+1, style=createStyle(bgFill="gray80"), type="expression", rule=">=0")
conditionalFormatting(northern_wb, "nightly_catch", cols=21, 
                      rows=0:nrow(catch.join)+1, style=createStyle(bgFill="gray80"), type="expression", rule=">=0")

writeData(northern_wb, sheet="length_frequency", x=lf.join)

writeData(northern_wb, sheet="biosampling", x=bio.join)

writeData(northern_wb, sheet="MGL_GSI_codes", x=MGL_region_codes)

writeData(northern_wb, sheet="PSC_scale_condition_codes", x=PSC_scale_codes)

# SAVE IT ALLLLLL!
saveWorkbook(northern_wb, "Northern_smolt_database_99-00_19-21.xlsx", overwrite = T)



##########################################################################################################################################
##########################################################################################################################################

#                                                     DATA SHARING


db.meta <- read_excel("Northern_smolt_database_99-00_19-21.xlsx",sheet="metadata")
db.env <- read_excel("Northern_smolt_database_99-00_19-21.xlsx",sheet="environmental") 
db.catch <- read_excel("Northern_smolt_database_99-00_19-21.xlsx",sheet="nightly_catch") 
db.lf <- read_excel("Northern_smolt_database_99-00_19-21.xlsx",sheet="length_frequency") 
db.bio <- read_excel("Northern_smolt_database_99-00_19-21.xlsx",sheet="biosampling") 
db.mgl <- read_excel("Northern_smolt_database_99-00_19-21.xlsx",sheet="MGL_GSI_codes")
db.psc <- read_excel("Northern_smolt_database_99-00_19-21.xlsx",sheet="PSC_scale_condition_codes")


# ================ Database to share with UFFCA and Nadleh Whut'en ================
db.env.uffca <- db.env %>%
  filter(site=="Nadleh")
db.catch.uffca <- db.catch %>%
  filter(year%in%c(2019,2021), site=="Nadleh")
db.lf.uffca <- db.lf %>%
  filter(site=="Nadleh")
db.bio.uffca <- db.bio %>%
  filter(site=="Nadleh")

# WB ----------------------
nadleh_uffca_wb <- createWorkbook()

# create empty sheets
addWorksheet(nadleh_uffca_wb, "metadata")
addWorksheet(nadleh_uffca_wb, "environmental")
addWorksheet(nadleh_uffca_wb, "nightly_catch")
addWorksheet(nadleh_uffca_wb, "length_frequency")
addWorksheet(nadleh_uffca_wb, "biosampling")
addWorksheet(nadleh_uffca_wb, "MGL_GSI_codes")
addWorksheet(nadleh_uffca_wb, "PSC_scale_condition_codes")

# write data to sheets (some with formatting) -----------------
# METADATA
writeData(nadleh_uffca_wb, sheet="metadata", x=db.meta)
conditionalFormatting(nadleh_uffca_wb, "metadata", cols=1:3, 
                      rows=2, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(nadleh_uffca_wb, "metadata", cols=1:3, 
                      rows=21, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(nadleh_uffca_wb, "metadata", cols=1:3, 
                      rows=49, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(nadleh_uffca_wb, "metadata", cols=1:3, 
                      rows=61, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(nadleh_uffca_wb, "metadata", cols = 1:ncol(db.meta), 
                      rows=123, style=createStyle(bgFill="#00e5ff"), type="contains", rule="o")
conditionalFormatting(nadleh_uffca_wb, "metadata", cols=1:ncol(db.meta), 
                      rows=37, style=createStyle(bgFill="gray80"), type="contains", rule="o")
conditionalFormatting(nadleh_uffca_wb, "metadata", cols = 1:ncol(db.meta), 
                      rows=42, style=createStyle(bgFill="gray80"), type="contains", rule="o")

# ENVIRO
writeData(nadleh_uffca_wb, sheet="environmental", x=db.env.uffca)

# NIGHTLY CATCH
writeData(nadleh_uffca_wb, sheet="nightly_catch", x=db.catch.uffca)
conditionalFormatting(nadleh_uffca_wb, "nightly_catch", cols=16, 
                      rows=0:nrow(db.catch.uffca)+1, style=createStyle(bgFill="gray80"), type="expression", rule=">=0")
conditionalFormatting(nadleh_uffca_wb, "nightly_catch", cols=21, 
                      rows=0:nrow(db.catch.uffca)+1, style=createStyle(bgFill="gray80"), type="expression", rule=">=0")

# L-F
writeData(nadleh_uffca_wb, sheet="length_frequency", x=db.lf.uffca)

# BIOSAMPLING
writeData(nadleh_uffca_wb, sheet="biosampling", x=db.bio.uffca)

# CODE SHEETS
writeData(nadleh_uffca_wb, sheet="MGL_GSI_codes", x=db.mgl)
writeData(nadleh_uffca_wb, sheet="PSC_scale_condition_codes", x=db.psc)

# SAVE IT ALLLLLL! ---------------------------
saveWorkbook(nadleh_uffca_wb, "Nadleh_smolt_database_19-21.xlsx", overwrite = T)




# ================ Database to share with CSTC ================
db.env.cstc <- db.env %>%
  filter(site=="Stellako")
db.catch.cstc <- db.catch %>%
  filter(site=="Stellako")
db.lf.cstc <- db.lf %>%
  filter(site=="Stellako")
db.bio.cstc <- db.bio %>%
  filter(site=="Stellako")

# WB ----------------------
stellako_cstc_wb <- createWorkbook()

# create empty sheets
addWorksheet(stellako_cstc_wb, "metadata")
addWorksheet(stellako_cstc_wb, "environmental")
addWorksheet(stellako_cstc_wb, "nightly_catch")
addWorksheet(stellako_cstc_wb, "length_frequency")
addWorksheet(stellako_cstc_wb, "biosampling")
addWorksheet(stellako_cstc_wb, "MGL_GSI_codes")
addWorksheet(stellako_cstc_wb, "PSC_scale_condition_codes")

# write data to sheets (some with formatting) -----------------
# METADATA
writeData(stellako_cstc_wb, sheet="metadata", x=db.meta)
conditionalFormatting(stellako_cstc_wb, "metadata", cols=1:3, 
                      rows=2, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(stellako_cstc_wb, "metadata", cols=1:3, 
                      rows=21, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(stellako_cstc_wb, "metadata", cols=1:3, 
                      rows=49, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(stellako_cstc_wb, "metadata", cols=1:3, 
                      rows=61, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(stellako_cstc_wb, "metadata", cols = 1:ncol(db.meta), 
                      rows=123, style=createStyle(bgFill="#00e5ff"), type="contains", rule="o")
conditionalFormatting(stellako_cstc_wb, "metadata", cols=1:ncol(db.meta), 
                      rows=37, style=createStyle(bgFill="gray80"), type="contains", rule="o")
conditionalFormatting(stellako_cstc_wb, "metadata", cols = 1:ncol(db.meta), 
                      rows=42, style=createStyle(bgFill="gray80"), type="contains", rule="o")

# ENVIRO
writeData(stellako_cstc_wb, sheet="environmental", x=db.env.cstc)

# NIGHTLY CATCH
writeData(stellako_cstc_wb, sheet="nightly_catch", x=db.catch.cstc)
conditionalFormatting(stellako_cstc_wb, "nightly_catch", cols=16, 
                      rows=0:nrow(db.catch.cstc)+1, style=createStyle(bgFill="gray80"), type="expression", rule=">=0")
conditionalFormatting(stellako_cstc_wb, "nightly_catch", cols=21, 
                      rows=0:nrow(db.catch.cstc)+1, style=createStyle(bgFill="gray80"), type="expression", rule=">=0")

# L-F
writeData(stellako_cstc_wb, sheet="length_frequency", x=db.lf.cstc)

# BIOSAMPLING
writeData(stellako_cstc_wb, sheet="biosampling", x=db.bio.cstc)

# CODE SHEETS
writeData(stellako_cstc_wb, sheet="MGL_GSI_codes", x=db.mgl)
writeData(stellako_cstc_wb, sheet="PSC_scale_condition_codes", x=db.psc)

# SAVE IT ALLLLLL! ---------------------------
saveWorkbook(stellako_cstc_wb, "Stellako_smolt_database_2021.xlsx", overwrite = T)





# ================ Database to share with EWatch, PSC ================
db.catch.share <- db.catch %>%
  filter(year%in%c(2019,2021))


# WB ----------------------
northern_share_wb <- createWorkbook()

# create empty sheets
addWorksheet(northern_share_wb, "metadata")
addWorksheet(northern_share_wb, "environmental")
addWorksheet(northern_share_wb, "nightly_catch")
addWorksheet(northern_share_wb, "length_frequency")
addWorksheet(northern_share_wb, "biosampling")
addWorksheet(northern_share_wb, "MGL_GSI_codes")
addWorksheet(northern_share_wb, "PSC_scale_condition_codes")

# write data to sheets (some with formatting) -----------------
# METADATA
writeData(northern_share_wb, sheet="metadata", x=db.meta)
conditionalFormatting(northern_share_wb, "metadata", cols=1:3, 
                      rows=2, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(northern_share_wb, "metadata", cols=1:3, 
                      rows=21, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(northern_share_wb, "metadata", cols=1:3, 
                      rows=49, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(northern_share_wb, "metadata", cols=1:3, 
                      rows=61, style=createStyle(bgFill="#00e5ff"), type="contains", rule=" ")
conditionalFormatting(northern_share_wb, "metadata", cols = 1:ncol(db.meta), 
                      rows=123, style=createStyle(bgFill="#00e5ff"), type="contains", rule="o")
conditionalFormatting(northern_share_wb, "metadata", cols=1:ncol(db.meta), 
                      rows=37, style=createStyle(bgFill="gray80"), type="contains", rule="o")
conditionalFormatting(northern_share_wb, "metadata", cols = 1:ncol(db.meta), 
                      rows=42, style=createStyle(bgFill="gray80"), type="contains", rule="o")

# ENVIRO
writeData(northern_share_wb, sheet="environmental", x=db.env)

# NIGHTLY CATCH
writeData(northern_share_wb, sheet="nightly_catch", x=db.catch.share)
conditionalFormatting(northern_share_wb, "nightly_catch", cols=16, 
                      rows=0:nrow(db.catch.share)+1, style=createStyle(bgFill="gray80"), type="expression", rule=">=0")
conditionalFormatting(northern_share_wb, "nightly_catch", cols=21, 
                      rows=0:nrow(db.catch.share)+1, style=createStyle(bgFill="gray80"), type="expression", rule=">=0")

# L-F
writeData(northern_share_wb, sheet="length_frequency", x=db.lf)

# BIOSAMPLING
writeData(northern_share_wb, sheet="biosampling", x=db.bio)

# CODE SHEETS
writeData(northern_share_wb, sheet="MGL_GSI_codes", x=db.mgl)
writeData(northern_share_wb, sheet="PSC_scale_condition_codes", x=db.psc)

# SAVE IT ALLLLLL! ---------------------------
saveWorkbook(northern_share_wb, "Nadleh-Stellako_smolt_database_19-21.xlsx", overwrite = T)














