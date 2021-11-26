


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

library(tidyverse)
library(readxl)
library(openxlsx)
library(janitor)
library(scales)
library(ggpubr)
library(padr)
library(stringr)
library(withr)
library(padr)

setwd("~/ANALYSIS/Data")
options(scipen = 9999999999)

##########################################################################################################################################


# 2019 NADLEH data (individual workbooks/data sources)
catch.19.raw <- read_excel("nadleh_ANALYTICAL_database_2019.xlsx", sheet="hourly_catch")
ind.smolts.19.raw <- read_excel("nadleh_ANALYTICAL_database_2019.xlsx", sheet="individual_smolts")   # Note this isn't the absolute original file, but changes and edits during verification etc were applied here, so this is the most accurate version
#MGLb1.19.raw <- read_excel("Nautley Combined data(Current) - verified JG.xlsx", sheet="Individual smolt data")  # as close as it gets now to original batch 1 DNA file
MGLcombo.19.raw <- read_excel("NautleyCombined(19)_2020-01-27.xlsx", sheet="Individual Region IDs", skip=3)
MGLb2.19.raw <- read_excel("Nautley_Batch2(19)_2020-01-27.xlsx", sheet="Individual Region IDs", skip=3)
lf.19.raw <- read_excel("nadleh_ANALYTICAL_database_2019.xlsx", sheet="individual_smolts")

# 2021 NADLEH data (workbook with sheets and individual GSI files)
Nenviro.21.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="environmentals")
Ncatch.21.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="nightly_catch")
Nlf.21.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="length_frequency")
Nbio.21.raw <- read_excel("nadleh_data_clean_2021.xlsx", sheet="biosampling")
NMGL.21.raw <- read_excel("kokaneee_sockeyeNadlehSm(21)_2021-10-12.xlsx", sheet="Individual Region IDs", skip=3)

# 2021 STELLAKO data (workbook with sheets and individual GSI files)
Senviro.21.raw <- read_excel("stellako_data_clean_2021.xlsx", sheet="environmentals")
Scatch.21.raw <- read_excel("stellako_data_clean_2021.xlsx", sheet="nightly_catch")
Slf.21.raw <- read_excel("stellako_data_clean_2021.xlsx", sheet="length_frequency")
Sbio.21.raw <- read_excel("stellako_data_clean_2021.xlsx", sheet="biosampling")                        
SMGL.21.raw <- read_excel("kokaneee_sockeyeStellakoSm(21)_2021-10-12.xlsx", sheet="Individual Region IDs", skip=3)


##########################################################################################################################################


#                                                 CREATING THE NIGHTLY CATCH SHEET


#----------- 2019 CLEAN/ORGANIZE
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


#----------- 2021 NADLEH CLEAN/ORGANIZE
Ncatch.21 <- Ncatch.21.raw %>% 
  mutate_at(vars(n_marked_released:n_chinook_smolts), as.numeric) %>%
  mutate(trap_type = "8' RST",
         year="2021") %>%
  rename(n_unmarked_lf = n_length_frequency) %>%
  print()


#----------- 2021 STELLAKO CLEAN/ORGANIZE
Scatch.21 <- Scatch.21.raw %>% 
  mutate_at(vars(n_marked_released:n_chinook_smolts), as.numeric) %>%
  mutate(trap_type = "6' RST",
         year="2021") %>%
  rename(n_unmarked_lf = n_length_frequency) %>%
  print()


#----------- JOIN
catch.join <- full_join(catch.19, Ncatch.21) %>%
  bind_rows(., Scatch.21) %>%
  select(year, site, trap_type, location, date_opened, date_closed, time_trap_open, time_trap_closed, crew, 
         markers, mark_type, n_marked_released, n_marked_dead, n_recaps_upper_clip, n_recaps_lower_clip, total_recaps,
         n_unmarked_sampled, n_unmarked_lf, n_unmarked_spilled, n_unmarked_dead, total_unmarked, 
         n_chinook_fry, n_chinook_smolts, other_bycatch, comments) %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  mutate(total_unmarked = ifelse(year=="2021", n_unmarked_sampled + n_unmarked_lf + n_unmarked_spilled + n_unmarked_dead, total_unmarked)) %>%
  mutate(across(where(is.character), ~na_if(., "NA"))) %>%
  print()

# omitted 'date', 'date_true', columns from 2019 and 2021 spreadsheets (respectively) and open_datetime, close_datetime 



##########################################################################################################################################

#                                              CREATING THE ENVIRONMENTALS SHEET

#--------- 2019 CLEAN/ORGANIZE
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


#--------- 2021 NADLEH CLEAN/ORGANIZE
Nenviro.21 <- Nenviro.21.raw %>%
  mutate_at(vars(rst_rpms, water_gauge_m:water_temp_C), as.numeric) %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  mutate(year = 2021,
         trap_type = "8' RST") %>%
  print()


#--------- 2021 STELLAKO CLEAN/ORGANIZE
Senviro.21 <- Senviro.21.raw %>% 
  mutate_at(vars(rst_rpms, water_gauge_m:water_temp_C), as.numeric) %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  mutate(year = 2021,
         trap_type = "6' RST") %>%
  print()


#----------- Final environmental data 
enviro.join <- full_join(Nenviro.21, enviro.19) %>%
  bind_rows(., Senviro.21) %>%
  mutate_at("rst_rpms", as.numeric) %>%
  select(year, site, trap_type, date_opened, date_closed, time, location, crew_initials, rst_rpms, debris_load, debris_type1, 
         debris_type2, water_gauge_m, air_temp_C, water_temp_C, comments) %>%
  mutate(across(where(is.character), ~na_if(., "NA"))) 





##########################################################################################################################################

#                                                 CREATING THE LENGTH-FREQUENCY SHEET



#--------- 2019 CLEAN/ORGANIZE
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


#--------- 2021 NADLEH CLEAN/ORGANIZE
Nlf.21 <- Nlf.21.raw[rep(1:nrow(Nlf.21.raw), Nlf.21.raw[["count"]]),]
Nlf.21 <- Nlf.21 %>% 
  mutate(year="2021",
         comments = ifelse(comments=="NA", NA, comments)) %>% 
  print()


#--------- 2021 STELLAKO CLEAN/ORGANIZE
Slf.21 <- Slf.21.raw[rep(1:nrow(Slf.21.raw), Slf.21.raw[["count"]]),]
Slf.21 <- Slf.21 %>% 
  mutate(year="2021") %>% 
  print()


#--------- JOIN 
lf.join <- full_join(lf.19, Nlf.21) %>% 
  bind_rows(., Slf.21) %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  select(year, site, date_opened, date_closed, crew, trap_type, data_type, length_mm, comments) %>% 
  mutate(across(where(is.character), ~na_if(., "NA"))) %>%
  print()

## removed 'count' as it is not needed anymore - length-frequency tables already expanded 



##########################################################################################################################################

#                                              CREATE BIOSAMPLE SHEET


#--------- 2019 CLEAN/ORGANIZE

#--- MGL FILES: GSI RESULTS 
# Batch 1 solo:                                              **** PLACEHOLDER UNTIL FIND ORIGINAL FILE **** 
dnab1.19 <- dnab1.19.raw %>%
  # Selected all of the samples that were analyzed including those that were analyzed but didn't amplify
  filter(grepl("did not amplify", `DNA Lab Comment`) | !is.na(`Sample Identifier`)) %>% 
  # Select only columns of interest
  select(`Sample Identifier`, Region1:Prob2, `DNA Lab Comment`) %>%
  # Rename columns
  rename(lab_identifier = `Sample Identifier`,
         region1=Region1,
         prob1 = Prob1,
         region2 = Region2,
         prob2 = Prob2,
         MGL_comments = `DNA Lab Comment`) %>%
  # Extract whatman cell from lab_id, apply analysis strategy variable and change the lab_id to match the lab_id returned from the combo batch
  mutate(whatman_cell=substring(lab_identifier, 30, 34),
         dna_analysis_strategy = "Batch 1 solo",
         lab_identifier = gsub("NautleyR(18)", "NautleyR(19)", lab_identifier, fixed=T),
         dna_data_source = "Nautley Combined data(Current) - verified JG.xlsx") %>%
  print()

# Combo DNA: batch 1 and 2
MGLcombo.19 <- MGLcombo.19.raw %>% 
  # remove the leading entry from the model run (not a real fish):
  filter(Fish != "19.000299999999999") %>% 
  # rename columns:
  rename(MGL_identifier = Fish,
         MGL_comments = Comment,
         b12_reg1 = `Region 1`,
         b12_prob1 = `Prob 1`,
         b12_reg2 = `Region 2`,
         b12_prob2 = `Prob 2`,
         b12_reg3 = `Region 3`,
         b12_prob3 = `Prob 3`,
         b12_reg4 = `Region 4`,
         b12_prob4 = `Prob 4`,
         b12_reg5 = `Region 5`,
         b12_prob5 = `Prob 5`,
         b12_reg6 = `...13`,
         b12_prob6 = `...14`,
         b12_reg7=`...15`,
         b12_prob7=`...16`) %>%
  # extract the day of year and unique identifier from the lab_id field for join to our data later: 
  mutate(whatman_cell = substring(MGL_identifier, 30, 33),
         dna_data_source = "NautleyCombined(19)_2020-01-27.xlsx") %>%
  # change variable format: 
  mutate_at(vars(c(b12_prob1, b12_reg2)), as.numeric) %>%
  print()


# Batch 2 solo
MGLb2.19 <- MGLb2.19.raw %>% 
  # remove the leading entry from the model run (not a real fish):
  filter(Fish != "19.000299999999999") %>% 
  # rename columns:
  rename(MGL_identifier = Fish,
         MGL_comments = Comment,
         b2_reg1 = `Region 1`,
         b2_prob1 = `Prob 1`,
         b2_reg2 = `Region 2`,
         b2_prob2 = `Prob 2`,
         b2_reg3 = `Region 3`,
         b2_prob3 = `Prob 3`,
         b2_reg4 = `Region 4`,
         b2_prob4 = `Prob 4`,
         b2_reg5 = `Region 5`,
         b2_prob5 = `Prob 5`,
         b2_reg6 = `...13`,
         b2_prob6 = `...14`,
         b2_reg7=`...15`,
         b2_prob7=`...16`) %>%
  # extract the day of year and unique identifier from the lab_id field for join to our data later: 
  mutate(whatman_cell=substring(MGL_identifier, 30, 33),
         dna_data_source = "Nautley_Batch2(19)_2020-01-27.xlsx") %>%
  # change variable format: 
  mutate_at(vars(c(b2_prob1, b2_reg2)), as.numeric) %>%
  print()


# Join MGL runs                                        #      ****** EVENTUALLY NEED TO ADD B1 AS WELL *******
dna.19 <- left_join(MGLcombo.19, MGLb2.19, by=c("MGL_identifier", "whatman_cell", "MGL_comments")) %>%
  unite("dna_data_sources", dna_data_source.x, dna_data_source.y, sep=",", remove = T) %>%
  mutate(doy = as.numeric(substring(MGL_identifier, 25, 27)))



#--- FIELD BIOSAMPLING DATA
ind.smolts.19 <- ind.smolts.19.raw %>%
  # rename columns. Note the original "whatman_sheet" included both sheet and cell number in 1 column.
  rename(whatman_uid=whatman_sheet,
         psc_uid = sample_id,
         PSC_lab_comments=dejan_comment,
         comments=comment,
         ewatch_uid=ewatch_fid,
         doy = jdate,
         MGL_identifier=lab_identifier,
         MGL_comments=dna_comment,
         b1_reg1=region1,
         b1_prob1=prob1,
         b1_reg2=region2,
         b1_prob2=prob2) %>%
  # remove columns not needed 
  select(-c(NEWregion1, NEWprob1, NEWregion2, NEWprob2, region3:prob6, ewatch_sample_bin, psc_dna_no, area)) %>%
  # create new columns for unique IDs, data sources, selections, etc. for join to 2021 data later: 
  mutate(whatman_uid = ifelse(grepl("19\\.0", as.character(whatman_uid)), 
                               as.character(str_pad(round(as.numeric(whatman_uid),6), width=9, side="right", pad="0")), 
                              as.character(whatman_uid)),
         whatman_cell = ifelse(grepl("19\\.", whatman_uid), substring(whatman_uid, 6, 9),
                          ifelse(grepl("-", whatman_uid), sub(".*-", "", whatman_uid), whatman_uid)), 
         whatman_cell = str_remove(whatman_cell, "^0+"),
         scales_select_bin = ifelse(is.na(age), 0, 1),
         ewatch_uid = ifelse(grepl("sacrificed for Dave Patterson", comments), "E-Watch unk ID", ewatch_uid)) %>% 
         #MGL_identifier = gsub("NautleyR(18)", "NautleyR(19)", MGL_identifier, fixed=T)) %>%  
  mutate(across(where(is.character), ~na_if(., "NA"))) %>%
  print()


#--- Join 2019 GSI results + field biosampling data
bio.19 <- full_join(ind.smolts.19, dna.19, by=c("whatman_cell", "doy", "MGL_identifier", "MGL_comments")) %>% 
  mutate_at(vars(weight_g, b1_reg1:b1_prob2), as.numeric) %>%
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  # rename columns
  rename(smolt_data_source = data_source,
         DOY_closed=doy,
         PSC_uid=psc_uid) %>% 
  # create new columns to indicate which fish were run for genetics and in which batch (batch 1 or batch 2)
  mutate(dna_select_bin = as.numeric(ifelse(grepl("NautSMOLTS\\_B2", MGL_identifier), 2, dna_select_bin)),
         across(b1_reg1:b1_prob2, ~ ifelse(grepl("missing Loci", MGL_comments),NA,.))) %>%
  # remove length-frequency fish to put on the LF page: 
  filter(smolt_data_source != "2019 Nautley - length frequency entry") %>% 
  # add new useful columns for the join later: 
  mutate(year="2019",
         site="Nadleh",
         samplers="BB,AR,AK/JN") %>%
  # re-organize columns for verification (omit unnecessary columns) 
  select(-c(ufid, psc_book_no, psc_sample_no)) %>%
  select(year, site, date_opened, date_closed, date_group, DOY_closed, trap_type, PSC_uid, whatman_uid, whatman_cell, ewatch_uid, samplers,
         length_mm, length_class, weight_g, age, 
         b1_reg1:b1_prob2, b12_reg1:b12_prob7, b2_reg1:b2_prob7,
         scales_select_bin, dna_select_bin, MGL_identifier,
         comments, MGL_comments, PSC_lab_comments, smolt_data_source, dna_data_sources) %>%
  print()


#--------- 2021 NADLEH CLEAN/ORGANIZE
#--- NADLEH DNA ANALYSIS RESULTS FROM MGL 
NMGL.21 <- NMGL.21.raw %>% 
  # rename columns 
  rename(MGL_identifier = Fish,
         MGL_comments = Comment,
         b1_reg1 = `Region 1`,
         b1_prob1 = `Prob 1`,
         b1_reg2 = `Region 2`,
         b1_prob2 = `Prob 2`,
         b1_reg3 = `Region 3`,
         b1_prob3 = `Prob 3`,
         b1_reg4 = `Region 4`,
         b1_prob4 = `Prob 4`,
         b1_reg5 = `Region 5`,
         b1_prob5 = `Prob 5`,
         b1_reg6 = `...13`,
         b1_prob6 = `...14`) %>%
  # remove leading entry from model run (not a real fish):
  filter(MGL_identifier != "114-135") %>%
  # extract unique whatman cell ID from lab_id field for join to our data later, and data source record: 
  mutate(whatman_cell = str_sub(MGL_identifier,30,33),
         dna_data_sources="kokaneee_sockeyeNadlehSm(21)_2021-10-12.xlsx") %>% 
  # change variable format:
  mutate_at(vars(b1_prob1, b1_reg2), as.numeric) %>%
  print()


#--- NADLEH FIELD BIOSAMPLING DATA
Nbio.21 <- Nbio.21.raw %>%
  rename(ewatch_uid=ewatch_id) %>%
  # change variable format: 
  mutate_at(vars(length_mm:weight_g), as.numeric) %>%
  # create new columns for join: 
  mutate(trap_type = ifelse(trap_type=="8'", "8' RST", trap_type),
         year="2021",
         ufid = paste("2019", seq(1:nrow(Nbio.21.raw)), sep="-")) %>% 
  print()


#--- Join Nadleh 2021 GSI results + field biosampling data
Nbio.21 <- left_join(Nbio.21, NMGL.21, by=c("whatman_cell")) %>% 
  # create new column indicating whether samples were run for DNA and which batch (batch 1 or 2)
  mutate(dna_select_bin = if_else(grepl("NadlehSm", MGL_identifier), 1, 0),
         DOY_closed = lubridate::yday(date_closed)) %>%
  print()



#--------- 2021 STELLAKO CLEAN/ORGANIZE
#--- STELLA DNA ANALYSIS RESULTS FROM MGL 
SMGL.21 <- SMGL.21.raw %>% 
  # rename columns 
  rename(MGL_identifier = Fish,
         MGL_comments = Comment,
         b1_reg1 = `Region 1`,
         b1_prob1 = `Prob 1`,
         b1_reg2 = `Region 2`,
         b1_prob2 = `Prob 2`,
         b1_reg3 = `Region 3`,
         b1_prob3 = `Prob 3`,
         b1_reg4 = `Region 4`,
         b1_prob4 = `Prob 4`,
         b1_reg5 = `Region 5`,
         b1_prob5 = `Prob 5`) %>%
  # remove leading entry from model run (not a real fish):
  filter(MGL_identifier != "101-141") %>%
  # extract unique whatman cell ID from lab_id field for join to our data later, and data source record: 
  mutate(whatman_cell = str_sub(MGL_identifier,30,34),
         dna_data_sources="kokaneee_sockeyeStellakoSm(21)_2021-10-12.xlsx") %>% 
  # change variable format:
  mutate_at(vars(b1_prob1, b1_reg2), as.numeric) %>%
  print()


#--- STELLA FIELD BIOSAMPLING DATA
Sbio.21 <- Sbio.21.raw %>%
  rename(ewatch_uid=ewatch_id) %>%
  # change variable format:
  mutate_at(vars(length_mm:weight_g), as.numeric) %>%
  # create new columns for join: 
  mutate(trap_type = ifelse(trap_type=="6'", "6' RST", trap_type),
         year="2021",
         ufid = paste("2019", seq(1:nrow(Sbio.21.raw)), sep="-")) %>% 
  print()


#--- Join 2021 Stellako GSI results + field biosampling data
Sbio.21 <- left_join(Sbio.21, SMGL.21, by=c("whatman_cell")) %>% 
  # create new column indicating whether samples were run for DNA and which batch (batch 1 or 2)
  mutate(dna_select_bin = if_else(grepl("StellakoSm", MGL_identifier), 1, 0),
         DOY_closed = lubridate::yday(date_closed)) %>%
  print()



#--------- JOIN 2019 + 2021
# Create vector of IDs for samples slated for priority scale analysis from the PSC for quick creation of a 2021 scales_select_bin variable.
# Note this was made in dna_pulls_2021.R file. 
PSC_scale_priorities_2021 <- read.csv("northern_smolt_scale_PRIORITIES_june2021.csv")$PSC_uid

bio.join <- full_join(Nbio.21, bio.19) %>%
  bind_rows(., Sbio.21) %>%
  # change variable format  
  mutate_at(vars(date_opened, date_closed), as.Date) %>%
  # create or edit variables for clenliness 
  mutate(species = ifelse(grepl("hinook", comments), "Chinook", "Sockeye"),
         PSC_uid = ifelse(PSC_uid=="NA-NA", NA, PSC_uid),
         whatman_uid = ifelse(whatman_uid=="NA-NA", NA, whatman_uid),
         length_class = ifelse(length_mm<80, "<80", 
                               ifelse(length_mm>=80 & length_mm <= 89, "80-89",
                                      ifelse(length_mm>=90 & length_mm <= 99, "90-99",
                                             ifelse(length_mm>=100 & length_mm <= 109, "100-109",
                                                    ifelse(length_mm>=110 & length_mm <= 119, "110-119",
                                                           ifelse(length_mm>=120 & length_mm <= 130, "120-130", ">130"))))))) %>%
  # Identify samples slated for priority analysis at PSC scale lab (they will analyze all samples eventually)
  mutate(scales_select_bin = ifelse(year=="2021" & PSC_uid%in%PSC_scale_priorities_2021, "1", 
                              ifelse(year=="2021" & !PSC_uid%in%PSC_scale_priorities_2021, "2", scales_select_bin))) %>%
  # re-organize columns/omit redundancies
  select(-c(whatman_sheet, PSC_book, PSC_cell)) %>%
  select(year, site, species, trap_type, date_opened, date_closed, date_group, time_trap_closed, samplers, 
         ufid, PSC_uid, whatman_uid, whatman_cell, ewatch_uid,
         length_mm, length_class, weight_g, age, b1_reg1:b1_prob6, b12_reg1:b12_prob7, b2_reg1:b2_prob7, 
         dna_select_bin, scales_select_bin, MGL_identifier, 
         comments, PSC_lab_comments, MGL_comments, smolt_data_source, dna_data_sources) %>%
  # Turn all character NAs to 'true' NAs 
  mutate(across(where(is.character), ~na_if(., "NA"))) %>%
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
         description = case_when(column=="year"~"Year of project",
                                 column=="site"~"Nautley River (Nadleh Koh) or Stellako River (Stella)",
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
                                       "PSC_lab_comments", "MGL_comments", "dna_data_sources", "smolt_data_source") ~ "-",
                           grepl("date", column)~"yyyy-mm-dd", grepl("time", column)~"hh:mm", grepl("reg", column)~"-", 
                           grepl("prob", column)~"proportion (*100 for %)", grepl("length", column)~"millimeters", grepl("bin", column)~"-",
                           column=="PSC_uid"~"book#-cell#", column=="whatman_uid"~"sheet#-cell#", column=="ufid"~"year-fish#",
                           column=="weight_g"~"grams", column=="age"~"year of life", column=="whatman_cell"~"-",  column=="ewatch_uid"~"-"),
         description = case_when(column=="year"~"Year of project", 
                                 column=="site"~"Nautley River (Nadleh Koh) or Stellako River (Stella)",
                                 column=="species"~"Species of smolt, Sockeye or Chinook",
                                 column=="trap_type"~"Rotary screw trap (RST), either 6' or 8' in diameter, or a fyke net",
                                 column=="samplers"~"Initials of nightly sampling crew",
                                 column=="date_opened"~"Date trap (RST or fyke net) was opened for each fishing interval/group, where nights with hourly checks are recorded as one open date and one close date (i.e., open date does not necessarily line up to open time). Usually spans one night, although some dates in 2019 will span days and/or day+night",
                                 column=="date_closed"~"Date trap (RST or fyke net) was closed for each fishing interval/group, where nights with hourly checks are recorded as one open date and one close date (i.e., open date does not necessarily line up to open time). Usually spans one night, although some dates in 2019 will span days and/or day+night",
                                 column=="date_group"~"Date grouping from 2019 data. Not sure what this was for - perhaps GSI or smooth plotting. Retained in case important for replication of results at some point",
                                 column=="time_trap_closed"~"Time trap (RST or fyke net) was checked/closed for each fishing interval in 24-hr format where midnight is 00:00. References the time when smolts were netted out of the trap and brought to shore to begin sampling.",
                                 column=="ufid"~"Unique fish ID for use across space/time (PSC/Whatman IDs not guaranteed to be unique over time)",
                                 column=="PSC_uid"~"Pacific Salmon Commission (PSC) ID for scale samples. Unique ID per year BUT NOT GUARANTEED TO BE UNIQUE OVER CONSEQUTIVE PROGRAM YEARS. Concatenation of PSC book number-cell number",
                                 column=="whatman_uid"~"Whatman ID for DNA samples. Unique ID per year BUT NOT GUARANTEED TO BE UNIQUE OVER CONSEQUTIVE PROGRAM YEARS. Concatenation of Whatman sheet number-cell number",
                                 column=="whatman_cell"~"Whatman cell (as captured in whatman_uid) used to join GSI data to bio data.",
                                 column=="ewatch_uid"~"Field IDs given to fish collected for EWatch. Not necessarily the true EWatch ID",
                                 column=="length_mm"~"Smolt fork length to nearest mm",
                                 column=="length_class"~"Smolt fork length classes in 10-mm increments. In 2019 this informed DNA and weight sampling; in 2021 this only informed weight sampling",
                                 column=="weight_g"~"Smolt wet weight in grams",
                                 column=="age"~"Smolt age obtained from scale samples, where age is typically 0, 1 or 2, representing age-0 (new fry), age-1 (smolt that completed 1 year of life and entering 2nd year), and age-2 (smolt completed 2 years of life and entering 3rd year)",
                                 grepl("b1_reg", column)~"Batch 1 genetic stock ID (GSI) regional assignments, where region 1 is the most probable and subsequent regions are the least probable. Samples were run in 2 batches, the 1st batch was priority and 2nd batch was follow-up. GSI results can change depending on the mixture of samples included in the analysis, so samples were run as just batch 1, batch 1+2 combined, and just batch 2, to assess the extent of stock-switching (i.e., reliability of stock ID).",
                                 grepl("b1_prob", column)~"Batch 1 genetic stock ID (GSI) probability associated with regional assignments, where prob1 is the most probable and subsequent probabilities decrease. Samples were run in 2 batches, the 1st batch was priority and 2nd batch was follow-up. GSI results can change depending on the mixture of samples included in the analysis, so samples were run as just batch 1, batch 1+2 combined, and just batch 2, to assess the extent of stock-switching (i.e., reliability of stock ID).",
                                 grepl("b12_reg", column)~"Batch 1+2 combination genetic stock ID (GSI) regional assignments, where region 1 is the most probable and subsequent regions are the least probable. Samples were run in 2 batches, the 1st batch was priority and 2nd batch was follow-up. GSI results can change depending on the mixture of samples included in the analysis, so samples were run as just batch 1, batch 1+2 combined, and just batch 2, to assess the extent of stock-switching (i.e., reliability of stock ID).",
                                 grepl("b12_prob", column)~"Batch 1+2 combination genetic stock ID (GSI) probability associated with regional assignments, where prob1 is the most probable and subsequent probabilities decrease. Samples were run in 2 batches, the 1st batch was priority and 2nd batch was follow-up. GSI results can change depending on the mixture of samples included in the analysis, so samples were run as just batch 1, batch 1+2 combined, and just batch 2, to assess the extent of stock-switching (i.e., reliability of stock ID).",
                                 grepl("b2_reg", column)~"Batch 2 genetic stock ID (GSI) regional assignments, where region 1 is the most probable and subsequent regions are the least probable. Samples were run in 2 batches, the 1st batch was priority and 2nd batch was follow-up. GSI results can change depending on the mixture of samples included in the analysis, so samples were run as just batch 1, batch 1+2 combined, and just batch 2, to assess the extent of stock-switching (i.e., reliability of stock ID).",
                                 grepl("b2_prob", column)~"Batch 2 genetic stock ID (GSI) probability associated with regional assignments, where prob1 is the most probable and subsequent probabilities decrease. Samples were run in 2 batches, the 1st batch was priority and 2nd batch was follow-up. GSI results can change depending on the mixture of samples included in the analysis, so samples were run as just batch 1, batch 1+2 combined, and just batch 2, to assess the extent of stock-switching (i.e., reliability of stock ID).",
                                 column=="dna_select_bin"~"Categorical code for samples selected for DNA analysis, where 0=not sent, 1=first batch, 2=second batch",
                                 column=="scales_select_bin"~"Categorical code for samples selected for scale analysis, where 0=not sent, 1=first batch/priority, 2=second batch (if applicable)",
                                 column=="MGL_identifier"~"Molecular Genetics Lab (MGL) unique identifier. Needed for reference to GSI files from MGL. Composed of: BatchID(year)  trap_type  day_of_year  whatman_cell",
                                 column=="MGL_comments"~"Comments from the MGL from GSI analysis process",
                                 column=="PSC_lab_comments"~"Comments from the PSC lab from scale analysis (Dejan Brkic in 2019)",
                                 column=="comments"~"Misc comments about conditions, sampling, observations, etc. each night",
                                 column=="smolt_data_source"~"Record for 2019 data compilation as many files existed over time",
                                 column=="dna_data_sources"~"Record of GSI analysis file name(s) returned from MGL"
                                 )) %>%
  add_row(column="BIOSAMPLING METADATA", units=" ", description=" ", .before=1) %>%
  print()


#--------- JOIN
metadata <- rbind(enviro.metadata, catch.metadata, lf.metadata, bio.metadata)


#--------- MGL code sheet 
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

# SAVE IT ALLLLLL!
saveWorkbook(northern_wb, "Northern_smolt_database_2019-2021.xlsx", overwrite = T)






























