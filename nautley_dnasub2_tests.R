
# Nautley DNA batch 1 vs 2 

library(tidyverse)
library(XLConnect)
library(xlsx)
library(openxlsx)
library(lubridate)

setwd("~/ANALYSIS/Data")


##################################################################################################################################################
                                                    
#                                                   STEP 1: COMPARE BATCH 1 RESULTS

  # Ensuring batch 1 stock IDs didn't change


#################
# BATCH 1 AND 2 #
#################

dna.combo <- read.xlsx("NautleyCombined(19)_2020-01-27.xlsx", sheet=5, detectDates=T, startRow=4)

dna.combo <- dna.combo %>% 
  filter(Prob.1 != "Nautley River-") %>% 
  rename(lab_identifier = Fish,
    comment = Comment,
    B2region1 = `Region.1`,
    B2prob1 = `Prob.1`,
    B2region2 = `Region.2`,
    B2prob2 = `Prob.2`,
    region3 = `Region.3`,
    prob3 = `Prob.3`,
    region4 = `Region.4`,
    prob4 = `Prob.4`,
    region5 = `Region.5`,
    prob5 = `Prob.5`, 
    region6 = X13,
    prob6 = X14) %>%
  print()

# Remove trailing columns
dna.combo <- dna.combo[,-c(15:16)]

dna.combo <- dna.combo %>% 
  select(lab_identifier, B2region1, B2prob1) %>%
  print()


###########################
# BATCH 1 SOLO (PREVIOUS) #
###########################

ind.dat <- read.xlsx("nautley_ANALYTICAL_database_2019.xlsx", sheet=3, detectDates=T)

inds <- ind.dat %>% 
  filter(dna_select_bin==1) %>%
  select(lab_identifier, date, psc_dna_no, region1, prob1, region2, prob2) %>% 
  mutate(jdate = lubridate::yday(date)) %>%
  select(lab_identifier, jdate, psc_dna_no, region1, prob1, region2, prob2, date) %>%
  print()

# Join 
comp <- left_join(inds, dna.combo, by="lab_identifier")

# Quick math to see if stock IDs are same
comp <- comp %>% 
  mutate(GSID_DIFF = region1 - B2region1) %>%
  print()

      # May 9 PSC DNA sample #622 ("129 622") changed from Nadina to Stellako 
      # May 3 PSC DNA sample #380 ("123 380") changed from Stellako to Nadina


##################################################################################################################################################
                                                    
#                                                   STEP 2: COMPARE BATCH 2 RESULTS

  # Comparing Batch 2 run solo vs. run with Batch 1 results 


#################
# BATCH 1 AND 2 #
#################

dna.combo <- read.xlsx("NautleyCombined(19)_2020-01-27.xlsx", sheet=5, detectDates=T, startRow=4)

dna.combo.b2 <- dna.combo %>% 
  filter(Fish != "19.000299999999999") %>% 
  filter(str_detect(Fish, "NautSMOLTS_B2")) %>%
  rename(lab_identifier = Fish,
    comment = Comment,
    COMBOregion1 = `Region.1`,
    COMBOprob1 = `Prob.1`,
    COMBOregion2 = `Region.2`,
    COMBOprob2 = `Prob.2`,
    region3 = `Region.3`,
    prob3 = `Prob.3`,
    region4 = `Region.4`,
    prob4 = `Prob.4`,
    region5 = `Region.5`,
    prob5 = `Prob.5`, 
    region6 = X13,
    prob6 = X14) %>%
  print()

# Remove trailing columns
dna.combo.b2 <- dna.combo.b2[,-c(15:16)]

dna.combo.b2 <- dna.combo.b2 %>% 
  select(lab_identifier, COMBOregion1, COMBOprob1) %>%
  print()


################
# BATCH 2 SOLO #
################

batch2.raw <- read.xlsx("Nautley_Batch2(19)_2020-01-27.xlsx", sheet=5, detectDates=T, startRow=4)

batch2 <- batch2.raw %>% 
  filter(Fish != "19.000299999999999") %>% 
  rename(lab_identifier = Fish,
    comment = Comment,
    B2region1 = `Region.1`,
    B2prob1 = `Prob.1`,
    B2region2 = `Region.2`,
    B2prob2 = `Prob.2`,
    region3 = `Region.3`,
    prob3 = `Prob.3`,
    region4 = `Region.4`,
    prob4 = `Prob.4`,
    region5 = `Region.5`,
    prob5 = `Prob.5`, 
    region6 = X13,
    prob6 = X14) %>%
  print()

# Remove trailing columns
batch2 <- batch2[,-c(15:16)]

batch2 <- batch2 %>% 
  select(lab_identifier, B2region1, B2prob1) %>%
  print()


# Join 
comp.b2 <- left_join(batch2, dna.combo.b2, by="lab_identifier")

# Quick math to see if stock IDs are same
comp.b2 <- comp.b2 %>% 
  mutate(GSID_DIFF = COMBOregion1 - B2region1) %>%
  print()

      # May 11 PSC DNA sample #690 ("131 690") changed from Tachie to Stellako




















