# TESA Web Series
# Lecture 1: Data extraction, exploration, QC - Mike McMahon (Population Ecology Division, Maritimes)
# Lecture date: 17-Oct-2019
# Lecture watched: 25-Oct-2019

# R packages used here (on github): Mar.utils, Mar.datawrangling, Mar.JoyofQC
  # Commercial data available for Maritimes: MARFIS (2002+), COMLAND (1967-2001)
  # Mixed data available for Maritimes: ISDB (Observer Data & unit-specific surveys from industry vessels), MDFG (Port sampling i.e., creel)
  # Other data: Assessment unit specific survey data, short-term/one-off surveys, VMS (Vessel Monitoring System, not fisheries but useful for ship movement)
  # All data stored in Oracle 

  # Mar.utils - functions useful on own 
  # Mar.datawrangling - extracting/exploring maritime fisheries data 
  # Mar.JoyofQC - shiny ap for exploring data 


######

# MAR.UTILS functions
# identify_area() - you have point locations and need to know what area (fishing strata, etc.) they are in, and/or have doubts about their records
GSINF <- identify_area(df=GSINF, agg.poly.shp="URL", agg.poly.field="StrataID")
GSINF_chk <- GSINF[which(GSINF$name != GSINF$strata),]

clip_by_poly_generic()   # 








