# Composite maker 

# Read in estimate generator spreadsheets for each system 

#library(xlsx)
library(data.table)
library(readxl)
library(XLConnect)

# EARLY SOUTH THOMPSON 
setwd("T:/SockeyeData/Sockeye/Adult/Early South Thompson")            # this should be changed to one common location for all composites if you want this code to run easier


###########
# From: https://www.reed.edu/data-at-reed/resources/R/excel.html

# load excel workbook
excel <- loadWorkbook("T:/SockeyeData/Sockeye/Adult/Early South Thompson/Early South Thompson 2019.xlsx") # change to match your path

# get sheet names
sheets <- getSheets(excel)
names(sheets) <- sheets


# put sheets into a list of data frames
sheet_list <- lapply(sheets, function(.sheet){readWorksheet(object=excel, .sheet)})

# remove all Scotch for now because it fucks everything up
sheet_list <- sheet_list[!names(sheet_list) %in% c("Scotch Creek - Summary", "Scotch Creek - Surveys", 
  "Scotch Creek - Fence", "Scotch Creek - Fence Extrapolat")]

# df placeholders - KD
new.df <- data.frame(system = names(sheet_list), 
  males = as.numeric(unlist(lapply(sheet_list,function(x) x[51,7]))), 
  females = as.numeric(unlist(lapply(sheet_list,function(x) x[51,8]))), 
  jacks = as.numeric(unlist(lapply(sheet_list,function(x) x[51,9]))), 
  eff_fem = as.numeric(unlist(lapply(sheet_list,function(x) x[50,18]))), 
  perc_spawn_fem = as.numeric(new.df$females-as.numeric(unlist(lapply(sheet_list,function(x) x[50,19])))), 
  row.names = NULL)












