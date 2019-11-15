# Composite maker 

# Read in estimate generator spreadsheets for each system 

#library(xlsx)
#library(data.table)
#library(readxl)
library(XLConnect)
library(dplyr)

# EARLY SOUTH THOMPSON 
setwd("T:/SockeyeData/Sockeye/Adult/Early South Thompson")            # this should be changed to one common location for all composites if you want this code to run easier


###########
# From: https://www.reed.edu/data-at-reed/resources/R/excel.html
# https://stackoverflow.com/questions/48862456/extract-specific-information-from-a-list-of-dataframes
# https://stackoverflow.com/questions/42115412/r-reading-multiple-excel-files-extract-first-sheet-names-and-create-new-colum


# load excel workbook
excel <- loadWorkbook("T:/SockeyeData/Sockeye/Adult/Early South Thompson/Early South Thompson 2019.xlsx") # change to match your path

# get sheet names
sheets <- getSheets(excel)
names(sheets) <- sheets


# put sheets into a list of data frames
sheet_list <- lapply(sheets, function(.sheet){readWorksheet(object=excel, .sheet)})

# read all sheets in as dataframes to look at them easily if you should chose 
for (i in 2:length(sheet_list)){
  assign(paste0("df", i), 
    as.data.frame(sheet_list[i]))
  }


# remove all Scotch for now because it fucks everything up
sheet_list2 <- sheet_list[!names(sheet_list) %in% c("Scotch Creek - Summary", "Scotch Creek - Surveys", 
  "Scotch Creek - Fence", "Scotch Creek - Fence Extrapolat")]

# df placeholders - KD
new.df <- data.frame(system = names(sheet_list2))
new.df$males = as.numeric(unlist(lapply(sheet_list2,function(x) x[51,7])))
new.df$females = as.numeric(unlist(lapply(sheet_list2,function(x) x[51,8])))
new.df$jacks = as.numeric(unlist(lapply(sheet_list2,function(x) x[51,9])))
new.df$eff_fem = as.numeric(unlist(lapply(sheet_list2,function(x) x[50,18]))) 
new.df$perc_spawn_fem = as.numeric(new.df$females-as.numeric(unlist(lapply(sheet_list2,function(x) x[50,19]))))

# add scotch creek manually 
sheet_list_s <- sheet_list[names(sheet_list) %in% c("Scotch Creek - Surveys")]

scotch <- data.frame(system = names(sheet_list_s))
scotch$males = as.numeric(unlist(lapply(sheet_list_s,function(y) y[59,21])))
scotch$females = as.numeric(unlist(lapply(sheet_list_s,function(y) y[59,22])))
scotch$jacks = as.numeric(unlist(lapply(sheet_list_s,function(y) y[59,23])))
scotch$eff_fem = as.numeric(unlist(lapply(sheet_list_s,function(y) y[58,32])))
scotch$perc_spawn_fem = as.numeric(scotch$females-as.numeric(unlist(lapply(sheet_list_s,function(y) y[58,33]))))

# combine 
comp <- rbind(new.df, scotch)


####
# calc final composite estimates 

comp.t <- comp %>% 
  summarize(males = sum(males), females=sum(females), jacks=sum(jacks), eff_fem=sum(eff_fem), perc_spawn_fem=sum(perc_spawn_fem))%>%
  print()

comp.sum <- comp %>% 
  summarize(perc_spawn_comp = sum(eff_fem)/sum(perc_spawn_fem),
    male_comp_ratio=sum(males)/sum(sum(males)+sum(females)+sum(jacks)),
    female_comp_ratio=sum(females)/sum(sum(males)+sum(females)+sum(jacks)),
    jack_comp_ratio=sum(jacks)/sum(sum(males)+sum(females)+sum(jacks))) %>% 
  print()







################################################################### N Thompson Summers

library(XLConnect)
library(dplyr)

# EARLY SOUTH THOMPSON 
setwd("T:/SockeyeData/Sockeye/Adult/North Thompson")            # this should be changed to one common location for all composites if you want this code to run easier

# load excel workbook
excel <- loadWorkbook("T:/SockeyeData/Sockeye/Adult/North Thompson/North Thompson (Summer) 2019.xlsx") # change to match your path

# get sheet names
sheets <- getSheets(excel)
names(sheets) <- sheets


# put sheets into a list of data frames
sheet_list <- lapply(sheets, function(x) {readWorksheet(object=excel, x, startRow=33)} )

# read all sheets in as dataframes to look at them easily if you should chose 
for (i in 1:length(sheet_list)){
  assign(paste0("df", i), 
    as.data.frame(sheet_list[i]))
  }


# df placeholders - KD
new.df <- data.frame(system = names(sheet_list))
new.df$males = as.numeric(unlist(sapply(sheet_list, function(x) max(as.numeric(x$Col7), na.rm=TRUE))))
new.df$females = as.numeric(unlist(sapply(sheet_list, function(x) max(as.numeric(x$Col8), na.rm=TRUE))))
new.df$jacks = as.numeric(unlist(sapply(sheet_list, function(x) (max(as.numeric(x$Col9)*1.26, na.rm=TRUE)))))
new.df$eff_fem = as.numeric(unlist(sapply(sheet_list, function(x) max(as.numeric(x$Col18), na.rm=TRUE))))
new.df$perc_spawn_fem = as.numeric(unlist(sapply(sheet_list, function(x) as.numeric(sub("%", "", (x[(nrow(x)-1),17])))))) 


####
# calc final composite estimates 

comp.t <- comp %>% 
  summarize(males = sum(males), females=sum(females), jacks=sum(jacks), eff_fem=sum(eff_fem), perc_spawn_fem=sum(perc_spawn_fem))%>%
  print()

comp.sum <- comp %>% 
  summarize(perc_spawn_comp = sum(eff_fem)/sum(perc_spawn_fem),
    male_comp_ratio=sum(males)/sum(sum(males)+sum(females)+sum(jacks)),
    female_comp_ratio=sum(females)/sum(sum(males)+sum(females)+sum(jacks)),
    jack_comp_ratio=sum(jacks)/sum(sum(males)+sum(females)+sum(jacks))) %>% 
  print()




