# Late Stuart Composite 

library(XLConnect)
library(tidyverse)

# EARLY SOUTH THOMPSON 
setwd("T:/SockeyeData/Sockeye/Adult/Late Stuart")            # this should be changed to one common location for all composites if you want this code to run easier

# load excel workbook
excel <- loadWorkbook("T:/SockeyeData/Sockeye/Adult/Late Stuart/Late Stuart 2019.xlsx") # change to match your path

# get sheet names
sheets <- getSheets(excel)
names(sheets) <- sheets


# put sheets into a list of data frames
sheet_list <- lapply(sheets, function(.sheet){readWorksheet(object=excel, .sheet)})

# read all sheets in as dataframes to look at them easily if you should chose 
for (i in 1:length(sheet_list)){
  assign(paste0("df", i), 
    as.data.frame(sheet_list[i]))
  }


# create comp df
new.df <- data.frame(system = names(sheet_list))
new.df$males = as.numeric(unlist(lapply(sheet_list,function(x) x[51,7])))
new.df$females = as.numeric(unlist(lapply(sheet_list,function(x) x[51,8])))
new.df$jacks = as.numeric(unlist(lapply(sheet_list,function(x) x[51,9])))
new.df$eff_fem = as.numeric(unlist(lapply(sheet_list,function(x) x[50,18]))) 
new.df$perc_spawn_fem = as.numeric(new.df$females-as.numeric(unlist(lapply(sheet_list,function(x) x[50,19]))))



####
# calc final composite estimates 

comp <- new.df %>% 
  summarize(males = sum(males), females=sum(females), jacks=sum(jacks), eff_fem=sum(eff_fem), perc_spawn_fem=sum(perc_spawn_fem))%>%
  print()

comp.sum <- comp %>% 
  summarize(perc_spawn_comp = sum(eff_fem)/sum(perc_spawn_fem),
    male_comp_ratio=sum(males)/sum(sum(males)+sum(females)+sum(jacks)),
    female_comp_ratio=sum(females)/sum(sum(males)+sum(females)+sum(jacks)),
    jack_comp_ratio=sum(jacks)/sum(sum(males)+sum(females)+sum(jacks))) %>% 
  print()



