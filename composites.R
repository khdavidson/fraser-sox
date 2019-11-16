# Composite maker 

# Good general composite code here, example using N Thompson Summers. More specific below. 

library(XLConnect)
library(dplyr)


##########################
# NORTH THOMPSON SUMMERS #
##########################

# working directory
setwd("T:/SockeyeData/Sockeye/Adult/North Thompson")            # this should be changed to one common location for all composites if you want this code to run easier

# load excel workbook that includes preliminaries for each system 
excel <- loadWorkbook("T:/SockeyeData/Sockeye/Adult/North Thompson/North Thompson (Summer) 2019.xlsx") # change to match your path

# extract sheet tab names
sheets <- getSheets(excel)
names(sheets) <- sheets

# put sheet tabs into a list of data frames - easy extraction and manipulation 
sheet_list <- lapply(sheets, function(.sheet){readWorksheet(object=excel, .sheet)})

  # read all sheets in as dataframes to look at them easily - for visual use only, this code is not necessary for any calculations/analysis
  for (i in 1:length(sheet_list)){
    assign(paste0("df", i), 
      as.data.frame(sheet_list[i]))
    }

# create new summary dataframe of all systems (extracting values from list of dataframes) and fill with summary values 
new.df <- data.frame(system = names(sheet_list))
new.df$males = as.numeric(unlist(sapply(sheet_list, function(x) max(as.numeric(x$Col7), na.rm=TRUE))))
new.df$females = as.numeric(unlist(sapply(sheet_list, function(x) max(as.numeric(x$Col8), na.rm=TRUE))))
new.df$jacks = as.numeric(unlist(sapply(sheet_list, function(x) (max(as.numeric(x$Col9)*1.26, na.rm=TRUE)))))
new.df$eff_fem = as.numeric(unlist(sapply(sheet_list, function(x) max(as.numeric(x$Col18), na.rm=TRUE))))
new.df$fem_nr = as.numeric(unlist(sapply(sheet_list, function (x) as.numeric(sub("%", "", (x[(nrow(x)-1),19]))))))
new.df$fem_perc_spawn_wgt = as.numeric(unlist(sapply(sheet_list, function(x) as.numeric(sub("%", "", (x[(nrow(x)-1),17]))))/100)) 
new.df$fem_perc_spawn_total = new.df$females-new.df$fem_nr


## FINAL CALCS
# total males, females, EFF and female spawners-nr
comp <- new.df %>% 
  summarize(males = sum(males), females=sum(females), jacks=sum(jacks), eff_fem=sum(eff_fem), fem_perc_spawn_total=sum(fem_perc_spawn_total))%>%
  print()

# composite sex ratio, spawn success
comp.sum <- comp %>% 
  summarize(perc_spawn_comp = sum(eff_fem)/sum(fem_perc_spawn_total),
    male_comp_ratio=males/sum(sum(males)+sum(females)),
    female_comp_ratio=females/sum(sum(males)+sum(females))) %>% 
  print()







##################################################################################################################################################

                                                             # SPECIAL COMPOSITES 2019 

library(XLConnect)
library(dplyr)

########################
# EARLY SOUTH THOMPSON #
########################

# working directory
setwd("T:/SockeyeData/Sockeye/Adult/Early South Thompson")            # this should be changed to one common location for all composites if you want this code to run easier

# load excel workbook that includes preliminaries for each system 
excel <- loadWorkbook("T:/SockeyeData/Sockeye/Adult/Early South Thompson/Early South Thompson 2019.xlsx")           # change to match your path

# extract sheet tab names
sheets <- getSheets(excel)
names(sheets) <- sheets

# put sheet tabs into a list of data frames - easy extraction and manipulation 
sheet_list <- lapply(sheets, function(.sheet){readWorksheet(object=excel, .sheet)})

  # read all sheets in as dataframes to look at them easily - for visual use only, this code is not necessary for any calculations/analysis
  for (i in 2:length(sheet_list)){
    assign(paste0("df", i), 
      as.data.frame(sheet_list[i]))
    }


## BEFORE GOING FORWARD- scotch creek tabs create mess ups because they are totally different, unstandardized formats. for work, I will remove
## them and re-add them below. 

# remove all Scotch tabs for now because they fuck it all up
sheet_list2 <- sheet_list[!names(sheet_list) %in% c("Scotch Creek - Summary", "Scotch Creek - Surveys", 
  "Scotch Creek - Fence", "Scotch Creek - Fence Extrapolat")]

# create new summary dataframe of all systems EXCEPT scotch (extracting values from list of dataframes) and fill with summary values 
new.df <- data.frame(system = names(sheet_list2))
new.df$males = as.numeric(unlist(lapply(sheet_list2,function(x) x[51,7])))
new.df$females = as.numeric(unlist(lapply(sheet_list2,function(x) x[51,8])))
new.df$jacks = as.numeric(unlist(lapply(sheet_list2,function(x) x[51,9])))
new.df$fem_nr = as.numeric(unlist(sapply(sheet_list2, function (x) as.numeric(sub("%", "", (x[(nrow(x)-1),19]))))))
new.df$fem_perc_spawn_wgt = as.numeric(unlist(sapply(sheet_list2, function(x) as.numeric(sub("%", "", (x[(nrow(x)-1),17]))))/100)) 
new.df$fem_perc_spawn_total = new.df$females-new.df$fem_nr


  ## SCOTCH ##
  # create scotch creek new dataframe (extracting from list of dataframes) and fill with summary values 
  sheet_list_s <- sheet_list[names(sheet_list) %in% c("Scotch Creek - Surveys")]
  
  scotch <- data.frame(system = names(sheet_list_s))
  scotch$males = as.numeric(unlist(lapply(sheet_list_s,function(y) y[59,21])))                                   # note use of function(y) so it doesn't screw up funciton(x) above
  scotch$females = as.numeric(unlist(lapply(sheet_list_s,function(y) y[59,22])))
  scotch$jacks = as.numeric(unlist(lapply(sheet_list_s,function(y) y[59,23])))
  scotch$fem_nr = as.numeric(unlist(sapply(sheet_list2, function (x) as.numeric(sub("%", "", (x[(nrow(x)-1),19]))))))
  scotch$fem_perc_spawn_wgt = as.numeric(unlist(sapply(sheet_list2, function(x) as.numeric(sub("%", "", (x[(nrow(x)-1),17]))))/100)) 
  scotch$fem_perc_spawn_total = scotch$females-scotch$fem_nr

# combine all systems dataframe with scotch dataframe - master summary! 
comp <- rbind(new.df, scotch)


## FINAL CALCS
# total males, females, EFF and female spawners-nr
comp <- new.df %>% 
  summarize(males = sum(males), females=sum(females), jacks=sum(jacks), eff_fem=sum(eff_fem), fem_perc_spawn_total=sum(fem_perc_spawn_total))%>%
  print()

# composite sex ratio, spawn success
comp.sum <- comp %>% 
  summarize(perc_spawn_comp = sum(eff_fem)/sum(fem_perc_spawn_total),
    male_comp_ratio=males/sum(sum(males)+sum(females)),
    female_comp_ratio=females/sum(sum(males)+sum(females))) %>% 
  print()



