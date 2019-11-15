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
sheet_list <- lapply(sheets, function(x) {readWorksheet(object=excel, x, startRow=37)} )

column.names <- c("date", "survey_type", "obs1", "obs2", "live_count_avg", "oe_wgt", "males", "females", "jacks", "unid_sex",
  "total", "cuml_total", "fem_nr", "fem_0", "fem_50", "fem_100", "perc_spawn_wgt", "eff", "nr_total")

sheet_list <- lapply(sheet_list, setNames, column.names)


# read all sheets in as dataframes to look at them easily if you should chose 
for (i in 1:length(sheet_list)){
  assign(paste0("df", i), 
    as.data.frame(sheet_list[i] ) )
}

colnam <- lapply(sheet_list, function(x)  colnames(x)[apply(x, 1, which.min)])


# create comp df
new.df <- data.frame(system = names(sheet_list))
new.df$males = as.numeric(unlist(sapply(sheet_list, function(x) max(sum(as.numeric(x$Col7), na.rm=TRUE)))))
new.df$females = as.numeric(unlist(sapply(sheet_list, function(x) max(as.numeric(x$Col8), na.rm=TRUE))))
new.df$jacks = as.numeric(unlist(sapply(sheet_list, function(x) (max(as.numeric(x$Col9)*1.26, na.rm=TRUE)))))
new.df$eff_fem = as.numeric(unlist(sapply(sheet_list, function(x) max(as.numeric(x$Col18), na.rm=TRUE))))
new.df$perc_spawn_fem = as.numeric(unlist(sapply(sheet_list, function(x) 
  ifelse((sum(as.numeric())), na.rm=TRUE))))    # this won't always work if the weighted % spawn is less than any 100% day

new5 <- pivot_longer(data=df5, cols=1:19, names_to = "Sex", values_to = "Counts")


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



