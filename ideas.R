library(tidyverse)
data <- readr::read_csv("Escape.csv")

# Re-organize 
pivot_longer(data, cols=3:40, names_to="year", values_to="escapement")

replace_na(df,replace = list(Year = 2000))   # could be useful for infilling? 

# and then use mutate grouped by date and hour and then do row-wise averages? 