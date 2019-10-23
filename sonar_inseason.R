# Code to take 'Data Entry' .csv and output in-season reporting tool 

##########
# SET UP #
##########

# Load libraries to use
library(dplyr)
library(tidyr)
library(ggplot2)

# Set working directory where .csv data file is stored. This will vary by computer.
setwd("~/Data/Sonar")

# Read in Data 
raw.data <- read.csv("Stellako Sonar_2018.csv", header=F)

# Clean up messy Excel formatting 
raw.data <- raw.data[-c(1:4),-c(13:18)] 
names(raw.data) <- lapply(raw.data[1, ], as.character)
raw.data <- raw.data[-1,] 




