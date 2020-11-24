# Clear memory
rm(list=ls())

# Call packages
# install.packages('WDI')
library(tidyverse)
library(WDI)

data_raw_population <- WDI(indicator=c('SP.POP.TOTL'), 
                          country="all", start=2019, end=2019)

# Save the raw data file
my_path <- "C:/Users/cosmi/Documents/homework_codesANDmore_Coding1_MScBA/Task2_Cosmin-Covid-Assignment/data/"
write_csv(data_raw_population, paste0(my_path,'raw/WDI_population_raw.csv'))
