# Clear memory
rm(list=ls())

# Call packages
library(tidyverse)

covid_cases_raw <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/11-04-2020.csv')

# Save the raw data file
my_path <- "C:/Users/cosmi/Documents/homework_codesANDmore_Coding1_MScBA/Task2_Cosmin-Covid-Assignment/data/"
write_csv(data_raw_population, paste0(my_path,'raw/CSSE_covid_raw.csv'))
