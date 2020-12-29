# Clear memory
rm(list=ls())

library(tidyverse)

# Call the data from github
cv <- read.csv('https://raw.githubusercontent.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/master/Task3_Term-Project-Analysis/data/raw/cosmin_manufacturing_survey.csv?token=AREBRJ5JWSXKTQLS3WWDM4S75NQBY')

# Check data
glimpse( cv )
