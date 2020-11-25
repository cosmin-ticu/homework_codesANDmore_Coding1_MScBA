################
# MERGE the two data table
##

# Clear memory
rm(list=ls())

library(tidyverse)

# Call the data from github
cv2 <- read.csv('https://raw.githubusercontent.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/master/Task2_Cosmin-Covid-Assignment/data/clean/covid_04_11_2020_clean.csv')
wd <- read.csv('https://raw.githubusercontent.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/master/Task2_Cosmin-Covid-Assignment/data/clean/WDI_population_clean.csv')

df <- full_join(cv2,wd)

# Correct some country names by hand
use_name <- c("Congo, Rep.","Congo, Dem. Rep.","Czech Republic","Korea, Rep.","Kyrgyz Republic",
              "Laos","St. Kitts and Nevis","St. Lucia","St. Vincent and the Grenadines",
              "Slovak Republic","United States","Myanmar")

alter_name <- c("Congo (Brazzaville)","Congo (Kinshasa)","Czechia","Korea, South","Kyrgyzstan",
                "Lao PDR","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines",
                "Slovakia","US","Burma")

# Simply use a for-cycle to change the name for the countries (note: ordering is important)
for ( i in seq_along( use_name ) ){
  df$country[ df$country == alter_name[ i ] ] <- use_name[ i ]
}

# Write a for-loop to find those which are partial or complete matches!
# 1) auxillary table for countries without any population value
aux <- df %>% filter( is.na(population) )
# 2) Get the name of the countries
countries_nm <- aux$country
# 3) Iterate through all potential partial matches
for ( i in seq_along( countries_nm ) ){
  # Select those observations where partial match exists
  log_select <- str_detect( df$country , countries_nm[ i ] )
  # Get the population values for partial matches
  c_partial <- df$population[ log_select ]
  # If there is a match: only two countries are selected and one is missing the other has population:
  if ( length( c_partial ) == 2 & sum( is.na( c_partial ) ) == 1 ){
    # Replace the missing value with the match
    df$population[ log_select & is.na(df$population)] = c_partial[ !is.na( c_partial ) ]
    # Remove the replaced variable
    df <- df %>% filter( !(log_select & is.na( df$confirmed ) ) )
  }
}

# 4) Check the results:
df %>% filter( is.na(population) )
# These are:
#   a) cruiser ships which stuck in national territory (Diamond Princess, MS Zaandam )
#   b) disputed territories which are accepted by covid statistics but not by world bank 
#       (Western Sahara, Taiwan or Kosovo)
#   c) we have no population data on them (Ertirea, Holy See (Vatican))

#####
# Handle missing values:
View( df %>% filter( !complete.cases(df) ) )
# Drop if population, confirmed cases or death is missing
#   This means we are keeping Chile and Canada, even though their active case count is missing
df <- df %>% filter( !( is.na( population ) | is.na( confirmed ) | is.na( death ) ))


#####
# Save clean data
my_path <- "C:/Users/cosmi/Documents/homework_codesANDmore_Coding1_MScBA/Task2_Cosmin-Covid-Assignment/data/"
# Final merged COVID data
write_csv( df , paste0(my_path,'clean/merged_covid_WDI_04_11_2020_clean.csv'))
