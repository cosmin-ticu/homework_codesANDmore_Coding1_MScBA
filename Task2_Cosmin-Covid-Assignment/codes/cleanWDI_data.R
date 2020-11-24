# Clear memory
rm(list=ls())

library(tidyverse)

# Call the data from github
df <- read.csv('https://raw.githubusercontent.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/master/Task2_Cosmin-Covid-Assignment/data/raw/WDI_population_raw.csv')

## Check the observations:
#   Lot of grouping observations
#   Filter these out
df <- df %>% filter( !grepl("[[:digit:]]", df$iso2c) )

# Some grouping observations are still there, check each of them
#   HK - Hong Kong, China
#   OE - OECD members
#   all with starting X, except XK which is Kosovo
#   all with starting Z, except ZA-South Africa, ZM-Zambia and ZW-Zimbabwe

# 1st drop specific values
drop_id <- c("EU","HK","OE")
# Check for filtering
df %>% filter( grepl( paste( drop_id , collapse="|"), df$iso2c ) ) 
# Save the opposite
df <- df %>% filter( !grepl( paste( drop_id , collapse="|"), df$iso2c ) )

# 2nd drop values with certain starting char
# Get the first letter from iso2c
fl_iso2c <- substr(df$iso2c, 1, 1)
retain_id <- c("XK","ZA","ZM","ZW") # one option is with code
retain_country <- c("Kosovo","South Africa", "Zambia","Zimbabwe") # one option is with name
# Check
d1 <- df %>% filter( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                       !grepl( paste( retain_country , collapse="|"), df$country )) # Kosovo gets removed regardless of method
# Save observations which are the opposite (use of !)
df <- df %>% filter( !( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                          !grepl( paste( retain_country , collapse="|"), df$country ) ) )

# Clear non-needed variables
rm( d1 , drop_id, fl_iso2c , retain_id )

### 
# Check for missing observations
m <- df %>% filter( !complete.cases( df ) )
# Drop if total population missing -> if not complete case except iso2c
df <- df %>% filter( complete.cases( df ) | is.na( df$iso2c ) )

###
# CLEAN VARIABLES
#
# Recreate table:
#   Rename variables
#   Drop all the others !! in this case write into readme it is referring to year 2019!!
df <- df %>% transmute( country = country,
                        population=SP.POP.TOTL)

###
# Check for extreme values
# all HISTOGRAMS
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# Check for summary as well
summary( df )

# Save the raw data file
my_path <- "C:/Users/cosmi/Documents/homework_codesANDmore_Coding1_MScBA/Task2_Cosmin-Covid-Assignment/data/"
write_csv( df, paste0(my_path,'clean/WDI_population_clean.csv'))