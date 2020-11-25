# Clear memory
rm(list=ls())

library(tidyverse)

# Call the data from github
wd <- read.csv('https://raw.githubusercontent.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/master/Task2_Cosmin-Covid-Assignment/data/raw/WDI_population_raw.csv')

## Check the observations:
#   Lot of grouping observations
#   Filter these out
wd <- wd %>% filter( !grepl("[[:digit:]]", wd$iso2c) )

# Some grouping observations are still there, check each of them
#   HK - Hong Kong, China
#   OE - OECD members
#   all with starting X, except XK which is Kosovo
#   all with starting Z, except ZA-South Africa, ZM-Zambia and ZW-Zimbabwe

# 1st drop specific values
drop_id <- c("EU","HK","OE")
# Check for filtering
wd %>% filter( grepl( paste( drop_id , collapse="|"), wd$iso2c ) ) 
# Save the opposite
wd <- wd %>% filter( !grepl( paste( drop_id , collapse="|"), wd$iso2c ) )

# 2nd drop values with certain starting char
# Get the first letter from iso2c
fl_iso2c <- substr(wd$iso2c, 1, 1)
retain_id <- c("XK","ZA","ZM","ZW") # one option is with code
retain_country <- c("Kosovo","South Africa", "Zambia","Zimbabwe") # one option is with name
# Check
d1 <- wd %>% filter( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                       !grepl( paste( retain_country , collapse="|"), wd$country )) # Kosovo gets removed regardless of method
# Save observations which are the opposite (use of !)
wd <- wd %>% filter( !( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                          !grepl( paste( retain_country , collapse="|"), wd$country ) ) )

# Clear non-needed variables
rm( d1 , drop_id, fl_iso2c , retain_id )

### 
# Check for missing observations
m <- wd %>% filter( !complete.cases( wd ) )
# Drop if total population missing -> if not complete case except iso2c
wd <- wd %>% filter( complete.cases( wd ) | is.na( wd$iso2c ) )

###
# CLEAN VARIABLES
#
# Recreate table:
#   Rename variables
#   Drop all the others !! in this case write into readme it is referring to year 2019!!
wd <- wd %>% transmute( country = country,
                        population=SP.POP.TOTL)

###
# Check for extreme values
# all HISTOGRAMS
wd %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# Check for summary as well
summary( wd )

# Save the clean data file
my_path <- "C:/Users/cosmi/Documents/homework_codesANDmore_Coding1_MScBA/Task2_Cosmin-Covid-Assignment/data/"
write_csv( wd, paste0(my_path,'clean/WDI_population_clean.csv'))
