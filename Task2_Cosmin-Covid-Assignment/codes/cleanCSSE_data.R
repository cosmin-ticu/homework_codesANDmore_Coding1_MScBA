# Clear memory
rm(list=ls())

library(tidyverse)

# Call the data from github
cv <- read.csv('https://raw.githubusercontent.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/master/Task2_Cosmin-Covid-Assignment/data/raw/covid_04_11_2020_raw.csv')

# Check covid data
glimpse( cv )

# Drop not needed variables
cv <- cv %>% select( -c( FIPS,Admin2,Last_Update,Lat,Long_,Combined_Key,Incidence_Rate,Case.Fatality_Ratio))

# One observation to be one country
# Check e.g. China:
cv %>% filter( Country_Region == 'China')

# Create new data table now only contains the countries
cv2 <- cv %>% 
  group_by( Country_Region ) %>% 
  summarise_if(is.numeric,lst( sum ) )

# Rename variables
cv2 <- cv2 %>% rename( country   = Country_Region ,
                       confirmed = Confirmed_sum,
                       death     = Deaths_sum,
                       recovered = Recovered_sum,
                       active    = Active_sum )

###
# Check for extreme values
# all HISTOGRAMS
cv2 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# Check for summary as well
summary( cv2 )

# Save the clean data file
my_path <- "C:/Users/cosmi/Documents/homework_codesANDmore_Coding1_MScBA/Task2_Cosmin-Covid-Assignment/data/"
write_csv( cv2, paste0(my_path,'clean/covid_04_11_2020_clean.csv'))
