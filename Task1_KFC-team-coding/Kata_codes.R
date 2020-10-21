library(tidyverse)
library(readxl)

pizza_data <- read_excel("C:/CEU/Fall_Term/Github_Repos/homework_codesANDmore_Coding1_MScBA/Task1_KFC-team-coding/KFC-team_pizza_data.xlsx")

# creating factor variables for delivery vs offline price
pizza_data$online <- factor( pizza_data$delivery )
table( pizza_data$online )
pizza_data %>% select( online, price_pizza ) %>% 
  group_by( online ) %>% 
  summarise( mean = mean( price_pizza ),
             sd = sd( price_pizza ),
             num_obs = n() )

# create ggplot for online vs offline prices: histogram
ggplot( data = pizza_data, aes( x = price_pizza, fill = online ) ) +
  geom_histogram( alpha = 0.4 ) +
  labs( x = 'Price of Pizza', y = 'Frequency', fill = 'Online' ) +
  facet_wrap( ~online )
#  xlim( -4, 4 )