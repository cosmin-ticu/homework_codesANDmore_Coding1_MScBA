# Setup
library(tidyverse)
library(readxl)
library(ggplot2)
data_in <- "C:/Users/acer/Desktop/OneDrive - Central European University/Courses/Fall_Term/Coding_1/data/Pizza"
pizza_data <- read_excel( paste0(data_in,"/raw/KFC-team_pizza_data.xlsx") )

### Histograms of the two product prices
## Kernel density plot overlays on each histogram showcase the skewness
# Descriptive graph on pizza
ggplot( data = pizza_data , aes( x = price_pizza ) ) +
  geom_histogram( aes(y=..density..), fill = 'navyblue' , binwidth = 100 ) +
  geom_density( aes(y = ..density..) , fill = 'red' , bw = 100, alpha = 0.5 ) +
  labs(x='Price of Margherita Pizza (HUF)',y='Density (Percentage of occurences)',
       title= 'Distribution of Margherita Pizza Prices',
       subtitle = 'Kernel density plot overlay showcases the skewness')+
  scale_y_continuous(labels = scales::percent)

# Descriptive graph on sparkling water
ggplot( data = pizza_data , aes( x = price_swater ) ) +
  geom_histogram( aes(y=..density..), fill = 'navyblue' , binwidth = 50 ) +
  geom_density( aes(y = ..density..) , fill = 'red' , bw = 50, alpha = 0.5 ) +
  labs(x='Price of 0.5L Sparkling Water (HUF)',y='Density (Percentage of occurences)',
       title= 'Distribution of Optional Sparkling Water Prices',
       subtitle = 'Kernel density plot overlay showcases the skewness')+
  scale_y_continuous(labels = scales::percent)

# ```{r, echo=FALSE, out.width='50%'}
# ggplot( data = pizza_data , aes( x = price_pizza ) ) +
#   geom_histogram( aes(y=..density..), fill = 'brown' , binwidth = 100 ) +
#   geom_density( aes(y = ..density..) , fill = 'red' , bw = 100, alpha = 0.5 ) +
#   labs(x='Price of Margherita Pizza (HUF)',y='Kernel Density (Percentage)',
#        title= 'Distribution of Margherita Pizza Prices')+
#   scale_y_continuous(labels = scales::percent)
# 
# ggplot( data = pizza_data , aes( x = price_swater ) ) +
#   geom_histogram( aes(y=..density..), fill = 'navyblue' , binwidth = 50 ) +
#   geom_density( aes(y = ..density..) , fill = 'white' , bw = 50, alpha = 0.5 ) +
#   labs(x='Price of 0.5L Sparkling Water (HUF)', y='Kernel Density (Percentage)',
#        title= 'Distribution of Optional Sparkling Water Prices')+
#   scale_y_continuous(labels = scales::percent)
# ```