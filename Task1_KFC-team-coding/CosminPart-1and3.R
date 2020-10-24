# Setup
library(tidyverse)
library(readxl)
library(ggplot2)
data_in <- "C:/Users/acer/Desktop/OneDrive - Central European University/Courses/Fall_Term/Coding_1/data/Pizza"
pizza_data <- read_excel( paste0(data_in,"/raw/KFC-team_pizza_data.xlsx") )

### Histograms of the two product prices
## Kernel density plot overlays on each histogram showcase the skewness

# The following plots describe the distribution of margherita pizza prices and sparkling water prices. 
# 
# It can be clearly seen that even though the price distribution for margherita pizza tends to have a few smaller modes, it closely resembles a normal distribution. 
# With the help of binning, the distribution can be brought to look much closer like a normal distribution.
# 
# The same cannot be said about the distribution of sparkling water prices, which showcase a heavy right skew, having a mean much higher than the median. 
# This is perhaps caused by the large differences in sparkling water brands (from local Hungarian to expensive imported Italian water). 
# As such, the range of products is much higher than with respect to the pizza. 
# For further analysis, it would be worthwhile to inspect distributions of local sparkling water prices and imported water prices in separate graphs.

# Descriptive graph on pizza
ggplot( data = pizza_data , aes( x = price_pizza ) ) +
  geom_histogram( aes(y=..density..), fill = 'brown' , binwidth = 100 ) +
  geom_density( aes(y = ..density..) , fill = 'red' , bw = 100, alpha = 0.5 ) +
  labs(x='Price of Margherita Pizza (HUF)',y='Kernel Density (Percentage)',
       title= 'Distribution of Margherita Pizza Prices')+
  scale_y_continuous(labels = scales::percent)+
  theme_light()

# Descriptive graph on sparkling water
ggplot( data = pizza_data , aes( x = price_swater ) ) +
  geom_histogram( aes(y=..density..), fill = 'navyblue' , binwidth = 50 ) +
  geom_density( aes(y = ..density..) , fill = 'white' , bw = 50, alpha = 0.5 ) +
  labs(x='Price of 0.5L Sparkling Water (HUF)', y='Kernel Density (Percentage)',
       title= 'Distribution of Optional Sparkling Water Prices')+
  scale_y_continuous(labels = scales::percent)+
  theme_light()

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