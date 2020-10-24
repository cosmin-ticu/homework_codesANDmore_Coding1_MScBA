library(tidyverse)
library(readxl)

# Import the data table.
pizza_data <- read_excel("C:/CEU/Fall_Term/Github_Repos/homework_codesANDmore_Coding1_MScBA/Task1_KFC-team-coding/data/KFC-team_pizza_data.xlsx")

# Create a factor variable for online vs offline price based on the delivery variable.
pizza_data$online <- factor( pizza_data$delivery )

# Create a histogram for the price of margherita pizza conditional on online vs offline price.
ggplot( data = pizza_data, aes( x = price_pizza, fill = online ) ) +
  geom_histogram( alpha = 0.7, binwidth = 60 ) +
  labs( x = '\n Price of Pizza', y = 'Frequency \n', fill = 'Online' ) +
  facet_wrap( ~online ) +
  annotate("rect",xmin=2500, xmax=3200, ymin=0, ymax=5, fill='gold', alpha=0.1) +
  theme_light() +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text(size = 12, face = "bold", hjust = 0.5 ) ) +
  ggtitle( 'Conditional Histogram for the Price of Margherita Pizza' )

# Create a boxplot for the price of margherita pizza conditional on online vs offline price.
ggplot( pizza_data, aes( x = online, y = price_pizza ) ) +
  geom_boxplot( color = "dodgerblue3", size = 0.7, width = 0.1, alpha = 1 ) +
  labs( x = '\n Online Price', y = 'Price of Pizza \n' ) +
  stat_boxplot( geom = "errorbar", width = 0.05,  size = 0.5 ) +
  stat_summary( fun = mean, geom = "point", shape = 20, size = 5, color = "deeppink4" ) +
  theme_light() +
  theme( plot.title = element_text(size = 12, face = "bold", hjust = 0.5 ) ) +
  ggtitle( 'Conditional Boxplot for the Price of Margherita Pizza' )

