################
# Analyze the data
##

# Clear memory
rm(list=ls())

# Packages to use
library(tidyverse)
# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
#install.packages("lspline")
library(lspline)
# Estimate robust SE
#install.packages("estimatr")
library(estimatr)
# Compare models with robust SE
#install.packages("texreg")
library(texreg)
# For different themes
#install.packages(ggthemes)
library(ggthemes)

# Call the data from github
df <- read_csv('https://raw.githubusercontent.com/cosmin-ticu/homework_codesANDmore_Coding1_MScBA/master/Task2_Cosmin-Covid-Assignment/data/clean/merged_covid_WDI_04_11_2020_clean.csv')

# For the purpose of this analysis, the active and recovered cases are not of interest
# But the per capita data is of interest; thus need to make data transformation to ratio

## one option could be to take an approach of per capita
# df <- transmute(df,
#           country = country,
#           cases_capita = confirmed/population*1000,
#           deaths_capita = death/population*1000,
#           population = population)

## other option could be to take an approach of per capita
df <- transmute(df,
                country = country,
                cases = confirmed,
                deaths = death,
                cases_capita = confirmed/population,
                deaths_capita = death/population,
                population = population)

####
# 
# Quick check on all HISTOGRAMS
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()

# Check quick summary; dealing with large cases everywhere
summary( df )

######
# Check basic scatter-plots!
#   Two competing models in the class assignment:
#     1) deaths = alpha + beta * cases
#     2) deaths_capita = alpha + beta * cases_capita

### This analysis only focuses on the latter case, working with ratio'd data
#
# Where to use log-transformation? - level-level vs level-log vs log-level vs log-log
#
# deaths_capita - cases_capita: level-level model without scaling
ggplot( df , aes(x = cases_capita, y = deaths_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Coronavirus cases per capita",y = "Coronavirus deaths per capita") 

# change the scale for cases per capita for checking log-transformation
# level - log
ggplot( df , aes(x = cases_capita, y = deaths_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Coronavirus cases per capita (ln scale )",y = "Coronavirus deaths per capita") +
  scale_x_continuous( trans = log_trans() )

# change the scale for deaths per capita for checking log-transformation
# log - level
ggplot( df , aes(x = cases_capita, y = deaths_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Coronavirus cases per capita",y = "Coronavirus deaths per capita (ln scale )") +
  scale_y_continuous( trans = log_trans() )

# the scale for cases per capita and deaths per capita for checking log-transformation
# log - log
ggplot( df , aes(x = cases_capita, y = deaths_capita ))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Coronavirus cases per capita (ln scale )",y = "Coronavirus deaths per capita (ln scale )") +
  scale_x_continuous( trans = log_trans() )+
  scale_y_continuous( trans = log_trans() )

####
# Conclusions:
#   1) taking log of cases per capita is needed 
#         - as the histogram shows, distribution has a long right tail
#   2) using only level models is not possible
#       - Substantive: our interest could also be to model percentage changes between confirmed cases and deaths
#       - Statistical: modeling non-linearity for extremely affected countries would overfit any model
#                      - level-level model shows much higher heteroskedasticity
#                      - interest is to reduce it even while using robust models (R-squared will benefit)
#   3) taking log of deaths per capita is needed; but ZERO values need to be manually inputted (log calculation cannot discern)
#       - there are 13 countries that have no deaths from coronavirus as of 04.11.2020
#   4) taking log of both variables is making the association closer to linear!
#   5) taking log for both variables makes sense
#       - Substantive: it gives easier interpretation than level-log or log-level, as now only percentage changes are concerned
#       - Statistical: heteroskedasticity appears reduced as opposed to the level - level model

## Take Log of both explanatory and explained variables
# A workaround was found in the case of the ZERO values
# it appears the consensus online around data analysts 
# is to add a small amount of the ZERO values
# the main suggested amount was half of the 
# lowest non-ZERO value
df <- df %>% mutate( ln_cases_capita = log( cases_capita ),
                     ln_deaths_capita = log(deaths_capita + 0.000000043362955))

# Plot the scatterplot to see position
ggplot( df , aes(x = ln_cases_capita, y = ln_deaths_capita ))  +
  geom_point() +
  geom_smooth(method="loess")

# create a test df to benchmark the scatterplot of the log workaround to
# the scatterplot without a fix to the ZERO values
# (R also produces a warning and displays values somehow below the lowest visible point of the axis to signify -Inf)
df_test <- df %>% mutate( ln_cases_capita = log( cases_capita ),
                     ln_deaths_capita = log(deaths_capita ))

# Plot the scatterplot of the non-manipulated ZERO log values to see position
ggplot( df_test , aes(x = ln_cases_capita, y = ln_deaths_capita ))  +
  geom_point() +
  geom_smooth(method="loess")

# It is fair to say that the distribution on the scatterplot looks very similar between the two
# Conclusion: It is worthwhile to manipulate the data using the half of the smallest non-ZERO values to keep
# what would otherwise be -Inf log values
#   - Substantive: This way we do not remove any more variables that contain (supposedly) correctly tracked data
#   - Statistical: This is the mathematical equivalent of adding about half of a death 
#                  to each of the countries with no-deaths for every 11 million citizens
#                  11 million people = population of Burundi; country with lowest deaths_per_capita (benchmakr for transformation)

######
# Five models to compare between each other with log-log model:
#     reg1: ln_deaths_capita = alpha + beta * ln_cases_capita
#     reg2: ln_deaths_capita = alpha + beta_1 * ln_cases_capita + beta_2 * ln_cases_capita^2
#     reg3: ln_deaths_capita = alpha + beta_1 * ln_cases_capita + beta_2 * ln_cases_capita^2 + beta_3 * ln_cases_capita^3
#     reg4: ln_deaths_capita = alpha + beta_1 * ln_cases_capita * 1(cases_capita < 0.01) + beta_2 * ln_cases_capita * 1(cases_capita >= 0.01)
#     reg5: ln_deaths_capita = alpha + beta_1 * ln_cases_capita * 1(cases_capita < 0.0002) + beta_2 * ln_cases_capita * 1(0.0002 <= cases_capita < 0.01) + beta_3 * ln_cases_capita * 1(cases_capita >= 0.01)
#     reg6: ln_deaths_capita = alpha + beta * ln_cases_capita, weights: population

###
# Two ways to handle polynomials: 
#
# 1) Add powers of the variable(s) to the dataframe:
df <- df %>% mutate( ln_cases_capita_sq = ln_cases_capita^2,
                     ln_cases_capita_cb = ln_cases_capita^3 )
#
# 2) Use 'poly(x,n)' function, which creates polynomials of x up to order n
#     use this approach for graphs! may use it for models: 
#                   positive - simpler, less new variables, 
#                   negative - uglier names, harder to compare
#     Note: poly() rotates your variables automatically to get mean independent variables (orthogonal polynomials; reduces correlation between polynomial variables)
#       use raw = TRUE if you don't want to rotate your variables.

# Built in regression in R
# reg_b <- lm( life_exp ~ ln_gdptot , data = df )
# reg_b
# summary( reg_b )
# formula: y ~ x1 + x2 + ..., note: intercept is automatically added
# drawback: no robust SE, only homoskedastic SEs...
# So instead of lm we use lm_robust from package estimatr

# first model
reg1 <- lm_robust( ln_deaths_capita ~ ln_cases_capita , data = df , se_type = "HC2" )
ggplot( data = df, aes( x = ln_cases_capita, y = ln_deaths_capita ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )
summary(reg1)

# second model
reg2 <- lm_robust( ln_deaths_capita ~ ln_cases_capita + ln_cases_capita_sq , data = df )
ggplot( data = df, aes( x = ln_cases_capita, y = ln_deaths_capita ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )
summary(reg2)

# third model
reg3 <- lm_robust( ln_deaths_capita ~ ln_cases_capita + ln_cases_capita_sq + ln_cases_capita_cb , data = df )
ggplot( data = df, aes( x = ln_cases_capita, y = ln_deaths_capita ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )
summary(reg3) # within a cubic regression, all 3 coefficients become statistically insignificant

# fourth model - regression with piecewise linear spline:
# 1st define the cutoff for cases_capita
cutoff <- 0.01
# 2nd we use a log transformation -> cutoff needs to be transformed as well
cutoff_ln <- log( cutoff )
# Use simple regression with the lspline function
reg4 <- lm_robust(ln_deaths_capita ~ lspline( ln_cases_capita , cutoff_ln ), data = df )
ggplot( data = df, aes( x = ln_cases_capita, y = ln_deaths_capita ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = 'red' )
summary( reg4 )
## the linear splines regression does not make sense in the case of setting a cutoff point that is so low as
## logs tend to make large differences much smaller and small differences much larger
# Thus, the

# fifth model - regression with piecewise linear spline:
# 1st define the cutoff for cases_capita
cutoff <- 0.01
cutoff2 <- 0.0002
# 2nd we use a log transformation -> cutoff needs to be transformed as well
cutoff_ln <- log( cutoff )
cutoff_ln2 <- log( cutoff2 )
# Use simple regression with the lspline function
reg5 <- lm_robust(ln_deaths_capita ~ lspline( ln_cases_capita , c(cutoff_ln,cutoff_ln2) ), data = df )
ggplot( data = df, aes( x = ln_cases_capita, y = ln_deaths_capita ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,c(cutoff_ln,cutoff_ln2)) , method = lm , color = 'red' )
summary( reg5 )

# sixth model - weighted OLS regression with population as weight
reg6 <- lm_robust(ln_deaths_capita ~ ln_cases_capita, data = df , weights = population)
summary( reg6 )

ggplot(data = df, aes(x = ln_cases_capita, y = ln_deaths_capita)) +
  geom_point(data = df, aes(size=population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='red') +
  scale_size(range = c(1, 15))

#####
# Creating model summary with texreg
data_out <- "C:/Users/cosmi/Documents/homework_codesANDmore_Coding1_MScBA/Task2_Cosmin-Covid-Assignment/output(s)/"
htmlreg( list(reg1 , reg2 , reg3 , reg4 , reg5 , reg6),
         type = 'html',
         custom.model.names = c("Linear",
                                "Quadratic",
                                "Cubic",
                                "PLS (1 knot)",
                                "PLS (2 knots)",
                                "Weight (population) linear"),
         caption = "Modelling Covid-19 cases and deaths per capita of countries",
         file = paste0( data_out ,'model_comparison_covid_regressions.html'), include.ci = FALSE)

######
# Based on model comparison the chosen model is reg6 - ln_deaths_capita = alpha + beta * ln_cases_capita, weights: population
#   Substantive: - 
#                - 
#   Statistical: - simple model, easy to interpret
#                - Occam's razor-friendly
#                - Comparatively high R2 and captures variation well