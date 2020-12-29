# Preparations for analysis -----------------------------------------------

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
library(mfx)
library(margins)
library(pscl)
library(modelsummary)
library(stargazer)



# Call data and transform -------------------------------------------------

# github
df <- read_csv('')
