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
# install.packages("caTools")
library(caTools)

# Call the data
w_dir <- "C:/Users/cosmi/Documents/homework_codesANDmore_Coding1_MScBA/Task3_Term-Project-Analysis/"
# non_office_OG <- read.csv(paste0(w_dir,"data/clean/manufacturing-project-services_data.csv"))
office_OG <- read.csv(paste0(w_dir,"data/clean/office_survey_data.csv"))

# Split office data into train and test samples for robustness check at the end
set.seed(123)
sample <- sample.split(office_OG, SplitRatio = 0.75)
office <- subset(office_OG, sample == TRUE)
office_test <- subset(office_OG, sample == FALSE)

# Call a function from a file:
source(paste0(w_dir,'/codes/sum_stat.R'))

# Make descriptive statistics for selected office variables
desc_stat_office <- sum_stat( office , var_names = c('yes_safe_office','no_safe_office','noresponse_safe_office','hazards','mission'),
                       stats = c('mean','median','min','max','sd') ) # likert scale variables are ignored for this
desc_office_risks <- table(office$office_familiar_risks)
# knitr::kable(desc_office_risks)

## In case the analysis is taken further on the non_office survey, below are summary statistics codes for the variables
# desc_stat_non_office <- sum_stat( non_office , var_names = c('routine','rush','manufacturing','project','services'),
#                               stats = c('mean','median','min','max','sd') ) # likert scale variables are ignored for this
# table(non_office$toolbox_talks)

# Descriptive statistics (bar charts) of variables of interest
ggplot(office) +
  geom_bar(aes(x = office_safety_mission_engagement))+ 
  theme_bw() +
  ggtitle('Distribution of safety mission engagement answers')+
  theme(plot.title = element_text(size = 16))
ggplot(office) +
  geom_bar(aes(x = safe_office))+ 
  theme_bw() +
  ggtitle('Distribution of office safety answers')+
  theme(plot.title = element_text(size = 16))
ggplot(office) +
  geom_bar(aes(x = office_familiar_risks))+ 
  theme_bw() +
  ggtitle('Distribution of office risks familiarity answers')+
  theme(plot.title = element_text(size = 16))
ggplot(office) +
  geom_bar(aes(x = office_hazards_reviewed))+ 
  theme_bw() +
  ggtitle('Distribution of office hazard review answers')+
  theme(plot.title = element_text(size = 16))

# Saturated (technically not because as.factor() wizardry ) LPM models office ----

# 1st model: safety mission engagement regressed on whether hazards have been reviewed
lpm1 <- lm( mission ~ hazards, data=office )
summary( lpm1, vcov=sandwich )

# Get the predicted values
office$pred1 <- predict( lpm1 )

# Compare hazard variable with predicted values and real outcomes
table(office$pred1, office$hazards)
table(office$mission, office$hazards)
table(office$mission, office$pred1)
# what the simple linear probability model does is that because 68 respondents who
# have not reviewed the office hazards also do not feel engaged in the safety mission,
# it just classifies all employees that have not reviewed the office hazards as
# not being engaged in the safety mission. While this is a naive model, it sets
# a statistically significant basis on which to create a richer model

# Use weights for prettier plot of first regression (important to see that distribution is heavily centered around "Yes" answers)
office<-office %>%
  group_by(mission, hazards) %>%
  mutate(weight = n())  %>%
  mutate(weight_2=(weight/1000))

ggplot(data = office) +
  geom_point( aes(x = hazards, y = pred1), size = 2, color="red", shape = 16) +
  geom_line(  aes(x = hazards, y = pred1), colour="red",  size = 0.7) +
  geom_point( aes(x = hazards, y = mission, size=weight_2), fill = "blue", color="blue",
              shape = 16, alpha=0.8, show.legend=F, na.rm=TRUE)  +
  labs(x = "Respondent has reviewed the hazards in the office", y = "Respondent feels engaged to corporate safety mission / Predicted probability of ")+
  coord_cartesian(xlim = c(0, 1), ylim=c(0,1)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,1))

# 2nd model: same as model 1 but with the office safety variable added
lpm2 <- lm( mission ~ hazards + as.factor(safe_office), data=office )
summary(lpm2, vcov=sandwich)

# 3rd model: same as model 1 but with the office risk familiarity variable added
lpm3 <- lm( mission ~ hazards + as.factor(office_familiar_risks), data=office )
summary(lpm3, vcov=sandwich)

# 4th model: same as model 3 but with the office safety variable added
lpm4 <- lm( mission ~ hazards + as.factor(office_familiar_risks) + as.factor(safe_office), data=office )
summary(lpm4, vcov=sandwich)

# Model appears fairly complete, however interpretation proves difficult with 2 factors
# The intercept becomes a combination of the 2 baseline variables, which are for
# respondents that are familiar with the risks but that also do not consider the office
# as safe.

# Create model comparison output file
stargazer(list(lpm1, lpm2, lpm3, lpm4), digits=3, out=paste(w_dir,"output(s)/training_lpm_with_dummies.html",sep=""))

# Check predicted probabilities: is there any interesting values?
# predicted probabilities
office$pred_lpm_4 <- predict(lpm4)
# Summary
summary(office$pred_lpm_4)
# Because we have an upper boundary that is larger than 1 for these values, we need to use
# a probit or a logit model for prediction. For the purpose of uncovering patterns of
# association, we can proceed with using this linear probability model

# Show the predicted probabilities' distribution
ggplot(data=office, aes(x=pred_lpm_4)) +
  geom_histogram( aes( y = ..density.. ), fill = 'navyblue', binwidth=0.02) +
  coord_cartesian(xlim = c(0, 1.2)) +
  labs(x = "Predicted probability of respondent being engaged in the safety mission",y = "Percent")

# We are interested in the top 10% and bottom 10% characteristics!
#   Is there any significant difference?

# Create bins which categorize the predicted values between 1-10
office <- office %>% 
  mutate(q10_pred_lpm_4 = ntile(pred_lpm_4, 10))

# Make summary statistics, using sum_stat for the bottom (q10_pred_lpm==1) 
#   and top 10% (q10_pred_lpm==10), using stats = c('mean','median','sd')

# Bottom 10% means lower probability (starting from 66%) of safety mission engagement
b1 <- office %>% filter( q10_pred_lpm_4 == 1 )
var_interest <- c('hazards',
                  'very_familiar_risks','familiar_risks','no_response_risks',
                  'somewhat_familiar_risks','not_familiar_risks',
                  'noresponse_safe_office','yes_safe_office','no_safe_office')
stat_interest <- c('mean','median','sd')
sum_stat(b1,var_interest,stat_interest,num_obs = F)
table(b1$office_familiar_risks)
table(b1$safe_office) # all the people that reported their office as not safe are in the bottom 10% of safety mission engagement

# Top 10% means high probability of safety mission engagement
t1 <- office %>% filter( q10_pred_lpm_4 == 10 )
sum_stat(t1,var_interest,stat_interest,num_obs = F)
table(t1$office_familiar_risks) # none of the employees that are not familiar with office risks are in the top 10% of safety mission engagement
table(t1$safe_office)

# Get rid of lpm models and residue
rm(b1, t1, lpm1, lpm2, lpm3, lpm4, stat_interest, var_interest)

# Probit and logit models office ----------------------------------------------

# Lets compare
# lpm versus logit and probit
# with all right-hand-side variables
# If comparing different estimation methods for the same model setup:
#   good practice to make a 'formula' variable!
model_formula <- formula( mission ~ hazards + as.factor(office_familiar_risks) + as.factor(safe_office), data=office )

# lpm (repeating the previous rich lpm regression)
lpm <-lm( model_formula , data=office)
summary(lpm, vcov=sandwich)

## 
# Logit coefficients
#   alternatively: familiy='binomial' automatically gives you logit, but not probit...
logit <- glm( model_formula , data=office, family=binomial(link="logit") )
summary(logit)
glance(logit)

# predicted probabilities 
office$pred_logit <- predict.glm(logit, type="response")
summary(office$pred_logit)

# Calculate logit marginal differences
logit_marg <- logitmfx( model_formula, data=office, atmean=FALSE, robust = T)
print(logit_marg)

##
# Probit coefficients
probit <- glm(model_formula, data=office, family=binomial(link="probit"))
summary(probit)
glance(probit)

# predicted probabilities 
office$pred_probit<- predict.glm(probit, type="response") 
summary(office$pred_probit)

# probit marginal differences
probit_marg <- probitmfx(model_formula, data=office, atmean=FALSE)
print(probit_marg)

###
# Creating a model summary output with msummary
cm <- c('(Intercept)' = 'Constant')
pmodels <- list(lpm, logit, logit_marg, probit, probit_marg)
msummary( pmodels ,
          fmt="%.3f",
          gof_omit = 'DF|Deviance|Log.Lik.|R2 Adj.|R2|PseudoR2',
          stars=c('*' = .05, '**' = .01),
          coef_rename = cm,
          output = paste0(w_dir,"output(s)/training_prob_models_coeff_with_marginal.html")
)

# adding pseudo R2 (not work for mfx)
glance_custom.glm <- function(x) data.frame(`PseudoR2` = pR2(x)["McFadden"])
msummary(list(lpm, logit, probit),
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01),
         coef_rename = cm,
         output = paste0(w_dir,"output(s)/training_prob_models_coeff_R2_no_marginal.html")
)

# Comparing predicted probabilities of logit and probit to LPM
ggplot(data = office) +
  geom_point(aes(x=pred_lpm_4, y=pred_probit, color="Probit"), size=1,  shape=16) +
  geom_point(aes(x=pred_lpm_4, y=pred_logit,  color="Logit"), size=1,  shape=16) +
  geom_line(aes(x=pred_lpm_4, y=pred_lpm_4,    color="45 degree line"), size=1) +
  labs(x = "Predicted probability of respondent safety mission engagement (LPM)", y="Predicted probability")+
  scale_y_continuous(expand = c(0.00,0.0), limits = c(0.5,1), breaks = seq(0,1,0.1)) +
  scale_x_continuous(expand = c(0.00,0.0), limits = c(0.5,1), breaks = seq(0,1,0.1)) +
  scale_color_manual(name = "", values=c("green", "red","blue"))+
  theme(legend.position=c(0.55,0.08),
        legend.direction = "horizontal",
        legend.text = element_text(size = 7))

####
# GOODNESS OF FIT
#

# Simple LPM model from first regression
ggplot(data = office,aes(x=pred1)) + 
  geom_histogram(data=subset(office[office$mission == 1, ]), 
                 aes(fill=as.factor(mission), color=as.factor(mission), y = (..count..)/sum(..count..)*100),
                 binwidth = 0.05, boundary=0, alpha=0.8) +
  geom_histogram(data=subset(office[office$mission == 0, ]), 
                 aes(fill=as.factor(mission), color=as.factor(mission), y = (..count..)/sum(..count..)*100), 
                 binwidth = 0.05, boundary=0, alpha=0) +
  scale_fill_manual(name="", values=c("0" = "white", "1" = "red"),labels=c("Not engaged in safety mission","Respondent engaged in safety mission")) +
  scale_color_manual(name="", values=c("0" = "blue", "1" = "red"),labels=c("Not engaged in safety mission","Respondent engaged in safety mission")) +
  ylab("Percent") +
  xlab("Fitted values") +
  scale_x_continuous(expand=c(0.01,0.01) ,limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0.00,0.00) ,limits = c(0,100), breaks = seq(0,100,20)) +
  theme(legend.position = c(0.3,0.9),
        legend.key.size = unit(x = 0.5, units = "cm"))

# LPM rich model
ggplot(data = office,aes(x=pred_lpm_4)) + 
  geom_histogram(data=subset(office[office$mission == 1, ]), 
                 aes(fill=as.factor(mission), color=as.factor(mission), y = (..count..)/sum(..count..)*100),
                 binwidth = 0.05, boundary=0, alpha=0.8) +
  geom_histogram(data=subset(office[office$mission == 0, ]), 
                 aes(fill=as.factor(mission), color=as.factor(mission), y = (..count..)/sum(..count..)*100), 
                 binwidth = 0.05, boundary=0, alpha=0) +
  scale_fill_manual(name="", values=c("0" = "white", "1" = "red"),labels=c("Not engaged in safety mission","Respondent engaged in safety mission")) +
  scale_color_manual(name="", values=c("0" = "blue", "1" = "red"),labels=c("Not engaged in safety mission","Respondent engaged in safety mission")) +
  ylab("Percent") +
  xlab("Fitted values") +
  scale_x_continuous(expand=c(0.01,0.01) ,limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0.00,0.00) ,limits = c(0,20), breaks = seq(0,20,4)) +
  theme(legend.position = c(0.3,0.9),
        legend.key.size = unit(x = 0.5, units = "cm"))

#####
# Summary statistics on predicted probabilities:
#
# TO DO:
#   Create a CONDITIONAL sum_stat on mission for:
#     "pred_lpm","pred_logit","pred_probit" 
#   use: "mean","median","min","max","sd"
#
ss_1 <- subset( office , office$mission==1 )
ss_0 <- subset( office , office$mission==0 )

ss_1s <- sum_stat(ss_1,c("pred_lpm_4","pred_logit","pred_probit"),
                  c("mean","median","min","max","sd"),num_obs = F)
ss_0s <- sum_stat(ss_0,c("pred_lpm_4","pred_logit","pred_probit"),
                  c("mean","median","min","max","sd"),num_obs = F)
ss_1s
ss_0s

###
# Bias and Calibration curve
#
# use the logit model
#
# bias = mean(prediction) - mean(actual)
mean_pred_logit <- mean( office$pred_logit )
bias <- mean_pred_logit - mean( office$mission )
# Not really biased... it is really tiny!


# Note dplyr:: is important to specify which package's 'select' is used!
actual_vs_predicted <- office %>%
  ungroup() %>% 
  dplyr::select(actual = mission, 
                predicted = pred_logit) 
num_groups <- 10

calibration_d <- actual_vs_predicted %>%
  mutate(predicted_score_group = dplyr::ntile(predicted, num_groups))%>%
  group_by(predicted_score_group) %>%
  dplyr::summarise(mean_actual = mean(actual), 
                   mean_predicted = mean(predicted), 
                   num_obs = n())

ggplot( calibration_d,aes(x = mean_actual, y = mean_predicted)) +
  geom_point( color='red', size=1.5, alpha=0.8) +
  geom_line(  color='red', size=1  , alpha=0.8) +
  geom_abline( intercept = 0, slope = 1, color='blue') +
  labs( x = "Actual event probability", y = "Predicted event probability") +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0.5,1), breaks = seq(0,1,0.1)) +
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0.5,1), breaks = seq(0,1,0.1))

# Run model of choice on test sample --------------------------------------

# Because the goal is to predict, the logit model is chosen to run the test sample
# and compare the findings to the training sample

logit_test <- glm( model_formula , data=office_test, family=binomial(link="logit") )
summary(logit_test)
glance(logit_test)

# predicted probabilities 
office_test$pred_logit <- predict.glm(logit_test, type="response")
summary(office_test$pred_logit)

# Calculate logit marginal differences
logit_marg_test <- logitmfx( model_formula, data=office_test, atmean=FALSE, robust = T)
print(logit_marg_test)

###
# Creating a model summary output with msummary
cm <- c('(Intercept)' = 'Constant')
msummary( list(logit_marg, logit_marg_test) ,
          fmt="%.3f",
          gof_omit = 'DF|Deviance|Log.Lik.|R2 Adj.|R2|PseudoR2',
          stars=c('*' = .05, '**' = .01),
          coef_rename = cm,
          output = paste0(w_dir,"output(s)/test_v_training_coeff_with_marginal.html")
)

# adding pseudo R2 (not work for mfx)
glance_custom.glm <- function(x) data.frame(`PseudoR2` = pR2(x)["McFadden"])
msummary(list(logit, logit_test),
         fmt="%.3f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01),
         coef_rename = cm,
         output = paste0(w_dir,"output(s)/test_v_training_coeff_R2_no_marginal.html")
)

###
# Bias and Calibration curve for the test sample
#
# using the logit model
#
# bias = mean(prediction) - mean(actual)
mean_pred_logit_test <- mean( office_test$pred_logit )
bias_test <- mean_pred_logit_test - mean( office_test$mission )
# Not really biased... it is really tiny!


# Note dplyr:: is important to specify which package's 'select' is used!
actual_vs_predicted_test <- office_test %>%
  ungroup() %>% 
  dplyr::select(actual = mission, 
                predicted = pred_logit) 
num_groups <- 10

calibration_d_test <- actual_vs_predicted_test %>%
  mutate(predicted_score_group = dplyr::ntile(predicted, num_groups))%>%
  group_by(predicted_score_group) %>%
  dplyr::summarise(mean_actual = mean(actual), 
                   mean_predicted = mean(predicted), 
                   num_obs = n())

ggplot( calibration_d_test,aes(x = mean_actual, y = mean_predicted)) +
  geom_point( color='red', size=1.5, alpha=0.8) +
  geom_line(  color='red', size=1  , alpha=0.8) +
  geom_abline( intercept = 0, slope = 1, color='blue') +
  labs( x = "Actual event probability", y = "Predicted event probability") +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0.5,1), breaks = seq(0,1,0.1)) +
  scale_y_continuous(expand = c(0.01,0.01), limits = c(0.5,1), breaks = seq(0,1,0.1))

####
# checking the goodness of fit of the logit model on the test data
#
ggplot(data = office_test,aes(x=pred_logit)) + 
  geom_histogram(data=subset(office_test[office_test$mission == 1, ]), 
                 aes(fill=as.factor(mission), color=as.factor(mission), y = (..count..)/sum(..count..)*100),
                 binwidth = 0.05, boundary=0, alpha=0.8) +
  geom_histogram(data=subset(office_test[office_test$mission == 0, ]), 
                 aes(fill=as.factor(mission), color=as.factor(mission), y = (..count..)/sum(..count..)*100), 
                 binwidth = 0.05, boundary=0, alpha=0) +
  scale_fill_manual(name="", values=c("0" = "white", "1" = "red"),labels=c("Not engaged in safety mission","Respondent engaged in safety mission")) +
  scale_color_manual(name="", values=c("0" = "blue", "1" = "red"),labels=c("Not engaged in safety mission","Respondent engaged in safety mission")) +
  ylab("Percent") +
  xlab("Fitted values") +
  scale_x_continuous(expand=c(0.01,0.01) ,limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_y_continuous(expand=c(0.00,0.00) ,limits = c(0,100), breaks = seq(0,100,20)) +
  theme(legend.position = c(0.3,0.9),
        legend.key.size = unit(x = 0.5, units = "cm"))

#####
# Summary statistics on predicted probabilities from the test sample:
#
ss_1_test <- subset( office_test , office_test$mission==1 )
ss_0_test <- subset( office_test , office_test$mission==0 )

ss_1s_test <- sum_stat(ss_1_test,"pred_logit",
                  c("mean","median","min","max","sd"),num_obs = F)
ss_0s_test <- sum_stat(ss_0_test,"pred_logit",
                  c("mean","median","min","max","sd"),num_obs = F)
ss_1s_test
ss_0s_test



