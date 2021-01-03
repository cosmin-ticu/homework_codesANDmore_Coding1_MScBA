# Clear memory
rm(list=ls())

library(tidyverse)

# Call the data
w_dir <- "C:/Users/cosmi/Documents/homework_codesANDmore_Coding1_MScBA/Task3_Term-Project-Analysis/data"
df <- read.csv(paste0(w_dir,"/raw/cosmin_manufacturing_survey.csv"))

# Check data
glimpse( df )

# Change faulty name
colnames(df)[1] <- "environment"

# Split dataset into 2 parts for analysis: 1 for offices, 1 for other environments
# Dummy variables will be created for each factor for drawing later summary statistics from
# predicted values and their quartiles. This is because we will be interested in knowing
# the characteristics of the bottom 10% and top 10% of respondents in terms of
# safety mission engagement feeling


# Create the office-side survey -------------------------------------------

office_survey <- df %>% filter(environment == "Office") %>% transmute(safe_office, office_hazards_reviewed, 
                                                                      office_familiar_risks = familiar_risks,
                                                                      office_safety_mission_engagement = safety_mission_engagement)
office_survey$yes_safe_office <- ifelse( office_survey$safe_office == "Yes", 1, 0) # binary variable for YES
office_survey$no_safe_office <- ifelse( office_survey$safe_office == "No", 1, 0) # binary variable for NO
office_survey$noresponse_safe_office <- ifelse( office_survey$safe_office == "No response", 1, 0) # binary variable for No Response
# Glance at distributions between responses; heavy skew towards "yes"
table(office_survey$yes_safe_office)
table(office_survey$no_safe_office)
table(office_survey$noresponse_safe_office)

# make binary outcome variable -> does not have "no response" observations; ideal for outcome
office_survey$mission <- ifelse( office_survey$office_safety_mission_engagement == "Yes", 1, 0)
table(office_survey$mission)

# make hazard review variable into binary -> does not have "no response" observations
office_survey$hazards <- ifelse( office_survey$office_hazards_reviewed == "Yes", 1, 0)
table(office_survey$hazards)

# office_survey <- subset(office_survey, select = -safe_office) # not necessary, can keep to use as.factor()

table(office_survey$office_familiar_risks)

# While there are suggestions on the internet to convert the likert scale variable to a simple binary
# by combining the Very familiar and Familiar into 1 and the Not Familiar into 0 while dropping everything else,
# it seems too arbitrary to do so. Rather, I will create binary variables for each answer. 
# I would rather not exclude the mere 5 "no response" observations, as they
# might be important in the outcome model.

office_survey$very_familiar_risks <- ifelse( office_survey$office_familiar_risks == "Very familiar", 1, 0)
office_survey$familiar_risks <- ifelse( office_survey$office_familiar_risks == "Familiar", 1, 0)
office_survey$somewhat_familiar_risks <- ifelse( office_survey$office_familiar_risks == "Somewhat familiar", 1, 0)
office_survey$not_familiar_risks <- ifelse( office_survey$office_familiar_risks == "Not familiar", 1, 0)
office_survey$no_response_risks <- ifelse( office_survey$office_familiar_risks == "No response", 1, 0)

# office_survey <- subset(office_survey, select = -office_familiar_risks) # not necessary, can keep to use as.factor()
# office_survey <- subset(office_survey, select = -c(office_hazards_reviewed, office_safety_mission_engagement)) # keep for bar charts

write.csv(office_survey, file = paste0(w_dir, "/clean/office_survey_data.csv"), row.names = F)


# Create the other environments side --------------------------------------

other_environments_survey <- df %>% filter(environment != "Office") %>% transmute(environment, routine_prepwork, rush_things, toolbox_talks)

# Glance at distribution of answers for each variable
table(other_environments_survey$routine_prepwork)
table(other_environments_survey$rush_things)
table(other_environments_survey$toolbox_talks)
table(other_environments_survey$environment) # fair distribution throughout

# Create binary variables for environments; do not keep anything as baseline; will use the as.factor()
other_environments_survey$manufacturing <- ifelse( other_environments_survey$environment == "Manufacturing", 1, 0)
other_environments_survey$project <- ifelse( other_environments_survey$environment == "Project", 1, 0)
other_environments_survey$services <- ifelse( other_environments_survey$environment == "Services", 1, 0)

# Create binary outcome variable
other_environments_survey$routine <- ifelse( other_environments_survey$routine_prepwork == "Yes", 1, 0)

# Create binary rush variable
other_environments_survey$rush <- ifelse( other_environments_survey$rush_things == "Yes", 1, 0)

# For the likert scale question of having toolbox talks, each answer will have a respective binary variable
other_environments_survey$frequent_talks <- ifelse( other_environments_survey$toolbox_talks == "Frequently", 1, 0)
other_environments_survey$occasional_talks <- ifelse( other_environments_survey$toolbox_talks == "Sometimes", 1, 0)
other_environments_survey$very_frequent_talks <- ifelse( other_environments_survey$toolbox_talks == "Very frequently", 1, 0)
other_environments_survey$constant_talks <- ifelse( other_environments_survey$toolbox_talks == "Before ever job task", 1, 0)
other_environments_survey$never_talks <- ifelse( other_environments_survey$toolbox_talks == "Never", 1, 0)

# other_environments_survey <- other_environments_survey %>% subset(select = -c(routine_prepwork, rush_things)) # keep for bar charts

write.csv(other_environments_survey, file = paste0(w_dir, "/clean/manufacturing-project-services_data.csv"), row.names = F)

