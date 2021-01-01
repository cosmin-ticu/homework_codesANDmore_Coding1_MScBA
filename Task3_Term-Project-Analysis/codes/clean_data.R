# Clear memory
rm(list=ls())

library(tidyverse)

# Call the data from github
w_dir <- "C:/Users/cosmi/Documents/homework_codesANDmore_Coding1_MScBA/Task3_Term-Project-Analysis/data"
df <- read.csv(paste0(w_dir,"/raw/cosmin_manufacturing_survey.csv"))

# Check data
glimpse( df )

# Change faulty name
colnames(df)[1] <- "environment"

# Split dataset into 2 parts for analysis: 1 for offices, 1 for other environments


# Create the office-side survey -------------------------------------------

office_survey <- df %>% filter(environment == "Office") %>% transmute(safe_office, office_hazards_reviewed, 
                                                                      office_familiar_risks = familiar_risks,
                                                                      office_safety_mission_engagement = safety_mission_engagement)
office_survey$yes_safe_office <- ifelse( office_survey$safe_office == "Yes", 1, 0) # binary variable for YES
office_survey$no_safe_office <- ifelse( office_survey$safe_office == "No", 1, 0) # binary variable for NO
# "no response" was left as the baseline
table(office_survey$yes_safe_office)
table(office_survey$no_safe_office)

# make binary outcome variable -> does not have "no response" observations; ideal for outcome
office_survey$office_safety_mission_engagement <- ifelse( office_survey$office_safety_mission_engagement == "Yes", 1, 0)
table(office_survey$office_safety_mission_engagement)

# make hazard review variable into binary -> does not have "no response" observations
office_survey$office_hazards_reviewed <- ifelse( office_survey$office_hazards_reviewed == "Yes", 1, 0)
table(office_survey$office_hazards_reviewed)

office_survey <- subset(office_survey, select = -safe_office)

table(office_survey$office_familiar_risks)

# While there is suggestions on the internet to convert the likert scale variable to a simple binary
# by combining the Very familiar and Familiar into 1 and the Not Familiar into 0 while dropping everything else,
# it seems too arbitrary to do so. Rather, I will create binary variables for each answer, while keeping the
# baseline at the "no response". I would rather not exclude the mere 5 "no response" observations, as they
# might be import in the outcome model.

office_survey$very_familiar_risks <- ifelse( office_survey$office_familiar_risks == "Very familiar", 1, 0)
office_survey$familiar_risks <- ifelse( office_survey$office_familiar_risks == "Familiar", 1, 0)
office_survey$somewhat_familiar_risks <- ifelse( office_survey$office_familiar_risks == "Somewhat familiar", 1, 0)
office_survey$not_familiar_risks <- ifelse( office_survey$office_familiar_risks == "Not familiar", 1, 0)

office_survey <- subset(office_survey, select = -office_familiar_risks)
office_survey <- mutate(office_survey, hazards_reviewed = office_hazards_reviewed,
            safety_mission_engagement = office_safety_mission_engagement) %>% 
  subset(select = -c(office_hazards_reviewed, office_safety_mission_engagement))

write.csv(office_survey, file = paste0(w_dir, "/clean/office_survey_data.csv"))


# Create the other environments side --------------------------------------

other_environments_survey <- df %>% filter(environment != "Office") %>% transmute(environment, routine_prepwork, rush_things, toolbox_talks)

table(other_environments_survey$routine_prepwork)
table(other_environments_survey$rush_things)
table(other_environments_survey$toolbox_talks)

# Create binary variables for environments; keep services as baseline
table(other_environments_survey$environment)

other_environments_survey$manufacturing <- ifelse( other_environments_survey$environment == "Manufacturing", 1, 0)
other_environments_survey$project <- ifelse( other_environments_survey$environment == "Project", 1, 0)

# Create binary outcome variable
other_environments_survey$routine_prepwork <- ifelse( other_environments_survey$routine_prepwork == "Yes", 1, 0)

# Create binary rush variable
other_environments_survey$rush_things <- ifelse( other_environments_survey$rush_things == "Yes", 1, 0)

# For the likert scale question of having toolbox talks, the "Never" answer will be keep as
# the baseline, while the rest of the answers will each have a respective binary variable
other_environments_survey$frequent_talks <- ifelse( other_environments_survey$toolbox_talks == "Frequently", 1, 0)
other_environments_survey$occasional_talks <- ifelse( other_environments_survey$toolbox_talks == "Sometimes", 1, 0)
other_environments_survey$very_frequent_talks <- ifelse( other_environments_survey$toolbox_talks == "Very frequently", 1, 0)
other_environments_survey$constant_talks <- ifelse( other_environments_survey$toolbox_talks == "Before ever job task", 1, 0)

other_environments_survey <- other_environments_survey %>% subset(select = -c(environment, toolbox_talks))

write.csv(other_environments_survey, file = paste0(w_dir, "/clean/manufacturing-project-services_data.csv"))

