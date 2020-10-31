#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voterstudygroup.org/publication/nationscape-data-set
# Author: Boyuan Cui, Zhaocheng Li
# Data: October 31, 2020
# Contact: 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from VoterStudyGroup and save to the folder inputs/data 


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("inputs/survey_data/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?
reduced_data <- reduced_data %>% na.omit()

reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))

reduced_data<-
  reduced_data %>%
  mutate(vote_biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0))

attach(reduced_data)
race <- case_when(race_ethnicity=="White"~"white",
                  race_ethnicity=="Black, or African American"~"black/african american/negro",
                  race_ethnicity=="American Indian or Alaska Native"~"american indian or alaska native",
                  race_ethnicity=="Asian (Chinese)"~"chinese",
                  race_ethnicity=="Asian (Japanese)"~"japanese",
                  race_ethnicity=="Asian (Asian Indian)"~"other asian or pacific islander",
                  race_ethnicity=="Asian (Filipino)"~"other asian or pacific islander",
                  race_ethnicity=="Asian (Korean)"~"other asian or pacific islander",
                  race_ethnicity=="Asian (Vietnamese)"~"other asian or pacific islander",
                  race_ethnicity=="Asian (Other)"~"other asian or pacific islander",
                  race_ethnicity=="Asian (Native Hawaiian)"~"other asian or pacific islander",
                  race_ethnicity=="Asian (Guamanian)"~"other asian or pacific islander",
                  race_ethnicity=="Asian (Samoan)"~"other asian or pacific islander",
                  race_ethnicity=="Asian (Other)"~"other asian or pacific islander",
                  race_ethnicity=="Some other race"~"other race, nec"
                  )
reduced_data$race_ethnicity <- race

edu_lvl <- case_when(education=="3rd Grade or less"~0,
                     education=="Middle School - Grades 4 - 8"~1,
                     education=="Completed some high school "~2,
                     education=="High school graduate"~3,
                     education=="Other post high school vocational training"~4,
                     education=="Completed some college, but no degree"~5,
                     education=="Associate Degree"~6,
                     education=="College Degree (such as B.A., B.S.)"~7,
                     education=="Completed some graduate, but no degree"~8,
                     education=="Masters degree"~9,
                     education=="Doctorate degree"~10
                     )
reduced_data <- reduced_data %>% mutate(edu_level=edu_lvl)

employ <- case_when(employment=="Full-time employed"~"employed",
                    employment=="Homemaker"~"not in labor force",
                    employment=="Retired"~"not in labor force",
                    employment=="Unemployed or temporarily on layoff"~"unemployed",
                    employment=="Part-time employed"~"employed",
                    employment=="Permanently disabled"~"not in labor force",
                    employment=="Student"~"not in labor force",
                    employment=="Self-employed"~"employed",
                    employment=="Other"~"not in labor force",
                    )
reduced_data$employment <- employ

inc_lvl <- as.numeric(household_income)-1
reduced_data <- reduced_data %>% mutate(income_level=inc_lvl)


detach(reduced_data)

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/survey_data.csv")

