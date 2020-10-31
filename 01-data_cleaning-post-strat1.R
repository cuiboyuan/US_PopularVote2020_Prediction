#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://usa.ipums.org/usa/index.shtml
# Author: Boyuan Cui, Zhaocheng Li
# Data: October 31, 2020
# Contact: 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the IPUMS USA data and saved it to inputs/census_data


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
raw_data <- read_dta("inputs/census_data/usa_00001.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(#region,
         #stateicp,
         sex, 
         age, 
         race, 
         #hispan,
         #marst, 
         #bpl,
         #citizen,
         educd,
         educ,
         inctot,
         empstat
         )
         #labforce,
         #labforce)
         

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)


reduced_data <- reduced_data %>% na.omit()

reduced_data <- reduced_data %>% filter(educd!="n/a" & empstat!="n/a")


reduced_data <- 
  reduced_data %>%
  count(age) %>%
  group_by(age) 
typeof(reduced_data$age)

reduced_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)")

reduced_data$age <- as.integer(reduced_data$age)





attach(reduced_data)

edu_lvl <- as.numeric(educd) %>% as.integer()
tmp <- data.frame(name=as.character(educd),edu_lvl)
tmp %>% group_by(name) %>% summarise(n=mean(edu_lvl))

edu_lvl <- replace(edu_lvl, edu_lvl>=43, 10)
edu_lvl <- replace(edu_lvl, edu_lvl<=42, 9)
edu_lvl <- replace(edu_lvl, edu_lvl<=41, )
edu_lvl <- replace(edu_lvl, edu_lvl<=36, 7)
edu_lvl <- replace(edu_lvl, edu_lvl<=31, 6)



detach(reduced_data)


# Saving the census data as a csv file
write_csv(reduced_data, "outputs/census_data.csv")



         