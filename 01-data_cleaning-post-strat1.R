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
         stateicp,
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
  replace(age == "less than 1 year old", 0) %>%
  replace(age == "90 (90+ in 1980 and 1990)", 90)

reduced_data$age <- as.integer(reduced_data$age)

attach(reduced_data)

edu_lvl <- as.numeric(educd) %>% as.integer()

edu_lvl <- replace(edu_lvl, edu_lvl>=43, 1000)
edu_lvl <- replace(edu_lvl, edu_lvl<=42 & edu_lvl>36, 900)
edu_lvl <- replace(edu_lvl, edu_lvl<=36 & edu_lvl>31, 700)
edu_lvl <- replace(edu_lvl, edu_lvl<=31 & edu_lvl>29, 600)
edu_lvl <- replace(edu_lvl, edu_lvl<=29 & edu_lvl>26, 500)
edu_lvl <- replace(edu_lvl, edu_lvl==26, 400)
edu_lvl <- replace(edu_lvl, edu_lvl<=25 & edu_lvl>23, 300)
edu_lvl <- replace(edu_lvl, edu_lvl<=23 & edu_lvl>18, 200)
edu_lvl <- replace(edu_lvl, edu_lvl<=18 & edu_lvl>10, 100)
edu_lvl <- replace(edu_lvl, edu_lvl<=10, 0)
edu_lvl <- edu_lvl/100

edu_lvl

reduced_data <- reduced_data %>% mutate(education_level=edu_lvl)

detach(reduced_data)


reduced_data <- 
  reduced_data %>%
  count(age) %>%
  group_by(age)

# Saving the census data as a csv file
write_csv(reduced_data, "outputs/census_data.csv")



         