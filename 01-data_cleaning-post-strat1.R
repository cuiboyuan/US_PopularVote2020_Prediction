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
  select(statefip,
         sex, 
         age, 
         race, 
         educd,
         educ,
         inctot
         )
         

#### Here I am only splitting cells by age, gender, state, race, education, ###
#### and income ####

## Remove n/a data
reduced_data <- reduced_data %>% na.omit()
reduced_data <- reduced_data %>% filter(educd!="n/a")

## Convert age all to numeric
reduced_data$age <- 
  case_when(reduced_data$age == "less than 1 year old"~0,
            reduced_data$age == "90 (90+ in 1980 and 1990)"~90,
            TRUE~as.numeric(reduced_data$age))

reduced_data$age <- as.integer(reduced_data$age)


attach(reduced_data)

## Convert education to numeric value of education level
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

reduced_data <- reduced_data %>% mutate(education_level=edu_lvl)

## Convert income to numeric value of income level
inc_lvl <- case_when(inctot<15000~0,
                     inctot>=15000&inctot<20000~1,
                     inctot>=20000&inctot<25000~2,
                     inctot>=25000&inctot<30000~3,
                     inctot>=30000&inctot<35000~4,
                     inctot>=35000&inctot<40000~5,
                     inctot>=40000&inctot<45000~6,
                     inctot>=45000&inctot<50000~7,
                     inctot>=50000&inctot<55000~8,
                     inctot>=55000&inctot<60000~9,
                     inctot>=60000&inctot<65000~10,
                     inctot>=65000&inctot<70000~11,
                     inctot>=70000&inctot<75000~12,
                     inctot>=75000&inctot<80000~13,
                     inctot>=80000&inctot<85000~14,
                     inctot>=85000&inctot<90000~15,
                     inctot>=90000&inctot<95000~16,
                     inctot>=95000&inctot<100000~17,
                     inctot>=100000&inctot<125000~18,
                     inctot>=125000&inctot<150000~19,
                     inctot>=150000&inctot<175000~20,
                     inctot>=175000&inctot<200000~21,
                     inctot>=200000&inctot<250000~22,
                     inctot>=250000~23
                     )
reduced_data <- reduced_data %>% mutate(income_level=inc_lvl)

## Convert some race to other race, just to be consistent with the survey data
reduced_data$race <- case_when(race=="two major races"~"other race, nec",
                                           race=="three or more major races"~"other race, nec",
                                           TRUE~as.character(race))

detach(reduced_data)

## Rename some variable to be consistent with the survey data
reduced_data <- reduced_data %>% rename(state=statefip, gender=sex)

## Split into cells
reduced_data <- reduced_data %>%
  count(age, gender, state, race, education_level, income_level) %>%
  group_by(age, gender, state, race, education_level, income_level)


## Saving the census data as a csv file
write_csv(reduced_data, "outputs/census_data.csv")



         