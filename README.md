# U.S. 2020 Presidential Election Popular Vote Prediction

## Introduction

This is a statistical report on predicting the popular vote in U.S. 2020 election for each candidate. We employ the post-stratification technique and incorporate variable age, gender, state, race, education, and income into our analysis.

## File Function

xxx.pdf is the final report of our prediction

xxx.Rmd is the code that helps us to generate the report

01-data_cleaning-survey1.R is the code that we used to clean the survey data for logistic model.

01-data_cleaning-post-strat1.R is the code that we used to clean the census data for post-stratification.

inputs folder stores the raw data we extracted from Democracy Fund + UCLA Nationscape and IPUMS USA

outputs folder stores the data after we cleaned the raw data. We have removed the data in both folders because of the license restriction. However, you are welcome to retrieve the data yourself at https://www.voterstudygroup.org/publication/nationscape-data-set and https://usa.ipums.org/usa/
