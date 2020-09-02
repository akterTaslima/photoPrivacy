library(ggpubr)
library(multcomp)
library(car)
library(FSA) 
library(ggplot2)
library(phia)
library(psych)
library(likert)
library(plyr)
library(tidyr)
#library(psycho)
library(emmeans)
library(lme4)
library(lmerTest)
#install.packages("devtools")
library(devtools)
library(sjmisc)
library(rsq)

data = read.csv("clean_data_gather_with_demo.csv",  stringsAsFactors=FALSE) [-c(1)]

data_demo = head(data, n=396)

data_demo$gender[data_demo$gender=="f" | data_demo$gender=="female" | data_demo$gender=="Female" |
                   data_demo$gender=="female " | data_demo$gender=="Cis female" | 
                   data_demo$gender=="FEMALE" | data_demo$gender=="woman" | data_demo$gender=="F" |
                   data_demo$gender=="Female " | data_demo$gender==" Female"] <- "Female"

data_demo$gender[data_demo$gender=="m" | data_demo$gender=="male" | data_demo$gender=="Male" |
                   data_demo$gender=="M" | data_demo$gender=="Demiboy" | 
                   data_demo$gender=="Male " | data_demo$gender=="man"|
                   data_demo$gender=="M A L E" | data_demo$gender=="MAN" |
                   data_demo$gender=="maile" | data_demo$gender=="ale" |
                   data_demo$gender=="Male." | data_demo$gender=="he/they" |
                   data_demo$gender=="MALE"] <- "Male"

data_demo$age[data_demo$age=="2"] <- "18-29"
data_demo$age[data_demo$age=="3"] <- "30-49"
data_demo$age[data_demo$age=="4"] <- "50-64"
data_demo$age[data_demo$age=="5"] <- "65+"


data_demo$education[data_demo$education=="2"] <- "high_school"
data_demo$education[data_demo$education=="3"] <- "undergrad"
data_demo$education[data_demo$education=="4"] <- "masters"
data_demo$education[data_demo$education=="5"] <- "professional"


data_demo$political_affiliation[data_demo$political_affiliation=="1"] <- "Democrat"
data_demo$political_affiliation[data_demo$political_affiliation=="2"] <- "Independent"
data_demo$political_affiliation[data_demo$political_affiliation=="3"] <- "Republican"
data_demo$political_affiliation[data_demo$political_affiliation=="4"] <- "Else"
data_demo$political_affiliation[data_demo$political_affiliation=="5"] <- "Dont_know"
data_demo$political_affiliation[data_demo$political_affiliation=="6"] <- "Other"

data_demo$race[data_demo$race=="6"] <- "white"
data_demo$race[data_demo$race=="1"] <- "hispanic/latino"
data_demo$race[data_demo$race=="3"] <- "asian"
data_demo$race[data_demo$race=="4"] <- "black/african"

data_demo$freq_sns[data_demo$freq_sns=="1"] <- "never"
data_demo$freq_sns[data_demo$freq_sns=="2"] <- "yearly"
data_demo$freq_sns[data_demo$freq_sns=="3"] <- "monthly"
data_demo$freq_sns[data_demo$freq_sns=="4"] <- "weekly"
data_demo$freq_sns[data_demo$freq_sns=="5"] <- "once_day"
data_demo$freq_sns[data_demo$freq_sns=="6"] <- "multiple_day"

age_count = table(data_demo$age)
age_percentage <- age_count / sum(age_count)
print(age_percentage)

gender_count = table(data_demo$gender)
gender_percentage <- gender_count / sum(gender_count)
print(gender_percentage)

education_count = table(data_demo$education)
education_percentage <- education_count / sum(education_count)
print(education_percentage)

race_count = table(data_demo$race)
race_percentage <- race_count / sum(race_count)
print(race_percentage)

political_affiliation_count = table(data_demo$political_affiliation)
political_affiliation_percentage <- political_affiliation_count / sum(political_affiliation_count)
print(political_affiliation_percentage)

freq_sns_count = table(data_demo$freq_sns)
freq_sns_percentage <- freq_sns_count / sum(freq_sns_count)
print(freq_sns_percentage)
