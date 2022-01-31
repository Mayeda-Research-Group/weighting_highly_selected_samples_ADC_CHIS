# Data management script

#---- load packages ----
library(tidyverse)
library(dplyr)

#---- read in data ----
df1 <- read_csv(paste0("/Users/ehlarson/Box/KD_bootstrapping/", 
                       "hoffman_cluster/data/adc_chis2_am_09.csv"))

#---- age/sex standardization ----
#Create age categories (60-<65, 65-<70, 70-<75, 75-<80, 80-<85)
df1 <- df1 %>% mutate(agecat_h = 0,
                      agecat_h = replace(agecat_h, 60 <= Nage & Nage < 65, 1),
                      agecat_h = replace(agecat_h, 65 <= Nage & Nage < 70, 2),
                      agecat_h = replace(agecat_h, 70 <= Nage & Nage < 75, 3),
                      agecat_h = replace(agecat_h, 75 <= Nage & Nage < 80, 4),
                      agecat_h = replace(agecat_h, 80 <= Nage & Nage < 85, 5),
                      agecat_h = replace(agecat_h, 85 <= Nage, 6))


df1 <- df1 %>% mutate(agecat_h2 = 0,
                      agecat_h2 = replace(agecat_h2, 60 <= Nage & Nage < 70, 1),
                      agecat_h2 = replace(agecat_h2, 70 <= Nage & Nage < 80, 2),
                      agecat_h2 = replace(agecat_h2, 80 <= Nage, 3))

# #Define age/sex distribution of standard population 
# #(full CHIS sample with CA weights--RAKEDW0)
# chis <- df1 %>% filter(adc == 0) 
# chis_total <- sum(chis$RAKEDW0)
# agesex_totals <- chis %>% group_by(agecat_h, male) %>% 
#   summarise(n = sum(RAKEDW0))
# agesex_totals$prop_agesex<-agesex_totals$n/chis_total

# #Define age/sex distribution of ADC (unweighted)
# adc<-df1 %>% filter(adc==1)
# adc_total<-nrow(adc)
# agesex_totals_a<-adc %>% group_by(agecat_h, male) %>% summarise(n = n())
# agesex_totals_a$prop_agesex<-agesex_totals_a$n/adc_total  

#---- Prep for bootstrap ---- 
# use df1, add variables for raking, GBM, SL that will need to be used in the 
#   bootstrap

#---- **RAKING ----
# Need to create categorical variables because raking doesn't work on cont vars
# BMI categories
df1$bmilt25 <- ifelse(df1$BMI < 25, 1, 0)
df1$bmi2530 <- ifelse(df1$BMI >= 25 & df1$BMI < 30, 1, 0)
df1$bmige30 <- ifelse(df1$BMI >= 30, 1, 0)

#---- **age categories ----
# <70, 70-80, 80+
df1$agelt70 <- ifelse(df1$Nage < 70, 1, 0)
df1$age7080 <- ifelse(df1$Nage >= 70 & df1$Nage < 80 , 1, 0)
df1$agege80 <- ifelse(df1$Nage >= 80, 1, 0)

# #---- **Sociodemographic factors ----
# race.dist <- data.frame(race = c("1", "2", "3"),
#                         Freq = nrow(adc) * c(285557/4622757, 828777/4622757, 3508423/4622757))
# male.dist <- data.frame(male = c("0", "1"),
#                         Freq = nrow(adc) * c(0.5550859, 0.4449141))
# agelt70.dist <- data.frame(agelt70 = c("0", "1"),
#                            Freq = nrow(adc) * c(0.5351118, 0.4648882 ))
# age7080.dist <- data.frame(age7080 = c("0", "1"),
#                            Freq = nrow(adc) * c(0.6723903, 0.3276097 ))
# agege80.dist <- data.frame(agege80 = c("0", "1"),
#                            Freq = nrow(adc) * c(0.7924979, 0.2075021 ))
# Edu_harm1.dist <- data.frame(Edu_harm1 = c("0", "1", "2", "3", "4", "5"),
#                              Freq = nrow(adc) * c(62599/4622757, 389523/4622757, 356556/4622757, 1352941/4622757, 1081071/4622757, 1380067/4622757))
# 
# # Other covariates that had poor balance to rake on
# diabetes.dist <- data.frame(diabetes = c("0", "1"),
#                             Freq = nrow(adc) * c(0.8380248, 0.1619752))
# Cholesterol.dist <- data.frame(Cholesterol = c("0", "1"),
#                                Freq = nrow(adc) * c(0.7122377, 0.2877623))
# 
# bmilt25.dist <- data.frame(bmilt25 = c("0", "1"),
#                            Freq = nrow(adc) * c(0.6027671, 0.3972329 ))
# bmi2530.dist <- data.frame(bmi2530 = c("0", "1"),
#                            Freq = nrow(adc) * c(0.6132905, 0.3867095 ))
# bmige30.dist <- data.frame(bmige30 = c("0", "1"),
#                            Freq = nrow(adc) * c(0.7839424, 0.2160576 ))

#---- **GBM ----
# The twang package (page 3) had recommended to use
# categorical variables stored as factors or ordered,
# Here is the ordered version (note the structure of Edu_harm1_cat vs. Edu_harm1)
# Decided on Apr 6, 2019 that according to Twang docu and website, to keep it as 
#   ordered
df1$Edu_harm1 <- ordered(df1$Edu_harm1)
df1$race <- factor(df1$race)
df1$language1 <- factor(df1$language1)

# Chis as the outcome variable
# We can take the complement of this after the gbm to get the pp of ADC
df1$chis <- ifelse(df1$adc == 1, 0, 1)

#---- **SUPERLEARNER ----
# Create indicator variables for education and language
# Because we don't know how SL is treating categorical variables, we dummy them 
#   all out
# Just these ones need to be dummied out of the 09 dataset 
#   (not the case in previous versions)
df1$edu1_noed <- ifelse(df1$Edu_harm1 == 0, 1, 0)
df1$edu1_elem <- ifelse(df1$Edu_harm1 == 1, 1, 0)
df1$edu1_somehs <- ifelse(df1$Edu_harm1 == 2, 1, 0)
df1$edu1_hs <- ifelse(df1$Edu_harm1 == 3, 1, 0)
df1$edu1_somecol <- ifelse(df1$Edu_harm1 == 4, 1, 0)
df1$edu1_colpstgrad <- ifelse(df1$Edu_harm1 == 5, 1, 0)

df1$lang1_eng <- ifelse(df1$language1 == 1, 1, 0)
df1$lang1_span <- ifelse(df1$language1 == 2, 1, 0)
df1$lang1_both <- ifelse(df1$language1 == 3, 1, 0)

#---- write out clean data ----
write_csv(df1, paste0("/Users/ehlarson/Box/KD_bootstrapping/", 
                 "hoffman_cluster/data/adc_chis2_am_09_clean.csv"))
