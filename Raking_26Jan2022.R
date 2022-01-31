# Reweighting for Highly Selected Samples
# Raking
# Kristina Van Dang
# January 26, 2022

# This code produces the weights of being in ADC using raking

rm(list=ls())

library(weights) # Raking; weights_1.0.4
library(survey) # Raking; survey_4.1-1
library(dplyr)

setwd("C:/Users/ehlarson/Box/KD_bootstrapping/hoffman_cluster/data")
dir()

df1<-read.csv("adc_chis2_am_09.csv")  

# ---- Data management -----
# Need to create categorical variables because raking doesn't work on continuous vars
# BMI categories
df1$bmilt25 <- ifelse(df1$BMI < 25, 1, 0)
df1$bmi2530 <- ifelse(df1$BMI >= 25 & df1$BMI < 30, 1, 0)
df1$bmige30 <- ifelse(df1$BMI >= 30, 1, 0)

# Age categories 
# <70, 70-80, 80+
df1$agelt70 <- ifelse(df1$Nage < 70, 1, 0)
df1$age7080 <- ifelse(df1$Nage >= 70 & df1$Nage < 80 , 1, 0)
df1$agege80 <- ifelse(df1$Nage >= 80, 1, 0)

# Expand dataset
df.expanded <- df1[rep(row.names(df1), df1$RAKEDW0), 1:114] 

chis<-df.expanded[df.expanded$adc==0, ]   #4622757
adc<-df.expanded[df.expanded$adc==1, ]    #856

# We need to obtain the marginal probabilities of CHIS and rake ADC to those
# For the marginal probabilities, CHIS
table(chis$race) # 2285557  828777 3508423
wpct(chis$male) # 0.5550859 0.4449141
wpct(chis$diabetes) # 0.8380248 0.1619752
wpct(chis$Cholesterol) # 0.7122377 0.2877623

wpct(chis$agelt70) # 0.5351118 0.4648882
wpct(chis$age7080) # 0.6723903 0.3276097
wpct(chis$agege80) # 0.7924979 0.2075021

wpct(chis$bmilt25) # 0.6027671 0.3972329
wpct(chis$bmi2530) # 0.6132905 0.3867095
wpct(chis$bmige30) # 0.7839424 0.2160576

table(chis$Edu_harm1) # 62599  389523  356556 1352941 1081071 1380067

# ---- Raking ----
# Un-raked dataset set-up 
# Creates a survey design object wo any weights
# ids argument means the data came all from one single primary sampling unit
adc.svy.unweighted <- svydesign(ids=~1, data=adc)

# Variables to be raked ( we need their marginal probabilities from CHIS)
# Compute weights based on the distribution of sex, education, age, and race
# Each dataframe consists of two vectors: one describing the level of the associated factor
# and the other the corresponding frequencies
# We multiply the theoretically known relative frequencies from CHIS with the number of rows in ADC

# Sociodemographic factors
race.dist <- data.frame(race = c("1", "2", "3"),
                        Freq = nrow(adc) * c(285557/4622757, 828777/4622757, 3508423/4622757))
male.dist <- data.frame(male = c("0", "1"),
                        Freq = nrow(adc) * c(0.5550859, 0.4449141))
agelt70.dist <- data.frame(agelt70 = c("0", "1"),
                           Freq = nrow(adc) * c(0.5351118, 0.4648882 ))
age7080.dist <- data.frame(age7080 = c("0", "1"),
                           Freq = nrow(adc) * c(0.6723903, 0.3276097 ))
agege80.dist <- data.frame(agege80 = c("0", "1"),
                           Freq = nrow(adc) * c(0.7924979, 0.2075021 ))
Edu_harm1.dist <- data.frame(Edu_harm1 = c("0", "1", "2", "3", "4", "5"),
                             Freq = nrow(adc) * c(62599/4622757, 389523/4622757, 356556/4622757, 1352941/4622757, 1081071/4622757, 1380067/4622757))

# Other covariates that had poor balance to rake on
diabetes.dist <- data.frame(diabetes = c("0", "1"),
                            Freq = nrow(adc) * c(0.8380248, 0.1619752))
Cholesterol.dist <- data.frame(Cholesterol = c("0", "1"),
                               Freq = nrow(adc) * c(0.7122377, 0.2877623))

bmilt25.dist <- data.frame(bmilt25 = c("0", "1"),
                           Freq = nrow(adc) * c(0.6027671, 0.3972329 ))
bmi2530.dist <- data.frame(bmi2530 = c("0", "1"),
                           Freq = nrow(adc) * c(0.6132905, 0.3867095 ))
bmige30.dist <- data.frame(bmige30 = c("0", "1"),
                           Freq = nrow(adc) * c(0.7839424, 0.2160576 ))


# Get the raked weights 
adc.svy.rake <- rake(design = adc.svy.unweighted,
                     sample.margins = list(~male, ~race,
                                           ~agelt70, ~age7080, ~agege80,
                                           ~Edu_harm1, ~diabetes, ~Cholesterol,
                                           ~bmilt25, ~bmi2530, ~bmige30),
                     population.margins = list(male.dist, race.dist,
                                               agelt70.dist, age7080.dist, agege80.dist,
                                               Edu_harm1.dist, diabetes.dist, Cholesterol.dist,
                                               bmilt25.dist, bmi2530.dist, bmige30.dist)
)

# Collect the weights from adc.svy.rake object and add them to our dataset (ADC)
raked.weight <- adc.svy.rake$postStrata[[1]][[1]] %>% attributes() %>%. [["weights"]]
adc$raked.weight <- raked.weight

# Write CSV file
write.csv(adc, file="C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_26Jan2022/results/Raking_09_26Jan2022.csv")


