# Reweighting for Highly Selected Samples
# Logistic Regression
# Kristina Van Dang
# January 26, 2022
# Script name "Logistic_26Jan2022.R"

# This code produces the predicted probabilities of being in ADC using logistic regression

rm(list=ls())

library(Hmisc) # logistic regression; Hmisc_4.6-0
library(rms) # restricted cubic splines; rms_6.2-0 
library(tidyverse) 
library(dplyr)

setwd("C:/Users/ehlarson/Box/KD_bootstrapping/hoffman_cluster/data")
dir()

df1<-read.csv("adc_chis2_am_09.csv") 


#EHL adding code to make race, education, and language factors
df1$Edu_harm1<-factor(df1$Edu_harm1, levels=c(3,0,1,2,4,5))
df1$race<-factor(df1$race, levels=c(3,1,2))
df1$language1<-factor(df1$language1, levels=c(1,2,3))

# ---- Logistic model ----
m1 <- glm(adc ~ race + male + Edu_harm1 + Cholesterol + diabetes +
            rcs(Nage,knots = c(61,71,84)) + 
            rcs(BMI,knots = c(20.99,25.76,33.31)) +
            race*male + race*Edu_harm1 + race*Cholesterol + race*diabetes +
            race*(rcs(Nage,knots = c(61,71,84))) + 
            race*(rcs(BMI,knots = c(20.99,25.76,33.31))), 
          data=df1, family=binomial(link=logit), 
          weights = df1$RAKEDW0)

df1$pp_logistic <- predict.glm(m1, type="response")

write.csv(df1, file="C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_26Jan2022/results/Logistic_09_26Jan2022.csv")
