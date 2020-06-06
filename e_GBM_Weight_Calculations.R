# GBM
# Sept. 22, 2019

rm(list=ls())
library(twang)
library(gbm)

setwd("/Users/kristinadang/Box Sync/UCD Health Cognition/Data/ADC-CHIS_DATA")
df1<-read.csv("adc_chis2_am_09.csv") 

# Data management
#------------------
# Leave education, race, and language as categorical
# Check about ordered/factor Edu_harm1, 
# https://datascience.stackexchange.com/questions/14025/ordinal-feature-in-decision-tree
# Here is the factor version:
# df1$Edu_harm1_cat<-factor(df1$Edu_harm1)

# The twang package (page 3) had recommended to use
# categorical variables stored as factors or ordered,
# Here is the ordered version (note the structure of Edu_harm1_cat vs. Edu_harm1)
# Decided on Apr 6, 2019 that according to Twang docu and website, to keep it as ordered
df1$Edu_harm1<-ordered(df1$Edu_harm1)
df1$race<-factor(df1$race)
df1$language1<-factor(df1$language1)

# Chis as the outcome variable
# We can take the complement of this after the gbm to get the pp of ADC
df1$chis<-ifelse(df1$adc==1,0,1)

# GBM run
#------------
# Here we have 4 different runs (each time with the corresponding interaction depth)
# We are asking GBM to give us the predicted probability of chis based on all those covs
# 10,000 trees, shrinkage rate of 0.01, es.mean stop method, ATT estimand, 
# with sampling weights for each of the 4 runs
set.seed(140111)
fit1.ps <- ps(chis~race+male+Nage+diabetes+stroke+BMI+Cholesterol+HeartDz+CongestiveHeart+
                Edu_harm1+language1+marry_bi,
              data = df1, 
              n.trees = 10000,
              interaction.depth = 1, 
              shrinkage = 0.01,
              stop.method="es.mean",
              estimand = "ATT",
              sampw = df1$RAKEDW0)

set.seed(140111)
fit2.ps <- ps(chis~race+male+Nage+diabetes+stroke+BMI+Cholesterol+HeartDz+CongestiveHeart+
                Edu_harm1+language1+marry_bi,
              data = df1, 
              n.trees = 10000,
              interaction.depth = 2, 
              shrinkage = 0.01,
              stop.method="es.mean",
              estimand = "ATT",
              sampw = df1$RAKEDW0)

set.seed(140111)
fit3.ps <- ps(chis~race+male+Nage+diabetes+stroke+BMI+Cholesterol+HeartDz+CongestiveHeart+
                Edu_harm1+language1+marry_bi,
              data = df1, 
              n.trees = 10000,
              interaction.depth = 3, 
              shrinkage = 0.01,
              stop.method="es.mean",
              estimand = "ATT",
              sampw = df1$RAKEDW0)

set.seed(140111)
fit4.ps <- ps(chis~race+male+Nage+diabetes+stroke+BMI+Cholesterol+HeartDz+CongestiveHeart+
                Edu_harm1+language1+marry_bi,
              data = df1, 
              n.trees = 10000,
              interaction.depth = 4, 
              shrinkage = 0.01,
              stop.method="es.mean",
              estimand = "ATT",
              sampw = df1$RAKEDW0)



# Obtain propensity scores
# We get this by pulling the propensity score from each of the GBM objects
# We can get the predicted probability of ADC by taking the complement of 
# the predicted probability from CHIS
df1$pp1_CHIS <- fit1.ps$ps[["es.mean.ATT"]]
df1$pp1_ADC <- 1 - df1$pp1_CHIS

df1$pp2_CHIS <- fit2.ps$ps[["es.mean.ATT"]]
df1$pp2_ADC <- 1 - df1$pp2_CHIS

df1$pp3_CHIS <- fit3.ps$ps[["es.mean.ATT"]]
df1$pp3_ADC <- 1 - df1$pp3_CHIS

df1$pp4_CHIS <- fit4.ps$ps[["es.mean.ATT"]]
df1$pp4_ADC <- 1 - df1$pp4_CHIS


write.csv(df1, file="GBMpp_09_092219.csv")

# Check about ordered/factor Edu_harm1
# set.seed(140111)
# fit1.ps_check <- ps(chis~race+male+Nage+diabetes+stroke+BMI+Cholesterol+HeartDz+CongestiveHeart+
#                 Edu_harm1_cat+language1+marry_bi,
#               data = df1, 
#               n.trees = 10000,
#               interaction.depth = 1, 
#               shrinkage = 0.01,
#               stop.method="es.mean",
#               estimand = "ATT",
#               sampw = df1$RAKEDW0)
# 
# df1$pp1_CHIS_check <- fit1.ps_check$ps[["es.mean.ATT"]]
# df1$pp1_ADC_check <- 1 - df1$pp1_CHIS_check


