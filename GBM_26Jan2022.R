# Reweighting for Highly Selected Samples
# Gradient Boosted Models (GBM)
# Kristina Van Dang
# January 26, 2022

# This code produces the predicted probabilities of being in CHIS using GBM

rm(list=ls())

library(twang) # GBM; twang_2.5
library(gbm) # gbm_2.1.8

setwd("C:/Users/ehlarson/Box/KD_bootstrapping/hoffman_cluster/data")

df1<-read.csv("adc_chis2_am_09.csv") 

# ---- Data management -----
# The twang package (page 3) had recommended to use
# categorical variables stored as factors or ordered,

#EHL updating code to make race, education, and language factors with uniform references across algorithms
df1$Edu_harm1<-factor(df1$Edu_harm1, levels=c(3,0,1,2,4,5))
df1$race<-factor(df1$race, levels=c(3,1,2))
df1$language1<-factor(df1$language1, levels=c(1,2,3))

# Chis as the outcome variable
# We can take the complement of this after the gbm to get the pp of ADC
df1$chis<-ifelse(df1$adc==1,0,1)

# ---- GBM -----
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
              sampw = df1$RAKEDW0, version = "legacy")

set.seed(140111)
fit2.ps <- ps(chis~race+male+Nage+diabetes+stroke+BMI+Cholesterol+HeartDz+CongestiveHeart+
                Edu_harm1+language1+marry_bi,
              data = df1, 
              n.trees = 10000,
              interaction.depth = 2, 
              shrinkage = 0.01,
              stop.method="es.mean",
              estimand = "ATT",
              sampw = df1$RAKEDW0, version = "legacy")

set.seed(140111)
fit3.ps <- ps(chis~race+male+Nage+diabetes+stroke+BMI+Cholesterol+HeartDz+CongestiveHeart+
                Edu_harm1+language1+marry_bi,
              data = df1, 
              n.trees = 10000,
              interaction.depth = 3, 
              shrinkage = 0.01,
              stop.method="es.mean",
              estimand = "ATT",
              sampw = df1$RAKEDW0, version = "legacy")

set.seed(140111)
fit4.ps <- ps(chis~race+male+Nage+diabetes+stroke+BMI+Cholesterol+HeartDz+CongestiveHeart+
                Edu_harm1+language1+marry_bi,
              data = df1, 
              n.trees = 10000,
              interaction.depth = 4, 
              shrinkage = 0.01,
              stop.method="es.mean",
              estimand = "ATT",
              sampw = df1$RAKEDW0, version = "legacy")



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


write.csv(df1, file="C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_26Jan2022/results/GBM_09_26Jan2022.csv")




