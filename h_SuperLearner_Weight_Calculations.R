# Reweighting for Highly Selected Samples
# SuperLearner
# Kristina Van Dang
# January 26, 2022

# This code produces the predicted probabilities of being in ADC using SuperLearner
# *** This code includes the creation of second-order interaction terms
# and modified SL wrappers to control which interaction terms are selected 
# for glm and step algorithms ***

rm(list=ls())

library(SuperLearner) # SuperLearner_2.0-28
library(arm) # arm_1.11-2
library(gam) # gam_1.20
library(earth) # earth_5.3.1
library(tidyverse)
library(dplyr)

setwd("C:/Users/ehlarson/Box/KD_bootstrapping/hoffman_cluster/data")

df1<-read.csv("adc_chis2_am_09.csv")  

# ---- Data management -----
createInteractions <- function(df1){
  df1 <- df1 %>% mutate(
    
    blackXmale = black*male,
    blackXNage = black*Nage,
    blackXdiabetes = black*diabetes,
    blackXstroke = black*stroke,
    blackXBMI = black*BMI,
    blackXCholesterol = black*Cholesterol,
    blackXHeartDz = black*HeartDz,
    blackXCongestiveHeart = black*CongestiveHeart,
    blackXedu1_noed = black*edu1_noed,
    blackXedu1_elem = black*edu1_elem,
    blackXedu1_somehs = black*edu1_somehs,
    # blackXedu1_hs = black*edu1_hs,
    blackXedu1_somecol = black*edu1_somecol,
    blackXedu1_colpstgrad = black*edu1_colpstgrad,
    # blackXlang1_eng = black*lang1_eng,
    # blackXlang1_span = black*lang1_span,
    # blackXlang1_both = black*lang1_both,
    blackXmarry_bi = black*marry_bi,
    
    hispanicXmale = hispanic*male,
    hispanicXNage = hispanic*Nage,
    hispanicXdiabetes = hispanic*diabetes,
    hispanicXstroke = hispanic*stroke,
    hispanicXBMI = hispanic*BMI,
    hispanicCholesterol = hispanic*Cholesterol,
    hispanicXHeartDz = hispanic*HeartDz,
    hispanicXCongestiveHeart = hispanic*CongestiveHeart,
    hispanicXedu1_noed = hispanic*edu1_noed,
    hispanicXedu1_elem = hispanic*edu1_elem,
    hispanicXedu1_somehs = hispanic*edu1_somehs,
    # hispanicXedu1_hs = hispanic*edu1_hs,
    hispanicXedu1_somecol = hispanic*edu1_somecol,
    hispanicXedu1_colpstgrad = hispanic*edu1_colpstgrad,
    # hispanicXlang1_eng = hispanic*lang1_eng,
    # hispanicXlang1_span = hispanic*lang1_span,
    # hispanicXlang1_both = hispanic*lang1_both,
    hispanicXmarry_bi = hispanic*marry_bi
  )
  return(df1)
}

# Create separate wrappers for glm and step that will take manually provided interactions
SL.glm.KVD.manual.2interaction <- function(Y, X, newX, family, obsWeights, model = TRUE, ...) 
{
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  X <- createInteractions(X)
  fit.glm <- glm(Y ~ ., data = X, family = family, weights = obsWeights, 
                 model = model)
  if (is.matrix(newX)) {
    
    newX = as.data.frame(newX)
    
  }
  newX <- createInteractions(newX)
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  class(fit) <- "SL.glm"
  out <- list(pred = pred, fit = fit)
  return(out)
}

SL.step.KVD.manual.2interaction <- function (Y, X, newX, family, direction = "both", trace = 0, 
                                             k = 2, ...) 
{
  X <- createInteractions(X)
  fit.glm <- glm(Y ~ ., data = X, family = family)
  fit.step <- step(fit.glm, direction = direction, trace = trace, 
                   k = k)
  newX <- createInteractions(newX)
  pred <- predict(fit.step, newdata = newX, type = "response")
  fit <- list(object = fit.step)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.step")
  return(out)
}

SL.step.forward.KVD.manual.2interaction <- function (Y, X, newX, family, direction = "forward", trace = 0, 
                                                     k = 2, ...) 
{
  X <- createInteractions(X)
  fit.glm <- glm(Y ~ ., data = X, family = family)
  fit.step <- step(glm(Y ~ 1, data = X, family = family), scope = formula(fit.glm), 
                   direction = direction, trace = trace, k = k)
  newX <- createInteractions(newX)
  pred <- predict(fit.step, newdata = newX, type = "response")
  fit <- list(object = fit.step)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.step")
  return(out)
}

df1$edu1_noed <- ifelse(df1$Edu_harm1==0, 1, 0)
df1$edu1_elem <- ifelse(df1$Edu_harm1==1, 1, 0)
df1$edu1_somehs <- ifelse(df1$Edu_harm1==2, 1, 0)
df1$edu1_hs <- ifelse(df1$Edu_harm1==3, 1, 0)
df1$edu1_somecol <- ifelse(df1$Edu_harm1==4, 1, 0)
df1$edu1_colpstgrad <- ifelse(df1$Edu_harm1==5, 1, 0)

df1$lang1_eng <- ifelse(df1$language1==1, 1, 0)
df1$lang1_span <- ifelse(df1$language1==2, 1, 0)
df1$lang1_both <- ifelse(df1$language1==3, 1, 0)

# Select just the variables we're going to use in SL
df2<-subset(df1, select=c("adc", 
                          "black", 
                          "hispanic", 
                          #"White", 
                          "male",
                          "Nage",
                          "diabetes", 
                          "stroke", 
                          "BMI", 
                          "Cholesterol",
                          "HeartDz",
                          "CongestiveHeart",
                          "edu1_noed",
                          "edu1_elem",
                          "edu1_somehs",
                          #"edu1_hs",
                          "edu1_somecol",
                          "edu1_colpstgrad",
                          #"lang1_eng",
                          "lang1_span",
                          "lang1_both",
                          "marry_bi",
                          "RAKEDW0"
                          
))

df3<-na.omit(df2) # Check (SL needs no NAs to run)

# Get data ready for SL
# Outcome
Y<-df3$adc
# SL needs only independent vars for the X input
X <- subset(df2, select= -c(adc, RAKEDW0))

# Updated library
SL.library <- c(
  "SL.glm.KVD.manual.2interaction",
  "SL.step.KVD.manual.2interaction",
  "SL.step.forward.KVD.manual.2interaction",
  "SL.earth", 
  "SL.gam", 
  "SL.nnet", 
  "SL.mean", 
  "SL.bayesglm"
  )

# ---- SuperLearner Run ----
set.seed(140111)
fit1<-SuperLearner(Y=Y, X=X, family=binomial(), SL.library = SL.library,
                   verbose=TRUE,
                   cvControl=list(stratifyCV=TRUE),
                   obsWeights = df3$RAKEDW0)

# Pull the predicted probability of ADC from the SL object
df1$pp_SL <- fit1$SL.predict

write.csv(df1, "C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_26Jan2022/results/SL_09_26Jan2022.csv")




