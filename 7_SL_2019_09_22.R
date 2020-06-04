# Superlearner
# Sept. 22, 2019

rm(list=ls())
library(SuperLearner)
library(arm)
library(gam)
library(earth)

#------------------------
## DATA MANAGEMENT
#------------------------

setwd("/Users/kristinadang/Box Sync/UCD Health Cognition/Data/ADC-CHIS_DATA")
# df1<-read.csv("adc_chis2_am_08.csv")  
df1<-read.csv("adc_chis2_am_09.csv")  

# Create indicator variables for education and language
# Because we don't know how SL is treating categorical variables, we dummy them all out
# Just these ones need to be dummied out of the 09 dataset (not the case in previous versions)
df1$edu1_noed <- ifelse(df1$Edu_harm1==0, 1, 0)
df1$edu1_elem <- ifelse(df1$Edu_harm1==1, 1, 0)
df1$edu1_somehs <- ifelse(df1$Edu_harm1==2, 1, 0)
df1$edu1_hs <- ifelse(df1$Edu_harm1==3, 1, 0)
df1$edu1_somecol <- ifelse(df1$Edu_harm1==4, 1, 0)
df1$edu1_colpstgrad <- ifelse(df1$Edu_harm1==5, 1, 0)


df1$lang1_eng <- ifelse(df1$language1==1, 1, 0)
df1$lang1_span <- ifelse(df1$language1==2, 1, 0)
df1$lang1_both <- ifelse(df1$language1==3, 1, 0)

# Check the number of observations
# dim(df1)  # [1] 12552   119

# Select just the variables we're going to use in SL
# That means omitting all the raked weights (and only keeping RAKEDWO to weight the population)
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
                          "RAKEDW0"))

df3<-na.omit(df2) # Check (SL needs no NAs to run)
# table(df3$adc) # Check ADC=856 CHIS=12257

# Get data ready for SL
# Outcome
Y<-df3$adc
# SL needs only independent vars for the X input
X <- subset(df3, select= -c(adc, RAKEDW0))
# Take out the replicate weights we will include these in the weight option later

# Kara's library:
SL.library <- c("SL.glm", 
                "SL.glm.interaction", 
                "SL.step", 
                "SL.step.forward", 
                "SL.step.interaction", 
                "SL.earth", 
                "SL.gam", 
                "SL.nnet", 
                "SL.mean", 
                "SL.bayesglm")

set.seed(140111)
fit1<-SuperLearner(Y=Y, X=X, family=binomial(), SL.library = SL.library,
                   verbose=TRUE,
                   cvControl=list(stratifyCV=TRUE),
                   obsWeights = df3$RAKEDW0)
# cvControl ensures that there are ADC people in every fold during cross-validation
# weight it by RAKEDWO (obsWeights)

# Pull the predicted probability of ADC from the SL object
df1$pp_SL <- fit1$SL.predict

# Create weight--not sure if these numbers are right--I think Audrey was just using the pp?
#AM: Removing weight calculation - we do this at the beginning of SAS program 01_Prev-OR-PR-PD_Automized_AM_v2_ERM.sas in to be sure everything is standardized.
#df1$weight_SL <- ifelse(df1$adc==0, 0, ((1-df1$pp_SL)/df1$pp_SL)*(856/4230283))
# df1$weight_SL <- ifelse(df1$adc==0, 0, ((1-df1$pp_SL)/df1$pp_SL)*(852/4230283))
# df1$weight_SL <- ifelse(df1$adc==0, 0, ((1-df1$pp_SL)/df1$pp_SL)*(856/4231139)/(1-(856/4231139)))

write.csv(df1, file="SLweights_09_092219.csv")
