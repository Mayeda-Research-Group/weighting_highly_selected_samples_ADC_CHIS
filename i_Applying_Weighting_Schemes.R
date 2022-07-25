# Reweighting for Highly Selected Samples
# Combining the 4 weighting schemes
# Kristina Van Dang
# January 26, 2022
# Script name "Combining_schemes_09_05May2022.R"

# This code produces the estimates from logistic, raking, SL, and GBM

rm(list=ls())

library(tidyverse)
library(dplyr)
library(Hmisc) # logistic
library(rms) # logistic, restricted cubic splines
library(weights) # raking
library(survey) # raking
library(SuperLearner) # SuperLearner
library(arm) # SuperLearner
library(gam) # SuperLearner
library(earth) # SuperLearner
library(twang) # GBM
library(gbm) # GBM
library(boot) # bootstrapping CIs

#setwd("/Users/kristinadang/Box Sync/KD_bootstrapping/hoffman_cluster/data")
 setwd("C:/Users/ehlarson/Box/KD_bootstrapping/hoffman_cluster/data")

df1 <- read.csv("adc_chis2_am_09.csv")  
chis <- df1 %>% filter(adc==0)
p_adc <- sum(df1$adc)/sum(chis$RAKEDW0)

# ---- For Age/Sex Standardization ----

#Create age categories (60-<65, 65-<70, 70-<75, 75-<80, 80-<85)
df1 <- df1 %>% mutate(agecat_h = 0,
                      agecat_h = replace(agecat_h, 60 <= Nage & Nage < 65, 1),
                      agecat_h = replace(agecat_h, 65 <= Nage & Nage < 70, 2),
                      agecat_h = replace(agecat_h, 70 <= Nage & Nage < 75, 3),
                      agecat_h = replace(agecat_h, 75 <= Nage & Nage < 80, 4),
                      agecat_h = replace(agecat_h, 80 <= Nage & Nage < 85, 5),
                      agecat_h = replace(agecat_h, 85 <= Nage, 6), 
                      
                      agecat_h2 = 0,
                      agecat_h2 = replace(agecat_h2, 60 <= Nage & Nage < 70, 1),
                      agecat_h2 = replace(agecat_h2, 70 <= Nage & Nage < 80, 2),
                      agecat_h2 = replace(agecat_h2, 80 <= Nage, 3))

#Define age/sex distribution of standard population (full CHIS sample with CA weights--RAKEDW0)
chis <- df1 %>% filter(adc==0) 
chis_total <- sum(chis$RAKEDW0)
agesex_totals <- chis %>% group_by(agecat_h2, male) %>% 
  summarise(n = sum(RAKEDW0))
agesex_totals$prop_agesex<-agesex_totals$n/chis_total

#Define age/sex distribution of ADC (unweighted)
adc<-df1 %>% filter(adc==1)
adc_total<-nrow(adc)
agesex_totals_a<-adc %>% group_by(agecat_h2, male) %>% summarise(n = n())
agesex_totals_a$prop_agesex<-agesex_totals_a$n/adc_total  

# ---- Pull in weighting schemes ----
# Create IOSW weights (for weighting) and 5-year age categories (for standardization)
#l <- read.csv("/Users/kristinadang/Box Sync/KD_bootstrapping/Scripts_26Jan2022/results/Logistic_09_26Jan2022.csv") %>%
 l <- read.csv("C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_26Jan2022/results/Logistic_09_26Jan2022.csv") %>%
  mutate(agecat_h = 0,
         agecat_h = replace(agecat_h, 60 <= Nage & Nage < 65, 1),
         agecat_h = replace(agecat_h, 65 <= Nage & Nage < 70, 2),
         agecat_h = replace(agecat_h, 70 <= Nage & Nage < 75, 3),
         agecat_h = replace(agecat_h, 75 <= Nage & Nage < 80, 4),
         agecat_h = replace(agecat_h, 80 <= Nage & Nage < 85, 5),
         agecat_h = replace(agecat_h, 85 <= Nage, 6), 
         
         agecat_h2 = 0,
         agecat_h2 = replace(agecat_h2, 60 <= Nage & Nage < 70, 1),
         agecat_h2 = replace(agecat_h2, 70 <= Nage & Nage < 80, 2),
         agecat_h2 = replace(agecat_h2, 80 <= Nage, 3),
         
         IOSW_l = ifelse(adc==0, 0, ((1-pp_logistic)/pp_logistic)*(p_adc)/(1-p_adc)))

#r <- read.csv("/Users/kristinadang/Box Sync/KD_bootstrapping/Scripts_26Jan2022/results/Raking_09_26Jan2022.csv") %>%
 r <- read.csv("C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_26Jan2022/results/Raking_09_26Jan2022.csv") %>%
  mutate(agecat_h = 0,
         agecat_h = replace(agecat_h, 60 <= Nage & Nage < 65, 1),
         agecat_h = replace(agecat_h, 65 <= Nage & Nage < 70, 2),
         agecat_h = replace(agecat_h, 70 <= Nage & Nage < 75, 3),
         agecat_h = replace(agecat_h, 75 <= Nage & Nage < 80, 4),
         agecat_h = replace(agecat_h, 80 <= Nage & Nage < 85, 5),
         agecat_h = replace(agecat_h, 85 <= Nage, 6), 
         
         agecat_h2 = 0,
         agecat_h2 = replace(agecat_h2, 60 <= Nage & Nage < 70, 1),
         agecat_h2 = replace(agecat_h2, 70 <= Nage & Nage < 80, 2),
         agecat_h2 = replace(agecat_h2, 80 <= Nage, 3))

#g <- read.csv("/Users/kristinadang/Box Sync/KD_bootstrapping/Scripts_26Jan2022/results/GBM_09_26Jan2022.csv") %>%
 g <- read.csv("C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_26Jan2022/results/GBM_09_26Jan2022.csv") %>%
  mutate(agecat_h = 0,
         agecat_h = replace(agecat_h, 60 <= Nage & Nage < 65, 1),
         agecat_h = replace(agecat_h, 65 <= Nage & Nage < 70, 2),
         agecat_h = replace(agecat_h, 70 <= Nage & Nage < 75, 3),
         agecat_h = replace(agecat_h, 75 <= Nage & Nage < 80, 4),
         agecat_h = replace(agecat_h, 80 <= Nage & Nage < 85, 5),
         agecat_h = replace(agecat_h, 85 <= Nage, 6), 
         
         agecat_h2 = 0,
         agecat_h2 = replace(agecat_h2, 60 <= Nage & Nage < 70, 1),
         agecat_h2 = replace(agecat_h2, 70 <= Nage & Nage < 80, 2),
         agecat_h2 = replace(agecat_h2, 80 <= Nage, 3),
         
         IOSW_g = ifelse(adc==0, 0, ((1-pp2_ADC)/pp2_ADC)*(p_adc)/(1-p_adc)))

#s <- read.csv("/Users/kristinadang/Box Sync/KD_bootstrapping/Scripts_26Jan2022/results/SL_09_26Jan2022.csv") %>%
 s <- read.csv("C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_26Jan2022/results/SL_09_26Jan2022.csv") %>%
  mutate(agecat_h = 0,
         agecat_h = replace(agecat_h, 60 <= Nage & Nage < 65, 1),
         agecat_h = replace(agecat_h, 65 <= Nage & Nage < 70, 2),
         agecat_h = replace(agecat_h, 70 <= Nage & Nage < 75, 3),
         agecat_h = replace(agecat_h, 75 <= Nage & Nage < 80, 4),
         agecat_h = replace(agecat_h, 80 <= Nage & Nage < 85, 5),
         agecat_h = replace(agecat_h, 85 <= Nage, 6), 
         
         agecat_h2 = 0,
         agecat_h2 = replace(agecat_h2, 60 <= Nage & Nage < 70, 1),
         agecat_h2 = replace(agecat_h2, 70 <= Nage & Nage < 80, 2),
         agecat_h2 = replace(agecat_h2, 80 <= Nage, 3),
         
         IOSW_s = ifelse(adc==0, 0, ((1-pp_SL)/pp_SL)*(p_adc)/(1-p_adc)))

# Only need to keep ADC
# Raked weights are for ADC only already
adc_g <- g %>% filter(adc==1)
adc_s <- s %>% filter(adc==1)

# ---- Unweighted estimates ----
# Naive ADC estimates:
# Overall:
est_phyp_unw <- adc_s %>% 
  summarise(phyp = mean(Hypertension))
est_phyp_unw

# By race:
est_phyp_race_unw <- adc_s %>% group_by(race) %>%
  summarise(phyp_race = mean(Hypertension)) %>%
  group_by(race) %>%
  summarise(est_prop_unw = mean(phyp_race))
est_phyp_race_unw

#Inequalities:


#EHL edit 5/18 adding unstandardized inequality estimates
    est_PRs_unw_unstd <- est_phyp_race_unw$est_prop_unw[1:2]/est_phyp_race_unw$est_prop_unw[3]  
    est_PRs_unw_unstd
      
    est_PDs_unw_unstd <- est_phyp_race_unw$est_prop_unw[1:2]-est_phyp_race_unw$est_prop_unw[3]  
    est_PDs_unw_unstd
      
    est_ORs_unw_unstd <- (est_phyp_race_unw$est_prop_unw[1:2]/(1-est_phyp_race_unw$est_prop_unw[1:2]))/(est_phyp_race_unw$est_prop_unw[3]/(1-est_phyp_race_unw$est_prop_unw[3]))
    est_ORs_unw_unstd

#End EHL edit 5/18



# get race, age, and sex-specific estimates of the mean
hyp_race_unw <- adc_s %>%
  group_by(race, agecat_h2, male) %>%
  summarise(phyp_race = mean(Hypertension))
hyp_race_unw

# merge on standard population distribution
hyp_race_unw_stds <- hyp_race_unw %>%
  group_by(race, agecat_h2, male) %>%
  summarise(est_prop_unw = mean(phyp_race)) %>%
  left_join(., agesex_totals_a, by=c("agecat_h2", "male"))
hyp_race_unw_stds

# take sum-product of estimated prev and std population for each race/ethnicity
est_phyp_race_unw_std <- hyp_race_unw_stds %>% group_by(race) %>%
  summarise(est_prop_unw_std = crossprod(est_prop_unw, prop_agesex))
est_phyp_race_unw_std

# Estimate inequalities
est_PRs_unw_std <- est_phyp_race_unw_std$est_prop_unw_std[1:2]/est_phyp_race_unw_std$est_prop_unw_std[3]
est_PRs_unw_std

est_PDs_unw_std<- est_phyp_race_unw_std$est_prop_unw_std[1:2]-est_phyp_race_unw_std$est_prop_unw_std[3]
est_PDs_unw_std

est_ORs_unw_std <- (est_phyp_race_unw_std$est_prop_unw_std[1:2]/(1-est_phyp_race_unw_std$est_prop_unw_std[1:2]))/(est_phyp_race_unw_std$est_prop_unw_std[3]/(1-est_phyp_race_unw_std$est_prop_unw_std[3]))
est_ORs_unw_std

#EHL edit 5/18 adding ADC inequalities standardized to CHIS population
    # merge on CHIS standard population distribution
    hyp_race_unw_stds_chispop <- hyp_race_unw %>%
      group_by(race, agecat_h2, male) %>%
      summarise(est_prop_unw = mean(phyp_race)) %>%
      left_join(., agesex_totals, by=c("agecat_h2", "male"))
    hyp_race_unw_stds_chispop
    
    # take sum-product of estimated prev and std population for each race/ethnicity
    est_phyp_race_unw_std_chispop <- hyp_race_unw_stds_chispop %>% group_by(race) %>%
      summarise(est_prop_unw_std = crossprod(est_prop_unw, prop_agesex))
    est_phyp_race_unw_std_chispop
    
    # Estimate inequalities
    est_PRs_unw_std_chispop <- est_phyp_race_unw_std_chispop$est_prop_unw_std[1:2]/est_phyp_race_unw_std_chispop$est_prop_unw_std[3]
    est_PRs_unw_std_chispop
    
    est_PDs_unw_std_chispop <- est_phyp_race_unw_std_chispop$est_prop_unw_std[1:2]-est_phyp_race_unw_std_chispop$est_prop_unw_std[3]
    est_PDs_unw_std_chispop
    
    est_ORs_unw_std_chispop <- (est_phyp_race_unw_std_chispop$est_prop_unw_std[1:2]/(1-est_phyp_race_unw_std_chispop$est_prop_unw_std[1:2]))/(est_phyp_race_unw_std_chispop$est_prop_unw_std[3]/(1-est_phyp_race_unw_std_chispop$est_prop_unw_std[3]))
    est_ORs_unw_std_chispop

#End EHL edit 5/18



# ---- Weighted estimates ----
# ---- *LOGISTIC ----
# Overall
est_phyp_wtd_l <- l %>% 
  summarise(weighted_phyp = weighted.mean(Hypertension, IOSW_l)) %>% 
  summarise(mean(weighted_phyp))
est_phyp_wtd_l

# By race
est_phyp_race_wtd_l <- l %>% group_by(race) %>% 
  summarise(phyp_race = weighted.mean(Hypertension, IOSW_l)) %>% 
  group_by(race) %>% 
  summarise(est_prop_wtd=mean(phyp_race))
est_phyp_race_wtd_l


#EHL edit 5/18 adding unstandardized inequality estimates
    est_PRs_wtd_unstd_l <- est_phyp_race_wtd_l$est_prop_wtd[1:2]/est_phyp_race_wtd_l$est_prop_wtd[3]  
    est_PRs_wtd_unstd_l
    
    est_PDs_wtd_unstd_l <- est_phyp_race_wtd_l$est_prop_wtd[1:2]-est_phyp_race_wtd_l$est_prop_wtd[3]  
    est_PDs_wtd_unstd_l
    
    est_ORs_wtd_unstd_l <- (est_phyp_race_wtd_l$est_prop_wtd[1:2]/(1-est_phyp_race_wtd_l$est_prop_wtd[1:2]))/(est_phyp_race_wtd_l$est_prop_wtd[3]/(1-est_phyp_race_wtd_l$est_prop_wtd[3]))
    est_ORs_wtd_unstd_l
    
#End EHL edit 5/18

#Inequalities standardized to age/sex
hyp_race_wtd_std_l <- l %>% group_by(race, agecat_h2, male) %>% 
  summarise(phyp_race = weighted.mean(Hypertension, IOSW_l)) %>% 
  group_by(race, agecat_h2, male) %>% 
  summarise(est_prop_wtd=mean(phyp_race)) %>% 
  left_join(.,agesex_totals, by=c("agecat_h2", "male"))
hyp_race_wtd_std_l

est_phyp_race_wtd_std_l <- hyp_race_wtd_std_l %>% group_by(race) %>% 
  summarise(est_prop_wtd_std = crossprod(est_prop_wtd, prop_agesex))
est_phyp_race_wtd_std_l

est_PRs_wtd_std_l <- est_phyp_race_wtd_std_l$est_prop_wtd_std[1:2]/est_phyp_race_wtd_std_l$est_prop_wtd_std[3]  
est_PRs_wtd_std_l

est_PDs_wtd_std_l <- est_phyp_race_wtd_std_l$est_prop_wtd_std[1:2]-est_phyp_race_wtd_std_l$est_prop_wtd_std[3]  
est_PDs_wtd_std_l

est_ORs_wtd_std_l <- (est_phyp_race_wtd_std_l$est_prop_wtd_std[1:2]/(1-est_phyp_race_wtd_std_l$est_prop_wtd_std[1:2]))/(est_phyp_race_wtd_std_l$est_prop_wtd_std[3]/(1-est_phyp_race_wtd_std_l$est_prop_wtd_std[3]))
est_ORs_wtd_std_l


# ---- *RAKING ----
# Overall
est_phyp_wtd_r <- r %>% 
  summarise(weighted_phyp = weighted.mean(Hypertension, raked.weight)) %>% 
  summarise(mean(weighted_phyp))
est_phyp_wtd_r

# By race
est_phyp_race_wtd_r <- r %>% group_by(race) %>% 
  summarise(phyp_race = weighted.mean(Hypertension, raked.weight)) %>% 
  group_by(race) %>% 
  summarise(est_prop_wtd=mean(phyp_race))
est_phyp_race_wtd_r


#EHL edit 5/18 adding unstandardized inequality estimates
    est_PRs_wtd_unstd_r <- est_phyp_race_wtd_r$est_prop_wtd[1:2]/est_phyp_race_wtd_r$est_prop_wtd[3]  
    est_PRs_wtd_unstd_r
  
    est_PDs_wtd_unstd_r <- est_phyp_race_wtd_r$est_prop_wtd[1:2]-est_phyp_race_wtd_r$est_prop_wtd[3]  
    est_PDs_wtd_unstd_r
  
    est_ORs_wtd_unstd_r <- (est_phyp_race_wtd_r$est_prop_wtd[1:2]/(1-est_phyp_race_wtd_r$est_prop_wtd[1:2]))/(est_phyp_race_wtd_r$est_prop_wtd[3]/(1-est_phyp_race_wtd_r$est_prop_wtd[3]))
    est_ORs_wtd_unstd_r

#End EHL edit 5/18

#Inequalities standardized to age/sex
hyp_race_wtd_std_r <- r %>% group_by(race, agecat_h2, male) %>% 
  summarise(phyp_race = weighted.mean(Hypertension, raked.weight)) %>% 
  group_by(race, agecat_h2, male) %>% 
  summarise(est_prop_wtd=mean(phyp_race)) %>% 
  left_join(.,agesex_totals, by=c("agecat_h2", "male"))
hyp_race_wtd_std_r

est_phyp_race_wtd_std_r <- hyp_race_wtd_std_r %>% group_by(race) %>% 
  summarise(est_prop_wtd_std = crossprod(est_prop_wtd, prop_agesex))
est_phyp_race_wtd_std_r

est_PRs_wtd_std_r <- est_phyp_race_wtd_std_r$est_prop_wtd_std[1:2]/est_phyp_race_wtd_std_r$est_prop_wtd_std[3]  
est_PRs_wtd_std_r

est_PDs_wtd_std_r <- est_phyp_race_wtd_std_r$est_prop_wtd_std[1:2]-est_phyp_race_wtd_std_r$est_prop_wtd_std[3]  
est_PDs_wtd_std_r

est_ORs_wtd_std_r <- (est_phyp_race_wtd_std_r$est_prop_wtd_std[1:2]/(1-est_phyp_race_wtd_std_r$est_prop_wtd_std[1:2]))/(est_phyp_race_wtd_std_r$est_prop_wtd_std[3]/(1-est_phyp_race_wtd_std_r$est_prop_wtd_std[3]))
est_ORs_wtd_std_r

# ---- *GBM ----
# Overall
est_phyp_wtd_g <- adc_g %>% 
  summarise(weighted_phyp = weighted.mean(Hypertension, IOSW_g)) %>% 
  summarise(mean(weighted_phyp))
est_phyp_wtd_g

# By race
est_phyp_race_wtd_g <- adc_g %>% group_by(race) %>% 
  summarise(phyp_race = weighted.mean(Hypertension, IOSW_g)) %>% 
  group_by(race) %>% 
  summarise(est_prop_wtd=mean(phyp_race))
est_phyp_race_wtd_g


#EHL edit 5/18 adding unstandardized inequality estimates
    est_PRs_wtd_unstd_g <- est_phyp_race_wtd_g$est_prop_wtd[1:2]/est_phyp_race_wtd_g$est_prop_wtd[3]  
    est_PRs_wtd_unstd_g
    
    est_PDs_wtd_unstd_g <- est_phyp_race_wtd_g$est_prop_wtd[1:2]-est_phyp_race_wtd_g$est_prop_wtd[3]  
    est_PDs_wtd_unstd_g
    
    est_ORs_wtd_unstd_g <- (est_phyp_race_wtd_g$est_prop_wtd[1:2]/(1-est_phyp_race_wtd_g$est_prop_wtd[1:2]))/(est_phyp_race_wtd_g$est_prop_wtd[3]/(1-est_phyp_race_wtd_g$est_prop_wtd[3]))
    est_ORs_wtd_unstd_g

#End EHL edit 5/18

#Inequalities standardized to age/sex
hyp_race_wtd_std_g <- adc_g %>% group_by(race, agecat_h2, male) %>% 
  summarise(phyp_race = weighted.mean(Hypertension, IOSW_g)) %>% 
  group_by(race, agecat_h2, male) %>% 
  summarise(est_prop_wtd=mean(phyp_race)) %>% 
  left_join(.,agesex_totals, by=c("agecat_h2", "male"))
hyp_race_wtd_std_g

est_phyp_race_wtd_std_g <- hyp_race_wtd_std_g %>% group_by(race) %>% 
  summarise(est_prop_wtd_std = crossprod(est_prop_wtd, prop_agesex))
est_phyp_race_wtd_std_g

est_PRs_wtd_std_g <- est_phyp_race_wtd_std_g$est_prop_wtd_std[1:2]/est_phyp_race_wtd_std_g$est_prop_wtd_std[3]  
est_PRs_wtd_std_g

est_PDs_wtd_std_g <- est_phyp_race_wtd_std_g$est_prop_wtd_std[1:2]-est_phyp_race_wtd_std_g$est_prop_wtd_std[3]  
est_PDs_wtd_std_g

est_ORs_wtd_std_g <- (est_phyp_race_wtd_std_g$est_prop_wtd_std[1:2]/(1-est_phyp_race_wtd_std_g$est_prop_wtd_std[1:2]))/(est_phyp_race_wtd_std_g$est_prop_wtd_std[3]/(1-est_phyp_race_wtd_std_g$est_prop_wtd_std[3]))
est_ORs_wtd_std_g

# ---- *SUPERLEARNER ----
# Overall
est_phyp_wtd_s <- adc_s %>% 
  summarise(weighted_phyp = weighted.mean(Hypertension, IOSW_s)) %>% 
  summarise(mean(weighted_phyp))
est_phyp_wtd_s

# By race
est_phyp_race_wtd_s <- adc_s %>% group_by(race) %>% 
  summarise(phyp_race = weighted.mean(Hypertension, IOSW_s)) %>% 
  group_by(race) %>% 
  summarise(est_prop_wtd=mean(phyp_race))
est_phyp_race_wtd_s

#EHL edit 5/18 adding unstandardized inequality estimates
    est_PRs_wtd_unstd_s <- est_phyp_race_wtd_s$est_prop_wtd[1:2]/est_phyp_race_wtd_s$est_prop_wtd[3]  
    est_PRs_wtd_unstd_s
  
    est_PDs_wtd_unstd_s <- est_phyp_race_wtd_s$est_prop_wtd[1:2]-est_phyp_race_wtd_s$est_prop_wtd[3]  
    est_PDs_wtd_unstd_s
  
    est_ORs_wtd_unstd_s <- (est_phyp_race_wtd_s$est_prop_wtd[1:2]/(1-est_phyp_race_wtd_s$est_prop_wtd[1:2]))/(est_phyp_race_wtd_s$est_prop_wtd[3]/(1-est_phyp_race_wtd_s$est_prop_wtd[3]))
    est_ORs_wtd_unstd_s

#End EHL edit 5/18

#Inequalities standardized to age/sex
hyp_race_wtd_std_s <- adc_s %>% group_by(race, agecat_h2, male) %>% 
  summarise(phyp_race = weighted.mean(Hypertension, IOSW_s)) %>% 
  group_by(race, agecat_h2, male) %>% 
  summarise(est_prop_wtd=mean(phyp_race)) %>% 
  left_join(.,agesex_totals, by=c("agecat_h2", "male"))
hyp_race_wtd_std_s

est_phyp_race_wtd_std_s <- hyp_race_wtd_std_s %>% group_by(race) %>% 
  summarise(est_prop_wtd_std = crossprod(est_prop_wtd, prop_agesex))
est_phyp_race_wtd_std_s

est_PRs_wtd_std_s <- est_phyp_race_wtd_std_s$est_prop_wtd_std[1:2]/est_phyp_race_wtd_std_s$est_prop_wtd_std[3]  
est_PRs_wtd_std_s

est_PDs_wtd_std_s <- est_phyp_race_wtd_std_s$est_prop_wtd_std[1:2]-est_phyp_race_wtd_std_s$est_prop_wtd_std[3]  
est_PDs_wtd_std_s

est_ORs_wtd_std_s <- (est_phyp_race_wtd_std_s$est_prop_wtd_std[1:2]/(1-est_phyp_race_wtd_std_s$est_prop_wtd_std[1:2]))/(est_phyp_race_wtd_std_s$est_prop_wtd_std[3]/(1-est_phyp_race_wtd_std_s$est_prop_wtd_std[3]))
est_ORs_wtd_std_s

# ---- Summarize results ----
Mean_results <- cbind(est_phyp_unw, t(est_phyp_race_unw[,2]),
                      (est_phyp_wtd_l), t(est_phyp_race_wtd_l[,2]),
                      (est_phyp_wtd_r), t(est_phyp_race_wtd_r[,2]),
                      (est_phyp_wtd_g), t(est_phyp_race_wtd_g[,2]),
                      (est_phyp_wtd_s), t(est_phyp_race_wtd_s[,2]), 
                      t(est_PRs_unw_std), t(est_PDs_unw_std), t(est_ORs_unw_std),
                      t(est_PRs_wtd_std_l), t(est_PDs_wtd_std_l), t(est_ORs_wtd_std_l),
                      t(est_PRs_wtd_std_r), t(est_PDs_wtd_std_r), t(est_ORs_wtd_std_r),
                      t(est_PRs_wtd_std_g), t(est_PDs_wtd_std_g), t(est_ORs_wtd_std_g),
                      t(est_PRs_wtd_std_s), t(est_PDs_wtd_std_s), t(est_ORs_wtd_std_s)
                      
                      #EHL edit 5/18 adding 
                      #     1. ADC inequalities stdzed to chis, 
                      #     2. Unstdzd inequality estimates from all approaches
                      #     3. Stdzd point prevalence estimates by race from all approaches
                              ,
                              
                              t(est_PRs_unw_std_chispop), t(est_PDs_unw_std_chispop), t(est_ORs_unw_std_chispop), #1
                              
                              t(est_PRs_unw_unstd), t(est_PDs_unw_unstd), t(est_ORs_unw_unstd),                   #2
                              t(est_PRs_wtd_unstd_l), t(est_PDs_wtd_unstd_l), t(est_ORs_wtd_unstd_l),             #2
                              t(est_PRs_wtd_unstd_r), t(est_PDs_wtd_unstd_r), t(est_ORs_wtd_unstd_r),             #2
                              t(est_PRs_wtd_unstd_g), t(est_PDs_wtd_unstd_g), t(est_ORs_wtd_unstd_g),             #2
                              t(est_PRs_wtd_unstd_s), t(est_PDs_wtd_unstd_s), t(est_ORs_wtd_unstd_s),             #2
                              
                              
                              t(est_phyp_race_unw_std[,2]),                                                       #3
                              t(est_phyp_race_unw_std_chispop[,2]),                                               #3
                              t(est_phyp_race_wtd_std_l[,2]),                                                     #3
                              t(est_phyp_race_wtd_std_r[,2]),                                                     #3
                              t(est_phyp_race_wtd_std_g[,2]),                                                     #3
                              t(est_phyp_race_wtd_std_s[,2])                                                      #3
                              
                      
                      #End EHL edit 5/18
                            
                      )
rownames(Mean_results) <- NULL
colnames(Mean_results) <- c("Overall_unw", "Black_unw", "Latino_unw", "White_unw",
                            "Overall_wtd_l", "Black_wtd_l", "Latino_wtd_l", "White_wtd_l",
                            "Overall_wtd_r", "Black_wtd_r", "Latino_wtd_r", "White_wtd_r",
                            "Overall_wtd_g", "Black_wtd_g", "Latino_wtd_g", "White_wtd_g",
                            "Overall_wtd_s", "Black_wtd_s", "Latino_wtd_s", "White_wtd_s",
                            "BW_PR_unw", "LW_PR_unw",
                            "BW_PD_unw", "LW_PD_unw",
                            "BW_OR_unw", "LW_OR_unw",
                            "BW_PR_wtd_l", "LW_PR_wtd_l",
                            "BW_PD_wtd_l", "LW_PD_wtd_l",
                            "BW_OR_wtd_l", "LW_OR_wtd_l",
                            "BW_PR_wtd_r", "LW_PR_wtd_r",
                            "BW_PD_wtd_r", "LW_PD_wtd_r",
                            "BW_OR_wtd_r", "LW_OR_wtd_r",
                            "BW_PR_wtd_g", "LW_PR_wtd_g",
                            "BW_PD_wtd_g", "LW_PD_wtd_g",
                            "BW_OR_wtd_g", "LW_OR_wtd_g",
                            "BW_PR_wtd_s", "LW_PR_wtd_s",
                            "BW_PD_wtd_s", "LW_PD_wtd_s",
                            "BW_OR_wtd_s", "LW_OR_wtd_s"
                            
                            
                            #EHL edit 5/18 adding 
                            #     1. ADC inequalities stdzed to chis, 
                            #     2. Unstdzd inequality estimates from all approaches
                            #     3. Stdzd point prevalence estimates by race from all approaches
                            ,
                            "BW_PR_unw_chispop", "LW_PR_unw_chispop",
                            "BW_PD_unw_chispop", "LW_PD_unw_chispop",
                            "BW_OR_unw_chispop", "LW_OR_unw_chispop",
                            
                            
                            
                            "BW_PR_unw_unstd", "LW_PR_unw_unstd",
                            "BW_PD_unw_unstd", "LW_PD_unw_unstd",
                            "BW_OR_unw_unstd", "LW_OR_unw_unstd",
                            "BW_PR_wtd_unstd_l", "LW_PR_wtd_unstd_l",
                            "BW_PD_wtd_unstd_l", "LW_PD_wtd_unstd_l",
                            "BW_OR_wtd_unstd_l", "LW_OR_wtd_unstd_l",
                            "BW_PR_wtd_unstd_r", "LW_PR_wtd_unstd_r",
                            "BW_PD_wtd_unstd_r", "LW_PD_wtd_unstd_r",
                            "BW_OR_wtd_unstd_r", "LW_OR_wtd_unstd_r",
                            "BW_PR_wtd_unstd_g", "LW_PR_wtd_unstd_g",
                            "BW_PD_wtd_unstd_g", "LW_PD_wtd_unstd_g",
                            "BW_OR_wtd_unstd_g", "LW_OR_wtd_unstd_g",
                            "BW_PR_wtd_unstd_s", "LW_PR_wtd_unstd_s",
                            "BW_PD_wtd_unstd_s", "LW_PD_wtd_unstd_s",
                            "BW_OR_wtd_unstd_s", "LW_OR_wtd_unstd_s",
                            
                            
                            "Black_unw_std", "Latino_unw_std", "White_unw_std",
                            "Black_unw_std_chispop", "Latino_unw_std_chispop", "White_unw_std_chispop",
                            "Black_wtd_std_l", "Latino_wtd_std_l", "White_wtd_std_l",
                            "Black_wtd_std_r", "Latino_wtd_std_r", "White_wtd_std_r",
                            "Black_wtd_std_g", "Latino_wtd_std_g", "White_wtd_std_g",
                            "Black_wtd_std_s", "Latino_wtd_std_s", "White_wtd_std_s"
                            
                            #End EHL edit 5/18
                            )

Mean_results$Est<-"Mean"

write.csv(Mean_results, file="C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_18May2022/results/results_18May2022.csv")
