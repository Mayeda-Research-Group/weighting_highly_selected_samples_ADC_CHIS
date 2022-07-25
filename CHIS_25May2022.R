# Reweighting for Highly Selected Samples
# CHIS Estimates and CIs
# Kristina Van Dang
# February 7, 2022

# This code generates CHIS estimates and CIs for hypertension by race

rm(list=ls())

library(tidyverse)
library(dplyr)

set.seed(45648)
setwd("C:/Users/ehlarson/Box/KD_bootstrapping/hoffman_cluster/data")
df1<-read.csv("adc_chis2_am_09.csv") 

# Create 3 age categories to age standardize
df1 <- df1 %>% mutate(agecat_h2 = 0,
                      agecat_h2 = replace(agecat_h2, 60 <= Nage & Nage < 70, 1),
                      agecat_h2 = replace(agecat_h2, 70 <= Nage & Nage < 80, 2),
                      agecat_h2 = replace(agecat_h2, 80 <= Nage, 3))

# Subset CHIS data
CHIS <- df1 %>% filter(adc==0)

#Define age/sex distribution of standard population (full CHIS sample with CA weights--RAKEDW0)
chis_total <- sum(CHIS$RAKEDW0)
agesex_totals <- CHIS %>% group_by(agecat_h2, male) %>% 
  summarise(n = sum(RAKEDW0))
agesex_totals$prop_agesex<-agesex_totals$n/chis_total

# ---- Estimates ----
# Code unstandardized, but weighted prevalences
est_phyp_race_wtd <- CHIS %>% group_by(race) %>% 
  summarise(phyp = weighted.mean(Hypertension, RAKEDW0))  %>% data.frame() %>% dplyr::select("phyp") %>% t() %>% as.numeric()

#EHL adding unstdardized inequliaty estimates
est_PRs_wtd_unstd <- est_phyp_race_wtd[1:2]/est_phyp_race_wtd[3]  
est_PDs_wtd_unstd <- est_phyp_race_wtd[1:2]-est_phyp_race_wtd[3]  
est_ORs_wtd_unstd <- (est_phyp_race_wtd[1:2]/(1-est_phyp_race_wtd[1:2]))/(est_phyp_race_wtd[3]/(1-est_phyp_race_wtd[3]))



# Prevalence of hypertension by race, age/sex standardized
est_phyp_race_wtd_std <- CHIS %>% 
  group_by(race, agecat_h2, male) %>% 
  summarise(phyp_race = weighted.mean(Hypertension, RAKEDW0)) %>% 
  left_join(.,agesex_totals, by=c("agecat_h2", "male")) %>%
  group_by(race) %>% 
  summarise(est_prop_wtd_std = crossprod(phyp_race, prop_agesex))

# Estimate inequalities
est_PRs_wtd_std <- est_phyp_race_wtd_std$est_prop_wtd_std[1:2]/est_phyp_race_wtd_std$est_prop_wtd_std[3]  
est_PDs_wtd_std <- est_phyp_race_wtd_std$est_prop_wtd_std[1:2]-est_phyp_race_wtd_std$est_prop_wtd_std[3]  
est_ORs_wtd_std <- (est_phyp_race_wtd_std$est_prop_wtd_std[1:2]/(1-est_phyp_race_wtd_std$est_prop_wtd_std[1:2]))/(est_phyp_race_wtd_std$est_prop_wtd_std[3]/(1-est_phyp_race_wtd_std$est_prop_wtd_std[3]))

# Estimates
estimates <- data.frame(cbind(t(est_phyp_race_wtd),
                              t(est_phyp_race_wtd_std[,"est_prop_wtd_std"]),
                              t(est_PRs_wtd_std),
                              t(est_PDs_wtd_std),
                              t(est_ORs_wtd_std),
                              t(est_PRs_wtd_unstd),
                              t(est_PDs_wtd_unstd),
                              t(est_ORs_wtd_unstd)))

rownames(estimates)<-NULL

colnames(estimates) <- c("Black_prev_wtd_chis", "Latino_prev_wtd_chis", "White_prev_wtd_chis",
                         "Black_prev_wtd_chis_std", "Latino_prev_wtd_chis_std", "White_prev_wtd_chis_std",
                         "BW_PR_wtd_chis", "LW_PR_wtd_chis",
                         "BW_PD_wtd_chis", "LW_PD_wtd_chis",
                         "BW_OR_wtd_chis", "LW_OR_wtd_chis",
                         "BW_PR_wtd_chis_unstd", "LW_PR_wtd_chis_unstd",
                         "BW_PD_wtd_chis_unstd", "LW_PD_wtd_chis_unstd",
                         "BW_OR_wtd_chis_unstd", "LW_OR_wtd_chis_unstd")
estimates$Est<-"Est"

# ---- Variance, Bootstrap ----
nboots <- 2000

for (i in 1:nboots){
  
  boot_chis<-sample_n(CHIS, nrow(CHIS), replace=T)
  
  res <- as.numeric(rep(NA, 18))
  names(res)<-colnames(estimates %>% select(-Est))
  
  res[c("Black_prev_wtd_chis", "Latino_prev_wtd_chis", "White_prev_wtd_chis")] <- boot_chis %>% group_by(race) %>% 
    summarise(phyp = weighted.mean(Hypertension, RAKEDW0))  %>% data.frame() %>% dplyr::select("phyp") %>% t() %>% as.numeric()
  
  res["BW_PR_wtd_chis_unstd"]<-res["Black_prev_wtd_chis"]/res["White_prev_wtd_chis"]
  res["LW_PR_wtd_chis_unstd"]<-res["Latino_prev_wtd_chis"]/res["White_prev_wtd_chis"]
  res["BW_PD_wtd_chis_unstd"]<-res["Black_prev_wtd_chis"]-res["White_prev_wtd_chis"]
  res["LW_PD_wtd_chis_unstd"]<-res["Latino_prev_wtd_chis"]-res["White_prev_wtd_chis"]
  res["BW_OR_wtd_chis_unstd"]<-(res["Black_prev_wtd_chis"]/(1-res["Black_prev_wtd_chis"]))/(res["White_prev_wtd_chis"]/(1-res["White_prev_wtd_chis"]))
  res["LW_OR_wtd_chis_unstd"]<-(res["Latino_prev_wtd_chis"]/(1-res["Latino_prev_wtd_chis"]))/(res["White_prev_wtd_chis"]/(1-res["White_prev_wtd_chis"]))
  
  
  est_phyp_race_wtd_std <- boot_chis %>%
    group_by(race, agecat_h2, male) %>% 
    summarise(phyp_race = weighted.mean(Hypertension, RAKEDW0)) %>% 
    left_join(.,agesex_totals, by=c("agecat_h2", "male")) %>%
    group_by(race) %>% 
    summarise(est_prop_wtd_std = crossprod(phyp_race, prop_agesex))
  
  res[c("Black_prev_wtd_chis_std", "Latino_prev_wtd_chis_std", "White_prev_wtd_chis_std")]<-t(est_phyp_race_wtd_std[,"est_prop_wtd_std"])
  
  # Estimate inequalities
  res[c("BW_PR_wtd_chis", "LW_PR_wtd_chis")] <- est_phyp_race_wtd_std$est_prop_wtd_std[1:2]/est_phyp_race_wtd_std$est_prop_wtd_std[3]  
  res[c("BW_PD_wtd_chis", "LW_PD_wtd_chis")] <- est_phyp_race_wtd_std$est_prop_wtd_std[1:2]-est_phyp_race_wtd_std$est_prop_wtd_std[3]  
  res[c("BW_OR_wtd_chis", "LW_OR_wtd_chis")] <- (est_phyp_race_wtd_std$est_prop_wtd_std[1:2]/(1-est_phyp_race_wtd_std$est_prop_wtd_std[1:2]))/(est_phyp_race_wtd_std$est_prop_wtd_std[3]/(1-est_phyp_race_wtd_std$est_prop_wtd_std[3]))
  
  write_csv(as.data.frame(t(res)),
            "C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_18May2022/results/CHIS_bootstraps_25May2022.csv", append = TRUE)

}

# Bootstrap
res <- read.csv("C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_18May2022/results/CHIS_bootstraps_25May2022.csv", header = FALSE)

colnames(res) <- c("Black_prev_wtd_chis", "Latino_prev_wtd_chis", "White_prev_wtd_chis",
                   "Black_prev_wtd_chis_std", "Latino_prev_wtd_chis_std", "White_prev_wtd_chis_std",
                   "BW_PR_wtd_chis", "LW_PR_wtd_chis",
                   "BW_PD_wtd_chis", "LW_PD_wtd_chis",
                   "BW_OR_wtd_chis", "LW_OR_wtd_chis",
                   "BW_PR_wtd_chis_unstd", "LW_PR_wtd_chis_unstd",
                   "BW_PD_wtd_chis_unstd", "LW_PD_wtd_chis_unstd",
                   "BW_OR_wtd_chis_unstd", "LW_OR_wtd_chis_unstd")

LowerCL <- data.frame(t(data.frame(sapply(res, function(x) quantile(x,0.025, na.rm=T)))))
UpperCL <- data.frame(t(data.frame(sapply(res, function(x) quantile(x,0.975, na.rm=T)))))

colnames(LowerCL)<-colnames(res)
rownames(LowerCL)<-NULL
LowerCL$Est<-"LowerCL"

colnames(UpperCL)<-colnames(res)
rownames(UpperCL)<-NULL
UpperCL$Est<-"UpperCL"

Final_results <- pivot_longer(rbind(estimates, LowerCL, UpperCL), !Est) %>% 
  pivot_wider(., names_from="Est")


# ---- Formatting ----
# To run with graphics script

#EHL overwriting names to have consistent format: group(s)_stat_(un)std_method
Final_results$namenew<-c("Black_prev_unstd_chis",      
                         "Latino_prev_unstd_chis",
                         "White_prev_unstd_chis",
                         "Black_prev_stdchis_chis",  
                         "Latino_prev_stdchis_chis",
                         "White_prev_stdchis_chis",
                         "BW_PR_stdchis_chis", "LW_PR_stdchis_chis",
                         "BW_PD_stdchis_chis", "LW_PD_stdchis_chis",
                         "BW_OR_stdchis_chis", "LW_OR_stdchis_chis",
                         "BW_PR_unstd_chis", "LW_PR_unstd_chis",
                         "BW_PD_unstd_chis", "LW_PD_unstd_chis",
                         "BW_OR_unstd_chis", "LW_OR_unstd_chis")

results <- Final_results %>%
  separate(namenew , sep="_", into=c("Group", "Statistic", "Stdpop", "Method")) %>%
  mutate(
    Group = replace(Group, Group=="BW", "Black"),
    Group = replace(Group, Group=="LW", "Latino"),
    
    Statistic = replace(Statistic, Statistic=="prev", "Prevalence"),
    Statistic = replace(Statistic, Statistic=="PR", "Rel Risk"),
    Statistic = replace(Statistic, Statistic=="PD", "Risk Diff"),
    Statistic = replace(Statistic, Statistic=="OR", "Odds Ratio")) %>%
      
      mutate(Estimate = case_when(
        Statistic=="Prevalence" ~ round((Est*100), 2),
        Statistic=="Risk Diff" ~ round(Est, digits=4),
        TRUE ~ round(Est, digits=2)),
    
    LowerCL = replace(LowerCL, Statistic=="Prevalence", LowerCL*100),
    UpperCL = replace(UpperCL, Statistic=="Prevalence", UpperCL*100),
    
    model = Method,
    model = replace(model, Method=="chis", "CHIS_Weighted"),
    
    model2 = Method,
    model2 = replace(model2, Method=="chis", "CHIS Weighted"),
    
    model3 = Method,
    model3 = replace(model3, Method=="chis", "G5")
  ) %>%
  dplyr::select(Group, Statistic, Stdpop, Estimate, LowerCL, UpperCL, model, model2, model3) 

write.csv(results, file = "C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_18May2022/results/CHIS_results_25May2022.csv")
