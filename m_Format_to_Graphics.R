# Reweighting for Highly Selected Samples
# Formatting for final graphics
# Kristina Van Dang, Eleanor Hayes-Larson
# May 25, 2022

# This code produces a formatted dataset to load for final graphics for the manuscript

rm(list=ls())

# ---- Setup ----
library(dplyr)
library(tidyverse)
library(magrittr)
library(twang) # covariate balance plot

#---- path to box ----
#Updating the path here will update everything throughout the script

# #EHL's path
 path_to_box <- "C:/Users/ehlarson/Box/"

#CS path
#path_to_box <- "/Users/crystalshaw/Library/CloudStorage/Box-Box/JZ_CS/"

# ---- *Estimates ----
estimates <- 
  read.csv(paste0(path_to_box, 
                  "KD_bootstrapping/Scripts_18May2022/results/results_18May2022.csv")) %>%
  dplyr::select(-X)

# ---- *Bootstrap ----
bootstrap <- 
  read.csv(paste0(path_to_box, 
                  "KD_bootstrapping/Scripts_18May2022/results/bootstrap_runs_24May2022.csv"), header=T) %>% 
  na.omit() %>% select(-time_min, -seed)


# ---- *CHIS estimates and CIs ----
chis_results <- 
  read.csv(paste0(path_to_box, 
                  "KD_bootstrapping/Scripts_18May2022/results/CHIS_results_25May2022.csv")) %>%
  dplyr::select(-X) 

# ---- *Original cleaned dataset ----
setwd(paste0(path_to_box, "KD_bootstrapping/hoffman_cluster/data"))
dat1 <- read.csv("adc_chis2_am_09.csv")
chis <- dat1 %>% filter(adc==0)
p_adc <- sum(dat1$adc)/sum(dat1$RAKEDW0)

# ---- *Scheme results----
# **** Note: CHIS == 0 (for density plots) versus CHIS == 1 (for covariate balance plots)

# ---- **Logistic results----
l <- read.csv(paste0(path_to_box, "KD_bootstrapping/Scripts_18May2022/results/Logistic_09_26Jan2022.csv")) %>%
  mutate(IOSW_l0 = ifelse(adc==0, 0, ((1-pp_logistic)/pp_logistic)*(p_adc)/(1-p_adc)),
         IOSW_l1 = ifelse(adc==0, 1, ((1-pp_logistic)/pp_logistic)*(p_adc)/(1-p_adc)),
         pp_l_rescale = pp_logistic*4622757/856) %>%
  dplyr::select(ID, pp_logistic, IOSW_l0, IOSW_l1, pp_l_rescale) %>%
  rename(IP_1 = pp_logistic)

# ---- **Raking results----
r <- read.csv(paste0(path_to_box, "KD_bootstrapping/Scripts_18May2022/results/Raking_09_26Jan2022.csv")) %>%
  dplyr::select(ID, raked.weight)

# ---- **GBM results----
g <- read.csv(paste0(path_to_box, "KD_bootstrapping/Scripts_18May2022/results/GBM_09_26Jan2022.csv")) %>%
  mutate(IOSW_g0 = ifelse(adc==0, 0, ((1-pp2_ADC)/pp2_ADC)*(p_adc)/(1-p_adc)),
         IOSW_g1 = ifelse(adc==0, 1, ((1-pp2_ADC)/pp2_ADC)*(p_adc)/(1-p_adc)),
         pp_g_rescale = pp2_ADC*4622757/856) %>%
  dplyr::select(ID, pp2_CHIS, pp2_ADC, IOSW_g0, IOSW_g1, pp_g_rescale)

# ---- **super learner results----
s <- read.csv(paste0(path_to_box, "KD_bootstrapping/Scripts_18May2022/results/SL_09_26Jan2022.csv")) %>%
  mutate(IOSW_s0 = ifelse(adc==0, 0, ((1-pp_SL)/pp_SL)*(p_adc)/(1-p_adc)),
         IOSW_s1 = ifelse(adc==0, 1, ((1-pp_SL)/pp_SL)*(p_adc)/(1-p_adc)),
         pp_s_rescale = pp_SL*4622757/856)

# ******************************
#
# ---- P, PR, PD, OR Plots ----
#
# ******************************
LCIS <- data.frame(t(data.frame(sapply(bootstrap, function(x) quantile(x,0.025, na.rm=T)))))
UCIS <- data.frame(t(data.frame(sapply(bootstrap, function(x) quantile(x,0.975, na.rm=T)))))

#Merge with means
rownames(estimates) <- NULL
estimates$Est<-"Mean"

colnames(LCIS)<-colnames(bootstrap)
rownames(LCIS)<-NULL
LCIS$Est<-"LCI"

colnames(UCIS)<-colnames(bootstrap)
rownames(UCIS)<-NULL
UCIS$Est<-"UCI"

Final_results <- pivot_longer(rbind(estimates, LCIS, UCIS), !Est) %>% 
  pivot_wider(., names_from="Est")

#EHL overwriting names to have consistent format: group(s)_stat_(un)std_method
Final_results$namenew<-c("Overall_prev_unstd_unw", "Black_prev_unstd_unw", "Latino_prev_unstd_unw", "White_prev_unstd_unw",
                         "Overall_prev_unstd_wtdl", "Black_prev_unstd_wtdl", "Latino_prev_unstd_wtdl", "White_prev_unstd_wtdl",
                         "Overall_prev_unstd_wtdr", "Black_prev_unstd_wtdr", "Latino_prev_unstd_wtdr", "White_prev_unstd_wtdr",
                         "Overall_prev_unstd_wtdg", "Black_prev_unstd_wtdg", "Latino_prev_unstd_wtdg", "White_prev_unstd_wtdg",
                         "Overall_prev_unstd_wtds", "Black_prev_unstd_wtds", "Latino_prev_unstd_wtds", "White_prev_unstd_wtds",
                         "BW_PR_stdadc_unw", "LW_PR_stdadc_unw",
                         "BW_PD_stdadc_unw", "LW_PD_stdadc_unw",
                         "BW_OR_stdadc_unw", "LW_OR_stdadc_unw",
                         "BW_PR_stdchis_wtdl", "LW_PR_stdchis_wtdl",
                         "BW_PD_stdchis_wtdl", "LW_PD_stdchis_wtdl",
                         "BW_OR_stdchis_wtdl", "LW_OR_stdchis_wtdl",
                         "BW_PR_stdchis_wtdr", "LW_PR_stdchis_wtdr",
                         "BW_PD_stdchis_wtdr", "LW_PD_stdchis_wtdr",
                         "BW_OR_stdchis_wtdr", "LW_OR_stdchis_wtdr",
                         "BW_PR_stdchis_wtdg", "LW_PR_stdchis_wtdg",
                         "BW_PD_stdchis_wtdg", "LW_PD_stdchis_wtdg",
                         "BW_OR_stdchis_wtdg", "LW_OR_stdchis_wtdg",
                         "BW_PR_stdchis_wtds", "LW_PR_stdchis_wtds",
                         "BW_PD_stdchis_wtds", "LW_PD_stdchis_wtds",
                         "BW_OR_stdchis_wtds", "LW_OR_stdchis_wtds",
                         "BW_PR_stdchis_unw", "LW_PR_stdchis_unw",
                         "BW_PD_stdchis_unw", "LW_PD_stdchis_unw",
                         "BW_OR_stdchis_unw", "LW_OR_stdchis_unw",
                         "BW_PR_unstd_unw", "LW_PR_unstd_unw",
                         "BW_PD_unstd_unw", "LW_PD_unstd_unw",
                         "BW_OR_unstd_unw", "LW_OR_unstd_unw",
                         "BW_PR_unstd_wtdl", "LW_PR_unstd_wtdl",
                         "BW_PD_unstd_wtdl", "LW_PD_unstd_wtdl",
                         "BW_OR_unstd_wtdl", "LW_OR_unstd_wtdl",
                         "BW_PR_unstd_wtdr", "LW_PR_unstd_wtdr",
                         "BW_PD_unstd_wtdr", "LW_PD_unstd_wtdr",
                         "BW_OR_unstd_wtdr", "LW_OR_unstd_wtdr",
                         "BW_PR_unstd_wtdg", "LW_PR_unstd_wtdg",
                         "BW_PD_unstd_wtdg", "LW_PD_unstd_wtdg",
                         "BW_OR_unstd_wtdg", "LW_OR_unstd_wtdg",
                         "BW_PR_unstd_wtds", "LW_PR_unstd_wtds",
                         "BW_PD_unstd_wtds", "LW_PD_unstd_wtds",
                         "BW_OR_unstd_wtds", "LW_OR_unstd_wtds",
                         "Black_prev_stdadc_unw", "Latino_prev_stdadc_unw", "White_prev_stdadc_unw",
                         "Black_prev_stdchis_unw", "Latino_prev_stdchis_unw", "White_prev_stdchis_unw",
                         "Black_prev_stdchis_wtdl", "Latino_prev_stdchis_wtdl", "White_prev_stdchis_wtdl",
                         "Black_prev_stdchis_wtdr", "Latino_prev_stdchis_wtdr", "White_prev_stdchis_wtdr",
                         "Black_prev_stdchis_wtdg", "Latino_prev_stdchis_wtdg", "White_prev_stdchis_wtdg",
                         "Black_prev_stdchis_wtds", "Latino_prev_stdchis_wtds", "White_prev_stdchis_wtds")


results1 <- Final_results %>%
  separate(namenew , sep="_", into=c("Group", "Statistic", "Stdpop", "Method")) %>%
  mutate(
    Group = replace(Group, Group=="BW", "Black"),
    Group = replace(Group, Group=="LW", "Latino"),
    
    Statistic = replace(Statistic, Statistic=="prev", "Prevalence"),
    Statistic = replace(Statistic, Statistic=="PR", "Rel Risk"),
    Statistic = replace(Statistic, Statistic=="PD", "Risk Diff"),
    Statistic = replace(Statistic, Statistic=="OR", "Odds Ratio")) %>%
  
  mutate(Estimate = case_when(
    Statistic=="Prevalence" ~ round((Mean*100), 2),
    Statistic=="Risk Diff" ~ round(Mean, digits=4),
    TRUE ~ round(Mean, digits=2)),
    
    LCI = case_when(Statistic=="Prevalence" ~ LCI*100,
                    TRUE ~ LCI),
    UCI = case_when(Statistic=="Prevalence" ~ UCI*100,
                    TRUE ~ UCI),    
    model = Method,
    model = replace(model, Method=="unw", "ADC_Unweighted"),
    model = replace(model, Method=="wtdl", "ADC_WT_Log"),
    model = replace(model, Method=="wtdr", "ADC_WT_Raked"),
    model = replace(model, Method=="wtdg", "ADC_WT_GBM"),
    model = replace(model, Method=="wtds", "ADC_WT_SL"),
    
    model2 = Method,
    model2 = replace(model2, Method=="unw", "ADC Unweighted"),
    model2 = replace(model2, Method=="wtdl", "ADC Logistic"),
    model2 = replace(model2, Method=="wtdr", "ADC Raked"),
    model2 = replace(model2, Method=="wtdg", "ADC SL GBM"),
    model2 = replace(model2, Method=="wtds", "ADC super learner"),
    
    model3 = Method,
    model3 = replace(model3, Method=="unw", "G1"),
    model3 = replace(model3, Method=="wtdl", "G2"),
    model3 = replace(model3, Method=="wtdr", "G3"),
    model3 = replace(model3, Method=="wtdg", "G6"),
    model3 = replace(model3, Method=="wtds", "G4")
  ) %>%
  dplyr::select(Group, Statistic, Stdpop, Estimate, LCI, UCI, model, model2, model3) %>%
  rename(LowerCL = LCI, UpperCL = UCI)

# For prevalence, prevalence ratio, prevalence difference, and odds ratio plots
results <- rbind(results1, chis_results) %>%
  rename(Value = Estimate)

# ************************
#
# ---- Density Plots ----
#
# ************************
s_l <- left_join(s,l)
s_l_r <- left_join(s_l, r)
density <- left_join(s_l_r, g)

# *********************************
#
# ---- Covariate Balance Plots ----
#
# *********************************
s_l <- left_join(s,l)
s_l_r <- left_join(s_l, r)
s_l_r_g <- left_join(s_l_r, g)

#---- **label races in one column ----
s_l_r_g %<>% mutate("race_label" = case_when(race == 1 ~ "Black", 
                                             race == 2 ~ "Latino", 
                                             race == 3 ~ "White"))

# #sanity check
# table(s_l_r_g$race, s_l_r_g$race_label, useNA = "ifany")

for(race in c("all", unique(s_l_r_g$race_label))){
  
  if(race != "all"){
    subset <- s_l_r_g[which(s_l_r_g$race_label == race), ]
  } else{
    subset <- s_l_r_g
  }
  
  combined <- subset %>%
    dplyr::select(adc, RAKEDW0, IOSW_l1, raked.weight, IOSW_g1, IOSW_s1,
                  Nage, black, hispanic, White, male, 
                  lang1_eng, lang1_span, lang1_both,
                  edu1_noed, edu1_elem, edu1_somehs, edu1_hs, edu1_somecol, edu1_colpstgrad,
                  marry_bi, stroke, diabetes, HeartDz, CongestiveHeart,  
                  Cholesterol, BMI, Hypertension 
    ) %>%
    mutate(raked.weight = replace(raked.weight, adc==0, 1),
           
           w.all_r = raked.weight*RAKEDW0, # Input for twang bal.stat w.all
           w.all_l = IOSW_l1*RAKEDW0,
           w.all_g = IOSW_g1*RAKEDW0,
           w.all_s = IOSW_s1*RAKEDW0, 
           
           chis = 1-adc) %>%
    data.frame()
  
  # Variables to balance
  if(race != "all"){
    vars <- c("Nage",
              "male", 
              "lang1_eng", "lang1_span", "lang1_both",
              "edu1_noed", "edu1_elem", "edu1_somehs", "edu1_hs", "edu1_somecol", "edu1_colpstgrad",
              "marry_bi", "stroke", "diabetes", "HeartDz", "CongestiveHeart",
              "Cholesterol", "BMI",  "Hypertension"
    )
    
  } else{
    vars <- c("Nage","black", "hispanic", "White", 
              "male", 
              "lang1_eng", "lang1_span", "lang1_both",
              "edu1_noed", "edu1_elem", "edu1_somehs", "edu1_hs", "edu1_somecol", "edu1_colpstgrad",
              "marry_bi", "stroke", "diabetes", "HeartDz", "CongestiveHeart",
              "Cholesterol", "BMI",  "Hypertension"
    )
  }
  
  
  # Make it so twang can read it (list)
  combined2 <- data.frame(lapply(combined, function(x) as.numeric(as.character(x))))
  
  temp_r<-bal.stat(data=combined2, 
                   vars=vars,
                   w.all=combined2$w.all_r,
                   treat.var='chis', 
                   sampw=combined2$RAKEDW0, 
                   estimand="ATT", 
                   multinom = F)
  
  temp_l<-bal.stat(data=combined2, 
                   vars=vars,
                   w.all=combined2$w.all_l,
                   treat.var='chis', 
                   sampw=combined2$RAKEDW0, 
                   estimand="ATT", 
                   multinom = F)
  
  temp_g<-bal.stat(data=combined2, 
                   vars=vars,
                   w.all=combined2$w.all_g,
                   treat.var='chis', 
                   sampw=combined2$RAKEDW0, 
                   estimand="ATT", 
                   multinom = F)
  
  temp_s<-bal.stat(data=combined2, 
                   vars=vars,
                   w.all=combined2$w.all_s,
                   treat.var='chis', 
                   sampw=combined2$RAKEDW0, 
                   estimand="ATT", 
                   multinom = F)
  
  temp_unw<-bal.stat(data=combined2, 
                     vars=vars,
                     w.all=combined2$RAKEDW0,
                     treat.var='chis', 
                     sampw=combined2$RAKEDW0, 
                     estimand="ATT", 
                     multinom = F)
  
  results_unw<-temp_unw$results
  results_r<-temp_r$results
  results_l<-temp_l$results
  results_g<-temp_g$results
  results_s<-temp_s$results
  
  results_unw$Covariate<-rownames(results_unw)
  results_r$Covariate<-rownames(results_r)
  results_l$Covariate<-rownames(results_l)
  results_g$Covariate<-rownames(results_g)
  results_s$Covariate<-rownames(results_s)
  
  label_rownames <- function(data, race) {
    if(race != "all"){
      data <- data %>% mutate(Covariate = recode(Covariate, Nage = "Age, years",
                                                 male = "Male",
                                                 lang1_eng = "English Only",
                                                 lang1_span = "Spanish Only",
                                                 lang1_both = "Both English and Spanish",
                                                 edu1_noed = "No Formal Education",
                                                 edu1_elem = "Grade 1-8",
                                                 edu1_somehs = "Grade 9-11",
                                                 edu1_hs = "Grade 12/High School Diploma",
                                                 edu1_somecol = "Some College",
                                                 edu1_colpstgrad = "College Degree/Post Graduate",
                                                 marry_bi = "Married/Living with Partner",
                                                 stroke = "Stroke",
                                                 diabetes = "Diabetes",
                                                 HeartDz = "Heart Disease",
                                                 CongestiveHeart = "Congestive Heart Failure",
                                                 Cholesterol = "Hypercholesterolemia",
                                                 BMI = "BMI",
                                                 Hypertension = "Hypertension"))
    } else{
      data <- data %>% mutate(Covariate = recode(Covariate, Nage = "Age, years",
                                                 black = "Black",
                                                 hispanic = "Latino",
                                                 White = "White",
                                                 male = "Male",
                                                 lang1_eng = "English Only",
                                                 lang1_span = "Spanish Only",
                                                 lang1_both = "Both English and Spanish",
                                                 edu1_noed = "No Formal Education",
                                                 edu1_elem = "Grade 1-8",
                                                 edu1_somehs = "Grade 9-11",
                                                 edu1_hs = "Grade 12/High School Diploma",
                                                 edu1_somecol = "Some College",
                                                 edu1_colpstgrad = "College Degree/Post Graduate",
                                                 marry_bi = "Married/Living with Partner",
                                                 stroke = "Stroke",
                                                 diabetes = "Diabetes",
                                                 HeartDz = "Heart Disease",
                                                 CongestiveHeart = "Congestive Heart Failure",
                                                 Cholesterol = "Hypercholesterolemia",
                                                 BMI = "BMI",
                                                 Hypertension = "Hypertension"))
    }
  }
  
  results_unw <- label_rownames(results_unw, race)
  results_r <- label_rownames(results_r, race)
  results_l <- label_rownames(results_l, race)
  results_g <- label_rownames(results_g, race)
  results_s <- label_rownames(results_s, race)
  
  # Construct data frame to read into graphics script
  results_unw$Model <- "Unweighted"
  results_r$Model <- "RAKED"
  results_l$Model <- "LOGSLPT2"
  results_g$Model <- "GBM"
  results_s$Model <- "SL"
  
  results_l1 <- rbind(results_l, results_unw) %>% mutate(Group="LOGSLPT2") %>% dplyr::select(Covariate, std.eff.sz, Model, Group) %>% rename(Effect_Size = std.eff.sz)
  results_g1 <- rbind(results_g, results_unw) %>% mutate(Group="GBM") %>% dplyr::select(Covariate, std.eff.sz, Model, Group) %>% rename(Effect_Size = std.eff.sz)
  results_r1 <- rbind(results_r, results_unw) %>% mutate(Group="RAKED") %>% dplyr::select(Covariate, std.eff.sz, Model, Group) %>% rename(Effect_Size = std.eff.sz)
  results_s1 <- rbind(results_s, results_unw) %>% mutate(Group="SL") %>% dplyr::select(Covariate, std.eff.sz, Model, Group) %>% rename(Effect_Size = std.eff.sz)
  
  df1 <- rbind(results_l1, results_g1, results_r1, results_s1) %>% mutate("race_label" = race)
  
  # *******************************************************
  #
  # ---- **aTable 4. Standardized mean differences ----
  #
  # *******************************************************
  # Standardized mean differences of covariates in the Aging Diversity Cohort (ADC) (highly-selected sample) with alternative sampling weights and the California Health Interview Survey (CHIS)* (representative sample) (presented graphically in Figure 2)
  results_unw2 <- results_unw %>%dplyr::select(Covariate, std.eff.sz) %>% mutate(std.eff.sz = -1*std.eff.sz)
  results_r2 <- results_r %>%dplyr::select(Covariate, std.eff.sz) %>% mutate(std.eff.sz = -1*std.eff.sz)
  results_l2 <- results_l %>% dplyr::select(Covariate, std.eff.sz) %>% mutate(std.eff.sz = -1*std.eff.sz)
  results_g2 <- results_g %>%dplyr::select(Covariate, std.eff.sz) %>% mutate(std.eff.sz = -1*std.eff.sz)
  results_s2 <- results_s %>%dplyr::select(Covariate, std.eff.sz) %>% mutate(std.eff.sz = -1*std.eff.sz)
  
  u_r <- left_join(results_unw2, results_r2, by="Covariate")
  u_r_l <- left_join(u_r, results_l2, by="Covariate")
  u_r_l_g <- left_join(u_r_l, results_g2, by="Covariate")
  aTable4 <- left_join(u_r_l_g, results_s2, by="Covariate") %>% 
    mutate("Race" = race)
  
  colnames(aTable4) <- c('Covariate',
                         'equal weights',
                         'raking weights', 
                         'sIOSW, algorithm: logistic regression',
                         'sIOSW, algorithm: GBM',
                         'sIOSW, algorithm: super learner', 
                         'Race')
  
  # *********************************
  
  #---- **save results ----
  write.csv(df1, paste0(path_to_box, 
                        "KD_bootstrapping/Scripts_18May2022/Output/Diagnostics_SEseize_FULL_021422_", 
                        race, ".csv"))
  
  write.csv(aTable4, paste0(path_to_box, 
                        "KD_bootstrapping/Scripts_18May2022/Final graphics/Manuscript_aTable4_021422_", 
                        race, ".csv"))
}


# ******************************************************
#
# ---- Table 1. Characteristics of ADC and CHIS ----
#
# ******************************************************
df2 <- dat1 %>%
  mutate(lang1_eng = 0,
         lang1_eng = replace(lang1_eng, language1==1, 1),
         
         lang1_span = 0,
         lang1_span = replace(lang1_span, language1==2, 1),
         
         lang1_both = 0,
         lang1_both = replace(lang1_both, language1==3, 1),
         
         edu1_noed = 0,
         edu1_noed = replace(edu1_noed, Edu_harm1==0, 1),
         
         edu1_elem = 0,
         edu1_elem = replace(edu1_elem, Edu_harm1==1, 1),
         
         edu1_somehs = 0,
         edu1_somehs = replace(edu1_somehs, Edu_harm1==2, 1),
         
         edu1_hs = 0,
         edu1_hs = replace(edu1_hs, Edu_harm1==3, 1),
         
         edu1_somecol = 0,
         edu1_somecol = replace(edu1_somecol, Edu_harm1==4, 1),
         
         edu1_colpostgrad = 0,
         edu1_colpostgrad = replace(edu1_colpostgrad, Edu_harm1==5, 1)
  )

#count_among formats a cell output either as a "prop": proportion with a particular "value" (for discrete variables) or a "mean": mean with sd (for continuous variables)
count_among <- function(data,variable,value=NA,type="prop"){
  if(type=="prop"){
    n_num <- sum(data[,variable]==value,na.rm=TRUE)
    n_denom <- nrow(data)
    perc <- round(n_num/n_denom,2) #Designate rounding here
    perc <- format(perc,nsmall=2) 
    s <- round(sd(data[[variable]],na.rm=TRUE),2)
    s <- format(s,nsmall=2) # keep the format #.## in printing
    result <- paste0(perc," (",s,")")
  }else if (type=="mean"){
    m <- round(mean(data[[variable]],na.rm=TRUE),2)
    m <- format(m,nsmall=2)
    s <- round(sd(data[[variable]],na.rm=TRUE),2)
    s <- format(s,nsmall=2)
    result <- paste0(m," (",s,")")
  }
  return(result)
}


#make_row creates a row for each model by applying count_among to every cell of interest
make_row <- function(data,variable,value,type="prop"){
  
  #For this example, I was creating 3 columns: a summary for each variable, and summaries of each variable stratified to exposed and unexposed 
  d_black <- data %>% filter(race==1)
  d_latino <- data %>% filter(race==2)
  d_white <- data %>% filter(race==3)
  
  row <- c(count_among(d_black,variable,value,type),
           count_among(d_latino,variable,value,type),
           count_among(d_white,variable,value,type))
  
  return(row)
}

df3 <- df2 %>% filter(adc==1)
df4 <- df2 %>% filter(adc==0)
df4 <- df4[rep(1:nrow(df4),times=df4$RAKEDW0),] #each person will be replicated RAKEDW0 times


# Filter to adc==1 for table1a function
table1a <- function(data){
  table1 <- data.frame(Black=character(), Latino=character(), White=character())
  
  #Manually putting in the counts
  table1["n",] <- c(sum(data$race==1),sum(data$race==2),sum(data$race==3))
  
  #These  variables are binary, so we care about proportion with value=1
  table1["Age, years",] <- make_row(data,"Nage",1, type="mean")
  table1["Male",] <- make_row(data,"male",1, type="prop")
  
  table1["English",] <- make_row(data,"lang1_eng",1, type="prop")
  table1["Spanish",] <- make_row(data,"lang1_span",1, type="prop")
  table1["Both English and Spanish",] <- make_row(data,"lang1_both",1, type="prop")
  
  table1["No formal education",] <- make_row(data,"edu1_noed",1, type="prop")
  table1["Grade 1-8",] <- make_row(data,"edu1_elem",1, type="prop")
  table1["Grade 9-11",] <- make_row(data,"edu1_somehs",1, type="prop")
  table1["Grade 12/High school diploma",] <- make_row(data,"edu1_hs",1, type="prop")
  table1["Some college",] <- make_row(data,"edu1_somecol",1, type="prop")
  table1["College degree/Post graduate ",] <- make_row(data,"edu1_colpostgrad",1, type="prop")
  
  table1["Married/Living with partner",] <- make_row(data,"marry_bi",1, type="prop")
  table1["Stroke",] <- make_row(data,"stroke",1, type="prop")
  table1["Diabetes",] <- make_row(data,"diabetes",1, type="prop")
  table1["Heart Disease",] <- make_row(data,"HeartDz",1, type="prop")
  table1["Congestive heart failure",] <- make_row(data,"CongestiveHeart",1, type="prop")
  table1["Hypercholesterolemia",] <- make_row(data,"Cholesterol",1, type="prop")
  table1["BMI",] <- make_row(data,"BMI",1, type="mean")
  table1["Hypertension",] <- make_row(data,"Hypertension",1, type="prop")
  
  return(table1)
}

adc_table1 <- table1a(df3)
chis_table1 <- table1a(df4) # this takes some time to run

table1 <- cbind(adc_table1, chis_table1)

# ********************************************
#
# ---- Table 2. Stabilized ADC Weights ----
#
# ********************************************
# Information for this table is in the density section
summ_swl <- density %>%
  filter(adc==1) %>%
  summarise(Mean = mean(IOSW_l0),
            `Standard Deviation` = sd(IOSW_l0),
            Minimum = min(IOSW_l0),
            `10th Percentile` = quantile(IOSW_l0, probs = 0.10),
            `25th Percentile` = quantile(IOSW_l0, probs = 0.25),
            Median = quantile(IOSW_l0, probs = 0.50),
            `75th Percentile` = quantile(IOSW_l0, probs = 0.75),
            `90th Percentile` = quantile(IOSW_l0, probs = 0.90),
            Maximum = max(IOSW_l0),
            
            `90th Percentile/10th Percentile` = `90th Percentile`/`10th Percentile`,
            `Maximum/Minimum` = Maximum/Minimum
  )

summ_swr <- density %>%
  filter(adc==1) %>%
  summarise(Mean = mean(raked.weight),
            `Standard Deviation` = sd(raked.weight),
            Minimum = min(raked.weight),
            `10th Percentile` = quantile(raked.weight, probs = 0.10),
            `25th Percentile` = quantile(raked.weight, probs = 0.25),
            Median = quantile(raked.weight, probs = 0.50),
            `75th Percentile` = quantile(raked.weight, probs = 0.75),
            `90th Percentile` = quantile(raked.weight, probs = 0.90),
            Maximum = max(raked.weight),
            
            `90th Percentile/10th Percentile` = `90th Percentile`/`10th Percentile`,
            `Maximum/Minimum` = Maximum/Minimum
  )

summ_swg <- density %>%
  filter(adc==1) %>%
  summarise(Mean = mean(IOSW_g0),
            `Standard Deviation` = sd(IOSW_g0),
            Minimum = min(IOSW_g0),
            `10th Percentile` = quantile(IOSW_g0, probs = 0.10),
            `25th Percentile` = quantile(IOSW_g0, probs = 0.25),
            Median = quantile(IOSW_g0, probs = 0.50),
            `75th Percentile` = quantile(IOSW_g0, probs = 0.75),
            `90th Percentile` = quantile(IOSW_g0, probs = 0.90),
            Maximum = max(IOSW_g0),
            
            `90th Percentile/10th Percentile` = `90th Percentile`/`10th Percentile`,
            `Maximum/Minimum` = Maximum/Minimum
  )

summ_sws <- density %>%
  filter(adc==1) %>%
  summarise(Mean = mean(IOSW_s0),
            `Standard Deviation` = sd(IOSW_s0),
            Minimum = min(IOSW_s0),
            `10th Percentile` = quantile(IOSW_s0, probs = 0.10),
            `25th Percentile` = quantile(IOSW_s0, probs = 0.25),
            Median = quantile(IOSW_s0, probs = 0.50),
            `75th Percentile` = quantile(IOSW_s0, probs = 0.75),
            `90th Percentile` = quantile(IOSW_s0, probs = 0.90),
            Maximum = max(IOSW_s0),
            
            `90th Percentile/10th Percentile` = `90th Percentile`/`10th Percentile`,
            `Maximum/Minimum` = Maximum/Minimum
  )

table2 <- t(rbind(summ_swr, summ_swl, summ_swg, summ_sws)) 
colnames(table2) <- c('Raking weights',
                      'sIOSW, algorithm: logistic regression',
                      'sIOSW, algorithm: GBM',
                      'sIOSW, algorithm: super learner')

# **************************************************************
#
# ---- aTable 2. Distribution of rescaled propensity scores ----
#
# **************************************************************
# Don't need to include raking (no propensity scores generated)

s_l <- left_join(s,l)
rescale_pp_results <- left_join(s_l, g)
rescale_pp_chis <- rescale_pp_results %>% filter(adc==0)
rescale_pp_chis <- rescale_pp_chis[rep(1:nrow(rescale_pp_chis),times=rescale_pp_chis$RAKEDW0),] #each person will be replicated RAKEDW0 times

rescale_l_adc <- rescale_pp_results %>%
  filter(adc==1) %>%
  summarise(Study = "ADC", Method = "Logistic Regression",
            Mean = mean(pp_l_rescale),
            `Standard Deviation` = sd(pp_l_rescale),
            Variance = sd(pp_l_rescale)^2,
            Minimum = min(pp_l_rescale),
            `10th Percentile` = quantile(pp_l_rescale, probs = 0.10),
            Median = quantile(pp_l_rescale, probs = 0.50),
            `90th Percentile` = quantile(pp_l_rescale, probs = 0.90),
            Maximum = max(pp_l_rescale))

rescale_g_adc <- rescale_pp_results %>%
  filter(adc==1) %>%
  summarise(Study = "ADC", Method = "GBM",
            Mean = mean(pp_g_rescale),
            `Standard Deviation` = sd(pp_g_rescale),
            Variance = sd(pp_g_rescale)^2,
            Minimum = min(pp_g_rescale),
            `10th Percentile` = quantile(pp_g_rescale, probs = 0.10),
            Median = quantile(pp_g_rescale, probs = 0.50),
            `90th Percentile` = quantile(pp_g_rescale, probs = 0.90),
            Maximum = max(pp_g_rescale))

rescale_s_adc <- rescale_pp_results %>%
  filter(adc==1) %>%
  summarise(Study = "ADC", Method = "super learner",
            Mean = mean(pp_s_rescale),
            `Standard Deviation` = sd(pp_s_rescale),
            Variance = sd(pp_s_rescale)^2,
            Minimum = min(pp_s_rescale),
            `10th Percentile` = quantile(pp_s_rescale, probs = 0.10),
            Median = quantile(pp_s_rescale, probs = 0.50),
            `90th Percentile` = quantile(pp_s_rescale, probs = 0.90),
            Maximum = max(pp_s_rescale))

# For CHIS
rescale_l_chis <- rescale_pp_chis %>%
  summarise(Study = "CHIS", Method = "Logistic Regression",
            Mean = mean(pp_l_rescale),
            `Standard Deviation` = sd(pp_l_rescale),
            Variance = sd(pp_l_rescale)^2,
            Minimum = min(pp_l_rescale),
            `10th Percentile` = quantile(pp_l_rescale, probs = 0.10),
            Median = quantile(pp_l_rescale, probs = 0.50),
            `90th Percentile` = quantile(pp_l_rescale, probs = 0.90),
            Maximum = max(pp_l_rescale))

rescale_g_chis <- rescale_pp_chis %>%
  summarise(Study = "CHIS", Method = "GBM",
            Mean = mean(pp_g_rescale),
            `Standard Deviation` = sd(pp_g_rescale),
            Variance = sd(pp_g_rescale)^2,
            Minimum = min(pp_g_rescale),
            `10th Percentile` = quantile(pp_g_rescale, probs = 0.10),
            Median = quantile(pp_g_rescale, probs = 0.50),
            `90th Percentile` = quantile(pp_g_rescale, probs = 0.90),
            Maximum = max(pp_g_rescale))

rescale_s_chis <- rescale_pp_chis %>%
  summarise(Study = "CHIS", Method = "super learner",
            Mean = mean(pp_s_rescale),
            `Standard Deviation` = sd(pp_s_rescale),
            Variance = sd(pp_s_rescale)^2,
            Minimum = min(pp_s_rescale),
            `10th Percentile` = quantile(pp_s_rescale, probs = 0.10),
            Median = quantile(pp_s_rescale, probs = 0.50),
            `90th Percentile` = quantile(pp_s_rescale, probs = 0.90),
            Maximum = max(pp_s_rescale)) 

aTable2 <- t(rbind(rescale_l_adc, rescale_g_adc, rescale_s_adc,
                   rescale_l_chis, rescale_g_chis, rescale_s_chis))

#---- **stratified table ----
# Don't need to include raking (no propensity scores generated)
s_l <- left_join(s,l)
rescale_pp_results <- left_join(s_l, g) %>% 
  mutate("race_label" = case_when(race == 1 ~ "Black", 
                                  race == 2 ~ "Latino", 
                                  race == 3 ~ "White"))
rescale_pp_chis <- rescale_pp_results %>% filter(adc==0)
rescale_pp_chis <- rescale_pp_chis[rep(1:nrow(rescale_pp_chis),times=rescale_pp_chis$RAKEDW0),] #each person will be replicated RAKEDW0 times

rescale_l_adc_strat <- rescale_pp_results %>%
  filter(adc==1) %>% group_by(race_label) %>%
  summarise(Study = "ADC", Method = "Logistic Regression",
            Mean = mean(pp_l_rescale),
            `Standard Deviation` = sd(pp_l_rescale),
            Variance = sd(pp_l_rescale)^2,
            Minimum = min(pp_l_rescale),
            `10th Percentile` = quantile(pp_l_rescale, probs = 0.10),
            Median = quantile(pp_l_rescale, probs = 0.50),
            `90th Percentile` = quantile(pp_l_rescale, probs = 0.90),
            Maximum = max(pp_l_rescale))

rescale_g_adc_strat <- rescale_pp_results %>%
  filter(adc==1) %>% group_by(race_label) %>%
  summarise(Study = "ADC", Method = "GBM",
            Mean = mean(pp_g_rescale),
            `Standard Deviation` = sd(pp_g_rescale),
            Variance = sd(pp_g_rescale)^2,
            Minimum = min(pp_g_rescale),
            `10th Percentile` = quantile(pp_g_rescale, probs = 0.10),
            Median = quantile(pp_g_rescale, probs = 0.50),
            `90th Percentile` = quantile(pp_g_rescale, probs = 0.90),
            Maximum = max(pp_g_rescale))

rescale_s_adc_strat <- rescale_pp_results %>%
  filter(adc==1) %>% group_by(race_label) %>%
  summarise(Study = "ADC", Method = "super learner",
            Mean = mean(pp_s_rescale),
            `Standard Deviation` = sd(pp_s_rescale),
            Variance = sd(pp_s_rescale)^2,
            Minimum = min(pp_s_rescale),
            `10th Percentile` = quantile(pp_s_rescale, probs = 0.10),
            Median = quantile(pp_s_rescale, probs = 0.50),
            `90th Percentile` = quantile(pp_s_rescale, probs = 0.90),
            Maximum = max(pp_s_rescale))

# For CHIS
rescale_l_chis_strat <- rescale_pp_chis %>% group_by(race_label) %>%
  summarise(Study = "CHIS", Method = "Logistic Regression",
            Mean = mean(pp_l_rescale),
            `Standard Deviation` = sd(pp_l_rescale),
            Variance = sd(pp_l_rescale)^2,
            Minimum = min(pp_l_rescale),
            `10th Percentile` = quantile(pp_l_rescale, probs = 0.10),
            Median = quantile(pp_l_rescale, probs = 0.50),
            `90th Percentile` = quantile(pp_l_rescale, probs = 0.90),
            Maximum = max(pp_l_rescale))

rescale_g_chis_strat <- rescale_pp_chis %>% group_by(race_label) %>%
  summarise(Study = "CHIS", Method = "GBM",
            Mean = mean(pp_g_rescale),
            `Standard Deviation` = sd(pp_g_rescale),
            Variance = sd(pp_g_rescale)^2,
            Minimum = min(pp_g_rescale),
            `10th Percentile` = quantile(pp_g_rescale, probs = 0.10),
            Median = quantile(pp_g_rescale, probs = 0.50),
            `90th Percentile` = quantile(pp_g_rescale, probs = 0.90),
            Maximum = max(pp_g_rescale))

rescale_s_chis_strat <- rescale_pp_chis %>% group_by(race_label) %>%
  summarise(Study = "CHIS", Method = "super learner",
            Mean = mean(pp_s_rescale),
            `Standard Deviation` = sd(pp_s_rescale),
            Variance = sd(pp_s_rescale)^2,
            Minimum = min(pp_s_rescale),
            `10th Percentile` = quantile(pp_s_rescale, probs = 0.10),
            Median = quantile(pp_s_rescale, probs = 0.50),
            `90th Percentile` = quantile(pp_s_rescale, probs = 0.90),
            Maximum = max(pp_s_rescale)) 

aTable2_strat <- 
  t(rbind(rescale_l_adc_strat, rescale_g_adc_strat, rescale_s_adc_strat,
          rescale_l_chis_strat, rescale_g_chis_strat, rescale_s_chis_strat))

# *******************************************************
#
# ---- aTable 3. Stratified Stabilized ADC Weights ----
#
# *******************************************************
density$race <- factor(density$race, levels = c(1, 2, 3), labels = c("Black", "Latino", "White"))

summ_swr_race <- density %>%
  filter(adc==1) %>%
  group_by(race) %>%
  summarise(Mean = mean(raked.weight),
            `Standard Deviation` = sd(raked.weight),
            Minimum = min(raked.weight),
            `10th Percentile` = quantile(raked.weight, probs = 0.10),
            `25th Percentile` = quantile(raked.weight, probs = 0.25),
            Median = quantile(raked.weight, probs = 0.50),
            `75th Percentile` = quantile(raked.weight, probs = 0.75),
            `90th Percentile` = quantile(raked.weight, probs = 0.90),
            Maximum = max(raked.weight),
            
            `90th Percentile/10th Percentile` = `90th Percentile`/`10th Percentile`,
            `Maximum/Minimum` = Maximum/Minimum
  )

summ_swl_race <- density %>%
  filter(adc==1) %>%
  group_by(race) %>%
  summarise(Mean = mean(IOSW_l0),
            `Standard Deviation` = sd(IOSW_l0),
            Minimum = min(IOSW_l0),
            `10th Percentile` = quantile(IOSW_l0, probs = 0.10),
            `25th Percentile` = quantile(IOSW_l0, probs = 0.25),
            Median = quantile(IOSW_l0, probs = 0.50),
            `75th Percentile` = quantile(IOSW_l0, probs = 0.75),
            `90th Percentile` = quantile(IOSW_l0, probs = 0.90),
            Maximum = max(IOSW_l0),
            
            `90th Percentile/10th Percentile` = `90th Percentile`/`10th Percentile`,
            `Maximum/Minimum` = Maximum/Minimum
  )

summ_swg_race <- density %>%
  filter(adc==1) %>%
  group_by(race) %>%
  summarise(Mean = mean(IOSW_g0),
            `Standard Deviation` = sd(IOSW_g0),
            Minimum = min(IOSW_g0),
            `10th Percentile` = quantile(IOSW_g0, probs = 0.10),
            `25th Percentile` = quantile(IOSW_g0, probs = 0.25),
            Median = quantile(IOSW_g0, probs = 0.50),
            `75th Percentile` = quantile(IOSW_g0, probs = 0.75),
            `90th Percentile` = quantile(IOSW_g0, probs = 0.90),
            Maximum = max(IOSW_g0),
            
            `90th Percentile/10th Percentile` = `90th Percentile`/`10th Percentile`,
            `Maximum/Minimum` = Maximum/Minimum
  )

summ_sws_race <- density %>%
  filter(adc==1) %>%
  group_by(race) %>%
  summarise(Mean = mean(IOSW_s0),
            `Standard Deviation` = sd(IOSW_s0),
            Minimum = min(IOSW_s0),
            `10th Percentile` = quantile(IOSW_s0, probs = 0.10),
            `25th Percentile` = quantile(IOSW_s0, probs = 0.25),
            Median = quantile(IOSW_s0, probs = 0.50),
            `75th Percentile` = quantile(IOSW_s0, probs = 0.75),
            `90th Percentile` = quantile(IOSW_s0, probs = 0.90),
            Maximum = max(IOSW_s0),
            
            `90th Percentile/10th Percentile` = `90th Percentile`/`10th Percentile`,
            `Maximum/Minimum` = Maximum/Minimum
  )

# Combine
aTable3 <- t(rbind(summ_swr_race, summ_swl_race, summ_swg_race, summ_sws_race))
colnames(aTable3) <- c('Raking weights','Raking weights','Raking weights',
                       'sIOSW, algorithm: logistic regression','sIOSW, algorithm: logistic regression','sIOSW, algorithm: logistic regression',
                       'sIOSW, algorithm: GBM','sIOSW, algorithm: GBM','sIOSW, algorithm: GBM',
                       'sIOSW, algorithm: super learner','sIOSW, algorithm: super learner','sIOSW, algorithm: super learner')

# ---- Save Datasets----
#
# *********************************

# For prevalence, PR, PD, OR Plots
write.csv(results, "C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_18May2022/Output/all_weights052522.csv")

# For density plots
write.csv(density, "C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_18May2022/Output/weights_comb_short_0214222.csv")

# # For covariate balance-- included in Covariate Balance portion of script above
# write.csv(df1, "C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_18May2022/Output/Diagnostics_SEseize_FULL_021422.csv")

# ----*Manuscript Tables----
# Table 1.
write.csv(table1, "C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_18May2022/Final graphics/Manuscript_Table1_021422.csv")

# Table 2. Summary of stabilized ADC weights by raking, logistic, GBM, and SL
write.csv(table2, "C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_18May2022/Final graphics/Manuscript_Table2_021422.csv")

# ----*Appendix Tables----
# aTable2. Distribution of rescaled propensity scores
write.csv(aTable2, "C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_18May2022/Final graphics/Manuscript_aTable2_021422.csv")

# aTable2 (strat). Stratified distribution of rescaled propensity scores
write.csv(aTable2_strat, 
          paste0(path_to_box, "KD_bootstrapping/Scripts_18May2022/Final graphics/Manuscript_aTable2_021422_strat.csv"))

# aTable 3. Stratified by race, Summary of stabilized ADC weights by raking, logistic, GBM, and SL
write.csv(aTable3, "C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_18May2022/Final graphics/Manuscript_aTable3_021422.csv")

# # aTable4-- included in Covariate Balance portion of script above
# write.csv(aTable4, "C:/Users/ehlarson/Box/KD_bootstrapping/Scripts_18May2022/Final graphics/Manuscript_aTable4_021422.csv")















