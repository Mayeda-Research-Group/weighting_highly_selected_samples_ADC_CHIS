# weighting_highly_selected_samples

Scrips for the project "Accounting for selection probabilities in health research: Are estimates of inequalities similar across weighting schemes?" The scripts listed below were used to construct the harmonized analytic dataset, estimate prevalences and inequalities in CHIS (target population sample), calculate weights using different weighting schemes, apply the weighting schemes to ADC (highly-selected sample), display the results in tables and graphs, and scripts for running code on computing cluster. The scripts are listed in order and briefly described. 

a_Harmonizing_ADC.sas  
This script provides data management of the ADC data in order to create harmonized variables ready to be merged with CHIS.

b_Harmonizing_CHIS.sas  
This script provides data management of the CHIS data in order to create harmonized variables ready to be merged with ADC.

c_Combining_ADC_CHIS.sas  
This script merges the harmonized ADC and CHIS datasets and creates additional new variables necessary for the analysis.

d_CHIS_truth.R  
This script calculates hypertension prevalence by race/ethnicity and racial/ethnic inequalities in hypertension in CHIS. 

e_Raking_Weight_Calculations.R  
This script runs a raking algorithm to calculate raking weights.

f_Logistic_Weight_Calculations.R  
This script runs logistic regression models to create propensity scores that will be used to calculate logistic regression weights.

g_GBM_Weight_Calculations.R  
This script runs a gradient boosting modeling (GBM) algorithm to create propensity scores that will be used to calculate GBM weights.

h_SuperLearner_Weight_Calculations.R  
This script runs super learner (SL) algorithms to calculate propensity scores that will be used to calculate SuperLearner weights.

i_Applying_Weighting_Schemes.R  
This script first calculates weights for each set of propensity scores (logistic, GBM, and SL) then applies each weighting scheme set to the ADC sample to estimate weighted prevalences by race/ethnicity and racial/ethnic inequalities in hypertension in CHIS.

j_Cluster_Computing_Analysis.R  
This script selects 2,000 boostrapped stamples to estimate 95% CIs for ADC prevalence and inequality estimates under each weighting scheme. This script is run on the computing cluster. 

k_Cluster_Computing_Submission_Scripts   
This folder includes two scripts (a_submit_bootstrap.sh and b_submit_bootstrap_missing_seeds.sh) that are used to push the code on the computing cluster.

l_Cluster_Computing_Seed_Checking.R  
This script checks seeds for cluster computing.

m_Format_to_Graphics.R  
This script prepares output for tables and figures and outputs some manuscript tables. 

n_Creating_Figures_Graphs.Rmd  
This script generates manuscript figures and additional manuscript tables.
