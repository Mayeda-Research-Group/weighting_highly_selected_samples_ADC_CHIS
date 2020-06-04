# weighting_highly_selected_samples

Scrips for the project "Accounting for selection probabilities in dementia research: Are estimates of inequalities similar across weighting schemes?" The scripts listed below were used to construct the harmonized analytic dataset, calculate weights using different weighting schemes, apply the weighting schemes, and display the results in tables and graphs. The scripts are listed in order and briefly described. 

1) 1_01_ADC_Harmonizing_AM_ERM.sas
This script provides data management of the ADC data in order to create harmonized variables ready to be merged with CHIS.

2) 2_02_CHIS_Weighted_Estimates_AM_ERM.sas
This script provides data management of the CHIS data in order to create harmonized variables ready to be merged with ADC.

3) 3_03_Combining ADC and CHIS_AM_ERM.sas
This script merges the ADC and CHIS pre-harmonized datasets and creates additional new variables necessary for the analysis.

4) 4_01_Weight_Calculations_AM_v2_ERM.sas	
This script runs logistic regression models to create propensity scores that will be used to calculate logistic regression weights.

5) 5_GBM_2019_09_22.Rmd
This script runs a GBM algorithm to create propensity scores that will be used to calculate GBM weights.

6) 6_Raking_2019_09_22.Rmd
This script runs a raking algorithm to calculate raking weights.

7) 7_SL_2019_09_22.Rmd
This script runs Super Learner algorithms to calculate propensity scores that will be used to calculate SuperLearner weights.

8) 8_01_Prev-OV-PR-PD_Automized_AM_v2_ERM.sas
This script first calculates weights for each set of propensity scores (logistic, GBM, and SL) then applies each weighting scheme set to the ADC and 	CHIS samples to estimate weighted prevalences, prevalence ratios, prevalence differences, and odds ratios of hypertension.

9) 9_02_Collapsing_Output_v2_ERM.sas	
This script provides data management to the output from applying the weighting schemes (estimated PR, PD, OR, and prevalences) to prepare the data for table and graph creation.

10) 10_03_Diagnostics_v3_ERM.sas
This script runs the Twang macro to calculate covariance balance statistics. 

11) 11_04_RWP_Graphs_092219_erm.Rmd
This script uses the output from the applied weighting schemes and covariate balance macro to create manuscript tables and graphs. 
