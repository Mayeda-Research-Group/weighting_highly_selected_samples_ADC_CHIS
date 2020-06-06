# weighting_highly_selected_samples

Scrips for the project "Accounting for selection probabilities in dementia research: Are estimates of inequalities similar across weighting schemes?" The scripts listed below were used to construct the harmonized analytic dataset, calculate weights using different weighting schemes, apply the weighting schemes, and display the results in tables and graphs. The scripts are listed in order and briefly described. 

1) a_Harmonizing_ADC.sas
This script provides data management of the ADC data in order to create harmonized variables ready to be merged with CHIS.

2) b_Harmonizing_CHIS.sas
This script provides data management of the CHIS data in order to create harmonized variables ready to be merged with ADC.

3) c_Combining_ADC_CHIS.sas
This script merges the ADC and CHIS pre-harmonized datasets and creates additional new variables necessary for the analysis.

4) d_Raking_Weight_Calculations.Rmd
This script runs a raking algorithm to calculate raking weights.

5) e_Logistic_Weight_Calculations.sas
This script runs logistic regression models to create propensity scores that will be used to calculate logistic regression weights.

6) f_GBM_Weight_Calculations.Rmd
This script runs a GBM algorithm to create propensity scores that will be used to calculate GBM weights.

7) g_SuperLearner_Weight_Calculations.Rmd
This script runs Super Learner algorithms to calculate propensity scores that will be used to calculate SuperLearner weights.

8) h_Applying_Weighting_Schemes.sas
This script first calculates weights for each set of propensity scores (logistic, GBM, and SL) then applies each weighting scheme set to the ADC and 	CHIS samples to estimate weighted prevalences, prevalence ratios, prevalence differences, and odds ratios of hypertension.

9) i_Collapsing_Output.sas
This script provides data management to the output from applying the weighting schemes (estimated PR, PD, OR, and prevalences) to 	prepare the data for table and graph creation.

10) j_Running_Diagnostics.sas
This script runs the Twang macro to calculate covariance balance statistics. 

11) k_Creating_Figures_Graphs.Rmd
This script uses the output from the applied weighting schemes and covariate balance macro to create manuscript tables and graphs. 
