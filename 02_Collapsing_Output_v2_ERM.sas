/* Date: 1/10/2017*******************************************************************************************************/
/* Analyst: Audrey Murchland ************************************************************************************/
/*Code review: ER Mayeda 01/17/2020, 3/20/2020
	*Adding comments: ERM edited this program on 3/20/2020 to also output the rescaled propensity score distributions. I used ARM's code in the RMD file for outputing the propensity score density plots as a guide (04_RWP_Graphs_092219_erm.Rmd).*/

/*Collapsing Reweighting Applied Weights Results***************************/

libname results 'C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA\Applied_Weights';
libname harm 'C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA';

proc format;
value ra  1='Black'
          2='Latino'
		  3='White';
		  run;

*Read in unweighted and various weighted estimates (prevalence, relative risk, risk difference, odds ratio estimates);
%macro dd1(dtin=, dtot=);
data &dtot;
set results.&dtin;
run;
%mend;
%dd1(dtin=ADC_unweighted, dtot=ADC_unweighted);
%dd1(dtin=adc_wt_log, dtot=adc_wt_log);
%dd1(dtin=adc_wt_raked, dtot=adc_wt_raked); 
%dd1(dtin=adc_wt_sl, dtot=adc_wt_sl); 
%dd1(dtin=adc_wt_gbm, dtot=adc_wt_gbm);
%dd1(dtin=chis_unweighted, dtot=chis_unweighted);
%dd1(dtin=chis_weighted, dtot=chis_weighted);

*Concatenate data sets;
data all_weights;
set ADC_unweighted
adc_wt_log
adc_wt_raked 
adc_wt_sl
adc_wt_gbm
chis_unweighted 
chis_weighted;
if Statistic='Relative R' then Statistic='Rel Risk';
run;

*Create new variables with clearer labeling of data set (ADC vs. CHIS) and weights applied;
data all_weights;
set all_weights;
length model2 $25.;
format model2 $25.;
if model='ADC_Unweighted' then model2='ADC Unweighted';
else if model='ADC_WT_Log' then model2='ADC Logistic';
else if model='ADC_WT_Raked' then model2='ADC Raked';
else if model='ADC_WT_SL' then model2='ADC SuperLearner';
else if model='ADC_WT_GBM' then model2='ADC SL GBM';
else if model='chis_unweighte' then model2='CHIS Unweighted';
else if model='chis_weighted' then model2='CHIS Weighted';
run;

*More clarity in labeling;
data all_weights;
set all_weights;
if Group='Hisp vs. Whites' then Group='Latino vs. Whites';
else if Group='Hispanic' then Group='Latino';

if model='ADC_Unweighted' then model3="G1";
else if model='ADC_WT_Raked' then model3="G3";
else if model='ADC_WT_SL' then model3="G4";
else if model='chis_weighted' then model3="G5";
else if model='ADC_WT_GBM' then model3="G6";
else model3="G2";

if model='chis_unweighte' then delete;

run;

proc sort data=all_weights; by model; run;

*More clarity in labeling;
data all_weights;
set all_weights;
informat model2 $30.;
format model2 $30.;
if Group="African American" then Group="Black";
if Group="AA vs. Whites" then Group="Black vs. White";
if Group="Blacks" then Group="Black";
run;

data results.all_weights092219;
set all_weights;
run;


/*Creating Density Plots and Diagnostic Plots Dataset*/
data harm.weights_comb_short_092219;
set harm.weights_comb_092219; /*This was created in 01_Prev-OR-PR_PD_Automized_AM_v2_ERM.sas*/
keep id psid adc race black white /*ERM note: not sure why "black" and "white" were retained and not "latino", but maybe none are needed since "race" was retained*/
/*logistic*/ log_wt ip_0 ip_1
/*raked*/ rak_wt
/*sl*/ sl_wt pp_sl_0 pp_SL
/*gbm*/ gbm_wt pp2_chis pp2_adc
;
run;

/******************Summary statistics on weights*****************/
data weights;
set harm.weights_comb_short_092219;
run;

ods pdf file="C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Output\RWP_Output_03_Analysis\Hypertension\Weight_Distributions_09222019.pdf";
*Table 2; 
proc means data=weights n nmiss mean std min p10 p25 p50 p25 p50 p75 p90 max; var LOG_wt RAK_wt SL_wt GBM_wt; where adc=1; run;
*eTable 3;
proc means data=weights n nmiss mean std min p10 p25 p50 p25 p50 p75 p90 max; var LOG_wt RAK_wt SL_wt GBM_wt; class race; where adc=1; run; 
ods pdf close;

proc contents data=weights; run;
proc freq data=weights; tables race/missing; run;


/******************Summary statistics on propensity scores*****************/
*As a reference for this code, I reviewed 04_RWP_Graphs_092219_erm.Rmd code for the propensity score density plots for areas of common support;
*We are multiplying the propensity scores by the ratio of sample sizes for interpretability;
data propensity_scores_rescaled;
set harm.weights_comb_short_092219;
pp_LOG2 = IP_1*(4622757/856); *rescaled propensity score for ADC participation \nestimated from logistic regression;
pp_GBM2 = pp2_ADC*(4622757/856); *rescaled propensity score for ADC participation \nestimated from GBM;
pp_SL2 = pp_SL*(4622757/856); *rescaled propensity score for ADC participation \nestimated from SuperLearner;
*no PP for Raking because it's not a propensity-score based weighting approach;
run;

ods excel file="C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Output\RWP_Output_03_Analysis\Hypertension\Propensity_scores_rescaled_Distributions_09222019.xlsx";
*eTable 2; 
proc means data=propensity_scores_rescaled n nmiss mean std var min p10 p25 p50 p25 p50 p75 p90 max; var pp_LOG2; class adc; run;
proc means data=propensity_scores_rescaled n nmiss mean std var min p10 p25 p50 p25 p50 p75 p90 max; var pp_GBM2; class adc; run;
proc means data=propensity_scores_rescaled n nmiss mean std var min p10 p25 p50 p25 p50 p75 p90 max; var pp_SL2; class adc; run;
*same table, stratified by race/ethnicity;
proc means data=propensity_scores_rescaled n nmiss mean std var min p10 p25 p50 p25 p50 p75 p90 max; var pp_LOG2 pp_GBM2 pp_SL2; class adc race; run; 
ods excel close;
