/*************************************************** Weight calculations ****************************************************************************************/
/* Primary Analyst: Audrey Murchland (2017); using Meena Doshi's previous code as a framework */
/* Code review and revisions by Elizabeth Rose Mayeda on September 22, 2019 */ 

/* STEPS:

1) Getting the datasets needed
2) Storing the overall sum of CHIS wts for ADC =0 and 1 in call Symput vars (to call later)
3) Running logistic models (pt1 and pt2) 
4) Meena previously created IOSW WEIGHTS (using IP_0, IP_1 and &adcrep/&chisrep) here, but ARM moved all weight calculations to one program, so ERM deleted this code. 
5) Saving data set. This will be used in the calculation of Prevalence, OR/RR/RD.

/***************************************** Step1: Getting the datasets needed from Lib Harm *********************************************************************/

libname harm 'C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA';
		/*proc univariate data=harm.m1b; var IP_0 IP_1; run; 
		proc univariate data=m1b; var IP_0 IP_1; run;*/

data adc_chis2;
set harm.adc_chis2_am_09s;/*13113*/
run;

proc format;
value ra  1='Black'
          2='Latino'
		  3='White';

value spk 1='English'
           2='Spanish'
		   3='Both'
		   4='Other' /*shouldn't be anyone in this category if using Language1*/
		  -9='Missing'; /*shouldn't be anyone in this category Language1*/
run;

*checking that dummied out race, language, and education variables are coded correctly;
/*proc contents data=adc_chis2; run;
		proc freq; format race ra.; tables race black hispanic white race*black race*hispanic race*white ; run;
		proc freq; tables language1 lang1_eng lang1_span lang1_both language1*lang1_eng language1*lang1_span language1*lang1_both; run;
		proc freq; tables edu_harm1 edu1_noed edu1_elem edu1_somehs edu1_hs edu1_somecol edu1_colpostgrad; run;
		proc freq data=adc_chis2; tables language12*dtnm language1*dtnm language*dtnm edu_harm12*dtnm edu_harm1*dtnm/missing; run;
			proc freq data=adc_chis2; tables edu_harm1_miss edu_harm12/missing;run;
					*ods pdf file="C:\Users\amurchland\Dropbox\Meena_ERM_MMG\Reweighting_ADC\Harmonizing ADC CHIS Vars\New_Heart_Vars.pdf" ;
					proc freq data=adc_chis2; tables congestiveheart heartdz heartdz*congestiveheart hypertension cholesterol/missing; by adc; run;
					proc freq data=adc_chis2; tables adc*(congestiveheart heartdz hypertension cholesterol)/missing; run;
					*ods pdf close;

	ods pdf file="C:\Users\amurchland\Dropbox\Meena_ERM_MMG\Reweighting_ADC\Harmonizing ADC CHIS Vars\Positivity_Cross_Tabs_M3B.pdf" ;
	proc freq data=adc_chis2; tables Nage_1*race Nage_1*male Nage_1*edu_harm12 Nage_1*language12
	race*male race*edu_harm1 race*language1 male*edu_harm1 male*language1 edu_harm1*language1/missing; by adc; run;
	ods pdf close;
	*/

/*********************** Step 2: Storing the overall sum of CHIS wts for ADC =0 and 1 in call Symput vars (to call later) ****************************************/
/*This will save the # of participants as a variable so that it can be used in weight calculations later. However, we do not use these weight calculations. I recalculate
the weights at the beginning of RWP_Code_03_Analysis after I bring in the SL, GBM, and Raking estimates from Kristina*/

/*Sum of overall CHIS sample weights RAKEW0, and ADC sample wts*/
/* CHIS Sample*/
title;
proc means data=adc_chis2 n sum;
where ADC=0; /* CHIS*/							
var RAKEDW0;
output out=tot sum=chistotre;
run;
/* store value of sum of RAKEDW0 in a macro var to call later.*/
data _null_;
set tot;
call symput('chisrep', chistotre);
put 'value of Chisrep is=' chistotre;/* log shows value*/
run;
/*Look at CHIS weighted sample size by race/ethnicity (not storing for later)--added by ERM 9/22/2019*/
proc sort data = adc_chis2; by adc race; run;
proc means data=adc_chis2 n sum;
where ADC=0; /* CHIS*/	
format race ra.;
by race;	
var RAKEDW0;
output out=tot sum=chistotre;
run;

/* ADC Sample*/
title;
proc means data=adc_chis2 n sum;
where ADC=1; 	/*ADC*/						
var RAKEDW0;
output out=tot1 sum=adctotre;
run;
/* store value of sum of RAKEDW0 in a macro var to call later.*/
data _null_;
set tot1;
call symput('adcrep', adctotre);
put 'value of adcrep is=' adctotre; /* log shows value*/ /*856*/
run;
/*Look at ADC sample size by race/ethnicity (not storing for later)--added by ERM 9/22/2019)*/
proc freq data = adc_chis2; format race ra.;  tables adc adc*race; run;


/***************** Step 3: Running various Proc Logistic Models to produce datasets with predicted probabilities (IP_0, IP_1) ************/
/*******USING PREVIOUSLY CREATED SPLINE TERMS********/
/*!!!!!! NOTE: IN THIS STEP, MAKE SURE THE CLASS VARS ARE CORRECT FOR EACH MODEL THAT IS RUN. Macro var is CL  !!!!!!*/
/*IP_0: Prob of being included in CHIS, IP_1: Prob of being included in ADC*/
	/*proc freq data=adc_chis2; tables edu_harm1 language1/missing;run;*/

%let educ_hsref= edu1_noed edu1_elem edu1_somehs /*hs=ref*/ edu1_somecol edu1_colpostgrad ;
%let lang_enref= /*eng=ref*/ lang1_span lang1_both; 

options symbolgen mprint;
%macro mod(dtin=, cl=, vars=, dtot=);
PROC SURVEYLOGISTIC DATA =&dtin VARMETHOD=JACKKNIFE;
format race ra. ;                                                             
WEIGHT rakedw0; REPWEIGHT rakedw1-rakedw80/JKCOEFS=1;
CLASS &cl/PARAM=REF;
MODEL ADC = &vars ;
output out=&dtot predprobs=(I);/* This gives pred prob for ADC=0 and ADC=1*/
RUN;
quit;
%mend;

/*Stepwise selection, new method*/
/*Part 1: similar to raking: age, sex,race, education, interactions of race with all covariates*/ 
/*Original Raking Variables*/	
%mod(dtin=adc_chis2,  cl=%str(ADC(REF="0") race (REF="White") male (REF="0") edu_harm1(REF="3")), vars=Nage Nage_1 race male edu_harm1 Nage*race Nage_1*race male*race edu_harm1*race, dtot=MSLPT1);

/*Part 2: Adding Health Measures after inspection 
/*Adding Health Measures based on inspection*/
%mod(dtin=adc_chis2,  cl=%str(ADC(REF="0") race (REF="White") male (REF="0") edu_harm1(REF="3")), vars=Nage Nage_1 race male edu_harm1 Nage*race Nage_1*race male*race edu_harm1*race Diabetes BMI BMI_1 Cholesterol Diabetes*race BMI*race BMI_1*race Cholesterol*race, dtot=MSLPT2);


/*************************** &&&&&&&&&&&&&&&&&&&& STEP 4: CREATING IOSW WEIGHTS AS PER Mehrota summary &&&&&&&&&&&&&&&&&&&&&&&& ********************************/
*ERM deleted this code on 9/26/2019 because it's not used -- all weights are calculated in the same program;


/**************************** Step 5: Saving data sets  ***************************************************************************/
libname harm 'C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA';

data Harm.MSLPT1_092219;
set MSLPT1;
run;

data Harm.MSLPT2_092219;
set MSLPT2;
run;

