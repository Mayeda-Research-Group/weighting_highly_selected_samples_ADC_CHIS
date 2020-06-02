/****************************************************************************************************************************************************************/
/************************************************ Variables needed from ADC (to harmonize with CHIS) ***********************************************************/
/** STEPS:
	- Keeping the required vars, excluding people missing race/ethnicity or health info. This results in 879 observations. 
	- Removing the duplicates observations (ADC is a tall data set, but we only want data from the baseline health evaluation).
	- Creating new varaibles: BMI, dtnm=AD
	-Creating harmonized version of other variables*/ 

/* ADC varaibles ready to harmonized with CHIS are: EDUC(to HEDUC),CHOL(to HChol1),Hypten(to HHTN)*/
/* Pl mote: variables Hyperten and Hyperten1; hypercho and hypercho1 and CVA_TIA and CVA_TIA1 are identical*/
/* Gender: 1:male and 2:female*/

/* Primary Analyst: Audrey Murchland (September 2017); using Meena Doshi's previous code as a framework */
/* Code review and revisions by Elizabeth Rose Mayeda on September 20, 2019 */ 
/****************************************************************************************************************************************************************/

/* Importing ADC data from UC Davis (Data received from Dan Mungas)*/
proc import datafile="C:\Users\emayeda\Box Sync\UCD Health Cognition\Data\UCDHC_DATA\senas_health_long_d_sas.csv"
	out=Datasenas_health_long_d
	dbms=csv
	replace;
	getnames=yes;
run;
proc contents data = Datasenas_health_long_d; run;

/*applying ADC formats*/
LIBNAME library 'C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Programs\RWP_Formats_ADC';
options fmtsearch=(library);

/*AM: Importing Cleaned ADC Dataset (restricted to Black/African American, Latino/Hispanic, and White participants & no dupkey)
libname am "C:\Users\emayeda\Box Sync\UCD Health Cognition\DHC_ARM_work\DHC_Data";*/

/***************************** Keeping variables of interest *******************************************/
/* 3687 observations */
data ADC_group;
set Datasenas_health_long_d;
keep
psid /*ID N*/
age /*age*/
gender /*sex*/
grp ethnic /* ethnicity*/
testlang /*languse frstlang preflang langused*/ converse /*watchtv talkfam talkoth readlang*/ /* primary lang*/
educ /* edu attain*/
/* Health Characteristics */
diabetes DIAB DIABRX diabet/* diabetes*/
htn hypten hyperten/* hypertension*/
heartdis cvchf/* heart disease congestive heart failure*/
Cbstroke /* Stroke*/
Hyperlip Chol HYPERCHO/* cholesterol*/ 
HYPERTEN HYPTEN /* cholesterol/ hypertention*/ 
psid time timevrf1 /*centered time at health assessment*/
se_sassessn firsteval
weight height
statin_med 
;
run;
proc contents data = ADC_group; run;
/************************** Exclude obs where race=. *******************************************************/

Proc freq data=ADC_group;
tables grp ethnic/missing; /*191*/
run;

data ADC_grp;/* 3496*/
retain grp;
set ADC_group;/* 3687*/
if grp=' ' then delete;
run;

/* Counting the number of unique ids: There are 918 unique ID's (psid's) in ADC*/
/* However, IMPORTANT to sort by psid and timevrf1 (centered time at health evaluation)*/
proc sort data=ADC_grp;by psid timevrf1;run; /*3496*/

/* Method 1: timevrf1=0*/
/*879 unique IDs with health measures @ first health evaluation*/
proc sort data=ADC_grp nodupkey; by psid timevrf1; /*3485*/
run;

data ADC_uni_ids; /*879*/
set ADC_grp; /*3485*/
if timevrf1=0;
run;

	/*proc contents data=ADC_uni_ids; run;
	proc freq data=ADC_uni_ids; tables heartdis cvchf hypercho*chol/missing; run;*/

/********************************** Creating the BMI varaible ***************************************/
/*BMI = (Weight in Kg / (Height in m x Height in m))*/
*Note: On 9/20/2019, ERM edited the code so BMI is only created for people with height or weight. The old code shouldn't be a problem, but the revised code avoids warning message in the log about missing values. 
I checked the proc means before and after revising the code and the summary stats are the same.;
data ADC_uni_ids; set ADC_uni_ids; 
heightm = (height*0.0254);
weightkg = (weight*0.453592);
run;
data ADC_uni_ids; set ADC_uni_ids; 
if weightkg NE . and heightm NE . then do;
	BMI = ((weightkg)/(heightm**2)); 
end;
else if weightkg = . or heightm = . then BMI = .;
run;
proc means data=ADC_uni_ids n nmiss mean stddev min max; var height weight heightm weightkg bmi; run;

	/*check*/
		/*BMI = (Weight in Pounds / (Height in inches x Height in inches)) x 703*/
		/*data ADC_uni_ids;
		set ADC_uni_ids;
		Hsq=height*height;
		num=weight*703;
		BMI2=num/hsq;
		if height=. or weight=. then BMI=.;
		run;*/
	/*proc means data=adc_uni_ids n nmiss; var bmi bmi2; run;*/
	/*proc print data= ADC_uni_ids (firstobs=10 obs=30);
	var height weight heightm weightkg bmi2 bmi;
	run;
	/* 71 with either height=. or weight=. at timevrf1=0
	proc print data= ADC_uni_ids;
	var height weight psid;
	where height=. or weight=. and timevrf1=0;
	run;*/
	/* 808 non-miss bmi
	proc freq data=ADC_grp;
	tables height weight;
	where  height ne . and weight ne . and timevrf1=0;
	run;*/
	/*data adc_uni_ids; set adc_uni_ids; drop bmi2 num hsq; run;*/

/************************************* Creating a varaible dtnm='AD' needed while concatinating with CHIS **************************************/
data ADC_uni_ids;
set ADC_uni_ids;
dtnm='AD';
run;
			/***** Higher level: looking at frequencies, means by race(variable- grp)

			proc freq data=ADC_uni_ids;
			tables gender grp educ/missing;
			run;

			proc freq data =ADC_uni_ids;
			tables htn hypten/missing;run;

			proc freq data=ADC_uni_ids;
			TABLES 
			DIABETES 
			DIAB 
			DIABRX 
			htn hypten
			Cbstroke
			Hyperlip chol hypercho
			HYPERTEN HYPTEN
			heartdis/missing;RUN;

			Proc means data =ADC_uni_ids; var heartdis;
			run;*/

/************************* SORT by RACE **********************/
Proc sort data=ADC_uni_ids;
by grp;
run;
/* NOTE: none missing grp*****/
proc freq data=ADC_uni_ids;
tables grp /missing;
run;

/**************************** Adding Marital Status to Dataset *******************************/
libname new "C:\Users\emayeda\Box Sync\UCD Health Cognition\Data\ADC-CHIS_DATA";
/*data new.adc_newvars_041918; set new_adc; run;*/

data new_adc;
set new.adc_newvars_041918; run;
	/*	proc contents data=new_adc; run;
		proc freq data=new_adc; tables Maristat; run;	*/

data adc_new2; 
set new_adc;
if Maristat <0 or Maristat=9 or Maristat=8 then Maristat=.;
run;

	/*	proc freq data=adc_new2; tables ClinAssessN*Maristat/missing;run;	*/

data new_adc_nmiss;
set adc_new2;
if Maristat ne .;
if ClinAssessN =. then delete;
run;
 
proc sort data=new_adc_nmiss; by psid clinassessn; run;

data maristat;
set new_adc_nmiss;
by psid clinassessn;
if first.psid;
run; 

/*Code to bring in dataset for merge if updating from later point*/
/*data adc_orig; set new.adc_chis2_am_09s; if adc=1; psid=id; run;*/

proc sort data=ADC_uni_ids; by psid; run;
proc sort data=maristat; by psid; run;

data ADC_uni_ids2;
merge ADC_uni_ids(in=A) maristat(in=B);
by psid;
if A;
run;

	/* Checks
	proc freq data=ADC_uni_ids2; tables  ClinAssessN Maristat/ missing;run;
	proc freq data=ADC_uni_ids2; tables maristat*race/missing;run;
	*/

/********************************************************************************************************************
									Harmonizing ADC w/ CHIS
***********************************************************************************************************************/
/*Checks
		proc freq data=ADC_uni_ids2; tables Heduc/missing; *3 missing; run;
		proc freq data=ADC_uni_ids2; tables converse converse*testlang/missing; format converse plang. testlang langint.; by grp; run;
		proc freq data=ADC_uni_ids2; tables cbstroke hypten*htn/missing; run;	*/

data ADC_uni_ids2; 
set ADC_uni_ids2;

/*******Marital Status********/
if maristat=. then marit2=.;
else if maristat=1 then marit2=1;
else if maristat=2 or maristat=3 or maristat=4 then marit2=3;
else if maristat=5 then marit2=4;
else if maristat=6 then marit2=2;

if marit2 ne . then do;
	marry_bi=(marit2=1 or marit2=2); /*married or living with partner*/
end;

/*Education*/
if educ=. then Heduc=.;
else if educ=0 then Heduc=0;
else If 1<=educ<=8 then Heduc=1;	/* equivalent to AHEDUC=1 in CHIS */
else  If 9<=educ<=11 then Heduc=2;	/* equivalent to AHEDUC=2 in CHIS */
else If educ=12 then Heduc=3;
else If 13<=educ<=15 then Heduc=4;	/* equivalent to AHEDUC=2 in CHIS. AH47=13,14,15 or 22 */
else If educ<=16 then Heduc=5;
else If educ>16 then Heduc=6;

/*stroke prevalence*/
if cbstroke=. then stroke=.;
else if cbstroke=0 then stroke =0; 
else if cbstroke=1 then stroke =1; 
else if cbstroke=2 then stroke = 1; 
else if cbstroke=9 then stroke=.;

/* creating race*/
if grp='African American' then do; race=1;SRAA=1; end; /*AA*/
else if grp='Hispanic' then do; race=2;SRH=1;end;/*Hispanic*/
else if grp='White' then do; race=3;SRW=1; end;/*White*/

If SRAA=. then SRAA=2; If SRH=. then SRH=2; If SRW=. then SRW=2;

/*diabetes: Ever having*/
if diabet=. then diabetes=.;
else if diabet=0 then diabetes =0; 
else if diabet=1 then diabetes =1; 
else if diabet=2 then diabetes = 1; 
else if diabet=9 then diabetes=.;

/*hypertension:Ever having*/
if hyperten=. then Hypertension=.;
else if hyperten=0 then Hypertension =0; 
else if hyperten=1 then Hypertension =1; 
else if hyperten=2 then Hypertension = 1; 
else if hyperten=9 then Hypertension=.;

/*Cholesterol: Most current */
if hypercho=. then Cholesterol=.;
else if hypercho=0 then Cholesterol =0; 
else if hypercho=1 then Cholesterol =1; 
else if hypercho=2 then Cholesterol = 0; 
else if hypercho=9 then Cholesterol=.;

/*Heart Disease: Ever having any indicator*/
if HeartDIS=. then HeartDz=.;
else if heartdis=0 then HeartDz=0;
else if heartdis>0 then HeartDz=1;

/*Congestive Heart Failure: ever having and if heart dz hx present*/
if HeartDz=. then CongestiveHeart=.;
else if HeartDz=1 and (cvchf=. or cvchf=9) then CongestiveHeart=.;
else if HeartDz=1 and cvchf=0 then CongestiveHeart=0; 
else if HeartDz=1 and (cvchf=1 or cvchf=2) then CongestiveHeart=1;
else if HeartDz=0 then CongestiveHeart=0;

/*renaming variables*/
Nage=age;
sex=gender;
Edu_harm=Heduc;
Language=Converse;
intvlang=testlang;
ID=psid;
run;

	/*checks*/
	/*proc freq data=ADC_uni_ids2; tables diabet*diabetes hyperten*Hypertension hypercho*Cholesterol heartdis*HeartDz HeartDz*CongestiveHeart HeartDz*cvchf cvchf*CongestiveHeart/missing; run;
	proc freq data=ADC_uni_ids2; tables cvchf*heartdis congestiveheart*heartdz congestiveheart*cvchf /missing; run;
	proc freq data=ADC_uni_ids2; tables congestiveheart*cvchf/missing; where heartdz=1; run;
	proc freq data=ADC_uni_ids2; tables hypercho cholesterol hypercho*statin_med /missing; run;*/
	/*proc freq data=ADC_uni_ids2; tables grp*(race SRAA SRH SRW)/missing nocol norow nopercent; run;
	proc freq data=ADC_uni_ids2; tables sex*gender diabetes*diab edu_harm*heduc cholesterol*chol hypertension*hypten language*converse/missing nocol norow nopercent; run;
	proc means data=ADC_uni_ids2; var nage age; run;*/

/********************* Saving the data set ADC_uni_ids in a library ****************************************************************************************/
libname ADC 'C:\Users\emayeda\Box Sync\UCD Health Cognition\Data\ADC-CHIS_DATA';

data adc.ADC_uni_ids_06;
set ADC_uni_ids2;
run;

/*NOTE: There were 879 observations read from the data set WORK.ADC_UNI_IDS2.
NOTE: The data set ADC.ADC_UNI_IDS_06 has 879 observations and 52 variables.*/

/*End of program*/
