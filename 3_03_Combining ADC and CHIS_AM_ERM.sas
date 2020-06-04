/****************************************************************************************************************************************************************/
/******************************************************** Combining ADC and CHIS data **************************************************************************/
/* Primary Analyst: Audrey Murchland (September 2017); using Meena Doshi's previous code as a framework */
/* Code review and revisions by Elizabeth Rose Mayeda on September 20-21, 2019 */ 
/** STEPS:
1)  In both ADC and CHIS data, restrict to variables we are using in our analyses. 
		Top code age at 85 years in the ADC data set to align with CHIS age variable.
2)  CONCATENATE ADC and CHIS (879 + 12,257 = 13,136 observations).
3)  Create indicator var for ADC, new variable "male" and assigning ADC participants CHIS weights of 1.0.
4)  Drop vars not required.
		Exclude participants with missing age or age < 60 years. 
5) 	Collapsing categories for later analyses (education and language).
6)	Evaluating Missingness in ADC_CHIS. Imputing missing values. Create a centerd BMI variable (bmic25).
7)  Saving the dataset without splines for use in R "harm.adc_chis2_am_09" (save as both SAS and R data sets).
8)  Creating Splines for Age, BMI, and BMIc25.
9)  Saving the dataset "harm.adc_chis2_am_09s."
10) Creating Dummies and save "harm.adc_chis2_am_09s" with splines and dummies.

/***Note: race: 1=AA 2=hisp 3=white
          adc_chis2 data set excludes Nage<60 or Nage=.*/
/*****************************************************************************************************************************/


/********************* Using the data set ADC_uni_ids in a library ****************************************************************************************/
libname ADC 'C:\Users\emayeda\Box Sync\UCD Health Cognition\Data\ADC-CHIS_DATA';

/*STEP 1: Restrict to variables we will be using*/
data ADC_r; /*879*/
set adc.ADC_uni_ids_06;
keep ID dtnm race sex diabetes Nage Edu_harm SRH SRAA SRW BMI Cholesterol Hypertension HeartDz CongestiveHeart Stroke 
	language intvlang marit2 marry_bi;
run;
/*NOTE: There were 879 observations read from the data set ADC.ADC_UNI_IDS_06.
NOTE: The data set WORK.ADC_R has 879 observations and 20 variables.*/
	/*Top Coding ADC age range - this is to match the CHIS data set, where age was top coded*/
	data ADC_r2;
	set ADC_r;
	if Nage>85 then Nage=85; 
	run;
/*NOTE: There were 879 observations read from the data set WORK.ADC_R.
NOTE: The data set WORK.ADC_R2 has 879 observations and 20 variables.*/
	/*	proc means data=adc_r n nmiss min q1 median q3 p90 max; var Nage; title "adc_r"; run;
		proc means data=ADC_r2 n nmiss min q1 median q3 p90 max; var Nage; title "adc_r2"; run;
		 */

/***********************************************Using the data set adult60_CHIS in the Lib ***********************************************************/
libname Chis 'C:\Users\emayeda\Box Sync\UCD Health Cognition\Data\ADC-CHIS_DATA\CHIS\CHIS 2005 Adult data';

/* STEP 1: Restrict to variables we will be using*/
data CHIS_r;/*12257*/
set chis.adult60_CHIS_AM_05;
keep ID dtnm race sex diabetes Nage SRH SRAA SRW Edu_harm BMI Cholesterol Hypertension HeartDz CongestiveHeart Stroke 
language intvlang marit2 marry_bi
/* weights*/
RAKEDW0 			
RAKEDW1-RAKEDW80; 
run;
/*NOTE: There were 12257 observations read from the data set CHIS.ADULT60_CHIS_AM_05.
NOTE: The data set WORK.CHIS_R has 12257 observations and 101 variables.
There are 19 vars in the ADC data set, but 101 variables in the CHIS data set because there are 81 CHIS weight variables
(19 + 81 = 100 variables)*/
	/*proc means data=CHIS_r n nmiss min q1 median q3 p90 max; var Nage; title " "; run;
	proc means data=chis_r n nmiss min median max; var Nage; where nage>85; run; */

/******************************************************************************************/
/* STEP 2: CONCATENATE*/
data ADC_CHIS; /*13,136*/
set adc_r2 CHIS_r;/*879, 12,257*/
run;
/*NOTE: There were 879 observations read from the data set WORK.ADC_R2.
NOTE: There were 12257 observations read from the data set WORK.CHIS_R.
NOTE: The data set WORK.ADC_CHIS has 13136 observations and 101 variables.*/

/*********************************************************************************************************/
/* STEP 3: Create indicator var for ADC, new var "male" and dummy variables for race ("Black", "Hispanic", and "White",
and assign all ADC partipants CHIS weights of 1.0*/
data ADC_CHIS1;
retain dtnm adc SRAA black SRH hispanic SRW White sex male race; 
set ADC_CHIS;

if dtnm='AD' then adc=1;
else ADC=0;

if sex=1 then male=1;
else male=0;

/*Creating dummy variables for race for use in the logistic regression models--this code was revised by ERM on 9/20/2019*/
if race = 1 then Black = 1; 
else Black = 0;
if race = 2 then Hispanic = 1; 
else Hispanic = 0;
if race = 3 then White = 1; 
else White = 0;

/*Assigning all ADC participants CHIS weights of 1.0*/
if ADC=1 then do;				
rakedw0=1;
    array rakedw{80} rakedw1-rakedw80;
     do i=1 to 80;
      rakedw{i}=1;
     end;
end;
run;
/*NOTE: There were 13136 observations read from the data set WORK.ADC_CHIS.
NOTE: The data set WORK.ADC_CHIS1 has 13136 observations and 107 variables.*/
	/*check
	proc freq data=adc_CHIS1;
	tables ADC male sex diabetes dtnm race/missing;
	tables ADC*diabetes /list missing; run;
	proc freq data=adc_chis1; tables race race*(SRAA SRH SRW Black Hispanic White) 
		black*SRAA hispanic*SRH white*SRW/missing norow nocol nopercent; run;
	proc means data = adc_CHIS1; where adc=1; var rakedw0 rakedw1 rakedw20 rakedw65 rakedw80; run; */


/* STEP 4: Drop vars not required*/
data adc_chis2;
set adc_chis1;
drop  i;
run;
/*NOTE: There were 13136 observations read from the data set WORK.ADC_CHIS1.
NOTE: The data set WORK.ADC_CHIS2 has 13136 observations and 106 variables.*/

proc format;
value ra  1='Black'
          2='Latino'
		  3='White';
		  run;


/***** Exclude Nage values that are missing or < 60 years *******/
	/*proc means data=adc_chis1 n nmiss range min max; var Nage; where Nage<60; run;
	proc print data=adc_chis1; var dtnm nage; where nage<60; run;*/

data adc_chis2;/*13113 obs*/
set adc_chis2;/*13136 obs*/
if Nage=. or Nage<60 then delete;/* 14 obs are missing age, and 9 obs have age <60. 14+9=23;  13136 - 13113 = 23*/
run;
/*NOTE: There were 13136 observations read from the data set WORK.ADC_CHIS2.
NOTE: The data set WORK.ADC_CHIS2 has 13113 observations and 106 variables.*/

	/* check
	proc freq data=adc_chis2; table adc/missing; where Nage=. or  Nage<60; run;
	proc print data=adc_chis2; where Nage=. or  Nage<60; run; */


/******************************STEP 5: Collapsing categories for later analyses ***************************************/
/*proc format ;
value spk 1='English'
           2='Spanish'
		   3='Both'
		   4='Other'
		  -9='Missing';
run;*/

/* Collapsing the categories for Edu_harm in data set: ADC_chis2 because we had issues with the surveylogistic model due to 
positivity violations*/

options symbolgen mprint;

proc sort data=adc_chis2; by adc; run;
Proc freq data=adc_chis2;
tables EDu_harm;
by adc;
run;

data adc_chis2a;
set adc_chis2;
/* Collapsing college degree and post-graduate education categories*/
if Edu_harm=0 then Edu_harm1=0;
else if Edu_harm=1 then Edu_harm1=1;
else if Edu_harm=2 then Edu_harm1=2;
else if Edu_harm=3 then Edu_harm1=3;
else if Edu_harm=4 then Edu_harm1=4;
else if Edu_harm in (5 6) then Edu_harm1=5;
run;
/*NOTE: There were 13113 observations read from the data set WORK.ADC_CHIS2.
NOTE: The data set WORK.ADC_CHIS2A has 13113 observations and 107 variables.*/
				/*check
				Proc freq data=adc_chis2a;
				tables Edu_harm Edu_harm1 /missing;
				run; */

proc format;
value lango 1='English'
          	2='Spanish'
		  	3='Both'
			4='Other';
value langn 1='English'
			2='Spanish'
			3='Both';
value intlg 1='English'
			2='Spanish';
		  run;

proc freq data = adc_chis2a; tables language intvlang language*adc intvlang*adc/missing; run;
/*Creating updated collapsed language variable: For participants who reported language spoken at home as "Other" or 
language spoken at home was missing (this variable was only missing for some ADC participants, no CHIS particpants), 
use language of interview.*/
data adc_chis2b;
set adc_chis2a;
if language=. or language=4 then do
	language1=intvlang;
end;
else language1=language;
run;
/*NOTE: There were 13113 observations read from the data set WORK.ADC_CHIS2A.
NOTE: The data set WORK.ADC_CHIS2B has 13113 observations and 108 variables.*/
	/*Check
	proc freq data = adc_chis2b; tables language language1 language*language1 adc*language*language1; run;*/



/**************************************STEP 6: Evaluating Missingness in ADC_CHIS****************************************/
proc freq data=adc_chis2b; tables adc/missing; run;

data adc; set adc_chis2b; if adc=1; run;
data chis; set adc_chis2b; if adc=0; run;

proc means data=chis nmiss n mean; 
var nage
SRAA 
SRH 
SRW 
White 
adc 
Black 
diabetes 
Hispanic 
language1 
male 
race 
sex 
stroke 
bmi 
cholesterol /*207*/
edu_harm 
edu_harm1 
hypertension 
heartdz
congestiveheart
language 
marit2 
marry_bi; 
run;
proc means data=adc nmiss n mean; 
var nage
SRAA 
SRH 
SRW 
White 
adc 
black 
diabetes /*19*/
hispanic 
language1 /*8*/
male 
race 
sex 
stroke /*20*/
bmi /*71*/
cholesterol /*50*/
edu_harm /*2*/
edu_harm1 /*2*/
hypertension /*21*/
heartdz /*11*/
congestiveheart/*100*/
language /*64*/
marit2 /*4*/
marry_bi /*4*/; 
run;
/*
data adc_look_miss; set adc; 
if diabetes=. then miss=1;
else if language1=. then miss=1;
else if stroke=. then miss=1;
else if bmi=. then miss=1;
else if cholesterol=. then miss=1;
else if edu_harm1=. then miss=1;
else if hypertension=. then miss=1;
else if heartdz=. then miss=1;
else if congestiveheart=. then miss=1;
else if marit2=. then miss=1;
run;
proc freq data=adc_look_miss; tables miss miss*race/missing;run;*/


/*******Hot deck imputation with replacement for missing values, stratified by race/ethnicity. This procedure was updated
by ERM on 9/21/2019--previously imputation was not stratified by race/ethnicity, although review of means overall and
within racial/ethnic group were very similar in ADC whether the impuatation was stratified by race/ethnicity or not. ***************/
/*ADC*/
proc sort data = adc; by race; run;
proc surveyimpute data=adc method=hotdeck(selection=srswr) seed=09212019;
	by race;
	var diabetes stroke BMI Cholesterol Edu_harm edu_harm1 Hypertension HeartDz CongestiveHeart 
			Language Language1 marit2 marry_bi;
	output out=adc_mi;
	run;
*NOTE: The data set WORK.ADC_MI has 856 observations and 110 variables. --> There are two new variables (UnitID and ImpIndex) 
	from the surveyimpute procedure.;
	/*Check: I looked at means before and after stratifying by race/ethnicity overall and by race/ethnicity. Right here, 
	I'm keeping the proc means for the overall sample for the imputation stratified by race/ethnicity:

	proc means data= adc_mi n nmiss maxdec=3 mean stddev min q1 median q3 max; 
	var diabetes stroke BMI Cholesterol edu_harm edu_harm1 Hypertension HeartDz CongestiveHeart Language Language1 
	marit2 marry_bi;
	run; */

/*CHIS*/
	/*199 Missing Cholesterol */
proc sort data = chis; by race; run;
proc surveyimpute data=chis method=hotdeck(selection=srswr) seed=09212019;
	by race;
	var cholesterol;
	output out=chis_mi;
	run;
*NOTE: The data set WORK.CHIS_MI has 12257 observations and 110 variables.;
	/*Check:
	proc means data= chis_mi n nmiss maxdec=3 mean stddev min q1 median q3 max; 
	var diabetes stroke BMI Cholesterol edu_harm edu_harm1 Hypertension HeartDz CongestiveHeart Language Language1 
	marit2 marry_bi;
	run; */
	
	
/***************************Concatinating MI Data Sets******************************/
/*In the concatenation step, drop 2 vars created during impuation. Also drop variable "marit2" since it doesn't appear 
	to be in the previously saved version of the data sets produced by this program, and the binary maritial status 
	variable is what is used in the analyses included in the paper*/
data adc_chis_mi;
set adc_mi chis_mi;
drop UnitID ImpIndex marit2; 
run;
/*NOTE: There were 856 observations read from the data set WORK.ADC_MI.
NOTE: There were 12257 observations read from the data set WORK.CHIS_MI.
NOTE: The data set WORK.ADC_CHIS_MI has 13113 observations and 107 variables.*/
	/*proc contents data=adc_chis_mi; run;*/

/*Creating centered BMI variable*/ 
data adc_chis_mi; 
set adc_chis_mi; 
bmic25 = .;
if bmi NE . then bmic25=bmi-25;
else if bmi = . then bmic25 = .;
run;
/*NOTE: There were 13113 observations read from the data set WORK.ADC_CHIS_MI.
NOTE: The data set WORK.ADC_CHIS_MI has 13113 observations and 108 variables.*/
	/*check
	proc means data=adc_chis_mi n nmiss mean stddev min q1 median q3 max; var bmi bmic25; run; */



/************************************** STEP 7: Saving the datasets in lib harm: datasets adc_chis2 *************************************************************/

libname harm 'C:\Users\emayeda\Box Sync\UCD Health Cognition\Data\ADC-CHIS_DATA';

data harm.adc_chis2_am_09;/*13113*/
set adc_chis_mi;/*13113*/
run;
/*NOTE: There were 13113 observations read from the data set WORK.ADC_CHIS_MI.
NOTE: The data set HARM.ADC_CHIS2_AM_09 has 13113 observations and 108 variables.*/

/*Saving the datataset here as a safety against anything going wrong with the macros - it doesn't, but just in case.
This is the version of the data set that goes to Kristina Dang to use in R for estimation of raking weights and 
propensity-scores based on SL and GBM propensity scores.*/

/*Export data set to a csv file for Kristina Dang*/
proc export data = harm.adc_chis2_am_09 dbms=csv 
	outfile="C:\Users\emayeda\Box Sync\UCD Health Cognition\Data\ADC-CHIS_DATA\adc_chis2_am_09.csv"; 
run;



/************************************** STEP 8: Creating Splines for Age and BMI *************************************************************/

/****************************************************************************************************************************************************************/
/******************************************************** Age and BMI SPLINE CREATION **************************************************************************/
/********************************************************* 3 Knots ********************************************************************************************/
/** STEPS:
1a) ASk SAS to run rcs_prg_Harrell.sas prg before running the rest of this program (lines 1597 to 1655 in the frank_harrell_macros.sas)
1b) Get data set adc_chis2 made in the Program Combining ADC and CHIS_2017Mar11.sas and saved under the libname harm
1c) Check Nage values that are missing or <60
2(i)     Creating Restricted Cubic Splines for Nage
         a)   Proc univaraite to get the knots - specifying 3 knots. The default percentile cut points for 3 knots are: 10 50 90
         b)   Call macro %rcspline from the rcs_prg_Harrell.sas prg to create the data set spline with the 1 RCS for Age- Nage1. 
              Look in the Log for rcs var calculation.
         c)   Check on calculation of the 1 rcs vars made from %rcspline macro. Rename RCS for Nage
2(ii)    Creating Restricted Cubic Splines for BMI
         Repeat a,b,c points in 2(i) above, for BMI
3) Saving the dataset in lib harm: AGEBMI_Spline_3
Date: 05/23/2017
Analyst: Meena Doshi*/

/*	This calls the macro based on where it is saved - update filepath for your computer:	*/
options symbolgen mprint;
%inc "C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Programs\RWP_Code_01_Harmonizing\Reference_Code\rcs_prg_Harrell.sas";

/********Step1b: Get data set adc_chis2 made in the Program Combining ADC and CHIS_2017Mar11.sas and saved under the libname harm **************************/
libname harm 'C:\Users\emayeda\Box Sync\UCD Health Cognition\Data\ADC-CHIS_DATA';

data adc_chis2;/*13113*/
set harm.adc_chis2_am_09;
run;
/*NOTE: There were 13113 observations read from the data set HARM.ADC_CHIS2_AM_09.
NOTE: The data set WORK.ADC_CHIS2 has 13113 observations and 108 variables.*/

proc format;
value ra  1='Black'
          2='Latino'
		  3='White';
		  run;
/*Checking that there are no Nage values that are missing or < 60 years
proc print data=adc_chis2;
where Nage=. or  Nage<60;run;*/

proc univariate data=adc_chis2 noprint;
var Nage;
output out=knotsage pctlpre=P_ pctlpts=10 50 90;/* default locations for 3 knots*/
run;
proc print data=knotsage; run;/*61, 71, 84*/

proc univariate data=adc_chis2 noprint;
var BMI;
output out=knotsbmi pctlpre=P_ pctlpts=10 50 90;/* default locations for 3 knots*/
run;
proc print data=knotsbmi; run;/*20.99, 25.76, 33.31*/

proc univariate data=adc_chis2 noprint;
var BMIc25;
output out=knotsbmic25 pctlpre=P_ pctlpts=10 50 90;/* default locations for 3 knots*/
run;
proc print data=knotsbmic25; run;/*-4.01, 0.76, 8.31*/

options symbolgen mprint;
data AGEBMI_Spline_3;
set adc_chis2;
%rcspline (Nage,61, 71, 84 ); /*this macro - lines 1597 to 1655 in the rcs_prg_harrell.sas prg*/
%rcspline (BMI,20.99, 25.76, 33.31 ); /*this macro - lines 1597 to 1655 in the rcs_prg_harrell.sas prg*/
%rcspline (BMIC25,-4.01, 0.76, 8.31); 
run;

/********************** c) Check on calculation of the 1 rcs vars made from %rcspline macro. Rename RCS for Nage *************************************************/
data checkage;
set AGEBMI_Spline_3;
/* doing the calculation here, to check the calcualtion from %rcspline macro*/
 _kd_= (84 - 61)**.666666666666 ;

cag1=max((Nage-61)/_kd_,0)**3+((71-61)*max((Nage-84)/_kd_,0)**3
 -(84-61)*max((Nage-71)/_kd_,0)**3)/(84-71);

/* these are zeros - as identical*/
chk1=Nage1-cag1;

rename nage1=Nage_1;
run;

proc means data=checkage;
var Nage Nage_1 chk1;run;

/********************** c) Check on calculation of the 1 rcs vars made from %rcspline macro. Rename RCS for BMI *************************************************/
data checkbmi;
set AGEBMI_Spline_3;
/* doing the calculation here, to check the calcualtion from %rcspline macro*/
 _kd_= (33.31 - 20.99)**.666666666666 ;

cbmi1=max((BMI-20.99)/_kd_,0)**3+((25.76-20.99)*max((BMI-33.31)/_kd_,0)**3 -(33.31-20.99)*max((BMI-25.76)/_kd_,0)**3)/(33.31-25.76);;

/* these are zeros - as identical*/
chk2=BMI1-cbmi1;

rename bmi1=BMI_1;/* spline*/
run;

proc means data=checkbmi;
var BMI BMI_1 chk2;run;


/******************************************************* renaming vars **************************************************************************/
data AGEBMI_Spline_3;
set AGEBMI_Spline_3;
rename nage1=Nage_1 bmi1=BMI_1 bmic251=BMIC25_1;/* spline*/
run;
/*NOTE: There were 13113 observations read from the data set WORK.AGEBMI_SPLINE_3.
NOTE: The data set WORK.AGEBMI_SPLINE_3 has 13113 observations and 111 variables.*/


/******************************************** STEP 9: Saving the dataset in lib harm: Spline *****************************************************************/

libname harm 'C:\Users\emayeda\Box Sync\UCD Health Cognition\Data\ADC-CHIS_DATA';

data harm.adc_chis2_am_09s;
set AGEBMI_Spline_3;
run;


/********************************************STEP 10: Creating Dummies*******************************************************/
libname harm 'C:\Users\emayeda\Box Sync\UCD Health Cognition\Data\ADC-CHIS_DATA';

data adc_chis2; set harm.adc_chis2_am_09s; run;

data adc_chis2;
set adc_chis2;
	if edu_harm ne . then do;
		edu_noed=(edu_harm=0);
		edu_elem=(edu_harm=1);
		edu_somehs=(edu_harm=2);
		edu_hs=(edu_harm=3);
		edu_somecol=(edu_harm=4);
		edu_col=(edu_harm=5);
		edu_postgrad=(edu_harm=6);
	end;

	if edu_harm1 ne . then do;
		edu1_noed=(edu_harm1=0);
		edu1_elem=(edu_harm1=1);
		edu1_somehs=(edu_harm1=2);
		edu1_hs=(edu_harm1=3);
		edu1_somecol=(edu_harm1=4);
		edu1_colpostgrad=(edu_harm1=5);
	end;

	if language ne . then do;
		lang_eng=(language=1);
		lang_span=(language=2);
		lang_both=(language=3);
		lang_oth=(language=4);
	end;

	if language1 ne . then do;
		lang1_eng=(language1=1);
		lang1_span=(language1=2);
		lang1_both=(language1=3);
	end;

run;
		/*checks
		proc freq data=adc_chis2; tables 
		edu_harm*(edu_noed edu_elem edu_somehs edu_hs edu_somecol edu_col edu_postgrad)
		edu_harm1*(edu1_noed edu1_elem edu1_somehs edu1_hs edu1_somecol edu1_colpostgrad)
		language*(lang_eng lang_span lang_both lang_oth)
		language1*(lang1_eng lang1_span lang1_both)/missing;run;
		*/

data harm.adc_chis2_am_09s; set adc_chis2; run;
/*NOTE: There were 13113 observations read from the data set WORK.ADC_CHIS2.
NOTE: The data set HARM.ADC_CHIS2_AM_09S has 13113 observations and 131 variables.*/

/*End of program*/
