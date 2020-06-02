
/********************************************************************************************************************************/
/************************************************* CHIS 2005 data - Weighted Estimates *****************************************/
/* Primary Analyst: Audrey Murchland (September 2017); using Meena Doshi's previous code as a framework */
/* Code review and revisions by Elizabeth Rose Mayeda on September 20, 2019 */ 
/*****************************************************************************************************************************/
/* Steps:
	1) Keep Vars required
	2) Restrict - SRAGE_P>=60 and race=white, AA and hisp - only
	3) Created new vars: 
		a) race ( ctreated from SRH, SRAA, and SRW 
	        1=AA 
	        2=hisp 
	        3=white 
	 	b) Language (creatd from  LANgHOME)
	           1='English'
	           2='Spanish'
			   3='Both'
			   4='Other'
			  -9='Missing';
		c) Diabetes(created from AB22)
			1: Yes
			0: No 
		d) USBORN(created from CNTRYS)
			0. Other
			1. United States
		e) Cholesterol(from AB36)
		    1: Yes
		    2: No
		    .: Missing
		fa) Hypertension(from AB29)
		    1: Yes
		    2: No
		    .: Missing
		fb) CongestiveHeart(from AB52)
		    1: Yes
		    2: No
		    .: Missing
			*Among those that answered 'Yes' to AB29
		g) Educ_harm (from AHEDUC)
		h) dtmn='CH' needed while concatonating with ADC
		i) Creating New Variable names
			Nage=SRAGE_p;
			sex=SRSEX;
			BMI=BMI_P;
		j) Stroke Prevalence from AC6
			1: Yes
		    2: No
		    .: Missing
		k) Marital Status 
	 4) Save data set Adult_60chis in library*/

/*****************************************************************************************************************************/

libname CH 'C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA\CHIS\CHIS 2005 Adult data\chis05_adult_sas';

proc contents data=ch.adult;
run;

/*applying ADC formats*/
LIBNAME library 'C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Programs\RWP_Formats_CHIS';
options fmtsearch=(library);
/*run program Adult_Format.sas in CHIS data folder*/

/**************************************STEP 1: Importing the variables required **********************************************************/
/*43020*/
data adult_touse;
retain SRAGE_P marit smoking;
set ch.adult; 
keep
PUF_ID
INTVLANG 
SRAGE_P
SRSEX
SRH
SRAA
SRW
CNTRYS
LANGHOME
marit
MARIT2
AHEDUC
WRKST
AB1/* quest*/
BMI_P
AB22 /* quest*/
DIAMED
AB29/* quest*/
AB30/* quest*/
AB34/* Heart Disease*/
AB52/*Congestive Heart Failure*/
AC6/* quest*/
AB35/*to check ab36, ELSE NOT REQUIRED*/
AB36/* quest*/
AJ32/* quest*/
AD57/* quest*/
SMOKING
AC6 /*stroke*/
/* weights*/
RAKEDW0 /* overall*/
RAKEDW1-RAKEDW80 /* replicate*/
/* ID's*/
PUF_ID /* PUBLIC USE FILE ID*/
FAMTYP_P /* FAMILY TYPE (PUF RECODE)*/

RACEHPR OMBSRREO RACEDOF
;
run;

/********************************************STEP 2: RESTRICT to SRAGE_P >=60 and Black/African American, Hispanic/Latino, or White *****************************************/ 
/* 12,257 observations*/
data adult60_CHISa;
retain SRAGE_P SRH SRAA SRW;
set  adult_touse;/* 43020 observations*/
if SRAGE_P>=60 and (SRH=1 or SRAA=1 or SRW=1);/* so will get value 1 for either SRH, SRAA or SRW. never a 2,2,2. But can have: 1,1,1; 1,1,2; 1,2,1; 2,1,1*/
run; 

/******************************** STEP 3a: CREATE a new var for race 1=AA 2=hisp 3=white ***********************************************************/
proc freq data=adult60_CHISa; tables SRW*SRAA/missing nocol norow nopercent; where SRH=1; run;
proc freq data=adult60_CHISa; table SRH*SRW*SRAA/missing nocol norow nopercent; run;

proc freq data=adult60_CHISa; table SRH SRW SRAA SRH*SRW SRH*SRAA SRW*SRAA; run;

proc freq data=adult60_CHISa; table SRH SRW SRAA RACEHPR OMBSRREO SRAA*RACEHPR SRAA*OMBSRREO; run;
proc freq data = adult60_CHISa; tables RACEHPR OMBSRREO racedof SRH SRAA SRW RACEHPR*(SRH SRAA SRW); run;
/*ERM on 9/20/19: Meena's original code restricted to people who were Black, Latino, or White, but ONLY one of the three categories. This resulted in dropping approximately 50% of people who identified as Latino.
I reviewed the CHIS race varaibles RACEDOF (Race – Former Department of Finance Definition), OMBSERRO (OMB Self-Reported Race Ethnicity), and RACEHPR (Race – UCLA CHPR Definition) -- I reviwed the response frequencies
and I reviewed CHIS documenation (2005PUF_constructs_a.pdf and CHIS2005_adult_q.pdf documentation). I considered using the RACEHPR variable, since it's the UCLA CHPR definition, but I don't particularly like the question
about racial/ethnic group people identify with most, since it means we drop people who identify as multi-racial and the description of the coding is a little confusing. So we will code participants as Latino if they
identified as Latino. For non-Latinos, we will code participants as Black/African American if they identify as Black and we will identify participants as White if they identify as White (but participants who identify
as Black and White will be coded as Black)*/

data adult60_CHISb; /*12257*/
retain SRAGE_P SRH SRAA SRW race;
set  adult60_CHISa; /*12257*/

if SRH = 1 then race = 2;
else if SRH = 2 then do;
    if SRAA = 1 then race = 1;
    else if SRW = 1 then race = 3;
end;
run;

	/* check work */
	proc freq data=adult60_CHISb; tables SRH SRAA SRW race race*(SRH SRAA SRW); run; 

/******************************** STEP 3b Create  new variable language (created for Langhome) *********************************************************/ 
/*1.English
2. Spanish
3. Both
4. other 
. miss
*/

data adult60_CHISc;
set adult60_CHISb;
if langhome in (1 9 10 11 12) then language=1;/*english*/
else if langhome=2 then language=2;/* spanish*/
else if langhome=8 then language=3;/* eng and spanish*/
else if langhome in (3 4 5 6 7 13) then language=4;/* other languages*/
else if langhome=-9 then language=.;/*not ascertained*/
run;
	/* check*/
	proc sort data=adult60_CHISc; by race; run;
	proc freq data=adult60_CHISc;
	tables /*langhome*language*/ language*intvlang langhome*intvlang/missing;
 	by race; 
	/*format language spk. langhome LANGMEF. race gg. intvlang langint.;*/
	run;


/**************************** STEP 3c: CREATE a new var for diabetes to harmonize with Diab1 from ADC called Diabetes ************************************/
/*1: Yes, 2: No, 3: borderline or prediabetes*/
	/*check
	proc freq data=adult60_CHIS; tables AB22/missing; run;
	*/

data adult60_CHISd;
set adult60_CHISc;
if AB22=1 THEN Diabetes=1;/* yes diab*/
If AB22 in (2 3) THEN Diabetes=0;/* no diab*/
run;

	/* check
	proc freq data=adult60_CHISd; tables AB22*Diabetes; run; */


/************************************** STEP 3d: CREATE a new var for born in the US or not called USBORN *************************************************/
/*Note from ERM on 9/20/2019: I'm dropping the nativity variable since we don't included it in our analyses*/


/********************************** STEP 3e: CREATE a new var for Cholesterol to harmonize with Cholesterol from ADC called Chol ***********************/
	/*check
	Proc freq data=adult60_CHIS; tables AB36;run;
	*/

data adult60_CHISe;
set adult60_CHISd;
if Ab36=1 then Cholesterol=1;/* cholesterol=Yes*/
else if AB36=2 then Cholesterol=0; /* cholesterol=No*/
/*else if AB36=-1 and AB29=2 then Cholesterol=0;*/ /*Missing*/
else if AB36=-1 then Cholesterol=.; /*Missing*/
run;

	/*check
	Proc freq data=adult60_CHISe; tables AB36*Cholesterol/missing;run;*/


/********************************** STEP 3fa: CREATE a new var for Hypertension to harmonize with Hypertention from ADC called HTN ***********************/
	/*check
	Proc freq data=adult60_CHISe; tables AB29;run;
	*/

data adult60_CHISf;
set adult60_CHISe;
if AB29=1 then Hypertension=1;/* Hypertention=Yes*/
else if AB29=2 then Hypertension=0; /* Hypertention=No*/
run;

	/*check
	Proc freq data=adult60_CHISf; tables AB29*Hypertension/missing; run;*/


/********************************** STEP 3fb: CREATE a new vars for Heart Disease and Congestive Heart Failure to harmonize with Hypertention from ADC called HTN ***********************/
	/*check
	Proc freq data=adult60_CHISf; tables AB34; run;
	*/

data adult60_CHISg;
set adult60_CHISf;
if AB34=1 then HeartDz=1;/*Heart Disease=Yes*/
else if AB34=2 then HeartDz=0; /*Heart Disease=No*/
run;
	/*check
	Proc freq data=adult60_CHISg; tables AB34*HeartDz/missing; run;
	*/
data adult60_CHISh;
set adult60_CHISg;
if AB52=1 then CongestiveHeart=1;/*Heart Disease=Yes*/
else if AB52=2 then CongestiveHeart=0; /*Heart Disease=No*/
else if HeartDz=0 then CongestiveHeart=0;
run;
	/*check
	Proc freq data=adult60_CHISh; tables AB34*AB52 AB52*CongestiveHeart/missing;run;
	*/

/********************************** STEP 3g: CREATE a new var for Education to harmonize with Education from ADC called HAHEDUC ***********************/
/* AHEDUC:
1: grade 1 through 8
2: Grade 9 through 11
3: Grade 12/HS Diploma
4: Some College
5: Vocational School
6: AA or AS Degree
7: BA or BS Degree
8: Some Grad School
9: MA or MS Degree
10:PHD or Equivalent
91: NO Formal Education (ARM found this value label in the SAS formats (ADULT_PROC_FORMAT.sas). This response value was not defined in 2005PUF_constructs_a.pdf)
*/
	/*check
	proc freq data=adult60_CHIS; tables AHEDUC/missing norow nocol nopercent; run;
	*/

data adult60_CHISi;
set adult60_CHISh;
if AHEDUC=91 then Edu_harm=0;
else if AHEDUC=1 then Edu_harm=1;
else if AHEDUC=2 then Edu_harm=2;
else if AHEDUC=3 then Edu_harm=3;
else if AHEDUC in (4 5 6) then Edu_harm=4;
else if AHEDUC=7 then Edu_harm=5;
else if AHEDUC in (8 9 10) then Edu_harm=6;
run;

	/*check
	proc freq data=adult60_CHISi; tables AHEDUC*Edu_harm/missing nocol norow nopercent; run;
	*/

/***************** Step 3h: Creating a variable dtnm=CH for Chis data needed while concatinating with ADC *************************************************/
data adult60_CHISj;
set adult60_CHISi;
length dtnm $2;
dtnm='CH';
run;

/****************** Step 3i: Creating new variables w/ different names for concatinating w/ ADC*********************************************************/
data adult60_CHISj;
set adult60_CHISj;
Nage=SRAGE_p;
sex=SRSEX;
BMI=BMI_P;
run;
/****************** Step 3j: Creating new stroke variable for concatinating w/ ADC *********************************************************/
data adult60_CHISk;
set adult60_CHISj;
if AC6=1 then Stroke=1;/* Stroke=Yes*/
else if AC6=2 then Stroke=0; /* Stroke=No*/
else Stroke=.;
run;
	/*check
	Proc freq data=adult60_CHISk; tables AC6*Stroke/missing; run;
	*/
data adult60_CHISl;
set adult60_CHISk;
ID = input(PUF_id, 8.);
run;

/****************** Step 3k: Creating marriage variable for concatinating w/ ADC *********************************************************/
data adult60_CHISm;
set adult60_CHISl;
if marit2 ne . then do;
	marry_bi=(marit2=1 or marit2=2);
	end;
run;

/*proc freq data=adult60_CHISm; tables marit2 marit2*marry_bi marry_bi/missing;run;*/


/***********************************************Step 4: Saving the data set adult60_CHIS in the Lib ***********************************************************/
libname Chis 'C:\Users\emayeda\Box Sync\UCD Health Cognition\Data\ADC-CHIS_DATA\CHIS\CHIS 2005 Adult data';

data chis.adult60_CHIS_AM_05;
set adult60_CHISm;
run;
/*NOTE: There were 12257 observations read from the data set WORK.ADULT60_CHISM.
NOTE: The data set CHIS.ADULT60_CHIS_AM_05 has 12257 observations and 127 variables.*/

/*End of Program*/
