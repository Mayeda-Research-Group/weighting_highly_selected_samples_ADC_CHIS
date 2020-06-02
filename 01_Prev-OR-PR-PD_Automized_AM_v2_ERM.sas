/*Automizing Prevalence*/

/*********************************************************************************************************************/
/****************************************** Prevalence Automized  ***************************************************/
/* Primary Analyst: Audrey Murchland (September 2017); using Meena Doshi's previous code as a framework */
/* Code review and revisions by Elizabeth Rose Mayeda on September 26, 2019 */ 
/**STEPS:
This program is to be run after the Weight calcualtion.sas prg

1)  Get data sets that are made in the Weight calcualtion.sas and saved under the libname harm
2a) Run macro Pre for the un-weighted estimate 
2b) Run Macro PreWT for Weighted Estiamtes
3)  Estimate for CHIS sample 

/***Note: grp: 1=Black 2=Latino 3=White*/
/*********************************************************************************************************************/

proc format;
value ra  1='Black'
          2='Latino'
		  3='White';
		  run;
		  
/**************************** Step 1: Get data sets M1, M1A, M1B M2, etc ***************************************************************************************/
libname harm 'C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA';


/*Import csv dataset using import if new SL/GBM/Raking weights are available - save as sas datafiles and then import below*/
proc import datafile="C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA\ADC_raked_09_092219.csv"
	out=harm.ADC_raked_09_092219
	dbms=csv
	replace;
	getnames=yes;
run;
proc import datafile="C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA\SLweights_09_092219.csv"
	out=harm.SLweights_09_092219
	dbms=csv
	replace;
	getnames=yes;
run;

proc import datafile="C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA\GBMpp_09_092219.csv"
	out=harm.GBMpp_09_092219
	dbms=csv
	replace;
	getnames=yes;
run;


%macro dd1(dtin=, dtot=);
data &dtot;
set Harm.&dtin;
run;
%mend;


/*datasets with current calculated weights/propensity scores - incorporating the chosen health measures as well*/
*%dd1(dtin=mslpt1_092219, dtot=adc_log); *Initial logistic regression propensity scores -- just demographic variables. ;
%dd1(dtin=MSLPT2_092219, dtot=adc_log); /*ERM note 1/20/2020: I evaluated covariate balance after applying weights based on the basic model above, and the same vars selected for this "final" logistic model as before code review.*/
 
/*Importing SL/GBM/Raking dataset if already created*/
/*Raking > current: ADC_raked_09_09222019.csv*/
%dd1(dtin=ADC_raked_09_092219, dtot=adc_raked); 

/*SL > current: SLweights_09_09222019.csv*/
%dd1(dtin=SLweights_09_092219, dtot=adc_sl); 

/*GBM > current: GBMpp_09_09222019.csv*/
%dd1(dtin=GBMpp_09_092219, dtot=adc_gbm); 

			/*
			data adc_chis_sl; set adc_chis_sl;
			pp_SL100= pp_SL*100000;
			pp_GBM100=pp_GBM*100000;
			run;
			proc means data=adc_chis_sl mean min p10 median p90 max; var pp_SL100 pp_GBM100; where adc=1; run;
			proc freq data=adc_log; tables adc/missing;run;*/

/***********CALCULATING WEIGHTS HERE SO THAT IT IS EASY TO TELL THEY ARE ALL CONSISTENT*****************/
*Check CHIS weighted sample size overall and by race/ethnicity;
data adc_chis2_am_09s;
set harm.adc_chis2_am_09s;
run;
proc sort data = adc_chis2_am_09s; by adc race; run;
proc freq data = adc_chis2_am_09s; tables adc race adc*race; run; *ADC sample size 856;
proc means data=adc_chis2_am_09s n sum;
where ADC=0; /* CHIS*/	
var RAKEDW0;
output out=tot sum=chistotre;
run;
*total weighted sample size 4622757;

proc means data=adc_chis2_am_09s n sum;
where ADC=0; /* CHIS*/	
format race ra.;
by race;	
var RAKEDW0;
output out=tot sum=chistotre;
run;
*weighted samples size by race: Black: 285557, Latino: 828777, White: 3508423;

data adc_log; set adc_log;
if psid=. then psid=id; 
	LOG_WT=(IP_0/IP_1)*((856/4622757)/(1-(856/4622757))); run; *ERM udpated CHIS weighted sample size to reflect Sept 2019 revisions on 9/26/2019;
	/*proc means data=adc_log n nmiss min max; var psid id; run;
	  proc means data=adc_log n nmiss min max; class adc; var LOG_WT; run;*/

data adc_raked; set adc_raked;
	if psid=. then psid=id; 
	/*Raking only provides weights, not predicted probs*/
	RAK_WT=raked_weight;
	keep psid id adc raked_weight RAK_WT; run;
	/*	proc means data=adc_raked n nmiss min max; var psid id; run;
		proc means data=adc_raked n nmiss min max; var rak_wt; class adc; run; */

data adc_sl; set adc_sl;
	if psid=. then psid=id; 
	pp_SL_0=1-pp_SL;
	SL_WT =(pp_SL_0/pp_SL)*((856/4622757)/(1-(856/4622757))); *ERM udpated CHIS weighted sample size to reflect Sept 2019 revisions on 9/26/2019;
	keep psid id adc pp_SL pp_SL_0 SL_WT; run;
	/*proc means data=adc_sl n nmiss min max; var psid id; run;
	  proc means data=adc_sl n nmiss min max; class adc; var SL_WT; run;*/

/*in previous evaluation, we determined an interaction depth of 2 was appropriate for gbm*/
data adc_gbm; set adc_gbm;
	if psid=. then psid=id; 
	GBM_WT =(pp2_CHIS/pp2_ADC)*((856/4622757)/(1-(856/4622757))); *ERM udpated CHIS weighted sample size to reflect Sept 2019 revisions on 9/26/2019;
	keep psid id adc pp2_CHIS pp2_ADC GBM_WT; run;
	/*proc means data=adc_gbm n nmiss min max; var psid id; run;
	  proc means data=adc_gbm n nmiss min max; class adc; var gbm_wt; run;*/

proc sort data=adc_log; by psid; run;
proc sort data=adc_raked; by psid; run;
proc sort data=adc_sl; by psid; run;
proc sort data=adc_gbm; by psid; run;

*Merge separate weight data files into one data set;
data weights_comb; 
merge adc_log(in=A) adc_raked(in=B) adc_sl(in=C) adc_gbm(in=D); 
by psid;
if A;
if adc=0 then rak_wt=0; *Since raking only provides weights this is why this is the only one mentioned here;
run;

proc sort data=weights_comb; by race; format race ra.; run;
proc freq data = weights_comb; tables race ; run;
proc freq data = weights_comb; tables  	SRAA black SRAA*black 
										SRH hispanic SRH*hispanic 
										SRW White SRW*White 
										race black hispanic white race*black race*hispanic race*white; run; *we should be using the derived race variables (Black, Latino, White, and Race), NOT CHIS variables (SRAA, SRH, SRW);

data harm.weights_comb_092219; 
set weights_comb; run;

*Run from start to finish to capture macro calculations for check in pdf;
ods pdf file="C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Output\RWP_Output_03_Analysis\Hypertension\RWP_AM_AppliedWts_MacroChk.pdf";
/************Table 2************/
proc means data=weights_comb mean std min p10 p50 p90 max; var rak_wt log_wt gbm_wt sl_wt;where adc=1; run;


/***************************  STEP 2: Prevalance & RR of hypertension in ADC  ***************************/

/*************************** STEP 2A: Prevalence and its CI's with binomial option (Un-weighted )*****************************************************/
Options symbolgen mprint;
%macro unwgt(din=, v=, sc=, t=, path=, mm=, nm=, sn=);
data &din;
set &din;
if race=1 then AA=1; if race=3 then AA=2; if race=2 then AA=.; /*Meena's old code: SRAA=1 then AA=1; if SRW=1 then AA=2; if SRH=1 then AA=.; /* African American - AA=1(African American),0 (white) and AA=. where Hispanic=1*/
if race=2 then H=1;  if race=3 then H=2;  if race=1 then H=.;  /*Meena's old code: if SRH=1 then H=1; if SRW=1 then H=2; if SRAA=1 then H=.; Hisp - H=1(Hispanic),0 (white) and AA=. where African American=1*/
label AA='Black' H='Latino';
run;


data &din;
set &din;
if &v=1 then DD=1;
else if &v=&sc then DD=2;
run;

proc sort data=&din; by race; format race ra.; run;

ods trace on;
proc freq data=&din;
Title1 "                  In ADC Sample. Un-weighted estimates for                              ";
title2 "                  Prevalence of &t by race/ethnicity             ";
title3 "                  wt calcualtion: model &mm (use data &din)                     ";

/* getting DEFAULT CI's*/
tables DD/   binomial CL;
by race;
where adc=1;/*ADC*/
/*EXACT binomial; */
ODS OUTPUT  OneWayFreqs=Tab1 binomial=bin(where=(name1 in ('XL_BIN', 'XU_BIN'))); 
run;
ods trace off;


/* MANIPULATIONS TO TAB1 AND BIN TO GET WHAT WE WANT*/
DATA tab2;
set tab1;
if DD=1; *hypertension=1;
drop cumFrequency cumpercent F_dd table;
run;

data tab2a;
set tab1;
if DD=2; *hypertension=0;
keep race cumFrequency;
run;

data bin1;
set bin; *ERM comment: I think the data set bin was created in the ODS OUTPUT statemetn in line 193?;
drop table nvalue1 label1;
run;

proc sort data=bin1; by race; run;

Proc transpose data=bin1 out=bin2;
by race;
var cvalue1;
id name1;
run;

data bin2;
set bin2;
xl_bin=input(xl_bin, 8.2);
xu_bin=input(xu_bin, 8.2);
xl_bin=xl_bin*100;

xu_bin=xu_bin*100;
run;

data final_PREV;
retain race dd frequency Cumfrequency percent xl_bin xu_bin;
merge tab2 tab2a bin2;
by race;
rename percent=Value;
LowerCL=input(xl_bin, 8.);
UpperCL=input(xu_bin, 8.);
if RACE=1 then Group='Black ';
else if RACE=2 then Group='Latino';
else if RACE=3 then Group='White ';
drop _name_ dd RACE xl_bin xu_bin;
Statistic="Prevalence";
run;

/* clean up*/
proc datasets lib=work nolist; 
  delete Bin bin1 bin2 tab1 tab2 tab2a; 
quit; 
run;

/********************************Getting OR, PR, and PD*****************************************/
ods graphics on;
ods trace on;
proc freq data=&din ;
Title1 '                  In ADC Sample. Unweighted estimates for                              ';
title2 '                  Prevalence and PD, OR, and PR of Hypertension among Blacks and Latinos compared to Whites             ';
title3 '                  wt calcualtion: model ADC = Nage race male (use data M1)                     ';
title4 '                  Proc Freq Statement:  Hypertension*AA Hypertension*H/  out=M1_Fout cmh relrisk             ';
tables AA*DD H*DD/ relrisk riskdiff(cl=(wald) correct) measures plots=(oddsratioplot relriskplot riskdiffplot)  /*out=M1_Fout*/;
/*CHISQ EXPECTED relrisk measures CL*/
where adc=1;/*ADC*/
ods output crosstabfreqs=freqs  /* frequencies*/
           riskdiffcol1=RDiff /* Countains prevalence & Risk Diff*/
           pdiffCLs=rDiff_CI /* Contains Risk Diff and its CI's*/
           Relativerisks=ORRR; /* Contains OR, RR and their CI's*/
run;
ods trace off;
ods graphics off;


/************************** Data Manipulations *************************************/
data ORR;
set ORRR;
if statistic='Relative Risk (Column 2)' then delete;
/*if table='Table AA * DD' then output AA;
if table='Table H * DD' then output H;*/
type='Exact';
run;

data Rdiff_CI;
set Rdiff_CI;
drop column;
rename RiskDifference=value; /*change riskdifference name -- it is confusing*/
statistic='Risk Diff';
run;

proc sort data=Rdiff_CI; by statistic;run;
proc sort data=ORR; by statistic;run;

data final_OR;
merge ORR Rdiff_CI;
by statistic;
if table='Table AA * DD' then table='Black vs. White';
if table='Table H * DD' then table='Latino vs. White';
Rename Table=Group;
drop studytype;
proc sort; by group;
run;

/* clean up*/
proc datasets lib=work nolist; 
  delete Freqs orr orrr rdiff rdiff_ci final; 
quit; 
run;

/*****merge PREV and OR tables for final table****/
data final;
retain Group Statistic Value LowerCL UpperCL Type Frequency Cumfrequency;
set final_PREV final_OR;
label value='Estimate';
run; 

libname out 'C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA\Applied_Weights';
data out.&sn; set final; model="&sn"; run;

proc export data=Final outfile="C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Output\RWP_Output_03_Analysis\&path.\&nm..xlsx" dbms=xlsx REPLACE;
sheet=&sn;

Title1 "                  In ADC Sample. Un-weighted estimates for                              ";
Title2 "                  Prevalence and PD, OR, and PR of Hypertension among Blacks and Latinos compared to Whites              ";
Title3 "                  wt calcualtion: model &mm (use data &din)                     ";
Title4 '                  Using Proc Freq w Binomial option                              ';
run;

%MEND;

%unwgt(din=weights_comb, v=Hypertension, sc=0, t=Hypertension, path=Hypertension, mm=Unweighted, nm=ADC_Hypertension_Weights_092219, sn=ADC_Unweighted);

	/*	proc freq data=weights_comb; tables hypertension*race/missing; where adc=1; run;
		proc freq data=weights_comb; tables hypertension*race/missing; where adc=0; run;	*/


/***************************************************************************************************************************************************************/
/************************* STEP 2B: Prevalence and its CI's with binomial option (WEIGHTED) ********************************************************************/
/**** Note: The CI's for weighted estiamtes are ASE not exact.  Exact confidence limits for the binomial proportion
      cannot be computed when there are noninteger frequencies.*/
%MACRO wgt (din=, v=, sc=, wt=, t=, path=, mm=, nm=, snm=);
Options symbolgen mprint;
data &din;
set &din;
if race=1 then AA=1; if race=3 then AA=2; if race=2 then AA=.; /*Meena's old code: if SRAA=1 then AA=1; if SRW=1 then AA=2; if SRH=1 then AA=.; /* African American - AA=1(African American),0 (white) and AA=. where Hispanic=1*/
if race=2 then H=1;  if race=3 then H=2;  if race=1 then H=.;  /*Meena's old code: if SRH=1 then H=1; if SRW=1 then H=2; if SRAA=1 then H=.; /* Hisp - H=1(Hispanic),0 (white) and AA=. where African American=1*/
label AA='Black' H='Latino';
run;

data &din;
set &din;
if &v=1 then DD=1;
else if &v=&sc then DD=2;
run;

proc sort data=&din; by race; run;

ods trace on;
proc freq data=&din;
Title1 "                  In ADC Sample. Weighted estimates for                              ";
title2 "                  Prevalence of &t among Blacks and Latinos compared to Whites             ";
title3 "                  wt calcualtion: model &mm (use data &din)                     ";
weight &wt;
/* getting DEFAULT CI's*/
tables DD/   binomial CL;
weight &wt;
by race;
where adc=1;/*ADC or CHIS*/
/*binomial; */
ODS OUTPUT  OneWayFreqs=Tab1 binomial=bin (where=(name1 in ('_BIN_', 'L_BIN', 'U_BIN'))); 
run;


/* MANIPULATIONS TO TAB1 AND BIN TO GET WHAT WE WANT*/
DATA tab2;
set tab1;
if DD=1;
drop cumFrequency cumpercent F_dd table;
run;

data tab2a;
set tab1;
if DD=2;
keep race cumFrequency;
run;

data bin1;
set bin;
drop table nvalue1 label1;
run;

Proc transpose data=bin1 out=bin2;
by race;
var cvalue1;
id name1;
run;

data bin2;
set bin2;
percent=input(_BIN_, 8.2);
xl_bin=input(l_bin, 8.2);
xu_bin=input(u_bin, 8.2);

percent=percent*100;
xl_bin=xl_bin*100;
xu_bin=xu_bin*100;
drop _bin_ l_bin u_bin;
run;

data final_PREV;
retain race dd frequency Cumfrequency percent xl_bin xu_bin;
merge tab2 tab2a bin2;
by race;
rename percent=Value xl_bin=LowerCL xu_bin=UpperCL;
if RACE=1 then Group='Black ';
else if RACE=2 then Group='Latino';
else if RACE=3 then Group='White ';
drop _name_ dd RACE;
Statistic="Prevalence";
run;

/* clean up*/
proc datasets lib=work nolist; 
  delete Bin bin1 bin2 tab1 tab2 tab2a; 
quit; 
run;

/*********************************OR, PR, and PD****************************************/

ods graphics on;
*ods select crosstabfreqs riskdiffcol1 pdiffCLs Relativerisks;
ods trace on;
proc freq data=&din ;
Title1 "                         In ADC Sample. Weighted estimates for                              ";
title2 '      					 OR, PR, and PD of Hypertension among Blacks and Latinos compared to Whites             ';
title3 "                         wt calcualtion: model &mm (use data &din)                     ";
weight &wt ;
tables AA*DD H*DD/ relrisk riskdiff(cl=(wald) correct) measures plots=(oddsratioplot relriskplot riskdiffplot)  /*out=M1_Fout*/;
/*CHISQ EXPECTED relrisk measures CL*/
where adc=1;/*ADC*/
ods output crosstabfreqs=freqs  /* frequencies*/
           riskdiffcol1=RDiff /*countains prevalence & Risk Diff*/
           pdiffCLs=rDiff_CI /* Contains Risk Diff and its CI's*/
           Relativerisks=ORRR; /* Contains OR, RR and their CI's*/
run;
ods trace off;
ods graphics off;
/************** Data Manipulations ************************/
data ORR;
set ORRR;
if statistic='Relative Risk (Column 2)' then delete;
/*if table='Table AA * DD' then output AA;
if table='Table H * DD' then output H;*/
type='Exact';
run;

data Rdiff_CI;
set Rdiff_CI;
drop column;
rename RiskDifference=value;
statistic='Risk Diff';
run;
proc sort data=Rdiff_CI; by statistic;run;
proc sort data=ORR; by statistic;run;

data final_OR;
merge ORR Rdiff_CI;
by statistic;
if table='Table AA * DD' then table='Blacks vs. Whites';
if table='Table H * DD' then table='Latinos vs. Whites';
Rename Table=Group;
drop studytype;
proc sort; by Group;
run;

/* clean up*/
proc datasets lib=work nolist; 
  delete Freqs orr orrr rdiff rdiff_ci final; 
quit; 
run;

/*****merge PREV and OR tables for final table****/
data final2;
retain Group Statistic Value LowerCL UpperCL Type Frequency Cumfrequency;
set final_PREV final_OR;
label value='Estimate';
run; 

libname out 'C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA\Applied_Weights';
data out.&snm; set final2; model= "&snm" ; run;

proc export data=Final2 outfile="C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Output\RWP_Output_03_Analysis\&path.\&nm..xlsx" dbms=xlsx REPLACE;
sheet=&snm;
Title1 "                      In ADC Sample. Weighted estimates for                              ";
title2 "                      Prevalance of &t among Blacks and Latinos compared to Whites             ";
title3 "                      wt calcualtion: model &mm (use data &din)                     ";
Title4 '                      Using Proc Freq w Binomial option [CI are AC (Agresti-Coull) not exact]                            ';
run;

%mend;


/******************************************************* Weighted in ADC **********************************************************************************/
%wgt(din=weights_comb, v=Hypertension, sc=0, wt=LOG_WT, t=Hypertension, path=Hypertension, mm=Logistic, nm=ADC_Hypertension_Weights_092219, snm=ADC_WT_Log);
%wgt(din=weights_comb, v=Hypertension, sc=0, wt=RAK_WT, t=Hypertension, path=Hypertension, mm=Raked, nm=ADC_Hypertension_Weights_092219, snm=ADC_WT_Raked);
%wgt(din=weights_comb, v=Hypertension, sc=0, wt=SL_WT, t=Hypertension, path=Hypertension, mm=SL, nm=ADC_Hypertension_Weights_092219, snm=ADC_WT_SL);
%wgt(din=weights_comb, v=Hypertension, sc=0, wt=GBM_WT, t=Hypertension, path=Hypertension, mm=GBM, nm=ADC_Hypertension_Weights_092219, snm=ADC_WT_GBM);


/************************************************* STEP 3A: CHIS ESTIMATES *********************************************************************************/

*ods excel file="C:\Users\amurchland\Dropbox\Meena_ERM_MMG\Reweighting_ADC\Harmonizing ADC CHIS Vars\prevalance and rr\Hypertension\CHIS_Wt_Hypertension_PreCI.xlsx" 
style=excel options(sheet_interval='none' sheet_name="CHIS_wt_Hypertension" embedded_titles='yes');

/**********************Prevalence**********************/
ods trace on;
PROC SURVEYFREQ DATA =weights_comb VARMETHOD=JACKKNIFE;
Title1 "                          In CHIS Sample. Weighted estimates for                              ";
title2 "                          Prevalance of Hypertension among Blacks and Latinos compared to Whites           ";
Title3 '                          Overall wt -rakedw0 and replicate wts-rakedw1-rakedw80                      ';
Title4 '                          Using Proc SurveyFreq                                    ';
WEIGHT rakedw0;
REPWEIGHT rakedw1-rakedw80/JKCOEFS=1;
TABLES Race*Hypertension/cl row;
where adc=0;/* CHIS*/
ods output crosstabs=MyStat;
run;
ods trace off;
/****Data Manipulation*****/
data PREV; set Mystat;
	if Hypertension=1;
	keep race Frequency WgtFreq RowPercent RowLowerCL RowUpperCL;
	run;
data prev; set prev;
	Statistic='Prevalence';
	format Statistic $22.;
	if RACE=1 then Group='Black ';
	else if RACE=2 then Group='Latino';
	else if RACE=3 then Group='White ';
	else if race=. then delete;
	Value=input(RowPercent, 8.);
	LowerCL=input(RowLowerCL, 8.);
	UpperCL=input(RowUpperCL, 8.);
	drop race RowPercent RowLowerCL RowUpperCL;
	run;

/*****************OR, PR, PD***************************/
ods trace on;
PROC SURVEYFREQ DATA =weights_comb VARMETHOD=JACKKNIFE;
Title1 "                         In CHIS Sample. Unweighted estimates for                              ";
title2 "  						 Odds Ratio, Prevalence Ratio & Risk Difference of Hypertension among Blacks and Latinos compared to Whites  ";
Title3 '                         Overall wt -rakedw0 and replicate wts-rakedw1-rakedw80                      ';
Title4 '                         Using Proc SurveyFreq                                    ';
WEIGHT rakedw0;
REPWEIGHT rakedw1-rakedw80/JKCOEFS=1;
tables AA*DD H*DD/ row risk or;
/*CHISQ EXPECTED relrisk measures CL*/
where adc=0;/*CHIS*/
ods output crosstabs=freqs  /* frequencies*/
           Risk1=RDiff /*contains prevalence & Risk Diff*/
           /*pdiffCLs=rDiff_CI Contains Risk Diff and its CI's*/
           OddsRatio=ORRR; /* Contains OR, RR and their CI's*/
run;
ods trace off;
ods graphics off;
/****Data Manipulation*****/
data ORR;
set ORRR;
if statistic='Column 2 Relative Risk' then delete;
/*if table='Table AA * DD' then output AA;
if table='Table H * DD' then output H;*/
rename Estimate=Value;
type='Exact';
run;

data Rdiff;
set Rdiff;
if _SkipLine=1; 
drop _SkipLine Row StdErr;
rename Risk=Value;
statistic='Risk Diff';
type='Wald';
run;
proc sort data=Rdiff; by statistic;run;
proc sort data=ORR; by statistic;run;

data final_OR;
merge ORR Rdiff;
by statistic;
if table='Table AA * DD' then table='Blacks vs. Whites';
if table='Table H * DD' then table='Latinos vs. Whites';
if statistic='Column 1 Relative Risk' then statistic='Rel Risk';
rename table=Group;
*drop table; 
proc sort; by group;
run;

data final_chisw;
retain Group Statistic Value LowerCL UpperCL Type Frequency /*Cumfrequency*/;
set prev final_OR;
run;

libname out 'C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA\Applied_Weights';
data out.chis_weighted; set final_chisw; model='chis_weighted'; run;


proc export data=final_chisw outfile="C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Output\RWP_Output_03_Analysis\Hypertension\ADC_Hypertension_Weights_092219.xlsx" dbms=xlsx REPLACE;
sheet=chis_weighted;
RUN;
ods trace off;

/* clean up*/
proc datasets lib=work nolist; 
  delete Freqs orr orrr rdiff rdiff_ci final; 
quit; 
run;



/*************************************Unweighted CHIS estimates***************************/
Options symbolgen mprint;
%macro unwgt(din=, v=, sc=, t=, path=, mm=, nm=, sn=);
data &din;
set &din;
if race=1 then AA=1; if race=3 then AA=2; if race=2 then AA=.; /*Meena's old code: if SRAA=1 then AA=1; if SRW=1 then AA=2; if SRH=1 then AA=.; /* African American - AA=1(African American),0 (white) and AA=. where Hispanic=1*/
if race=2 then H=1;  if race=3 then H=2;  if race=1 then H=.;  /*Meena's old code: if SRH=1 then H=1; if SRW=1 then H=2; if SRAA=1 then H=.; /* Hisp - H=1(Hispanic),0 (white) and AA=. where African American=1*/
label AA='Black' H='Latino';

run;

data &din;
set &din;
if &v=1 then DD=1;
else if &v=&sc then DD=2;
run;

proc sort data=&din; by race; format race ra.; run;

ods trace on;
proc freq data=&din;
Title1 "                         In CHIS Sample. Un-weighted estimates for                              ";
title2 "                  		 Prevalence of &t among Blacks and Latinos compared to Whites             ";
title3 "                         wt calcualtion: model &mm (use data &din)                     ";

/* getting DEFAULT CI's*/
tables DD/   binomial CL;
by race;
where adc=0;/*CHIS*/
/*EXACT binomial; */
ODS OUTPUT  OneWayFreqs=Tab1 binomial=bin(where=(name1 in ('XL_BIN', 'XU_BIN'))); 
run;
ods trace off;


/* MANIPULATIONS TO TAB1 AND BIN TO GET WHAT WE WANT*/
DATA tab2;
set tab1;
if DD=1;
drop cumFrequency cumpercent F_dd table;
run;

data tab2a;
set tab1;
if DD=2;
keep race cumFrequency;
run;

data bin1;
set bin;
drop table nvalue1 label1;
run;

proc sort data=bin1; by race; run;

Proc transpose data=bin1 out=bin2;
by race;
var cvalue1;
id name1;
run;

data bin2;
set bin2;
xl_bin=input(xl_bin, 8.2);
xu_bin=input(xu_bin, 8.2);
xl_bin=xl_bin*100;

xu_bin=xu_bin*100;
run;

data final_PREV;
retain race dd frequency Cumfrequency percent xl_bin xu_bin;
merge tab2 tab2a bin2;
by race;
rename percent=Value;
LowerCL=input(xl_bin, 8.);
UpperCL=input(xu_bin, 8.);
if RACE=1 then Group='Black ';
else if RACE=2 then Group='Latino';
else if RACE=3 then Group='White ';
drop _name_ dd RACE xl_bin xu_bin;
Statistic="Prevalence";
run;

/* clean up*/
proc datasets lib=work nolist; 
  delete Bin bin1 bin2 tab1 tab2 tab2a; 
quit; 
run;

/********************************Getting OR, PR, and PD*****************************************/
ods graphics on;
ods trace on;
proc freq data=&din ;
Title1 '                         In CHIS Sample. Unweighted estimates for                              ';
title2 '                  		 Prevalence and PR, OR, and RD of Hypertension among Blacks and Latinos compared to Whites             ';
title3 '                         wt calcualtion: model ADC = Nage race male (use data M1)                     ';
title4 '                         Proc Freq Statement:  Hypertension*AA Hypertension*H/  out=M1_Fout cmh relrisk             ';
tables AA*DD H*DD/ relrisk riskdiff(cl=(wald) correct) measures plots=(oddsratioplot relriskplot riskdiffplot)  /*out=M1_Fout*/;
/*CHISQ EXPECTED relrisk measures CL*/
where adc=0;/*CHIS*/ 									/*ERM note: I updated this code from adc=1 to adc=0*/
ods output crosstabfreqs=freqs  /* frequencies*/
           riskdiffcol1=RDiff /*countains prevalence & Risk Diff*/
           pdiffCLs=rDiff_CI /* Contains Risk Diff and its CI's*/
           Relativerisks=ORRR; /* Contains OR, RR and their CI's*/
run;
ods trace off;
ods graphics off;


/************************** Data Manipulations *************************************/
data ORR;
set ORRR;
if statistic='Relative Risk (Column 2)' then delete;
/*if table='Table AA * DD' then output AA;
if table='Table H * DD' then output H;*/
type='Exact';
run;

data Rdiff_CI;
set Rdiff_CI;
drop column;
rename RiskDifference=value; /*change riskdifference name -- it is confusing*/
statistic='Risk Diff';
run;

proc sort data=Rdiff_CI; by statistic;run;
proc sort data=ORR; by statistic;run;

data final_OR;
merge ORR Rdiff_CI;
by statistic;
if table='Table AA * DD' then table='Blacks vs. Whites';
if table='Table H * DD' then table='Latinos vs. Whites';
Rename Table=Group;
drop studytype;
proc sort; by group;
run;

/* clean up*/
proc datasets lib=work nolist; 
  delete Freqs orr orrr rdiff rdiff_ci final; 
quit; 
run;

/*****merge PREV and OR tables for final table****/
data final;
retain Group Statistic Value LowerCL UpperCL Type Frequency Cumfrequency;
set final_PREV final_OR;
label value='Estimate';
run; 

libname out 'C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA\Applied_Weights';
data out.&sn; set final; model="&sn"; run;

proc export data=Final outfile="C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Output\RWP_Output_03_Analysis\&path.\&nm..xlsx" dbms=xlsx REPLACE;
sheet=&sn;

Title1 "                         In CHIS Sample. Un-weighted estimates for                              ";
Title2 "                  		 Prevalance of &t among Blacks and Latinos compared to Whites             ";
Title3 "                         wt calcualtion: model &mm (use data &din)                     ";
Title4 '                         Using Proc Freq w Binomial option                              ';
run;

%MEND;

%unwgt(din=weights_comb, v=Hypertension, sc=0, t=Hypertension, path=Hypertension, mm=CHIS Unweighted, nm=ADC_Hypertension_Weights_092219, sn=chis_unweighted);


ods pdf close;
