

/*Current Version of Dataset */ 	
									%let current= harm.weights_comb_092219;
/*Audrey's Dropbox*/				*%let Box = C:\Users\amurchland\Box Sync;
/*ERM*/								%let Box = C:\Users\emayeda\Box;

libname harm 'C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA';
*libname harm 'C:\Users\emayeda\Box\UCD Health Cognition\Data\ADC-CHIS_DATA\OLD_DATA\data_sets_prior_to_erm_code_review_2019Sept';

 proc format;
value ra  1='Black'
          2='Latino'
		  3='White';
		  run;

 data diagnos;
 set &current; 

	chis_wts=RAKEDW0;

	if adc=0 then do;
		log_wt=1;
		rak_wt=1;
		sl_wt=1;
		gbm_wt=1;
	end;

	if edu_harm ne . then do;
		edu_noed=(edu_harm=0);
		edu_elem=(edu_harm=1);
		edu_somehs=(edu_harm=2);
		edu_hs=(edu_harm=3);
		edu_somecol=(edu_harm=4);
		edu_col_postgrad=(edu_harm=5 or edu_harm=6);
	end;

		if language ne . then do;
		lang_eng=(language=1);
		lang_span=(language=2);
		lang_both=(language=3);
		lang_oth=(language=4);
	end;

	if intvlang ne . then do;
		intv_span=(intvlang=2);
		intv_eng=(intvlang=1);
	end;
run;

/*Calling RAND Diagnostics Macro
	lang1_eng lang1_span lang1_both edu1_noed edu1_somehs edu1_hs edu1_somecol edu1_colpostgrad marry_bi stroke Hypertension diabetes*/
%include "C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Programs\RWP_Code_03_Analysis\twang_mac.sas";

ods excel file="C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Output\RWP_Output_03_Analysis\Hypertension\RWP_Diagnostics_092219.xlsx";
title "Covariance Tables for Reweighting Models";
 %dxwts(treatvar=adc,
             vars= Nage black hispanic white male marry_bi diabetes BMI stroke Hypertension Cholesterol HeartDz CongestiveHeart edu_noed edu_elem edu_somehs edu_hs
			edu_somecol edu_col_postgrad lang_eng lang_span lang_both ,
             class=,
             dataset=diagnos,
             weightvars= log_wt rak_wt sl_wt gbm_wt,
             estimand=ATE,
             sampw=inputds$chis_wts,
             permtestiters=0,
	         Rcmd=C:\Program Files\R\R-3.6.2\bin\x64\R.exe,
             objpath=C:\Users\emayeda\Dropbox\Meena_ERM_MMG\Reweighting_ADC\SAS Output); /*temporary location*/
ods excel close;

/*To create importable dataset for Rplots, update 
