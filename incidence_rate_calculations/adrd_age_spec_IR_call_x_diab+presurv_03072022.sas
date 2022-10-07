/* -------- */
/* run age specific IR on the analytic datasets */
/* after subsetting on membership critera */

libname diab 'C:\Users\Staff\Box\Asian_Americans_dementia_data\analysis_data_tables';

data tte_data;
	set diab.aa_adrd_cardiometabolic_tte;
run;

data subset_data;
	set tte_data;
	where MAIN_DEM_V1_SAMPLE = 1 AND (ETHNICITY_REV = 1 OR ETHNICITY_REV = 2 OR ETHNICITY_REV = 3 OR ETHNICITY_REV = 5 OR ETHNICITY_REV = 9);
	* add exposure*membership flag;
	if PRESURV5YR_SAMPLE = 0 THEN DIAB_PRESURV_5YR_FLAG = 2;
	if DIAB_DX5YR_FLAG = 1 AND PRESURV5YR_SAMPLE  = 1 THEN DIAB_PRESURV_5YR_FLAG  = 1;
	if DIAB_DX5YR_FLAG = 0 AND PRESURV5YR_SAMPLE  = 1 THEN DIAB_PRESURV_5YR_FLAG  = 0;
	if PRESURV7YR_SAMPLE = 0 THEN DIAB_PRESURV_7YR_FLAG = 2;
	if DIAB_DX7YR_FLAG = 1 AND PRESURV7YR_SAMPLE  = 1 THEN DIAB_PRESURV_7YR_FLAG  = 1;
	if DIAB_DX7YR_FLAG = 0 AND PRESURV7YR_SAMPLE  = 1 THEN DIAB_PRESURV_7YR_FLAG  = 0;
	if PRESURV9YR_SAMPLE = 0 THEN DIAB_PRESURV_9YR_FLAG = 2;
	if DIAB_DX9YR_FLAG = 1 AND PRESURV9YR_SAMPLE  = 1 THEN DIAB_PRESURV_9YR_FLAG  = 1;
	if DIAB_DX9YR_FLAG = 0 AND PRESURV9YR_SAMPLE  = 1 THEN DIAB_PRESURV_9YR_FLAG  = 0;
run;

* sanity check;

proc freq data=subset_data;
	where PRESURV5YR_SAMPLE = 1;
	tables DIAB_DX5YR_FLAG*ASIAN;
run;

proc freq data=subset_data;
	where PRESURV5YR_SAMPLE = 1;
	tables DIAB_DX5YR_FLAG*ETHNICITY_REV;
run;

proc freq data=subset_data;
	where PRESURV7YR_SAMPLE = 1;
	tables DIAB_DX7YR_FLAG*ASIAN;
run;

proc freq data=subset_data;
	where PRESURV9YR_SAMPLE = 1;
	tables DIAB_DX9YR_FLAG*ASIAN;
run;

*set up the macro;

%include "C:\Users\Staff\Box\Asian_Americans_dementia\Manuscripts\AA_ADRD_diabetes\Code\Incidence Rates Macro\inc_rate_ancillary_macros_20210413.sas";                                                                    
* %include "C:\Users\Staff\Box\Asian_Americans_dementia\Manuscripts\AA_ADRD_diabetes\Code\Incidence Rates Macro\age_adj_IRmacro_20210413.sas";
%include "C:\Users\Staff\Box\Asian_Americans_dementia\Manuscripts\AA_ADRD_diabetes\Code\Incidence Rates Macro\age_strat_IRmacro_85+.sas";

*using the subset data;

*stratify on DIAB_PRESURV_5YR_FLAG; 

*Total Sample;
%AGE_STRATIR(subset_data(where=(DIAB_PRESURV_5YR_FLAG = 0)), 
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Total Sample + DIAB_PRESURV_5YR = 0', incrate_tot_5yr_0);
%AGE_STRATIR(subset_data(where=(DIAB_PRESURV_5YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Total Sample + DIAB_PRESURV_5YR = 1', incrate_tot_5yr_1);

*South Asian;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 1 AND DIAB_PRESURV_5YR_FLAG = 0)), 
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'South Asian + DIAB_PRESURV_5YR = 0', incrate_1_5yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 1 AND DIAB_PRESURV_5YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'South Asian + DIAB_PRESURV_5YR = 1', incrate_1_5yr_1);

*Chinese;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 2 AND DIAB_PRESURV_5YR_FLAG = 0)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Chinese + DIAB_PRESURV_5YR = 0', incrate_2_5yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 2 AND DIAB_PRESURV_5YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Chinese + DIAB_PRESURV_5YR = 1', incrate_2_5yr_1);

*Japanese;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 3 AND DIAB_PRESURV_5YR_FLAG = 0)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Japanese + DIAB_PRESURV_5YR = 0', incrate_3_5yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 3 AND DIAB_PRESURV_5YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Japanese + DIAB_PRESURV_5YR = 1', incrate_3_5yr_1);

*Filipino;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 5 AND DIAB_PRESURV_5YR_FLAG = 0)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Filipino + DIAB_PRESURV_5YR = 0', incrate_5_5yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 5 AND DIAB_PRESURV_5YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Filipino + DIAB_PRESURV_5YR = 1', incrate_5_5yr_1);

*White;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 9 AND DIAB_PRESURV_5YR_FLAG = 0)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'White + DIAB_PRESURV_5YR = 0', incrate_9_5yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 9 AND DIAB_PRESURV_5YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'White + DIAB_PRESURV_5YR = 1', incrate_9_5yr_1);



data incrate_race_x_diab_dx5yr_0;
	set incrate_tot_5yr_0 incrate_1_5yr_0 incrate_2_5yr_0 incrate_3_5yr_0 incrate_5_5yr_0 incrate_9_5yr_0;
run;

data incrate_race_x_diab_dx5yr_1;
	set incrate_tot_5yr_1 incrate_1_5yr_1 incrate_2_5yr_1 incrate_3_5yr_1 incrate_5_5yr_1 incrate_9_5yr_1;
run;

/*------------*/

*stratify on DIAB_PRESURV_7YR_FLAG; 

*Total Sample;
%AGE_STRATIR(subset_data(where=(DIAB_PRESURV_7YR_FLAG = 0)), 
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Total Sample + DIAB_PRESURV_7YR = 0', incrate_tot_7yr_0);
%AGE_STRATIR(subset_data(where=(DIAB_PRESURV_7YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Total Sample + DIAB_PRESURV_7YR = 1', incrate_tot_7yr_1);

*South Asian;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 1 AND DIAB_PRESURV_7YR_FLAG = 0)), 
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'South Asian + DIAB_PRESURV_7YR = 0', incrate_1_7yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 1 AND DIAB_PRESURV_7YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'South Asian + DIAB_PRESURV_7YR = 1', incrate_1_7yr_1);

*Chinese;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 2 AND DIAB_PRESURV_7YR_FLAG = 0)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Chinese + DIAB_PRESURV_7YR = 0', incrate_2_7yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 2 AND DIAB_PRESURV_7YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Chinese + DIAB_PRESURV_7YR = 1', incrate_2_7yr_1);

*Japanese;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 3 AND DIAB_PRESURV_7YR_FLAG = 0)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Japanese + DIAB_PRESURV_7YR = 0', incrate_3_7yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 3 AND DIAB_PRESURV_7YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Japanese + DIAB_PRESURV_7YR = 1', incrate_3_7yr_1);

*Filipino;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 5 AND DIAB_PRESURV_7YR_FLAG = 0)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Filipino + DIAB_PRESURV_7YR = 0', incrate_5_7yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 5 AND DIAB_PRESURV_7YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Filipino + DIAB_PRESURV_7YR = 1', incrate_5_7yr_1);

*White;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 9 AND DIAB_PRESURV_7YR_FLAG = 0)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'White + DIAB_PRESURV_7YR = 0', incrate_9_7yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 9 AND DIAB_PRESURV_7YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'White + DIAB_PRESURV_7YR = 1', incrate_9_7yr_1);


data incrate_race_x_diab_dx7yr_0;
	set incrate_tot_7yr_0 incrate_1_7yr_0 incrate_2_7yr_0 incrate_3_7yr_0 incrate_5_7yr_0 incrate_9_7yr_0;
run;

data incrate_race_x_diab_dx7yr_1;
	set incrate_tot_7yr_1 incrate_1_7yr_1 incrate_2_7yr_1 incrate_3_7yr_1 incrate_5_7yr_1 incrate_9_7yr_1;
run;


/*---------*/

*stratify on DIAB_PRESURV_9YR_FLAG; 

*Total Sample;
%AGE_STRATIR(subset_data(where=(DIAB_PRESURV_9YR_FLAG = 0)), 
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Total Sample + DIAB_PRESURV_9YR = 0', incrate_tot_9yr_0);
%AGE_STRATIR(subset_data(where=(DIAB_PRESURV_9YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Total Sample + DIAB_PRESURV_9YR = 1', incrate_tot_9yr_1);

*South Asian;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 1 AND DIAB_PRESURV_9YR_FLAG = 0)), 
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'South Asian + DIAB_PRESURV_9YR = 0', incrate_1_9yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 1 AND DIAB_PRESURV_9YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'South Asian + DIAB_PRESURV_9YR = 1', incrate_1_9yr_1);

*Chinese;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 2 AND DIAB_PRESURV_9YR_FLAG = 0)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Chinese + DIAB_PRESURV_9YR = 0', incrate_2_9yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 2 AND DIAB_PRESURV_9YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Chinese + DIAB_PRESURV_9YR = 1', incrate_2_9yr_1);

*Japanese;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 3 AND DIAB_PRESURV_9YR_FLAG = 0)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Japanese + DIAB_PRESURV_9YR = 0', incrate_3_9yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 3 AND DIAB_PRESURV_9YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Japanese + DIAB_PRESURV_9YR = 1', incrate_3_9yr_1);

*Filipino;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 5 AND DIAB_PRESURV_9YR_FLAG = 0)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Filipino + DIAB_PRESURV_9YR = 0', incrate_5_9yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 5 AND DIAB_PRESURV_9YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'Filipino + DIAB_PRESURV_9YR = 1', incrate_5_9yr_1);

*White;
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 9 AND DIAB_PRESURV_9YR_FLAG = 0)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'White + DIAB_PRESURV_9YR = 0', incrate_9_9yr_0);
%AGE_STRATIR(subset_data(where=(ETHNICITY_REV = 9 AND DIAB_PRESURV_9YR_FLAG = 1)),
			 MAIN_DEM_V1_END_DEM_AGE, survey_age, MAIN_DEM_V1_END_AGE, stddem, 'White + DIAB_PRESURV_9YR = 1', incrate_9_9yr_1);


data incrate_race_x_diab_dx9yr_0;
	set incrate_tot_9yr_0 incrate_1_9yr_0 incrate_2_9yr_0 incrate_3_9yr_0 incrate_5_9yr_0 incrate_9_9yr_0;
run;

data incrate_race_x_diab_dx9yr_1;
	set incrate_tot_9yr_1 incrate_1_9yr_1 incrate_2_9yr_1 incrate_3_9yr_1 incrate_5_9yr_1 incrate_9_9yr_1 ;
run;


/*------*/
*print out results;

ods excel file='C:\Users\Staff\Box\Asian_Americans_dementia\Manuscripts\AA_ADRD_diabetes\Code\Incidence Rates Macro\adrd_age_spec_IR_x_diab+presurv_03072022.xlsx';

proc print data=incrate_race_x_diab_dx5yr_0;
run;

proc print data=incrate_race_x_diab_dx5yr_1;
run;

proc print data=incrate_race_x_diab_dx7yr_0;
run;

proc print data=incrate_race_x_diab_dx7yr_1;
run;

proc print data=incrate_race_x_diab_dx9yr_0;
run;

proc print data=incrate_race_x_diab_dx9yr_1;
run;

ods excel close;
