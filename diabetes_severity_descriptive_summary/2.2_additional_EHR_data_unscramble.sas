libname rawehr "C:\Users\Staff\Box\Asian_Americans_dementia_data\raw_data_tables\Membership_EHR_mortality_data";
libname ehrout "C:\Users\Staff\Box\Asian_Americans_dementia_data\diabetes_adrd\Additional_EHR_data";

%macro ehr_unscramble(indata, outdata, indate, outage);
data &outdata;
	set &indata;
	* format subjid;
	SUBJID_NUM = SUBJID*1;
	FORMAT SUBJID_NUM BEST13.;
	DROP SUBJID;
	RENAME SUBJID_NUM = SUBJID;
	* un-scramble date;
	TEMP_DATE = DATEPART(&indate);
	REF_DATE = '01JAN1900'D;
	IF TEMP_DATE NE '01JAN1600'D THEN
		DO;
			&outage = YRDIF(REF_DATE, TEMP_DATE, 'AGE');
		END;
	ELSE &outage=90;
	FORMAT REF_DATE MMDDYY10. TEMP_DATE MMDDYY10.;
run;
%mend ehr_unscramble;

* all_obs_dx: only one date variable, DX_DATE;
proc contents data=rawehr.C10049_all_obs_dx20220531; run;
%ehr_unscramble(indata=rawehr.C10049_all_obs_dx20220531, 
				outdata=ehrout.all_obs_dx, indate=DX_DATE, outage=DX_AGE);

* CVD: 3 date variables;
proc contents data=rawehr.C10049_cvd20220531; run;

%ehr_unscramble(indata=rawehr.C10049_cvd20220531, 
				outdata=ehrout.cvd, indate=FIRST_CHF_DX_DATE, outage=FIRST_CHF_DX_AGE);

%ehr_unscramble(indata=ehrout.cvd, 
				outdata=ehrout.cvd, indate=FIRST_PVD_DX_DATE, outage=FIRST_PVD_DX_AGE);

%ehr_unscramble(indata=ehrout.cvd, 
				outdata=ehrout.cvd, indate=FIRST_P_MI_DX_DATE, outage=FIRST_P_MI_DX_AGE);

* Diabetes: 1 date variable NOTIFICATION_DATE;
proc contents data=rawehr.C10049_diabetes20220531; run;
%ehr_unscramble(indata=rawehr.C10049_diabetes20220531, 
				outdata=ehrout.diabetes, indate=NOTIFICATION_DATE, outage=NOTIFICATION_AGE);

* Diab_comp: 9 date variables;
proc contents data=rawehr.C10049_diab_comp20220531; run;
%ehr_unscramble(indata=rawehr.C10049_diab_comp20220531, 
				outdata=ehrout.diab_comp, indate=FIRST_DIABCC_DX_DATE, outage=FIRST_DIABCC_DX_AGE);
%ehr_unscramble(indata=ehrout.diab_comp, 
				outdata=ehrout.diab_comp, indate=FIRST_DIABCOMP_DX_DATE, outage=FIRST_DIABCOMP_DX_AGE);
%ehr_unscramble(indata=ehrout.diab_comp, 
				outdata=ehrout.diab_comp, indate=FIRST_DIABHM_DX_DATE, outage=FIRST_DIABHM_DX_AGE);
%ehr_unscramble(indata=ehrout.diab_comp, 
				outdata=ehrout.diab_comp, indate=FIRST_DIABKC_DX_DATE, outage=FIRST_DIABKC_DX_AGE);
%ehr_unscramble(indata=ehrout.diab_comp, 
				outdata=ehrout.diab_comp, indate=FIRST_DIABKETO_DX_DATE, outage=FIRST_DIABKETO_DX_AGE);
%ehr_unscramble(indata=ehrout.diab_comp, 
				outdata=ehrout.diab_comp, indate=FIRST_DIABNEURO_DX_DATE, outage=FIRST_DIABNEURO_DX_AGE);
%ehr_unscramble(indata=ehrout.diab_comp, 
				outdata=ehrout.diab_comp, indate=FIRST_DIABNKF_DX_DATE, outage=FIRST_DIABNKF_DX_AGE);
%ehr_unscramble(indata=ehrout.diab_comp, 
				outdata=ehrout.diab_comp, indate=FIRST_DIABRETINO_DX_DATE, outage=FIRST_DIABRETINO_DX_AGE);
%ehr_unscramble(indata=ehrout.diab_comp, 
				outdata=ehrout.diab_comp, indate=FIRST_GANGLLC_DX_DATE, outage=FIRST_GANGLLC_DX_AGE);
proc contents data=ehrout.diab_comp; run;

* Dx: 3 date variables;
proc contents data=rawehr.C10049_dx20220531; run;
%ehr_unscramble(indata=rawehr.C10049_dx20220531, 
				outdata=ehrout.dx, indate=FIRST_DR362_DX_DATE, outage=FIRST_DR362_DX_AGE);
%ehr_unscramble(indata=ehrout.dx, 
				outdata=ehrout.dx, indate=FIRST_HT_CKD_DX_DATE, outage=FIRST_HT_CKD_DX_AGE);
%ehr_unscramble(indata=ehrout.dx, 
				outdata=ehrout.dx, indate=FIRST_SD_HM_DX_DATE, outage=FIRST_SD_HM_DX_AGE);

* ESRD: 3 date variables;
proc contents data=rawehr.C10049_esrd20220531; run;
%ehr_unscramble(indata=rawehr.C10049_esrd20220531, 
				outdata=ehrout.esrd, indate=CASE_START_DT, outage=CASE_START_AGE);
%ehr_unscramble(indata=ehrout.esdr, 
				outdata=ehrout.esrd, indate=TRMT_END_DT, outage=TRMT_END_AGE);
%ehr_unscramble(indata=ehrout.esdr, 
				outdata=ehrout.esrd, indate=TRMT_START_DT, outage=TRMT_START_AGE);

* MD: 1 date variable;
proc contents data=rawehr.C10049_md20220531; run;
%ehr_unscramble(indata=rawehr.C10049_md20220531, 
				outdata=ehrout.md, indate=FIRST_SEC_DM_DX_DATE, outage=FIRST_SEC_DM_DX_AGE);





