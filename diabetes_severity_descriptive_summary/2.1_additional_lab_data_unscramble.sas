libname rawlab "C:\Users\Staff\Box\Asian_Americans_dementia_data\raw_data_tables\Lab_data";
libname labout "C:\Users\Staff\Box\Asian_Americans_dementia_data\diabetes_adrd\Additional_lab_data";

proc contents data=rawlab.C10049_lab2_20220531; run;

%macro labunscramble(indata, outdata);
data &outdata;
	set &indata;
	* format subjid;
	SUBJID_NUM = SUBJID*1;
	FORMAT SUBJID_NUM BEST13.;
	DROP SUBJID;
	RENAME SUBJID_NUM = SUBJID;
	* un-scramble date;
	LAB_DATE = DATEPART(LAB_DT);
	REF_DATE = '01JAN1900'D;
	IF LAB_DATE NE '01JAN1600'D THEN
		DO;
			LAB_AGE = YRDIF(REF_DATE, LAB_DATE, 'AGE');
		END;
	ELSE LAB_AGE=90;
	FORMAT REF_DATE MMDDYY10. LAB_DATE MMDDYY10.;
run;
%mend labunscramble;

%labunscramble(indata=rawlab.C10049_lab2_20220531, outdata=labout.lab2_ds);
proc contents data=labout.lab2_ds; run;

%labunscramble(indata=rawlab.lab1_cr_s_1, outdata=labout.lab1_cr_s_1_ds);
%labunscramble(indata=rawlab.lab1_cr_s_2, outdata=labout.lab1_cr_s_2_ds);
%labunscramble(indata=rawlab.lab1_cr_s_3, outdata=labout.lab1_cr_s_3_ds);
%labunscramble(indata=rawlab.lab1_cr_s_4, outdata=labout.lab1_cr_s_4_ds);
%labunscramble(indata=rawlab.lab1_cr_s_5, outdata=labout.lab1_cr_s_5_ds);

%labunscramble(indata=rawlab.lab1_cr_u, outdata=labout.lab1_cr_u_ds);
%labunscramble(indata=rawlab.lab1_gfr, outdata=labout.lab1_gfr_ds);
%labunscramble(indata=rawlab.lab1_hba1c, outdata=labout.lab1_hba1c_ds);
