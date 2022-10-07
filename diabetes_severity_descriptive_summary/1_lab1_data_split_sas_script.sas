* Splitting diabetes-related lab data into smaller size;

libname labdata "C:\Users\Staff\Box\Asian_Americans_dementia_data\raw_data_tables\Lab_data";

proc contents data=labdata.C10049_lab1_20220531; run;

proc freq data=labdata.C10049_lab1_20220531;
	tables LAB_TEST_NAME;
run;

* split by test;
data labdata.lab1_cr_s labdata.lab1_cr_u labdata.lab1_gfr labdata.lab1_hba1c;
	set labdata.C10049_lab1_20220531;
	if lab_test_name = "Creatinine: Serum" then output labdata.lab1_cr_s;
	if lab_test_name = "Creatinine: Urine" then output labdata.lab1_cr_u;
	if lab_test_name = "GFR: non-African American" then output labdata.lab1_gfr;
	if lab_test_name = "HbA1C" then output labdata.lab1_hba1c;
run;

* creatine: serum takes up half of the original dataset, about 1G;
* further split;
proc contents data=labdata.lab1_cr_s; run;
proc freq data=labdata.lab1_cr_s;
	tables LAB_CODE;
run;

data lab1_cr_s_temp;
	set labdata.lab1_cr_s;
	SUBJID_num = SUBJID * 1;
run;

proc contents data=lab1_cr_s_temp; run;

proc univariate data=lab1_cr_s_temp;
	histogram subjid_num;
run;

data labdata.lab1_cr_s_1 labdata.lab1_cr_s_2 labdata.lab1_cr_s_3 labdata.lab1_cr_s_4 labdata.lab1_cr_s_5;
	set lab1_cr_s_temp;
	if SUBJID_num <= 200000000000 then output labdata.lab1_cr_s_1;
	if SUBJID_num > 200000000000 & SUBJID_num <= 400000000000 then output labdata.lab1_cr_s_2;
	if SUBJID_num > 400000000000 & SUBJID_num <= 600000000000 then output labdata.lab1_cr_s_3;
	if SUBJID_num > 600000000000 & SUBJID_num <= 800000000000 then output labdata.lab1_cr_s_4;
	if SUBJID_num > 800000000000 then output labdata.lab1_cr_s_5;
run;

