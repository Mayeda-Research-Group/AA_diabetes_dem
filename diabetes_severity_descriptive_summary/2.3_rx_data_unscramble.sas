libname rx_raw "C:\Users\Staff\Box\Asian_Americans_dementia_data\raw_data_tables\Rx_data";
libname out "C:\Users\Staff\Box\Asian_Americans_dementia_data\diabetes_adrd\Rx_data";

proc contents data=rx_raw.C10049_rx; run;

/* reformat and unscramble date variables */
data out.rx_data;
	set rx_raw.C10049_rx;
	SUBJID_NUM = SUBJID*1;
	FORMAT SUBJID_NUM BEST13.;
	DROP SUBJID;
	RENAME SUBJID_NUM = SUBJID;
	RX_START = DATEPART(RX_START_DATE);
	RX_END   = DATEPART(RX_END_DATE);
	REF_DATE = '01JAN1900'D;
	IF RX_START NE '01JAN1600'D THEN
		DO;
			START_AGE = YRDIF(REF_DATE, RX_START, 'AGE');
		END;
	ELSE START_AGE=90;
	IF RX_END NE '01JAN1600'D THEN
		DO;
			END_AGE = YRDIF(REF_DATE, RX_END, 'AGE');
		END;
	ELSE END_AGE=90;
	FORMAT REF_DATE MMDDYY10. RX_END MMDDYY10. RX_START MMDDYY10.;
run;

proc contents data=out.rx_data; run;
