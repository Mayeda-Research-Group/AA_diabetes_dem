/*********************************************************************/
/* TMM edited version (from inc_rate_macros.sas program from ERM)
	for RPGEH analyses 												*/
/*********************************************************************/

/*********************************************************************/
/*5 macros included here for calculating age-adjusted incidence rates:
pers_yrs, calcage, nwords, logmsg, termnate*/
/*********************************************************************/


%macro pers_yrs(indata=,outdata=,agegrps=,                             
                dx_age=DX_AGE,                        
                beg_age=BEG_AGE,end_age=END_AGE,   
                xtracode=,test=N);

	/*Note: TMM removed dob, dx_date, beg_date, and end_date from
				variable list called in pers_years*/
 
%**------------------------------------------------------------------*;
%*¦  This macro allocates Person Years to age groups based on age    ¦;
%*¦  at the start and at the end of follow-up.                       ¦;
%*¦  As the person ages during observation, the time is allocated    ¦;
%*¦  to successive age groups.                                       ¦;
%*¦                                                                  ¦;
%*¦  Documentation stored as:  DOR.MUG6916.SAS.MACRO.DOC(PERS#YRS)   ¦;
%**------------------------------------------------------------------*;
  %local /*ndxdates*/ ndx_age nagegrps word age dx grp x i nbreaks nranges;
                                                                       
  %let test = %upcase(%substr(&test,1,1));                             
     
		/*Note: TMM commented out ndxdates*/

  %*------------------------------------------------------------------;
  %*  Count the number of variables passed in the DX_DATE parameter.  ;
  %*  If no variable(s) is passed vis DX_DATE, set the counter to 1   ;
  %*  to cause the execution of the person year allocation loop below.;
  %*------------------------------------------------------------------;
  /*%nwords(dx_date);                                                    
  %let ndxdates = &nwords;                                             
  %if &ndxdates = 0 %then %let ndxdates = 1; */ 

		/*Note: TMM commented out*/ 
                                                                       
  %*------------------------------------------------------------------;
  %*  Count the number variable names passed in the DX_AGE parameter. ;
  %*  When multiple variable names are passed, there must be one      ;
  %*  variable name for each variable passed in the DX_DATE parameter.;
  %*  If this condition is not met, then abort further  processing.   ;
  %*------------------------------------------------------------------;
  %nwords(dx_age);                                                     
  %let ndx_age = &nwords;                                              
  /*%if &ndx_age gt 1 and &ndxdates ne &ndx_age %then %do;               
    %termnate(DATASTEP,PERS_YRS,Differing number of DX variables listed
              DX_DATE DX_AGE);                                         
    %goto quit;                                                        
  %end;                                                                
  %else %if &ndxdates gt 1 and &ndx_age = 1 %then %do;                 
    %if &ndxdates gt 9 and %length(&dx_age) gt 6 %then                 
      %let dx_age = %substr(&dx_age,1,6);                              
    %else %if %length(&dx_age) gt 7 %then                              
      %let dx_age = %substr(&dx_age,1,7);                              
  %end;      */    
	
		/*Note: TMM commented out section that considers number of 
			variables passed to dx_age and compares/checks this with
			dx_date.*/
                                                                       
  %*------------------------------------------------------------------;
  %*  Check that required parameters are passed. Abort if not present.;
  %*------------------------------------------------------------------;
  %if %length(&indata) = 0 %then %do;                                  
    %termnate(DATASTEP,PERS_YRS,1,INDATA,&indata);                     
    %goto quit;                                                        
  %end;                                                                
  /*%if %length(&beg_date) = 0 %then %do;                                
    %termnate(DATASTEP,PERS_YRS,1,BEG_DATE,&beg_date);                 
    %goto quit;                                                        
  %end;                                                                
  %if %length(&end_date) = 0 %then %do;                                
    %termnate(DATASTEP,PERS_YRS,1,END_DATE,&end_date);                 
    %goto quit;                                                        
  %end;*/   

		/*Note: TMM removed date checks - could try modifying to run
			checks on age variables instead*/ 
                                                                       
  %*------------------------------------------------------------------;
  %*  Count the number of age breaks passed in the AGEGRPS parameter. ;
  %*  Since this is a required parameter, abort further processing    ;
  %*  if this parameter is null.                                      ;
  %*------------------------------------------------------------------;
  %if %length(&agegrps) = 0 %then %do;                                 
    %termnate(DATASTEP,PERS_YRS,1,AGEGRPS,&agegrps);                   
    %goto quit;                                                         
  %end;                                                                 
  %nwords(agegrps,test=&test);                                          
  %let nagegrps = %eval(&nwords+1);                                     
  %let nbreaks=%eval(&nagegrps-1);                                      
  %let nranges=%eval(&nagegrps-2);                                      
                                                                        
  %*------------------------------------------------------------------; 
  %*  If no output data set is specified, then set the output data    ; 
  %*  set name to the input data set name.  This will cause this      ; 
  %*  macro to overwrite the passed input data set.                   ; 
  %*------------------------------------------------------------------; 
  %if %length(&outdata) = 0 %then %let outdata = &indata;               
                                                                        
  %*------------------------------------------------------------------; 
  %*  When the TEST parameter is on, set the MPRINT option and        ; 
  %*  write macro variable values to the log.   This is a useful aid  ; 
  %*  for de-bugging purposes.                                        ;
  %*------------------------------------------------------------------;
  %if &test = Y %then %do;                                             
    %put nagegrps=&nagegrps;                                           
    OPTIONS MPRINT;                                                    
  %end;                                                                
                                                                       
  %*------------------------------------------------------------------;
  %*  Begin the data step that allocates person years.                ;
  %*------------------------------------------------------------------;
  DATA &outdata;                                                       
    SET &indata;                                                       
                                                                       
    %*----------------------------------------------------------------;
    %*  Drop the temporary variables used in this data step.          ;
    %*----------------------------------------------------------------;
    DROP _A1-_A&nbreaks _INDEX _BA _LA _DX _END                        
         _INTER1-_INTER&nranges;                                       
                                                                       
    %*----------------------------------------------------------------;
    %*  Execute any SAS statements that are passed by the user.       ;
    %*----------------------------------------------------------------;
    %str(&xtracode);                                                   
                                                                       
    %*----------------------------------------------------------------;
    %*  Create AGE at BEGINNING of FOLLOW-UP                          ;
    %*----------------------------------------------------------------;
    /*%calcage(dob=&dob,asof=&beg_date,age=&beg_age)*/                   
			
			/*Note: TMM removed calcage from macro*/ 
 
    %*----------------------------------------------------------------;
    %*  Create AGE at END of FOLLOW-UP.  This age is used for         ;
    %*  allocating person years when either there is no DX_DATE       ;
    %*  variable(s) or the DX_DATE is missing for an observation.     ;
    %*----------------------------------------------------------------;
    /*%calcage(dob=&dob,asof=&end_date,age=&end_age)*/ 

			/*Note: TMM removed calcage from macro*/
                                                                       
    %*----------------------------------------------------------------;
    %*  When a DX_DATE is specified, calculate the AGE at diagnosis.  ;
    %*  This age, if present, is the age used for allocating person   ;
    %*  years.                                                        ;
    %*  Since many variables may be passed via the DX_DATE parameter  ;
    %*  and the DX_AGE paremeter, this code loops thru all variables  ;
    %*  passed.  The following macro variables are used to hold       ;
    %*  the individual variable names in these parameters:            ;
    %*    WORD   Contains each individual variable name from DX_DATE  ;
    %*    AGE    Contains the name of the age variable to be created. ;
    %*             When only 1 DX_DATE is passed, this contains the   ;
    %*               actual value passed via DX_AGE.                  ;
    %*             When multiple variables are passed in DX_DATE      ;
    %*               and only 1 DX_AGE variable name is passed, then  ;
    %*               an index digit is appended to the root variable  ;
    %*               name passed via DX_AGE.                          ; 
    %*             When multiple variables are passed in both DX_DATE ; 
    %*               and in DX_AGE, then passed age variable names    ; 
    %*               are matched 1-to-1 with the passed DX_DATE       ; 
    %*               variables.                                       ; 
    %*----------------------------------------------------------------; 
    %if %length(&dx_age) gt 0 %then %do;                               
      %** Create AGE at DIAGNOSIS              **;                      
      %do i = 1 %to &ndx_age;                                          
        /*%let word=%scan(&dx_date,&i,%str( ));                           
        %if &ndxdates = 1 %then %let age = &dx_age;                  
        %else*/ %if &ndx_age = 1 %then %let age = &dx_age&i;              
        %else %let age=%scan(&dx_age,&i,%str( ));                       
        /*%if &test=Y %then %put dob=&dob asof=&word age=&age;            
        %calcage(dob=&dob,asof=&word,age=&age);*/                         
        LABEL &age = "Subject AGE as of &word";                         
      %end;                                                            
    %end;                                                              
          
			/*Note: TMM commented out date references. When "let word" 
				line is included in macro with "dx_age" instead of date,
				macro does not calculate correct number of dates per
				group*/
			 
    %*----------------------------------------------------------------; 
    %*  Define arrays:                                                ; 
    %*    AGECUT   Contains the age breaks passed via AGEGRPS         ; 
    %*    AGEINT   Contains the range of years contained in each      ; 
    %*               age group.                                       ; 
    %*----------------------------------------------------------------; 
     ARRAY AGECUT(&nbreaks) _A1-_A&nbreaks;                             
     ARRAY AGEINT(&nranges) _INTER1-_INTER&nranges;                     
     RETAIN                                                             
       %do i = 1 %to &nbreaks;                                          
         _A&i  %scan(&agegrps,&i)                                       
       %end;;                                                           
                                                                        
     DO _INDEX = 1 TO &nranges;                                         
       AGEINT(_INDEX) = AGECUT(_INDEX+1) - AGECUT(_INDEX);              
     END;                                                              
                                                                       
    %*----------------------------------------------------------------;
    %*----------------------------------------------------------------;
    %* Perform the remaining code for EACH variable passed via DX_DATE;
    %* This loop is always executed once, even if no variables are    ;
    %* passed in DX_DATE.  Recall, if no variables are specified in   ;
    %* the DX_DATE parameter, the person years are allocated via the  ;
    %* variable passed in the END_DATE parameter.                     ;
    %*----------------------------------------------------------------;
    %*----------------------------------------------------------------;
    %do i = 1 %to &ndx_age;     /*Note: TMM changed to age variable*/                                      
      %*--------------------------------------------------------------;
      %* Initialize variables to be created and define arrays.        ;
      %* Person Year variables are always created.  Diagnosis         ;
      %* indicator variables are only created when a DX_DATE variable ;
      %* is available.                                                ;
      %*                                                              ; 
      %* DXDATE MUST be MISSING for all non-event observations !      ; 
      %*                                                              ; 
      %* The created variables are named as follows:                  ; 
      %*    PY_GRP1,.... PY_GRP5      Person year allocations for     ; 
      %*                                5 age groups and only 1       ; 
      %*                                DX_DATE or no DX_DATE variable; 
      %*    PY1_GRP1, ... PY3_GRP5    Person year allocations for 5   ; 
      %*                                age groups and 3 separate     ; 
      %*                                DX_DATE variables             ; 
      %*    PY1_G1, ..... PY3_G11     Person year allocations for 11  ; 
      %*                                age groups and 3 separate     ; 
      %*                                DX_DATE variables             ; 
      %*                                                              ; 
      %*  Additionally, indicator variables are created only when     ; 
      %*    DX_DATE variable(s) passed.  These variable names follow  ; 
      %*    the above pattern, with the 'PY' replaced by 'DX'.        ; 
      %*    The values of thess variables are:                        ; 
      %*      0  =  Diagnosis did not occur in the specified age range; 
      %*      1  =  Diagnosis did occur in the specified age range    ; 
      %*                                                              ; 
      %*  The following macro variables are used to construct the     ; 
      %*  names of the created variables:                             ; 
      %*    DX    This is a sequential number indicating which        ; 
      %*            variable from the list passed via DX_DATE is      ; 
      %*            being used.  When only 1 DX_DATE variable is      ; 
      %*            passed, this value is null.  Thus, only in cases  ; 
      %*            where multiple DX_DATE variables are passed will  ; 
      %*            a diagnosis number be a part of the created       ; 
      %*            variable names.                                   ; 
      %*    GRP   This is character string that preceeds the age      ; 
      %*            group number in the created variable name.  The   ; 
      %*            values are:                                       ; 
      %*               GRP    when 9 or fewer variable names are      ; 
      %*                        passed via DX_DATE                    ; 
      %*               G      when 10 or more variable names are      ; 
      %*                        passed via DX_DATE.  This is          ; 
      %*                        necessary to prevent the created      ; 
      %*                        variable name from being more than    ; 
      %*                        8 characters.                         ; 
      %*--------------------------------------------------------------; 
      %if &ndx_age = 1 %then %do;                                      
        %let dx =;                                                      
        %let grp=GRP;                                                   
      %end;                                                             
      %else %do;                                                        
        %let dx = &i;                                                   
        %if &ndx_age gt 9 or &nagegrps gt 9 %then %let grp=G;          
        %else %let grp=GRP;                                             
      %end;                                                             
      LENGTH PY&dx._&grp.1-PY&dx._&grp&nagegrps 8                       
     %if %length(&dx_age) gt 0 %then %do;                             
            DX&dx._&grp.1-DX&dx._&grp&nagegrps 3;                      
     ARRAY DX&dx(&nagegrps) DX&dx._&grp.1-DX&dx._&grp&nagegrps;        
     %end;;                                                            
     ARRAY PY&dx(&nagegrps) PY&dx._&grp.1-PY&dx._&grp&nagegrps;        
     DO _INDEX = 1 TO &nagegrps;                                       
       PY&dx(_INDEX)   = 0;                                            
     %if %length(&dx_age) gt 0 %then                                  
       DX&dx(_INDEX) = 0;;                                             
     END;                                                              

	 		/*Note: TMM changed initializing variable to ndx_age.
				Also changed "if length" functions to call age vars*/
 
     %*--------------------------------------------------------------; 
     %* _BA=INDEX OF BEGINNING AGEGROUP BASED ON AGE AT BEG_DATE;    ; 
     %* _LA=INDEX OF ENDING AGEGROUP BASED ON AGE AT END_DATE;       ; 
     %* _DX=INDEX OF AGEGROUP BASED ON AGE AT DIAGNOSIS;             ; 
     %* _END = Age used for end of person year allocation.  This is  ; 
     %*          the value of the END_DATE variable when there is    ; 
     %*          no DX_DATE variable or when the DX_DATE variable    ;
     %*          being processed is missing.  Otherwise, this is the ;
     %*          value of the DX_DATE variable being processed.      ;
     %*--------------------------------------------------------------;
     _BA = 1;                                                         
     _LA = 1;                                                         
     _DX = 1;                                                         
     %if %length(&dx_age) gt 0 %then %do;                            
       %if &ndx_age = 1 %then %let age = &dx_age;                    
       %else %if &ndx_age = 1 %then %let age = &dx_age&i;             
       %else %let age=%scan(&dx_age,&i,%str( ));                      
       IF &age NE . THEN _END = &age;                                 
       ELSE _END = &end_age;                                          
     %end;                                                            
     %else %do;                                                       
       _END = &end_age;                                               
     %end;                                                            
      DO _INDEX = 1 TO &nbreaks;                                        
        IF &beg_age GE AGECUT(_INDEX) THEN _BA = _INDEX + 1;            
        IF _END GE AGECUT(_INDEX) THEN _LA = _INDEX + 1;                
        %if %length(&dx_age) gt 0 %then                                
        IF &age NE . THEN                                               
          IF &age GE AGECUT(_INDEX) THEN _DX = _INDEX + 1;;             
      END;     

			/*Note: TMM commented out date references. When "let word" 
				line is included in macro with "dx_age" instead of date,
				macro does not calculate correct number of dates per
				group*/	
	 
      %*--------------------------------------------------------------; 
      %* Assign a value to the diagnosis indicator variable when      ; 
      %* it is being created.                                         ; 
      %*--------------------------------------------------------------; 
      %if %length(&dx_age) gt 0 %then %do;   /*Note: TMM changed to age*/                          
         IF &age NE . THEN                                              
           DX&dx(_DX) = 1;                                              
      %end;                                                             
      %*--------------------------------------------------------------; 
      %* CREATE PERSON YEARS VARS FOR AGE CATEGORIES                  ; 
      %* ALLOCATE PERSON YEARS TO AGE CATEGORIES BASED ON             ;
      %* INDEX FOR BEGINNING (_BA) AND ENDING (_LA) AGE GROUPS        ;
      %*--------------------------------------------------------------;
      IF &beg_age NE . AND _END NE . THEN DO;                          
        IF _BA = _LA THEN DO;                                          
          PY&dx(_BA) = _END - &beg_age;                                
        END;                                                           
        ELSE DO _INDEX = _BA TO _LA;                                   
          IF _INDEX = _BA THEN                                         
            PY&dx(_BA) = AGECUT(_BA) - &beg_age;                       
          ELSE IF _INDEX = _LA THEN                                    
            PY&dx(_LA) = _END - AGECUT(_LA-1);                         
          ELSE IF _BA < _INDEX < _LA THEN                              
            PY&dx(_INDEX) = AGEINT(_INDEX-1);                          
        END;                                                           
      END;                                                             
      %*--------------------------------------------------------------;
      %* Assign variable labels for the created variables.            ; 
      %* Use the macro variable WORD to contain a diagnosis number    ; 
      %* when multiple variables are passed via DX_DATE.              ; 
      %*--------------------------------------------------------------; 
      LABEL                                                             
         /*&beg_age  = "Subject AGE as of &beg_date"*/                   
         /*&end_age  = "Subject AGE as of &end_date"*/ /*04/02/2021: JOF temporarily removing beg and end date b/c not used*/                    
    %if &ndx_age gt 0 %then %let word = %str( #&i);                    
    %else %let word=;                                                   
         PY&dx._&grp.1 =                                                
                 "Person years for DX&word: Under %scan(&agegrps,1)"    
    %if %length(&dx_age) gt 0 %then                                    
         DX&dx._&grp.1 =                                                
                   "DX&word in age group: Under %scan(&agegrps,1)";     
      %do x = 2 %to &nbreaks;                                           
         PY&dx._&grp&x =                                                
               "Person years for DX&word: %scan(&agegrps,%eval(&x-1)) - 
%eval((%scan(&agegrps,&x))-1).9"                                        
    %if %length(&dx_age) gt 0 %then                                    
         DX&dx._&grp&x =                                                
                    "DX&word in age group: %scan(&agegrps,%eval(&x-1)) -
 %eval((%scan(&agegrps,&x))-1).9";                                      
      %end;                                                             
         PY&dx._&grp&nagegrps =                                         
         "Person years for DX&word: %scan(&agegrps,&nbreaks) and above" 
    %if %length(&dx_age) gt 0 %then                                    
         DX&dx._&grp&nagegrps =                                         
           "DX&word in age group: %scan(&agegrps,&nbreaks) and above";;

		/*Note: TMM changed all "if length" fxn calls here to dx_age*/	
	 
    %*----------------------------------------------------------------; 
    %*----------------------------------------------------------------; 
    %* This is the end of the loop that executes for EACH variable    ; 
    %* passed in the DX_DATE parameter.                               ; 
    %*----------------------------------------------------------------; 
    %*----------------------------------------------------------------; 
    %end;                                                               
  RUN;                                                                  
                                                                        
  %*------------------------------------------------------------------; 
  %* When in the test mode, print the created data set and produce    ; 
  %* a PROC CONTENTS.                                                 ; 
  %*------------------------------------------------------------------; 
  %if &test = Y %then %do;                                              
    TITLE3 "Ages & allocations created by macro PERS_YRS";              
    TITLE4 "First 40 observations only";                                
    FOOTNOTE3 "Input data set  :  &indata";                             
    FOOTNOTE4 "Created data set:  &outdata";                            
    PROC PRINT  DATA=&outdata (OBS=40);                                 
    RUN;                                                                
    TITLE3 'CONTENTS of data set created by macro PERS_YRS';            
    PROC CONTENTS DATA=&outdata;                                        
    RUN;    
    TITLE3;         
    FOOTNOTE3;      
  %end;             
 %quit:             
%mend pers_yrs;     
 

/*********************************************************************/

/*Note: TMM commented out calcage macro since we will have age vars*/

/*
%macro calcage(dob=,asof=,age=,dobfmt=SASDATE,asoffmt=SASDATE,
               round=,test=N);
%*---------------------------------------------------------------------;
%*                                                                     ;
%* CALCAGE     Calculate an age as of any specified SAS date.          ;
%*                                                                     ;
%*     PARAMETERS:                                                     ;
%*      DOB      Date of birth variable name                           ;
%*               Defaults to DOB                                       ;
%*                                                                     ;
%*      ASOF     Date used for determining age at a certain date.      ;
%*               Defaults to today.                                    ;
%*                                                                     ;
%*      AGE      Variable created to contain the returned numeric age  ;
%*               Defaults to AGE.                                      ;
%*                                                                     ;
%*      DOBFMT   Format of DOB variable.  Either a SASDATE or          ;
%*                 MMDDYY, YYMMDD, DDMMYY,                             ;
%*                 MMYY,   YYMM               (numeric variables)      ;
%*                 $MMDDYY, $YYMMDD, $DDMMYY                           ;
%*                 $MMYY,   $YYMM             (character variables)    ;
%*               Defaults to SASDATE.                                  ;
%*                                                                     ;
%*      ASOFFMT  Format of ASOF variable. See above for acceptable     ;
%*                 values.                                             ;
%*               Defaults to SASDATE.                                  ;
%*                                                                     ;
%*      ROUND    Round-off unit for created variable &AGE.             ;
%*                                                                     ;
%* Documentation stored as 'DOR.MUG6916.SAS.MACRO.DOC(CALCAGE)'        ;
%*                                                                     ;
%*---------------------------------------------------------------------;
%local infmt ddpos mmpos yypos;
%if %length(&dob)   = 0 %then %let dob   = DOB;
%if %length(&asof)  = 0 %then %let asof  = TODAY();
%if %length(&age)   = 0 %then %let age   = AGE;
%if %length(&asoffmt)  = 0 %then %let asoffmt   = SASDATE;
%else %let asoffmt = %upcase(&asoffmt);
%if %length(&dobfmt)  = 0 %then %let dobfmt   = SASDATE;
%else %let dobfmt = %upcase(&dobfmt);

%if %substr(&asoffmt,1,3) ne SAS %then %do;
  LENGTH _INASOF  4  __MM __DD __YY 2;
  DROP   _INASOF __MM __DD __YY;
  %if %substr(&asoffmt,1,1) = $ %then %do;
    %let infmt = $;
    %let asoffmt = %substr(&asoffmt,2);
  %end;
  %let ddpos = %index(&asoffmt,DD);
  %let mmpos = %index(&asoffmt,MM);
  %let yypos = %index(&asoffmt,YY);
  %if &infmt ne $ %then %do;
    %if &ddpos gt 0 %then %let asof = %str(PUT%(&asof,Z6.%));
    %else %let asof = %str(PUT%(&asof,Z4.%));
  %end;
  %if &test = Y %then %do;
    %put ----------------------------------------------------------;
    %put ddpos=&ddpos mmpos=&mmpos yypos=&yypos;
    %put asof=&asof;
    %put ----------------------------------------------------------;
  %end;
  %if &ddpos gt 0 %then
  __DD = INPUT(SUBSTR(&asof,&ddpos,2),2.);
  %else
  __DD = 01;;
  %if &mmpos gt 0 %then
  __MM = INPUT(SUBSTR(&asof,&mmpos,2),2.);
  %else
  __MM = 01;;
  %if &yypos gt 0 %then
  __YY = INPUT(SUBSTR(&asof,&yypos,2),2.);
  %else
  __YY = .;;
  _INASOF = MDY(__MM,__DD,__YY);
  %let asof = _INASOF;
%end;

IF &asof = . THEN &age = .;
ELSE
%if %substr(&dobfmt,1,3) ne SAS %then %do;
  DO;
    LENGTH _INDOB  4  __MM __DD __YY 2;
    DROP   _INDOB __MM __DD __YY;
    %if %substr(&dobfmt,1,1) = $ %then %do;
      %let infmt = $;
      %let dobfmt = %substr(&dobfmt,2);
    %end;
    %let ddpos = %index(&dobfmt,DD);
    %let mmpos = %index(&dobfmt,MM);
    %let yypos = %index(&dobfmt,YY);
    %if &infmt ne $ %then %do;
      %if &ddpos gt 0 %then %let dob = %str(PUT%(&dob,Z6.%));
      %else %let dob = %str(PUT%(&dob,Z4.%));
    %end;
    %if &test = Y %then %do;
      %put ----------------------------------------------------------;
      %put ddpos=&ddpos mmpos=&mmpos yypos=&yypos;
      %put dob=&dob;
      %put ----------------------------------------------------------;
    %end;
    %if &ddpos gt 0 %then
    __DD = INPUT(SUBSTR(&dob,&ddpos,2),2.);
    %else
    __DD = 01;;
    %if &mmpos gt 0 %then
    __MM = INPUT(SUBSTR(&dob,&mmpos,2),2.);
    %else
    __MM = 01;;
    %if &yypos gt 0 %then
    __YY = INPUT(SUBSTR(&dob,&yypos,2),2.);
    %else
    __YY = .;;
    _INDOB = MDY(__MM,__DD,__YY);
  END;
  %let dob = _INDOB;
%end;
%if %length(&round) gt 0 %then
 &age = ROUND(((&asof - &dob) / 365.25),&round);
%else
 &age = (&asof - &dob) / 365.25;;
%if %substr(&dobfmt,1,3) ne SAS %then %do;
IF PUT(&dob,YYMMDD6.) GT PUT(&asof,YYMMDD6.) THEN &age = &age + 100;
%end;
%mend calcage;

*/

/*********************************************************************/


%macro nwords(macrovar,test=N);                                         
%*---------------------------------------------------------------------;
%*                                                                     ;
%* NWORDS       Words are character strings that are separated by      ;
%*              delimiters.  This macro counts the number of words in a;
%*              macro variable.  The count and the last word in        ;
%*              the macro variable are placed in two global macro      ;
%*              variables, &nwords and &lastword, respectively.        ;
%*                                                                     ;
%* Documentation stored as 'DOR.MUG6916.SAS.MACRO.DOC(NWORDS)'.        ;
%*                                                                     ;
%*---------------------------------------------------------------------;
%global nwords lastword;                                                
%local macrovar test;                                                   
                                                                        
%if %upcase(%substr(&test,1,1)) = Y %then %do;                          
  %logmsg(NWORDS,I,MACROVAR,&macrovar,I,TEST,&test)                     
%end;                                                                   
                                                                        
%*--------------------------------------------------------------------; 
%*--  This macro requires different code depending upon             --; 
%*--  whether it is executing in SAS Version 5 or Version 6         --; 
%*--------------------------------------------------------------------; 
%if %substr(&sysver,1,1)=6 %then %do;                                   
                                                                        
%if %length(&&&macrovar) gt 0 %then %do;                                
  %let nwords=1;                                                        
  %do %until(%qscan(%superq(&macrovar),&nwords,%str( ))=);              
    %let lastword=%qscan(%superq(&macrovar),&nwords,%str( ));           
    %let nwords=%eval(&nwords + 1);                                     
  %end;                                                                 
  %let nwords=%eval(&nwords - 1);                                       
%end;                                                                   
%else %do;                                                              
  %let nwords = 0;                                                    
  %let lastword =;                                                    
%end;                                                                 
%end;                                           %*  END of V6 code ;  
                                                                      
%else %do;                                      %*  BEGIN V5 code  ;  
%let nwords=1;                                                        
%do %until(%scan(&&&macrovar,&nwords,%str( ))=);                      
  %let lastword=%scan(&&&macrovar,&nwords,%str( ));                   
  %let nwords=%eval(&nwords + 1);                                     
%end;                                                                 
%let nwords=%eval(&nwords - 1);                                       
                                                                      
%end;                                                                 
%mend nwords;                                                         


/*********************************************************************/

%macro logmsg(macname) / parmbuff;                                      
%local argno nwords delimit;                                            
%*---------------------------------------------------------------------;
%*                                                                     ;
%* logmsg     Print a message to the SASLOG.  This macro is invoked by ;
%*            another macro.                                           ;
%*                                                                     ;
%* Arguments:                                                          ;
%* ---------                                                           ;
%* &macname   The macro name from which this macro was invoked         ;
%*                                                                     ;
%* &1 ---                                                              ;
%* &2   |---> Triplet of parameters for each macro variable in the     ;
%* &3 ---     parameter list of the calling macro.  Each triplet       ;
%*            represents respectively: input or output, name of macro  ;
%*            variable, value of macro variable.                       ;
%*                                                                     ;
%* This macro saved as 'DOR.MUG6916.SAS.MACROS(LOGMSG)'.               ;
%* Documentation saved as 'DOR.MUG6916.SAS.MACRO.DOC(LOGMSG)'.         ;
%*---------------------------------------------------------------------;
  %let delimit=%str(,);                                                 
                                                                        
  %put %str( );                                                         
  %put %str(----------------------------------------------------------);
  %put %str(Macro &macname invoked.);                                   
  %put %str(Documentation stored as "DOR.MUG6916.SAS.MACRO.DOC(&macname)
".);                                                                    
  %put %str( );                                                         
                                                                        
  /* Next statement included to obviate infinite looping */             
                                                                        
  %let buff                                                             
    =%substr(&syspbuff,1,%eval(%length(&syspbuff)-1)),EOL,EOL,EOL);     
                                                                        
                                                                       
  %let argno=0;                                                        
  %let nwords=2;                                                       
  %do %until(%upcase(%scan(&buff,&nwords,&delimit))=EOL);              
    %let word1=%scan(&buff,&nwords,&delimit);                          
    %let nwords=%eval(&nwords + 1);                                    
    %let word2=%scan(&buff,&nwords,&delimit);                          
    %let nwords=%eval(&nwords + 1);                                    
    %let word3=%scan(&buff,&nwords,&delimit);                          
    %let argno=%eval(&argno + 1);                                      
                                                                       
    %put ARGNO=&argno  %str                                            
    (IN/OUT=&word1   ARGUMENT_NAME=&word2   ARGUMENT_VALUE=&word3);    
                                                                       
    %let nwords=%eval(&nwords + 1);                                    
  %end;                                                                
                                                                       
                                                                        
  %put %str(----------------------------------------------------------);
  %put %str( );                                                         
%mend logmsg;                                                           


/*********************************************************************/


%macro termnate(env,name,reason,argname,argvalue);                      
%*---------------------------------------------------------------------;
%*                                                                     ;
%* TERMNATE   In batch perform ABORT RETURN 99  -or-                   ;
%*            in SAS/DM perform STOP.                                  ;
%*                                                                     ;
%* Arguments:                                                          ;
%* ---------                                                           ;
%* &env       REQUIRED: specify the environment that code from the     ;
%*            macro termnate will execute in: either MACRO or DATASTEP.;
%*            When the DATA statement already is coded externally,     ;
%*            this option should be set to DATASTEP.                   ;
%* &name      SUGGESTED but not REQUIRED: the macro name from which    ;
%*            this macro was invoked                                   ;
%* &reason    OPTIONAL: see numeric reason codes printed below -or-    ;
%*                      specify your own textual reason                ;
%* &argname   OPTIONAL: can specify one or more suspicious arguments   ;
%* &argvalue  OPTIONAL: can specify one suspicious argument value      ;
%*                                                                     ;
%* This macro saved as 'LMA5.SAS.TEST.MACROS(TERMNATE)'                ;
%* Documentation saved as 'LMA5.SAS.MACRO.DOC(TERMNATE)'               ;
%*---------------------------------------------------------------------;
                                                                        
  %let env     = %trim(%upcase(&env));                                  
                                                                        
  %let l0=(INVOCATION OF MACRO &name);                                  
  %let l1=%str( );                                                      
  %let l2=%str(------------------------------------------------------); 
  %let l3=TERMINATION BY WAY OF CALL TO MACRO TERMNATE DURING;          
  %if &sysenv = BACK %then %do;                                         
    %let l4=BATCH EXECUTION &l0..;                                      
  %end; %else %do;                                                      
    %let l4=SAS/DISPLAY MANAGER EXECUTION &l0..;                        
  %end;                                                                
  %if &env=MACRO %then %do;                                            
    %put &l1;                                                          
    %put &l2;                                                          
    %put &l3;                                                          
    %put &l4;                                                          
  %end; %else %do;                                                     
    put "&l1";                                                         
    put "&l2";                                                         
    put "&l3";                                                         
    put "&l4";                                                         
  %end;                                                                
                                                                       
  %if &reason=1 %then %do;                                             
    %let l3=REASON: REQUIRED MACRO ARGUMENT(S) MISSING.;               
    %let l4=LOOK AT ARGUMENT(S): &argname;                             
  %end;                                                                
  %else                                                                
  %if &reason=2 %then %do;                                              
    %let l3=REASON: VALUE OF MACRO ARGUMENT(S) OUT OF ACCEPTABLE RANGE.;
    %let l4=LOOK AT ARGUMENT(S): &argname  %str                         
            (VALUE %(OR DATASTEP VAR%): &argvalue);                     
  %end;                                                                 
  %else %do;                                                            
    %let l3=REASON=&reason%str                                          
             (    )ARGNAME=&argname%str                                 
             (    )ARGVALUE=&argvalue;                                  
    %let l4= ;                                                          
  %end;                                                                 
  %if &env=MACRO %then %do;                                             
    %put &l3;                                                           
    %put &l4;                                                           
    %put &l2;                                                           
    %put &l1;                                                           
  %end; %else %do;                                                      
    put "&l3";                                                         
    put "&l4";                                                         
    put "&l2";                                                         
    put "&l1";                                                         
  %end;                                                                
                                                                       
  %if &env = MACRO %then %str(DATA _NULL_;);                           
  %if &sysenv = BACK %then %str(ABORT RETURN 99;);                     
    %else %str(STOP;);                                                 
  %if &env = MACRO %then %str(RUN;);                                   
                                                                       
%mend termnate;                                                        
