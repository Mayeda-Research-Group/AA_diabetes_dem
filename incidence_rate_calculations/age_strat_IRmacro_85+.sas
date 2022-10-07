/*******************************************/
/*****Age-adjusted incidence rate code******/
/*******************************************/
/*The purpose of this program is to generate the standard population and compile the macro to estimate 
	age-standardized dementia incidence rates for Asian American ADRD grant data from KPRB. 

*Code was copied and edited from program "BP_dem_age_std_dem_inc_rates_2019Dec10.sas" for the paper on cumulative exposure
to elevated blood pressure and late-life dementia risk. 
	2019Dec10 version was edited from "age_std_dem_inc_rates_2015Feb5.sas" used in 2016 Alzheimer's & Dementia paper on
	racial/ethnic inequalities in dementia incidence. Changes from 2019Feb5 to 2019Dec10 include:
		(1) Updating data files and directories.
		(2) Standardizing to 2000 US Census population ages 60+ vs. 64+, which meant adding another age category in standardization
		code.
		(3) Removed sex-stratified rates.

	*Revisions between May 2019 and Dec 2019 versions of this program:
		(1) Added code to estimate dementia incidence rates by AUC quartiles for SBP and DBP. This entails reading in BLUP data sets for SBP and DBP.
		(2) Edited macro code to round IR and adjusted IR to 2 decimal places.

*Revisions between Dec 2019 and Sept 2020 versions of this program:
	(1) Remove format and dataset sections
	(2) Remove date variables & change to age variables to accommodate de-identified KP data

*/       

/******************************************/
/*****Defining the standard population*****/
/******************************************/
data stddem;                                                                                                                                                                  
input agegrp $1-10 count total;                                                                                                                                               
datalines;                                                                                                                                                                     
(1) 60-<65 10805447 45797200 
(2) 65-<70 9533545  45797200 
(3) 70-<75 8857441  45797200 
(4) 75-<80 7415813  45797200 
(5) 80-<85 4945367  45797200 
(6) 85+    4239587  45797200 
;
run;                                                                                                                                                                          
proc print data = stddem; 
run; 

  
/******************************************/
/*****Defining the incidence macro    *****/
/******************************************/

%macro age_stratir(inds,case_age, start, end, stdpop, group, outds);  *TMM changed case_date to case_age and removed birth var call;                                                                                                 
                                                                                                                                                                              
DATA TEMP; 															*TMM removed date variables, including DOB;                                                                                                                                                                    
     SET &inds;                                                                                                                                                               
     *DOB=BIRTH_DT, DX_DATE=REG_DT, BEG_DATE=START;                                                                                                                           
     *END_DATE=END, END_AGE=AGE_END;                                                                                                                                          
     START=&start;                                                                                                                                                            
     END=&end;                                                                                                                                                                
     DX_AGE=&case_age;                                                                                                                                 
     *DX_DATE=&case_date;                                                                                                                                                      
     *BIRTH_DT=&birth;                                                                                                                                                         
     BEG_AGE=&start;                                                                                                                                     
     *END_DATE=MIN(&END,&case_date);                                                                                                                                           
     END_AGE=&end;                                                                                                                                        
 RUN;                                                                                                                                                                         
                                                                                                                                                                              
%pers_yrs(indata=temp,                                                                                                                                                        
                  outdata=pyage,                                                                                                                                              
                  agegrps= 65 70 75 80 85,                                                                                                                                        
                  /*dob=birth_dt,                                                                                                                                               
                  dx_date=dx_date,*/                                                                                                                                            
                  dx_age=dx_age,                                                                                                                                              
                  /*beg_date=start,                                                                                                                                             
                  end_date=end_date,*/                                                                                                                                         
                  beg_age=beg_age,                                                                                                                                            
                  end_age=end_age,                                                                                                                                            
                  xtracode=,                                                                                                                                                  
                  test=N);                                                                                                                                                    
                                                                                                                                                                              
** sum person-years **;                                                                                                                                                       
proc means data=pyage noprint;                                                                                                                                                
        var dx_grp1-dx_grp6 py_grp1-py_grp6;                                                                                                                                  
        output out=sumall                                                                                                                                                     
                   sum=totdx1-totdx6 totpy1-totpy6;                                                                                                                           
run;                                                                                                                                                                          
                                                                                                                                                                              
proc transpose data=sumall out=event;                                                                                                                                         
        var totdx1-totdx6;                                                                                                                                                    
run;                                                                                                                                                                          
                                                                                                                                                                              
proc transpose data=sumall out=py;                                                                                                                                            
  var totpy1-totpy6;                                                                                                                                                          
run;                                                                                                                                                                          
                                                                                                                                                                              
data event (drop= _name_ rename= (col1=event)); set event;                                                                                                                    
  length agegrp $10;                                                                                                                                                          
  if _name_='totdx1'      then agegrp='(1) 60-<65';  
  else if _name_='totdx2' then agegrp='(2) 65-<70';   
  else if _name_='totdx3' then agegrp='(3) 70-<75';                                                                                                                           
  else if _name_='totdx4' then agegrp='(4) 75-<80';                                                                                                                           
  else if _name_='totdx5' then agegrp='(5) 80-<85';                                                                                                                           
  else if _name_='totdx6' then agegrp='(6) 85+';
run;                                                                                                                                                                          
                                                                                                                                                                              
data py (drop= _name_ rename= (col1=py)); set py;                                                                                                                             
  length agegrp $10;                                                                                                                                                          
  if _name_='totpy1'      then agegrp='(1) 60-<65';      
  else if _name_='totpy2' then agegrp='(2) 65-<70';     
  else if _name_='totpy3' then agegrp='(3) 70-<75';                                                                                                                          
  else if _name_='totpy4' then agegrp='(4) 75-<80';                                                                                                                            
  else if _name_='totpy5' then agegrp='(5) 80-<85';                                                                                                                            
  else if _name_='totpy6' then agegrp='(6) 85+';
run;                                                                                                                                                                          
                                                                                                                                                                              
proc sort data=event; by agegrp;                                                                                                                                              
proc sort data=py; by agegrp;                                                                                                                                                 
                                                                                                                                                                              
data ir1; merge event py &stdpop; by agegrp;                                                                                                                                  
        ir=(event/py);                                                                                                                                                        
        var=event/(py**2);                                                                                                                                                    
        irwt=ir*(count/total);                                                                                                                                                
        varwt=((count/total)**2)*var;                                                                                                                                         
run;                                                                                                                                                                          
                                                                                                                                                                              
proc means data=ir1 noprint;                                                                                                                                                  
        var irwt varwt event py;                                                                                                                                              
        output out=&outds sum=adj_ir adj_var event py;                                                                                                                        
run;                                                                                                                                                                          
/*                                                                                                                                                                              */
/*data &outds;    set &outds;                                                                                                                                                   */
/*        group=&group;                                                                                                                                                         */
/*        IR=round((event/py)*1000,.01);             		*Note: ERM edited code to round IR to two decimal places on 12/12/19;                                                                                                                                                       */
/*        SE=sqrt(event/(py**2));                                                                                                                                               */
/*        IR_L=IR-(1.96*SE*1000);                                                                                                                                              */
/*    IR_U=IR+(1.96*SE*1000);                                                                                                                                                  */
/*        ADJ_IR=round(ADJ_IR*1000,.01);             *Note: ERM edited code to round IR to two decimal places on 12/12/19;                                                                                                                                     */
/*    ADJ_IR_L=ADJ_IR-(1.96*SQRT(ADJ_VAR)*1000);                                                                                                                               */
/*    ADJ_IR_U=ADJ_IR+(1.96*SQRT(ADJ_VAR)*1000);                                                                                                                               */
/*        ADJ_IR_CI='('||trim(left(round(ADJ_IR_L,.01)))||','||trim(left(round(ADJ_IR_U,.01)))||')';                                                                            */
/*        IR_CI='('||trim(left(round(IR_L,.01)))||','||trim(left(round(IR_U,.01)))||')';                                                                                        */
/*        drop _type_ _freq_ IR_L IR_U ADJ_IR_L ADJ_IR_U ADJ_VAR SE;                                                                                                            */
/*run;*/

/*for age-stratified output*/
/*editing to make per 1000 PYers*/
data &outds;    set ir1;                                                                                                                                                   
        group=&group;                                                                                                                                                         
        IR=round((event/py)*1000,.01);                                                                                                                                                 
        SE=sqrt(event/(py**2));                                                                                                                                               
        IR_L=IR-(1.96*SE*100);                                                                                                                                              
    IR_U=IR+(1.96*SE*100);                                                                                                                                                  
                                                                          
        IR_CI='('||trim(left(round(IR_L,.01)))||','||trim(left(round(IR_U,.01)))||')';                                                                                        
        drop IR_L IR_U ;                                                                                                            
run;     
                                                                                                                                                                              
%mend age_stratir;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
