# AA_diabetes_dem

This repo contains R and SAS scripts for the paper named Heterogeneity in the effect of diabetes on dementia incidence in a diverse cohort of Asian and White older Americans. 

1. multiple_imputation.R performs imputation for some demographic and health-related variables in the RPGEH dataset. 

2. table1.R prepares tables of summary statistics pre- and post-imputation. 

3. analysis_main_cox/aalen.R fits Cox PH and Aalen models to estimate hazard ratios and hazard differences between non-Latino Whites and Asians and also across Asian ethnic groups. 

4. sensitivity_analysis_1/2/3.R performs sensitivity analyses by fitting variations of the Cox PH and Aalen models. 

Auxiliary scripts include: 
* figure.R generates figures for incidence rates and hazard ratios/differences. 

* functions_tlb1.R contains functions called in 2_table1.R. 

* functions_analysis.R contains functions called in 3 and 4. 

Folders: 

* diabetes_severity_descriptive_summary contains SAS and R scripts for cleaning EHR and lab data and produce summary statistics for diabetes severity.
  - 1-3 read and clean the EHR and lab data. 
  - 4 calculates summary statistics of interest. 
  
  
* incidence_rate_calculations 
  - `adrd_age_adj_IR_call_x_diab+presurv_04042022.sas` and `adrd_age_spec_IR_call_x_diab+presurv_03072022.sas` calculates the age-adjusted and age-specific incidence rates stratified by diabetes status and race/ethnciity. 
  - `age_adj_IRmacro_20210413.sas`, `age_strat_IRmacro_85+.sas`, and `inc_rate_ancillary_macros_20210413.sas` are macros and auxiliary functions called in the previous SAS scripts. 
