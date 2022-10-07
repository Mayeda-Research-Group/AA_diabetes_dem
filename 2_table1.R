# ---- set up packages ----
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("tidyverse", "table1", "labelled", "openxlsx"
       # "magrittr", "foreign", "ggplot2", "haven", 
       # "survey", "tableone", "mice", "survival", "miceadds", "mgcv",
       # "ggpubr", "mitools", "lmtest", "huxtable", "kableExtra"
)

options(scipen = 999, digits = 8)

# ---- set up paths and datasets ----

path_to_box <- "/Users/julietzhou/Library/CloudStorage/Box-Box/"
path_to_imputed <- "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/"

pre_MI_tte_data <- readRDS(file = paste0(path_to_box, path_to_imputed,
                                         "aa_adrd_diabetes_pre_MI_tte_data.rds"))
imputed_tbl1_data <- readRDS(file = paste0(path_to_box, path_to_imputed, 
                                           "aa_adrd_diabetes_imputed_tbl1_data.rds"))

# load functions for cleaning up table 1
source(paste0(path_to_box, 
              "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/",
              "Code/Cleaned_Scripts/functions_tbl1.R"))

# check dimensions
dim(pre_MI_tte_data)
dim(imputed_tbl1_data %>% filter(imp == 10))
# the one additional variable in imputed dataset is imp

# ---- pre imputation Table 1 (5/7/9+yr membership) ----

# add membership criteria to pre-imputation dataset
tbl1_data_pre_MI_5 <- pre_MI_tte_data %>% 
  filter(PRESURV5YR_SAMPLE == 1) %>% t1_relabel()
tbl1_data_pre_MI_7 <- pre_MI_tte_data %>% 
  filter(PRESURV7YR_SAMPLE == 1) %>% t1_relabel()
tbl1_data_pre_MI_9 <- pre_MI_tte_data %>% 
  filter(PRESURV9YR_SAMPLE == 1) %>% t1_relabel()

tbl1_df_pre_MI_5 <- tbl1_data_pre_MI_5 %>% 
  table1(~ SURVEY_AGE + FEMALE + EDU_GE_COLLEGE +
           # EDUCATION_REV + EDU_3 +
           USABORN_REV +
           MARITALSTATUS + INCOME_PP +
           GENERALHEALTH + EHR_HT_MEDIAN +
           HTN_DX5YR_FLAG + FIRST_PREVSTROKE_FLAG + 
           MAIN_DEM_V1_END_TYPE + 
           MAIN_DEM_V1_FU_TIME
         | ETHNICITY_REV + DIAB_DX5YR_FLAG, 
         data = ., overall = NULL, render.continuous = my.render.cont,
         caption = paste0(
           "Summary statistics stratified on race/ethnicity and diabetes ",
           "diagnosis 5+yrs pre-survey using the pre-imputation dataset based ", 
           "on 5+1 yr membership criteria.") 
  ) 

tbl1_df_pre_MI_7 <- tbl1_data_pre_MI_7 %>% 
  table1(~ SURVEY_AGE + FEMALE + EDU_GE_COLLEGE +
           # EDUCATION_REV + EDU_3 +
           USABORN_REV +
           MARITALSTATUS + INCOME_PP +
           GENERALHEALTH + EHR_HT_MEDIAN +
           HTN_DX7YR_FLAG + FIRST_PREVSTROKE_FLAG + 
           MAIN_DEM_V1_END_TYPE + 
           MAIN_DEM_V1_FU_TIME
         | ETHNICITY_REV + DIAB_DX7YR_FLAG, 
         data = ., overall = NULL, render.continuous = my.render.cont,
         caption = paste0(
           "Summary statistics stratified on race/ethnicity and diabetes ",
           "diagnosis 7+yrs pre-survey using the pre-imputation dataset based ", 
           "on 7+1 yr membership criteria."
         ) 
  ) 

tbl1_df_pre_MI_9 <- tbl1_data_pre_MI_9 %>% 
  table1(~ SURVEY_AGE + FEMALE + EDU_GE_COLLEGE +
           # EDUCATION_REV + EDU_3 +
           USABORN_REV +
           MARITALSTATUS + INCOME_PP +
           GENERALHEALTH + EHR_HT_MEDIAN +
           HTN_DX9YR_FLAG + FIRST_PREVSTROKE_FLAG + 
           MAIN_DEM_V1_END_TYPE + 
           MAIN_DEM_V1_FU_TIME
         | ETHNICITY_REV + DIAB_DX9YR_FLAG, 
         data = ., overall = NULL, render.continuous = my.render.cont,
         caption = paste0(
           "Summary statistics stratified on race/ethnicity and diabetes ",
           "diagnosis 9+yrs pre-survey using the pre-imputation dataset based ",
           "on 9+1 yr membership criteria."
         )
  ) 

# ETHNICITY_REV by DIAB_DX*YR_FLAG header percentages
table(tbl1_data_pre_MI_5$ETHNICITY_REV, tbl1_data_pre_MI_5$DIAB_DX5YR_FLAG) %>% 
  prop.table(margin = 1) 
table(tbl1_data_pre_MI_7$ETHNICITY_REV, tbl1_data_pre_MI_7$DIAB_DX7YR_FLAG) %>% 
  prop.table(margin = 1) 
table(tbl1_data_pre_MI_9$ETHNICITY_REV, tbl1_data_pre_MI_9$DIAB_DX9YR_FLAG) %>% 
  prop.table(margin = 1) 

pre_MI_tbl1s <- list("5+yr" = tbl1_df_pre_MI_5, 
                     "7+yr" = tbl1_df_pre_MI_7, 
                     "9+yr" = tbl1_df_pre_MI_9)

# write.xlsx(pre_MI_tbl1s,
#            file = paste0(path_to_box,
#                          "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/",
#                          "Code/Cleaned_Scripts/output/table1s/pre_MI_tbl1s_subgrp.xlsx"))

# ---- post imputation Table 1 (5/7/9+yr membership) ----
# Note that education, household-adjusted income, marital status, nativity, and 
# EHR height are imputed. Therefore, only these summary statistics can change 
# between imputed datasets. The rest of the variables have fixed summary 
# statistics across imputations, the same as pre-imputation summary statistics. 

# subset on membership criteria to get imputed table1 datasets
tbl1_data_post_MI_5 <- imputed_tbl1_data %>% 
  filter(PRESURV5YR_SAMPLE == 1) %>% t1_relabel()
tbl1_data_post_MI_7 <- imputed_tbl1_data %>% 
  filter(PRESURV7YR_SAMPLE == 1) %>% t1_relabel()
tbl1_data_post_MI_9 <- imputed_tbl1_data %>% 
  filter(PRESURV9YR_SAMPLE == 1) %>% t1_relabel()

## categorical ----

tbl1_df_post_MI_5 <- tbl1_data_post_MI_5 %>% 
  table1(~ # SURVEY_AGE + 
           FEMALE + EDU_GE_COLLEGE +
           # EDUCATION_REV  + EDU_3 +
           USABORN_REV +
           MARITALSTATUS + 
           # INCOME_PP +
           GENERALHEALTH + 
           # EHR_HT_MEDIAN +
           HTN_DX5YR_FLAG + FIRST_PREVSTROKE_FLAG + 
           MAIN_DEM_V1_END_TYPE 
           # MAIN_DEM_V1_FU_TIME
         | ETHNICITY_REV + DIAB_DX5YR_FLAG, 
         data = ., overall = NULL, 
         render.continuous = my.render.cont,
         render.categorical = my.render.cat_imp,
         caption = paste0(
           "Summary statistics stratified on race/ethnicity and diabetes ",
           "diagnosis 5+yrs pre-survey using the post-imputation dataset based ", 
           "on 5+1 yr membership criteria.") 
  ) 

tbl1_df_post_MI_7 <- tbl1_data_post_MI_7 %>% 
  table1(~ # SURVEY_AGE + 
           FEMALE + EDU_GE_COLLEGE +
           # EDUCATION_REV  + EDU_3 +
           USABORN_REV +
           MARITALSTATUS + 
           # INCOME_PP +
           GENERALHEALTH + 
           # EHR_HT_MEDIAN +
           HTN_DX7YR_FLAG + FIRST_PREVSTROKE_FLAG + 
           MAIN_DEM_V1_END_TYPE 
         # MAIN_DEM_V1_FU_TIME
         | ETHNICITY_REV + DIAB_DX7YR_FLAG, 
         data = ., overall = NULL, 
         render.continuous = my.render.cont,
         render.categorical = my.render.cat_imp,
         caption = paste0(
           "Summary statistics stratified on race/ethnicity and diabetes ",
           "diagnosis 7+yrs pre-survey using the post-imputation dataset based ", 
           "on 7+1 yr membership criteria.") 
  ) 

tbl1_df_post_MI_9 <- tbl1_data_post_MI_9 %>% 
  table1(~ # SURVEY_AGE + 
           FEMALE + EDU_GE_COLLEGE +
           # EDUCATION_REV  + EDU_3 +
           USABORN_REV +
           MARITALSTATUS + 
           # INCOME_PP +
           GENERALHEALTH + 
           # EHR_HT_MEDIAN +
           HTN_DX9YR_FLAG + FIRST_PREVSTROKE_FLAG + 
           MAIN_DEM_V1_END_TYPE 
         # MAIN_DEM_V1_FU_TIME
         | ETHNICITY_REV + DIAB_DX9YR_FLAG, 
         data = ., overall = NULL, 
         render.continuous = my.render.cont,
         render.categorical = my.render.cat_imp,
         caption = paste0(
           "Summary statistics stratified on race/ethnicity and diabetes ",
           "diagnosis 9+yrs pre-survey using the post-imputation dataset based ", 
           "on 9+1 yr membership criteria.") 
  ) 

cat_post_MI_tbl1s <- list("5+yr" = tbl1_df_post_MI_5, 
                          "7+yr" = tbl1_df_post_MI_7, 
                          "9+yr" = tbl1_df_post_MI_9) 

# write.xlsx(cat_post_MI_tbl1s, rowNames = TRUE,
#            file = paste0(path_to_box,
#                          "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/",
#                          "Code/Cleaned_Scripts/output/table1s/cat_post_MI_tbl1s_subgrp.xlsx"))

## continuous ----
## SURVEY_AGE and MAIN_DEM_V1_FU_TIME are complete
## EHR_HT_MEDIAN and INCOME_PP are imputed
tbl1_cts_post_MI_5 <- imp_cts_var_tbl1(tbl1_data_post_MI_5, "DIAB_DX5YR_FLAG")
tbl1_cts_post_MI_7 <- imp_cts_var_tbl1(tbl1_data_post_MI_7, "DIAB_DX7YR_FLAG")
tbl1_cts_post_MI_9 <- imp_cts_var_tbl1(tbl1_data_post_MI_9, "DIAB_DX9YR_FLAG")

cts_post_MI_tbl1s <- list("5+yr" = tbl1_cts_post_MI_5, 
                          "7+yr" = tbl1_cts_post_MI_7, 
                          "9+yr" = tbl1_cts_post_MI_9)

# write.xlsx(cts_post_MI_tbl1s, rowNames = TRUE,
#            file = paste0(path_to_box,
#                          "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/",
#                          "Code/Cleaned_Scripts/output/table1s/cts_post_MI_tbl1s_subgrp.xlsx"))

# ---- minor pieces for manuscript ----
tbl1_data_pre_MI_5$SURVEY_AGE %>% mean()
tbl1_data_pre_MI_5$SURVEY_AGE %>% sd()

