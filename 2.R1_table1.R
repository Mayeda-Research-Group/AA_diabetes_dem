# set up packages ----
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("tidyverse", "table1", "labelled", "openxlsx"
       # "magrittr", "foreign", "ggplot2", "haven", 
       # "survey", "tableone", "mice", "survival", "miceadds", "mgcv",
       # "ggpubr", "mitools", "lmtest", "huxtable", "kableExtra"
)

options(scipen = 999, digits = 8)

# set up paths and datasets ----

path_to_box <- "/Users/julietzhou/Library/CloudStorage/Box-Box/"
path_to_imputed <- "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/"
path_to_proj <- "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/Code/Cleaned_Scripts/"

pre_MI_tte_data <- readRDS(file = paste0(path_to_box, path_to_imputed,
                                         "aa_adrd_diabetes_pre_MI_tte_data.rds"))
imputed_tbl1_data <- readRDS(file = paste0(path_to_box, path_to_imputed, 
                                           "aa_adrd_diabetes_imputed_tbl1_data.rds"))


# check dimensions
dim(pre_MI_tte_data)
dim(imputed_tbl1_data %>% filter(imp == 10))
# the only additional variable in imputed dataset is imp

# remove Type 1 DM 5+ yrs from exposure ----
# run the script that pulls diabetes type according to R1 revision
source(paste0(path_to_box, path_to_proj, "2.0.R1_data_prep.R"))

# load functions for cleaning up table 1
source(paste0(path_to_box, path_to_proj, "functions_tbl1.R"))


# pre imputation Table 1 (5/7/9+yr membership) ----

# merge in BMI and SR depression from original data, for R1
pre_MI_tte_data <- original_data %>% 
  select(SUBJID, SR_BMI, SR_DEPRESS) %>% 
  right_join(pre_MI_tte_data, by = "SUBJID")

# add membership criteria to pre-imputation dataset
tbl1_data_pre_MI_5 <- pre_MI_tte_data %>% 
  left_join(diab_info, by = "SUBJID") %>% 
  filter(PRESURV5YR_SAMPLE == 1, DIAB_DX5YR_T1 != 1) %>% 
  t1_relabel() %>% t1_relabel_add()
tbl1_data_pre_MI_7 <- pre_MI_tte_data %>% 
  left_join(diab_info, by = "SUBJID") %>% 
  filter(PRESURV7YR_SAMPLE == 1, DIAB_DX7YR_T1 != 1) %>% 
  t1_relabel() %>% t1_relabel_add()
tbl1_data_pre_MI_9 <- pre_MI_tte_data %>% 
  left_join(diab_info, by = "SUBJID") %>% 
  filter(PRESURV9YR_SAMPLE == 1, DIAB_DX9YR_T1 != 1) %>% 
  t1_relabel() %>% t1_relabel_add()

# check dimensions again
dim(tbl1_data_pre_MI_5)
dim(tbl1_data_pre_MI_7)
dim(tbl1_data_pre_MI_9)

# do table 1's
tbl1_df_pre_MI_5 <- tbl1_data_pre_MI_5 %>% 
  table1(~ SURVEY_AGE + FEMALE + EDU_GE_COLLEGE +
           # EDUCATION_REV + EDU_3 +
           USABORN_REV +
           MARITALSTATUS + INCOME_PP +
           GENERALHEALTH + EHR_HT_MEDIAN +
           SR_BMI + SR_DEPRESS + SMOKING_STATUS + # added in R1
           HTN_DX5YR_FLAG + FIRST_PREVSTROKE_FLAG + 
           MAIN_DEM_V1_END_TYPE + 
           MAIN_DEM_V1_FU_TIME
         | ETHNICITY_REV + DIAB_DX5YR_FLAG, 
         data = ., overall = NULL, render.continuous = my.render.cont,
         rowlabelhead = "Diabetes Status (assessed 5+ years before baseline)", 
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
           SR_BMI + SR_DEPRESS + SMOKING_STATUS + # added in R1
           HTN_DX7YR_FLAG + FIRST_PREVSTROKE_FLAG + 
           MAIN_DEM_V1_END_TYPE + 
           MAIN_DEM_V1_FU_TIME
         | ETHNICITY_REV + DIAB_DX7YR_FLAG, 
         data = ., overall = NULL, render.continuous = my.render.cont,
         rowlabelhead = "Diabetes Status (assessed 7+ years before baseline)", 
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
           SR_BMI + SR_DEPRESS + SMOKING_STATUS + # added in R1
           HTN_DX9YR_FLAG + FIRST_PREVSTROKE_FLAG + 
           MAIN_DEM_V1_END_TYPE + 
           MAIN_DEM_V1_FU_TIME
         | ETHNICITY_REV + DIAB_DX9YR_FLAG, 
         data = ., overall = NULL, render.continuous = my.render.cont,
         rowlabelhead = "Diabetes Status (assessed 9+ years before baseline)", 
         caption = paste0(
           "Summary statistics stratified on race/ethnicity and diabetes ",
           "diagnosis 9+yrs pre-survey using the pre-imputation dataset based ",
           "on 9+1 yr membership criteria."
         )
  ) 

# ETHNICITY_REV by DIAB_DX*YR_FLAG header percentages
ethn_n <- cbind(
  with(tbl1_data_pre_MI_5, table(ETHNICITY_REV)),
  with(tbl1_data_pre_MI_7, table(ETHNICITY_REV)),
  with(tbl1_data_pre_MI_9, table(ETHNICITY_REV))
) 

ethn_x_diab <- cbind(
  with(tbl1_data_pre_MI_5, table(ETHNICITY_REV, DIAB_DX5YR_FLAG)) %>% 
    proportions(margin = 1), 
  with(tbl1_data_pre_MI_7, table(ETHNICITY_REV, DIAB_DX7YR_FLAG)) %>% 
    proportions(margin = 1), 
  with(tbl1_data_pre_MI_9, table(ETHNICITY_REV, DIAB_DX9YR_FLAG)) %>% 
    proportions(margin = 1)
) %>% round_pad(digits = 3)

pre_MI_tbl1s <- list("5+yr" = tbl1_df_pre_MI_5, 
                     "7+yr" = tbl1_df_pre_MI_7, 
                     "9+yr" = tbl1_df_pre_MI_9, 
                     "ethn_n" = ethn_n, 
                     "ethn_x_diab" = ethn_x_diab)

# write.xlsx(pre_MI_tbl1s, rowNames = TRUE,
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
  left_join(diab_info, by = "SUBJID") %>% 
  filter(PRESURV5YR_SAMPLE == 1, DIAB_DX5YR_T1 != 1) %>% t1_relabel()
tbl1_data_post_MI_7 <- imputed_tbl1_data %>% 
  left_join(diab_info, by = "SUBJID") %>% 
  filter(PRESURV7YR_SAMPLE == 1, DIAB_DX7YR_T1 != 1) %>% t1_relabel()
tbl1_data_post_MI_9 <- imputed_tbl1_data %>% 
  left_join(diab_info, by = "SUBJID") %>% 
  filter(PRESURV9YR_SAMPLE == 1, DIAB_DX9YR_T1 != 1) %>% t1_relabel()

# check dimensions
nrow(tbl1_data_post_MI_5) / 20

## categorical ----

tbl1_df_post_MI_5 <- tbl1_data_post_MI_5 %>% 
  table1(~ EDU_GE_COLLEGE + USABORN_REV + MARITALSTATUS + 
           GENERALHEALTH + SMOKING_STATUS
         | ETHNICITY_REV + DIAB_DX5YR_FLAG, 
         data = ., overall = NULL, 
         rowlabelhead = "Diabetes Status (assessed 5+ years before baseline)", 
         render.continuous = my.render.cont,
         render.categorical = my.render.cat_imp,
         caption = paste0(
           "Summary statistics stratified on race/ethnicity and diabetes ",
           "diagnosis 5+yrs pre-survey using the post-imputation dataset based ", 
           "on 5+1 yr membership criteria.") 
  ) 

tbl1_df_post_MI_7 <- tbl1_data_post_MI_7 %>% 
  table1(~ EDU_GE_COLLEGE + USABORN_REV + MARITALSTATUS + 
           GENERALHEALTH + SMOKING_STATUS
         | ETHNICITY_REV + DIAB_DX7YR_FLAG, 
         data = ., overall = NULL, 
         rowlabelhead = "Diabetes Status (assessed 7+ years before baseline)", 
         render.continuous = my.render.cont,
         render.categorical = my.render.cat_imp,
         caption = paste0(
           "Summary statistics stratified on race/ethnicity and diabetes ",
           "diagnosis 7+yrs pre-survey using the post-imputation dataset based ", 
           "on 7+1 yr membership criteria.") 
  ) 

tbl1_df_post_MI_9 <- tbl1_data_post_MI_9 %>% 
  table1(~ EDU_GE_COLLEGE + USABORN_REV + MARITALSTATUS + 
           GENERALHEALTH + SMOKING_STATUS
         | ETHNICITY_REV + DIAB_DX9YR_FLAG, 
         data = ., overall = NULL, 
         rowlabelhead = "Diabetes Status (assessed 9+ years before baseline)", 
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
## SURVEY_AGE and MAIN_DEM_V1_FU_TIME are complete;
## summary of these two variables are copied from the pre-MI tables

## EHR_HT_MEDIAN and INCOME_PP are imputed
cts_imp_vars <- list("INCOME_PP" = 0, "EHR_HT_MEDIAN" = 1)
tbl1_cts_post_MI_5 <- imp_cts_var_tbl1(tbl1_data_post_MI_5, cts_imp_vars,
                                       c("ETHNICITY_REV", "DIAB_DX5YR_FLAG"))
tbl1_cts_post_MI_7 <- imp_cts_var_tbl1(tbl1_data_post_MI_7, cts_imp_vars,
                                       c("ETHNICITY_REV", "DIAB_DX7YR_FLAG"))
tbl1_cts_post_MI_9 <- imp_cts_var_tbl1(tbl1_data_post_MI_9, cts_imp_vars,
                                       c("ETHNICITY_REV", "DIAB_DX9YR_FLAG"))

cts_post_MI_tbl1s <- list("5+yr" = tbl1_cts_post_MI_5, 
                          "7+yr" = tbl1_cts_post_MI_7, 
                          "9+yr" = tbl1_cts_post_MI_9)

# write.xlsx(cts_post_MI_tbl1s, rowNames = TRUE,
#            file = paste0(path_to_box,
#                          "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/",
#                          "Code/Cleaned_Scripts/output/table1s/cts_post_MI_tbl1s_subgrp.xlsx"))

# minor pieces for manuscript ----
tbl1_data_pre_MI_5$SURVEY_AGE %>% mean() # 71.99
tbl1_data_pre_MI_5$SURVEY_AGE %>% sd() # 7.75
tbl1_data_pre_MI_5$MAIN_DEM_V1_FU_TIME %>% mean() # 9.3

