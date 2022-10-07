# Data Construction, Part 1
# for additional descriptive analysis for diabetes severity using lab data


# load packages ----
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "table1", "openxlsx", "labelled"
       # "magrittr", "foreign", 
       # "survey", "tableone", "mice", "survival", "miceadds", "mgcv",
       # "ggpubr", "mitools", "lmtest", "huxtable", "kableExtra"
)

options(scipen = 999, digits = 8)


# set up paths ----
path_to_box <- "/Users/julietzhou/Library/CloudStorage/Box-Box/"
path_to_lab_data <- "Asian_Americans_dementia_data/diabetes_adrd/Additional_lab_data/"
# path_to_EHR_data <- "Asian_Americans_dementia_data/diabetes_adrd/Additional_EHR_data/"
path_to_analytical_data <- "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/"

# note: the original datasets are in 
# /Box/Asian_Americans_dementia_data/raw_data_tables/Lab_data
# datasets in .sas7bdat format in Additional_lab/EHR_data folders are split 
# and have unscrambled dates from the original datasets
# SAS scripts for these steps are included in this directory 


# read in analytical dataset ----
pre_MI_tte <- read_rds(paste0(path_to_box, path_to_analytical_data, 
                              "aa_adrd_diabetes_pre_MI_tte_data.rds"))
# subset to subjects with diabetes 5 yrs pre-survey 
# also meeting pre-survey membership criteria
pre_MI_tte_diab_5 <- pre_MI_tte %>% 
  select(SUBJID, SURVEY_AGE, 
         FEMALE, ETHNICITY_REV, ASIAN, 
         starts_with("MAIN"), starts_with("DIAB"), starts_with("PRESURV")) %>% 
  filter(PRESURV5YR_SAMPLE == 1, # pre-survey membership criteria
         DIAB_DX5YR_FLAG == 1) # with diabetes 5+ yrs pre-survey

# check with table 1
# table(pre_MI_tte_diab_5$ETHNICITY_REV)


# process unscrambled datasets ----
# subset to subjects in the analytical dataset with diabetes (5+yrs)

## Serum Creatinine ----

# a function that reads in the unscrambled dataset,  
# filter to subjects of interest, and add pre-survey 1 yr flag
cr_s_prep <- function(i) {
  outdata <- read_sas(paste0(path_to_box, path_to_lab_data, 
                             "lab1_cr_s_", i, "_ds.sas7bdat")) %>% 
    select(-starts_with("MODIFIER"), -starts_with("NORMAL"), -REF_DATE) %>% 
    filter(SUBJID %in% pre_MI_tte_diab_5$SUBJID) %>% 
    left_join(pre_MI_tte_diab_5, by = "SUBJID") %>% 
    mutate(
      pre_surv_1y_flag = 
        ifelse(LAB_AGE >= SURVEY_AGE - 1 & LAB_AGE <= SURVEY_AGE, 1, 0)
    )
  return(outdata)
}

cr_s_tte <- cr_s_prep(1)
for (i in 2:5) {
  cr_s_tte <- cr_s_tte %>% add_row(cr_s_prep(i))
}

# check 
# cr_s_tte %>% distinct(SUBJID) %>% nrow() # n = 14213

# there are inexact results in serum creatinine: <=0.14, >5.83
# table(cr_s_tte$RESULT, useNA = "ifany")

# convert lab results to numeric, and add inexact flag
cr_s_tte <- cr_s_tte %>% 
  mutate(
    RESULT_num = case_when(
      RESULT == "<=0.14" ~ 0.14,
      RESULT == ">5.83" ~ 5.83,
      TRUE ~ as.numeric(RESULT)
    ), 
    INEXACT_FLAG = case_when(
      RESULT == "<=0.14" ~ "SE",
      RESULT == ">5.83" ~ "GT",
      TRUE ~ "EXACT"
    )
  ) 
# there is a warning message: NAs introduced by coercion
# this is because case_when evaluates RHS for all the obs

# save the processed dataset
saveRDS(cr_s_tte, paste0(path_to_box, path_to_lab_data, "cleaned_data/crs_s_tte.rds"))


## Urine Tests (NOT DOING THIS ANYMORE) ----
# a function that reads in the unscrambled dataset and 
# filter to subjects of interest
u_test_prep <- function(datapath) {
  outdata <- read_rds(datapath) %>% 
    select(-starts_with("MODIFIER"), -starts_with("NORMAL"), -REF_DATE) %>% 
    filter(SUBJID %in% pre_MI_tte_diab_5$SUBJID) %>% 
    left_join(pre_MI_tte_diab_5, by = "SUBJID")
  return(outdata)
}


## HbA1c ----
hba1c_tte <- read_sas(paste0(path_to_box, path_to_lab_data, 
                             "lab1_hba1c_ds.sas7bdat")) %>% 
  select(-starts_with("MODIFIER"), -starts_with("NORMAL"), -REF_DATE) %>% 
  filter(SUBJID %in% pre_MI_tte_diab_5$SUBJID) %>%  
  left_join(pre_MI_tte_diab_5, by = "SUBJID") %>% 
  mutate(
    pre_surv_1y_flag = 
      ifelse(LAB_AGE >= SURVEY_AGE - 1 & LAB_AGE <= SURVEY_AGE, 1, 0)
  )

# 2 subjects in the analytical dataset are missing from the hba1c dataset
hba1c_tte %>% distinct(SUBJID) %>% nrow() # n = 14211 subjects
# SUBJID: 633710057724, 842582930591
# no rows with these 2 ID's in the original dataset
# read_sas(paste0(path_to_box, path_to_lab_data, "lab1_hba1c_ds.sas7bdat")) %>% 
#   filter(SUBJID %in% c(633710057724, 842582930591)) 

# there are inexact lab values in the dataset: <=4 and >12.1
# convert lab results to numeric, and add inexact flag
hba1c_tte <- hba1c_tte %>% 
  mutate(
    RESULT_num = case_when(
      RESULT == "<=4" ~ 4,
      RESULT == ">12.1" ~ 12.1, 
      TRUE ~ as.numeric(RESULT)
    ), 
    INEXACT_FLAG = case_when(
      RESULT == "<=4" ~ "LE",
      RESULT == ">12.1" ~ "GT", 
      TRUE ~ "EXACT"
    ), 
  ) 
# there is a warning message: NAs introduced by coercion
# this is because case_when evaluates RHS for all the obs

# save 
saveRDS(hba1c_tte, 
        paste0(path_to_box, path_to_lab_data, "cleaned_data/hba1c_tte.rds"))


