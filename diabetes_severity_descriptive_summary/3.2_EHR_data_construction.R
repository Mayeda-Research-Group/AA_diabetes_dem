# Data Construction, Part 2
# for additional descriptive analysis for diabetes severity using EHR data


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
# path_to_lab_data <- "Asian_Americans_dementia_data/diabetes_adrd/Additional_lab_data/"
path_to_EHR_data <- "Asian_Americans_dementia_data/diabetes_adrd/Additional_EHR_data/"
path_to_analytical_data <- "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/"

# note: the original datasets are in 
# /Box/Asian_Americans_dementia_data/raw_data_tables/EHR_data
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

## dialysis status ----
ESRD <- read_sas(paste0(path_to_box, path_to_EHR_data, "esrd.sas7bdat")) %>% 
  select(-starts_with("MODIFIER"), -starts_with("NORMAL"), 
         -REF_DATE, -TEMP_DATE) %>% 
  filter(SUBJID %in% pre_MI_tte_diab_5$SUBJID)

# save the processed dataset
saveRDS(ESRD, paste0(path_to_box, path_to_EHR_data, "cleaned_data/ESRD.rds"))

## all_obs_dx ----
all_obs_dx <- read_sas(paste0(path_to_box, path_to_EHR_data, 
                              "all_obs_dx.sas7bdat")) %>% 
  select(-starts_with("MODIFIER"), -starts_with("NORMAL"), 
         -REF_DATE, -TEMP_DATE) %>% 
  filter(
    # all subjects with missing DX_AGE have AMI and HYPOGLY flags of 0
    !is.na(DX_AGE), 
    SUBJID %in% pre_MI_tte_diab_5$SUBJID, 
  )
# save 
saveRDS(all_obs_dx, 
        paste0(path_to_box, path_to_EHR_data, "cleaned_data/all_obs_dx.rds"))

### NOTE: each of the following datasets are joined into the tte dataset
## cvd ----
cvd_tte <- read_sas(paste0(path_to_box, path_to_EHR_data, "cvd.sas7bdat")) %>% 
  select(-starts_with("MODIFIER"), -starts_with("NORMAL"), 
         -REF_DATE, -TEMP_DATE) %>% 
  right_join(pre_MI_tte_diab_5, by = "SUBJID")

# save 
saveRDS(cvd_tte, paste0(path_to_box, path_to_EHR_data, "cleaned_data/cvd_tte.rds"))

## diab comp ----
diab_comp_tte <- read_sas(paste0(path_to_box, path_to_EHR_data, 
                                 "diab_comp.sas7bdat")) %>% 
  select(-starts_with("MODIFIER"), -starts_with("NORMAL"), 
         -REF_DATE, -TEMP_DATE) %>% 
  right_join(pre_MI_tte_diab_5, by = "SUBJID")

# save 
saveRDS(diab_comp_tte, 
        paste0(path_to_box, path_to_EHR_data, "cleaned_data/diab_comp_tte.rds"))

## diabetes (types) ----
diabetes_tte <- read_sas(paste0(path_to_box, path_to_EHR_data, 
                            "diabetes.sas7bdat")) %>% 
  select(-starts_with("MODIFIER"), -starts_with("NORMAL"), 
         -REF_DATE, -TEMP_DATE) %>% 
  right_join(pre_MI_tte_diab_5, by = "SUBJID") 

# note that 2 subjects are missing diabetes type 
# save 
saveRDS(diabetes_tte, 
        paste0(path_to_box, path_to_EHR_data, "cleaned_data/diabetes_tte.rds"))

## dx ----
dx_tte <- read_sas(paste0(path_to_box, path_to_EHR_data, "dx.sas7bdat")) %>% 
  select(-starts_with("MODIFIER"), -starts_with("NORMAL"), 
         -REF_DATE, -TEMP_DATE) %>% 
  right_join(pre_MI_tte_diab_5, by = "SUBJID")
# save 
saveRDS(dx_tte, paste0(path_to_box, path_to_EHR_data, "cleaned_data/dx_tte.rds"))

