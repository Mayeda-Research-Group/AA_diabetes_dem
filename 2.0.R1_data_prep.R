# This script contains additional data preparation for R1.
# Type 1 DM is removed from the definition of exposure, 
# and a flag is generated for subjects who developed DM (types 1 and 2)
# 0-5/7/9 yrs pre-survey. 

# This script is sourced at the beginning of 2_table1, 3.x.R1 and 4.x.R1 analysis scripts. 

# set up additional diabetes information ----
# added in R1
## load DM type from diabetes registry data
path_to_EHR_data <- "Asian_Americans_dementia_data/diabetes_adrd/Additional_EHR_data/"
diabetes_tte <- read_sas(paste0(path_to_box, path_to_EHR_data, 
                                "diabetes.sas7bdat"))

## read in original data 
# that contains the variable PRESURV_DIAB_REG_YR, which is the difference 
# in years between survey age and age at diabetes registry entry
original_data <- read_sas(
  paste0(path_to_box,
         "Asian_Americans_dementia_data/analysis_data_tables/",
         "aa_adrd_cardiometabolic_tte.sas7bdat")) # n = 184929

# generate flags for subjects who developed Type 1 DM 5/7/9+ years pre-survey
# DMTYPE_0: Diabetes type defined by modified Klompas & Schroeder alogorithm
diab_info <- left_join(
  original_data %>% select(SUBJID, PRESURV_DIAB_REG_YR),
  diabetes_tte %>% select(SUBJID, DMTYPE_0),
  by = "SUBJID"
) %>% 
  mutate(
    DIAB_DX5YR_T1 = ifelse(PRESURV_DIAB_REG_YR >= 5 & DMTYPE_0 == 1, 1, 0),
    DIAB_DX7YR_T1 = ifelse(PRESURV_DIAB_REG_YR >= 7 & DMTYPE_0 == 1, 1, 0),
    DIAB_DX9YR_T1 = ifelse(PRESURV_DIAB_REG_YR >= 9 & DMTYPE_0 == 1, 1, 0)
  ) %>%
  mutate(across(
    starts_with("DIAB_DX"), function(x) ifelse(is.na(PRESURV_DIAB_REG_YR), 0, x)
  ))

# check counts
diab_info %>% group_by(DIAB_DX5YR_T1) %>% tally()
# FLAG = 1: exclude in analysis
# there are 2 subjects who developed diabetes but their DM type is NA
# they are still kept in the analysis, along with those whose DM type is 
# indeterminate

# apply inclusion/exclusion criteria to 5/7/9+ yr dataset ----
if (exists("imputed_tte_data")) {
  
  imputed_tte_data_5 <- lapply(
    imputed_tte_data, 
    function (x) {
      x %>% left_join(diab_info, by = "SUBJID") %>% 
        filter(
          PRESURV5YR_SAMPLE == 1, # add membership criteria (5+ yrs pre-survey)
          DIAB_DX5YR_T1 != 1 # exclude T1DM 5+ yrs 
        ) %>% 
        mutate( # define 3-level exposure
          DIAB_DX5YR_FLAG_3 = case_when(
            PRESURV_DIAB_REG_YR >= 5 ~ "5+",
            PRESURV_DIAB_REG_YR >= 0 ~ "0-5",
            TRUE ~ "no"
          ) %>% factor(levels = c("no", "0-5", "5+"))
        )
    }
  )
  
  imputed_tte_data_7 <- lapply(
    imputed_tte_data, 
    function (x) {
      x %>% left_join(diab_info, by = "SUBJID") %>% 
        filter(
          PRESURV7YR_SAMPLE == 1, # add membership criteria (7+ yrs pre-survey)
          DIAB_DX7YR_T1 != 1 # exclude T1DM 7+ yrs 
        ) %>% 
        mutate( # define 3-level exposure
          DIAB_DX7YR_FLAG_3 = case_when(
            PRESURV_DIAB_REG_YR >= 7 ~ "7+",
            PRESURV_DIAB_REG_YR >= 0 ~ "0-7",
            TRUE ~ "no"
          ) %>% factor(levels = c("no", "0-7", "7+"))
        )
    }
  )
  
  imputed_tte_data_9 <- lapply(
    imputed_tte_data, 
    function (x) {
      x %>% left_join(diab_info, by = "SUBJID") %>% 
        filter(
          PRESURV9YR_SAMPLE == 1, # add membership criteria (9+ yrs pre-survey)
          DIAB_DX9YR_T1 != 1 # exclude T1DM 9+ yrs 
        ) %>% 
        mutate(# define 3-level exposure
          DIAB_DX9YR_FLAG_3 = case_when(
            PRESURV_DIAB_REG_YR >= 9 ~ "9+",
            PRESURV_DIAB_REG_YR >= 0 ~ "0-9",
            TRUE ~ "no"
          ) %>% factor(levels = c("no", "0-9", "9+"))
        )
    }
  )
  
  # note that PRESURVXYR_SAMPLE should be the same across each imputation.
  # so after subsetting on membership criteria, we should have the same people
  # in each imputed dataset; we check this below
  dim(imputed_tte_data_5[[10]])
  sum(imputed_tte_data_5[[1]]$SUBJID == imputed_tte_data_5[[10]]$SUBJID)
  
  dim(imputed_tte_data_7[[10]])
  sum(imputed_tte_data_7[[1]]$SUBJID == imputed_tte_data_7[[10]]$SUBJID)
  
  dim(imputed_tte_data_9[[10]])
  sum(imputed_tte_data_9[[1]]$SUBJID == imputed_tte_data_9[[10]]$SUBJID)
  
} 