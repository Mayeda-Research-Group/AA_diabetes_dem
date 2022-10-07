# EHR Descriptive Analysis
# generate never/ever flags by baseline for diabetes complications
# and pull corresponding SUBJID's

# Note: one logic used throughout this script for generating prevalent flags
# is that Dx date is missing for subjects with Dx flag of 0

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
path_to_EHR_data <- "Asian_Americans_dementia_data/diabetes_adrd/Additional_EHR_data/"
path_to_analytical_data <- "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/"


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

# generate never/ever flags ----
## MI: acute and prior ----
# two data sources: all_obs_dx and cvd_tte

all_obs_dx <- read_rds(paste0(path_to_box, path_to_EHR_data, 
                              "cleaned_data/all_obs_dx.rds")) 
all_obs_dx %>% distinct(SUBJID) %>% nrow()
# not all subjects have a record for acute MI or hypoglycemic events

PREV_AMI_SUBJID <- all_obs_dx %>% 
  # join in tte data, mainly for SURVEY_AGE
  left_join(pre_MI_tte_diab_5, by = "SUBJID") %>% 
  # subset to obs before baseline/survey
  filter(DX_AGE <= SURVEY_AGE) %>% 
  # subjects with at least one AMI flag of 1 have had AMI by baseline
  group_by(SUBJID) %>%
  summarise(AMI_sum = sum(AMI)) %>% 
  filter(AMI_sum > 0) %>% 
  # pull the SUBJID for these subjects
  pull(SUBJID)
# length(PREV_AMI_SUBJID)

cvd_tte <- read_rds(paste0(path_to_box, path_to_EHR_data, 
                           "cleaned_data/cvd_tte.rds")) 

PREV_PMI_SUBJID <- cvd_tte %>% 
  # subset to prior MI before baseline/survey
  filter(FIRST_P_MI_DX_AGE <= SURVEY_AGE) %>% 
  pull(SUBJID)
# length(PREV_PMI_SUBJID)

PREV_MI_SUBJID <- c(PREV_AMI_SUBJID, PREV_PMI_SUBJID) %>% unique()
# length(PREV_MI_SUBJID)

# add flag for MI in tte dataset
pre_MI_tte_diab_5 <- pre_MI_tte_diab_5 %>% 
  mutate(PREV_MI_FLAG = ifelse(SUBJID %in% PREV_MI_SUBJID, 1, 0))
# length(MI_ever_pre_surv_SUBJID)

## peripheral vascular disease ----
PREV_PVD_SUBJID <- cvd_tte %>% 
  # subset to prior MI before baseline/survey
  filter(FIRST_PVD_DX_AGE <= SURVEY_AGE) %>%
  pull(SUBJID)

# add flag for PVD in tte dataset
pre_MI_tte_diab_5 <- pre_MI_tte_diab_5 %>% 
  mutate(PREV_PVD_FLAG = ifelse(SUBJID %in% PREV_PVD_SUBJID, 1, 0))

## gangrene or lower limb ulcer ----
diab_comp_tte <- read_rds(paste0(path_to_box, path_to_EHR_data, 
                                 "cleaned_data/diab_comp_tte.rds")) 

PREV_GANGLLC_SUBJID <- diab_comp_tte %>%
  filter(FIRST_GANGLLC_DX_AGE <= SURVEY_AGE) %>%
  pull(SUBJID)
pre_MI_tte_diab_5 <- pre_MI_tte_diab_5 %>% 
  mutate(PREV_GANGLLC_FLAG = ifelse(SUBJID %in% PREV_GANGLLC_SUBJID, 1, 0))

## congestive heart failure ----
PREV_CHF_SUBJID <- cvd_tte %>% 
  # subset to CHF before baseline/survey
  filter(FIRST_CHF_DX_AGE <= SURVEY_AGE) %>% 
  pull(SUBJID)

# add flag for CHF in tte dataset
pre_MI_tte_diab_5 <- pre_MI_tte_diab_5 %>% 
  mutate(PREV_CHF_FLAG = ifelse(SUBJID %in% PREV_CHF_SUBJID, 1, 0))

## hypoglycemic event ----
PREV_HYPOGLY_SUBJID <- all_obs_dx %>% 
  # join in tte data, mainly for SURVEY_AGE
  left_join(pre_MI_tte_diab_5, by = "SUBJID") %>% 
  # subset to obs before baseline/survey
  filter(DX_AGE <= SURVEY_AGE) %>% 
  # subjects with at least one AMI flag of 1 have had AMI by baseline
  group_by(SUBJID) %>%
  summarise(HYPOGLY_sum = sum(HYPOGLY)) %>% 
  filter(HYPOGLY_sum > 0) %>% 
  # pull the SUBJID for these subjects
  pull(SUBJID)

# add flag for hypoglycemic events in tte dataset
pre_MI_tte_diab_5 <- pre_MI_tte_diab_5 %>% 
  mutate(PREV_HYPOGLY_FLAG = ifelse(SUBJID %in% PREV_HYPOGLY_SUBJID, 1, 0))

## diabetic hyperosmolarity and secondary diabetes mellitus with hyperosmolarity ----
PREV_DIABHM_SUBJID <- diab_comp_tte %>% 
  filter(FIRST_DIABHM_DX_AGE <= SURVEY_AGE) %>% 
  pull(SUBJID)

# add flag for diabetic hyperosmolarity in tte dataset
pre_MI_tte_diab_5 <- pre_MI_tte_diab_5 %>% 
  mutate(PREV_DIABHM_FLAG = ifelse(SUBJID %in% PREV_DIABHM_SUBJID, 1, 0))

## (NOT doing this) diabetic ketoacidosis  ----
PREV_DIABKETO_SUBJID <- diab_comp_tte %>% 
  filter(FIRST_DIABKETO_DX_AGE <= SURVEY_AGE) %>% 
  pull(SUBJID)
  
diab_comp_tte %>% 
  filter(DIABKETO_DX_FLAG == 1) %>% 
  select(FIRST_DIABKETO_DX_DATE, FIRST_DIABKETO_DX_AGE, SURVEY_AGE) %>% 
  View()
# it seems that all diabetic ketoacidosis events happened after survey
# there are only 132 subjects in our diabetic sample with this flag
# Google: this complication usually happens to type 1 diabetes patients 
# (thus accounting for our small number?)

# add flag for diabetic hyperosmolarity in tte dataset
pre_MI_tte_diab_5 <- pre_MI_tte_diab_5 %>% 
  mutate(PREV_DIABHM_FLAG = ifelse(SUBJID %in% PREV_DIABHM_SUBJID, 1, 0))

## diabetic neurological complications ----
PREV_DIABNEURO_SUBJID <- diab_comp_tte %>% 
  filter(FIRST_DIABNEURO_DX_AGE <= SURVEY_AGE) %>% 
  pull(SUBJID)
pre_MI_tte_diab_5 <- pre_MI_tte_diab_5 %>% 
  mutate(PREV_DIABNEURO_FLAG = ifelse(SUBJID %in% PREV_DIABNEURO_SUBJID, 1, 0))

## diabetic retinopathy: collapase 250.5 and 362 ----
# two data sources: diab_comp_tte (250.5) and dx_tte (362)
PREV_DIABRETINO250_SUBJID <- diab_comp_tte %>% 
  filter(FIRST_DIABRETINO_DX_DATE <= SURVEY_AGE) %>%
  pull(SUBJID)
# length(PREV_DIABRETINO250_SUBJID)

dx_tte <- read_rds(paste0(path_to_box, path_to_EHR_data, 
                          "cleaned_data/dx_tte.rds")) 

PREV_DIABRETINO362_SUBJID <- dx_tte %>% 
  filter(FIRST_DR362_DX_AGE <= SURVEY_AGE) %>% 
  pull(SUBJID)
# length(PREV_DIABRETINO362_SUBJID)

PREV_DIABRETINO_SUBJID <- c(PREV_DIABRETINO250_SUBJID, 
                            PREV_DIABRETINO362_SUBJID) %>% unique()
# length(PREV_DIABRETINO_SUBJID)

# add flag for diabetic retinopathy in tte dataset
pre_MI_tte_diab_5 <- pre_MI_tte_diab_5 %>% 
  mutate(PREV_DIABRETINO_FLAG = ifelse(SUBJID %in% PREV_DIABRETINO_SUBJID, 1, 0))

## diabetic circulatory complications ----
PREV_DIABCC_SUBJID <- diab_comp_tte %>% 
  filter(FIRST_DIABCC_DX_AGE <= SURVEY_AGE) %>% 
  pull(SUBJID)
pre_MI_tte_diab_5 <- pre_MI_tte_diab_5 %>% 
  mutate(PREV_DIABCC_FLAG = ifelse(SUBJID %in% PREV_DIABCC_SUBJID, 1, 0))

## dialysis status ----
# binary flag for never/ever before baseline/survey
esrd <- read_rds(paste0(path_to_box, path_to_EHR_data,
                        "cleaned_data/esrd.rds"))
PREV_DIAL_SUBJID <- esrd %>% 
  left_join(pre_MI_tte_diab_5, by = "SUBJID") %>%
  # subset to dialysis that started before survey
  filter(TRMT_START_AGE <= SURVEY_AGE) %>% 
  distinct(SUBJID) %>% 
  pull(SUBJID)
pre_MI_tte_diab_5 <- pre_MI_tte_diab_5 %>% 
  mutate(PREV_DIAL_FLAG = ifelse(SUBJID %in% PREV_DIAL_SUBJID, 1, 0))


# diabetes types and duration ----
diabetes_tte <- read_rds(paste0(path_to_box, path_to_EHR_data, 
                                "cleaned_data/diabetes_tte.rds")) 

# check: 2 subjects have missing info

pre_MI_tte_diab_5 <- read_sas(
  paste0(path_to_box,
         "Asian_Americans_dementia_data/analysis_data_tables/",
         "aa_adrd_cardiometabolic_tte.sas7bdat")) %>% 
  select(SUBJID, DIAB_REG_AGE) %>% 
  right_join(diabetes_tte, by = "SUBJID") %>% 
  mutate(
    # calculate diabetes duration
    DIAB_DURATION = SURVEY_AGE - DIAB_REG_AGE
    ) %>% 
  select(SUBJID, DIAB_DURATION, starts_with("DMTYPE"),
         NOTIFICATION_AGE, DIAB_REG_AGE) %>%  
  right_join(pre_MI_tte_diab_5, by = "SUBJID")

# checking notification age vs diabetes registry age from the full tte data
# pre_MI_tte_diab_5 %>% 
#   select(SUBJID, DIAB_REG_AGE, NOTIFICATION_AGE) %>% 
#   mutate(match = DIAB_REG_AGE == NOTIFICATION_AGE) %>% 
#   View()

# # distribution of diabetes duration by ethnicity
# pre_MI_tte_diab_5 %>% 
#   mutate(ETHNICITY_REV = factor(ETHNICITY_REV, 
#                                 levels = c(9, 1, 2, 3, 5),
#                                 labels = c("White", 
#                                            "South Asian", "Chinese", "Japanese", "Filipino")
#   )) %>% 
#   filter(ETHNICITY_REV != "White") %>% 
#   ggplot(aes(x = DIAB_DURATION, group = ETHNICITY_REV)) +
#   geom_histogram(aes(fill = ETHNICITY_REV), alpha = 0.5) + 
#   facet_wrap(~ETHNICITY_REV)
# 
# pre_MI_tte_diab_5 %>% 
#   mutate(ETHNICITY_REV = factor(ETHNICITY_REV, 
#                                 levels = c(9, 1, 2, 3, 5),
#                                 labels = c("White", 
#                                            "South Asian", "Chinese", "Japanese", "Filipino")
#   )) %>% 
#   filter(ETHNICITY_REV == "White") %>% 
#   ggplot(aes(x = DIAB_DURATION, group = ETHNICITY_REV)) +
#   geom_histogram(aes(fill = ETHNICITY_REV), alpha = 0.5) + 
#   facet_wrap(~ETHNICITY_REV)

# pre_MI_tte_diab_5 %>% 
#   mutate(ETHNICITY_REV = factor(ETHNICITY_REV, 
#                                 levels = c(9, 1, 2, 3, 5),
#                                 labels = c("White", 
#                                            "South Asian", "Chinese", "Japanese", "Filipino")
#   )) %>% 
#   group_by(ETHNICITY_REV) %>% 
#   summarise(median = median(DIAB_DURATION),
#             max = max(DIAB_DURATION), 
#             min = min(DIAB_DURATION))



# summary for tte data with flags for all diabetic complications ----

t1_relabel <- function(df) {
  tb1_df <- df %>% 
    mutate(
      FEMALE = factor(FEMALE, levels = c(1, 0), labels = c("Yes", "No")),
      
      ASIAN = factor(ASIAN, levels = c(0, 1), labels = c("White", "Asian")), 
      
      ETHNICITY_REV = factor(ETHNICITY_REV, 
                             levels = c(9, 1, 2, 3, 5),
                             labels = c("White", 
                                        "South Asian", "Chinese", "Japanese", "Filipino")
      ),
      
      # label diabetes types
      DMTYPE_0 = factor(DMTYPE_0, levels = c(1, 2, 3),
                        labels = c("Type 1", "Type 2", "Indeterminate")),
      DMTYPE_ES = factor(DMTYPE_ES, levels = c(1, 2, 3),
                         labels = c("Type 1", "Type 2", "Indeterminate")),
      DMTYPE_S = factor(DMTYPE_S, levels = c(1, 2, 3),
                        labels = c("Type 1", "Type 2", "Indeterminate")),
  
      # label flags for complications    
      PREV_MI_FLAG = factor(PREV_MI_FLAG, levels = c(1, 0), 
                            labels = c("Yes", "No")), 
      PREV_PVD_FLAG = factor(PREV_PVD_FLAG, levels = c(1, 0), 
                             labels = c("Yes", "No")), 
      PREV_GANGLLC_FLAG = factor(PREV_GANGLLC_FLAG, levels = c(1, 0), 
                                 labels = c("Yes", "No")), 
      PREV_CHF_FLAG = factor(PREV_CHF_FLAG, levels = c(1, 0), 
                             labels = c("Yes", "No")), 
      PREV_HYPOGLY_FLAG = factor(PREV_HYPOGLY_FLAG, levels = c(1, 0), 
                                 labels = c("Yes", "No")), 
      PREV_DIABHM_FLAG = factor(PREV_DIABHM_FLAG, levels = c(1, 0), 
                                labels = c("Yes", "No")), 
      PREV_DIABNEURO_FLAG = factor(PREV_DIABNEURO_FLAG, levels = c(1, 0), 
                                   labels = c("Yes", "No")), 
      PREV_DIABRETINO_FLAG = factor(PREV_DIABRETINO_FLAG, levels = c(1, 0), 
                                    labels = c("Yes", "No")), 
      PREV_DIABCC_FLAG = factor(PREV_DIABCC_FLAG, levels = c(1, 0), 
                                labels = c("Yes", "No")),
      PREV_DIAL_FLAG = factor(PREV_DIAL_FLAG, levels = c(1, 0), 
                              labels = c("Yes", "No"))
    )
  
  var_label(tb1_df) <- list(
    SURVEY_AGE = "Survey age [Mean (SD)]", 
    FEMALE = "Female", 
    DIAB_DURATION = "Diabetes duration [Mean (SD)]",
    DMTYPE_0 = "Diabetes type: modified Klompas & Schroeder alogorithm",
    DMTYPE_ES = "Diabetes type: enhanced simple algorithm",
    DMTYPE_S = "Diabetes type: simple algorithm",
    PREV_MI_FLAG = "History of MI", 
    PREV_PVD_FLAG = "History of peripheral vascular disease", 
    PREV_GANGLLC_FLAG = "History of gangrene or lower limb ulcer", 
    PREV_CHF_FLAG = "History of congestive heart failure", 
    PREV_HYPOGLY_FLAG = "History of hypoglycemic event", 
    PREV_DIABHM_FLAG = "History of diabetic hyperosmolarity", 
    PREV_DIABNEURO_FLAG = "History of diabetic neurological complications", 
    PREV_DIABRETINO_FLAG = "History of diabetic retinopathy", 
    PREV_DIABCC_FLAG = "History of diabetic circulatory complications",
    PREV_DIAL_FLAG = "History of dialysis"
  )
  
  return(tb1_df)
}


pre_MI_tte_diab_5 <- t1_relabel(pre_MI_tte_diab_5)

EHR_tbl1 <- table1(
  ~ SURVEY_AGE + FEMALE + 
    DIAB_DURATION + 
    DMTYPE_0 + DMTYPE_ES + DMTYPE_S + 
    PREV_MI_FLAG + PREV_PVD_FLAG + PREV_GANGLLC_FLAG + PREV_CHF_FLAG + 
    PREV_HYPOGLY_FLAG + PREV_DIABHM_FLAG + PREV_DIABNEURO_FLAG + 
    PREV_DIABRETINO_FLAG + PREV_DIABCC_FLAG + 
    PREV_DIAL_FLAG
  | ETHNICITY_REV,
  overall = NULL, 
  data = pre_MI_tte_diab_5)

write.xlsx(
  EHR_tbl1, rowNames = TRUE,
  file = paste0(path_to_box,
                "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/Code/",
                "Cleaned_Scripts/diabetes_severity_descriptive_summary/",
                "EHR_tbl1.xlsx"))



