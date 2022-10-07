# HbA1c, eGFR Calculation and CKD Staging
# using serum creatinine measure, dialysis status from renal registry,
# and urine protein tests

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


# HbA1c ----

hba1c_tte <- read_rds(paste0(path_to_box, path_to_lab_data, 
                             "cleaned_data/hba1c_tte.rds"))

# recall: there are subjects with multiple obs on the same, most recent day 
# within the 1-yr pre-survey period
# hba1c_tte %>%
#   filter(pre_surv_1y_flag == 1) %>%
#   group_by(SUBJID) %>%
#   slice_max(LAB_AGE) %>% # n = 13717
#   distinct(SUBJID) %>% # n = 13565
#   nrow()

# checking to make sure that subjects with multiple obs on the same, most recent 
# day in the 1-yr pre-survey period do not have inexact lab results, 
# so that we can take an average

# id_list <- hba1c_tte %>%
#   filter(pre_surv_1y_flag == 1) %>%
#   group_by(SUBJID) %>%
#   slice_max(LAB_AGE) %>% # n = 13717
#   summarise(nobs = n()) %>% 
#   filter(nobs > 1) %>% 
#   pull(SUBJID)
# 
# hba1c_tte %>%
#   filter(pre_surv_1y_flag == 1, 
#          SUBJID %in% id_list) %>%
#   View()

# taking an average of the multiple obs  
pre_MI_tte_diab_5 <- hba1c_tte %>%
  filter(pre_surv_1y_flag == 1) %>%
  group_by(SUBJID) %>%
  slice_max(LAB_AGE) %>%
  summarise(hba1c_1yr_pre_surv = mean(RESULT_num)) %>% 
  right_join(pre_MI_tte_diab_5, by = "SUBJID") %>% 
  mutate(
    hba1c_1yr_pre_surv_cat = case_when(
      is.na(hba1c_1yr_pre_surv) ~ NA_character_, 
      hba1c_1yr_pre_surv < 7 ~ "<7", 
      hba1c_1yr_pre_surv < 8 ~ "7~8", 
      hba1c_1yr_pre_surv < 10 ~ "8~10", 
      TRUE ~ ">=10"
    )
  )


# calculate eGFR ----
cr_s_tte <- readRDS(paste0(path_to_box, path_to_lab_data, 
                           "cleaned_data/crs_s_tte.rds"))

# cr_s_tte %>%
#   # filter to 1yr pre-survey
#   filter(pre_surv_1y_flag == 1) %>%
#   # take the most recent obs for each subject
#   group_by(SUBJID) %>%
#   slice_max(LAB_AGE) %>% # n = 13689
#   distinct(SUBJID) %>% # n = 13320
#   nrow()

pre_MI_tte_diab_5 <- cr_s_tte %>% 
  # filter to 1yr pre-survey
  filter(pre_surv_1y_flag == 1) %>% 
  # take the most recent obs for each subject
  group_by(SUBJID) %>% 
  slice_max(LAB_AGE) %>% 
  mutate(
    # calculate eGFR
    k = ifelse(FEMALE == 1, 0.7, 0.9),
    alpha = ifelse(FEMALE == 1, -0.241, -0.302),
    mult = ifelse(FEMALE == 1, 1.012, 1),
    eGFR = 142 * min(RESULT_num/k, 1)^alpha * max(RESULT_num/k, 1)^(-1.2) *
      0.9938^LAB_AGE * mult
  ) %>%
  group_by(SUBJID) %>%
  summarise(eGFR_1yr_pre_surv = mean(eGFR)) %>%
  right_join(pre_MI_tte_diab_5, by = "SUBJID") %>% 
  mutate(
    eGFR_1yr_pre_surv_cat = case_when(
      is.na(eGFR_1yr_pre_surv) ~ NA_character_, 
      eGFR_1yr_pre_surv < 15 ~ "<15",   # stage 5
      eGFR_1yr_pre_surv < 30 ~ "15~30", # stage 4
      eGFR_1yr_pre_surv < 60 ~ "30~60", # stage 3
      eGFR_1yr_pre_surv < 90 ~ "60~90", # stage 2
      TRUE ~ ">=90"                     # stage 0 and 1
    )
  )

# some thresholds for inexact results
# to verify that these creatinine values will belong to the extreme categories
# in CKD staging
tibble(
  FEMALE = rep(c(1, 0), each = 4),  
  LAB_AGE = rep(c(60, 60, 90, 90), 2), 
  RESULT_num = rep(c(0.14, 5.83), 4), 
  k = ifelse(FEMALE == 1, 0.7, 0.9),
  alpha = ifelse(FEMALE == 1, -0.241, -0.302),
  mult = ifelse(FEMALE == 1, 1.012, 1),
  eGFR = 142 * pmin(RESULT_num/k, 1)^alpha * pmax(RESULT_num/k, 1)^(-1.2) *
    0.9938^LAB_AGE * mult
)

# summary for the lab data ----

t1_relabel <- function(df) {
  tb1_df <- df %>% 
    mutate(
      FEMALE = factor(FEMALE, levels = c(1, 0), labels = c("Yes", "No")),
      
      ASIAN = factor(ASIAN, levels = c(0, 1), labels = c("White", "Asian")), 
      
      ETHNICITY_REV = factor(
        ETHNICITY_REV, levels = c(9, 1, 2, 3, 5),
        labels = c("White", "South Asian", "Chinese", "Japanese", "Filipino")
      ),
      
      eGFR_1yr_pre_surv_cat = factor(
        eGFR_1yr_pre_surv_cat, 
        levels = c(">=90", "60~90", "30~60", "15~30", "<15"), 
        labels = c("Stages 0 and 1 (>=90)", 
                   "Stage 2 (60~90)", 
                   "Stage 3 (30~60)", 
                   "Stage 4 (15~30)", 
                   "Stage 5 (<15)")
      ), 
      
      hba1c_1yr_pre_surv_cat = factor(
        hba1c_1yr_pre_surv_cat, 
        levels = c("<7", "7~8", "8~10", ">=10")
      )
      
    )
  
  var_label(tb1_df) <- list(
    SURVEY_AGE = "Survey age [Mean (SD)]", 
    FEMALE = "Female", 
    eGFR_1yr_pre_surv_cat = "CKD stage (eGFR)",
    hba1c_1yr_pre_surv_cat = "HbA1c"
  )
  
  return(tb1_df)
}

lab_tbl1 <- pre_MI_tte_diab_5 %>% 
  t1_relabel() %>% 
  table1(~ 
           # eGFR_1yr_pre_surv + hba1c_1yr_pre_surv +
           eGFR_1yr_pre_surv_cat + hba1c_1yr_pre_surv_cat
         | ETHNICITY_REV, 
         overall = NULL, data = .)

write.xlsx(
  lab_tbl1, rowNames = TRUE,
  file = paste0(path_to_box,
                "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/Code/",
                "Cleaned_Scripts/diabetes_severity_descriptive_summary/",
                "lab_tbl1.xlsx"))


