# Rx Data Construction
# read in reformatted and unscrambled data generated using SAS
# and count number of distinct generic drugs during 
# the 1-year pre-survey period ("the defined period")

# set up packages ----
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse" 
       # "magrittr", "foreign", "flextable", "huxtable", "survival", 
       # "survey", "tableone",  "miceadds", "mgcv", "mice", "openxlsx", "mitools"
       # "ggpubr",  "lmtest", "table1", "labelled", "gtsummary", "gt"
)

options(scipen = 999, digits = 8)

# set up paths and datasets ----
path_to_box <- "/Users/julietzhou/Library/CloudStorage/Box-Box/"
path_to_analytical_data <- "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/"


# read in Rx data 
rx_data <- read_sas(
  paste0(path_to_box, 
         "Asian_Americans_dementia_data/diabetes_adrd/Rx_data/", 
         "rx_data.sas7bdat"))

# read in analytical dataset 
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

# Rx data prep ----
rx_tte <- rx_data %>% 
  select(SUBJID, UCLA_AHFS, GENERIC_NAME, START_AGE, END_AGE) %>% 
  # keep only diabetes treatments, n = 461802
  filter(str_starts(UCLA_AHFS, "68")) %>%
  # merge into tte data, n = 597255
  right_join(., pre_MI_tte_diab_5, by = "SUBJID") %>% 
  mutate(
    # add insulin flag
    INSULIN_FLAG = ifelse(
      UCLA_AHFS %in% c(
        "68:20.08.12 Intermediate-acting Insulins",
        "68:20.08.04 Rapid-acting Insulins",
        "68:20.08.08 Short-acting Insulins",
        "68:20.08.16 Long-acting Insulins"
      ), 1, 0),
    
    # add 1yr pre-survey flag
    # logic here is that Rx events that end before 1yr pre-survey
    # or start after survey should be excluded
    rx_1yr_pre_surv = ifelse(
      START_AGE > SURVEY_AGE | END_AGE < SURVEY_AGE - 1, 0, 1
    )
  ) 

# there are subjects with diabetes who do not have any Rx events here 

# insulin summary ----
INSULIN_SUBJID <- rx_tte %>% 
  filter(rx_1yr_pre_surv == 1) %>% 
  group_by(SUBJID) %>% 
  summarise(sum_insulin = sum(INSULIN_FLAG)) %>% 
  filter(sum_insulin > 0) %>% 
  pull(SUBJID)
# these subjects have at least 1 insulin Rx event
  
pre_MI_tte_diab_5 <- pre_MI_tte_diab_5 %>% 
  mutate(
    INSULIN_1yr_PRE_SURV_FLAG = ifelse(
      SUBJID %in% INSULIN_SUBJID, 1, 0
    )
  )

# table(pre_MI_tte_diab_5$INSULIN_1yr_PRE_SURV_FLAG, 
#       pre_MI_tte_diab_5$ETHNICITY_REV)

# generic drug summary ----
# we are counting the number of distinct generic drugs other than insulins
# during the 1yr pre-survey period

# function for counting 
# for each drug in a given list of generic drug names, 
# it records the list of distinct SUBJID with this Rx event during 1yr pre-survey
# then concatenates the lists associated with each drug
# the number of occurrences of each SUBJID is the count of interest
rx_gen_count <- function(drug_names) {
  id_list <- c()
  for (one_drug in drug_names) {
    id_append <- rx_tte %>% 
      filter(rx_1yr_pre_surv == 1) %>% 
      mutate(
        GENERIC_FLAG = ifelse(GENERIC_NAME == one_drug, 1, 0) 
      ) %>% 
      group_by(SUBJID) %>% 
      summarise(sum = sum(GENERIC_FLAG)) %>% 
      filter(sum > 0) %>% 
      pull(SUBJID)
    
    id_list <- c(id_list, id_append)
  }
  gen_count <- table(id_list) %>% as_tibble() %>% 
    rename("SUBJID" = "id_list")
  return(gen_count)
}

# produce the list of drug names of interest
all_generic_drugs <- rx_tte %>% 
  # remove all insulins
  filter(str_detect(GENERIC_NAME, "Insulin", negate = TRUE)) %>% 
  distinct(GENERIC_NAME) %>% 
  pull(GENERIC_NAME)

GEN_DRUG_COUNT <- rx_gen_count(all_generic_drugs) %>% 
  mutate(SUBJID = as.numeric(SUBJID)) %>% 
  right_join(pre_MI_tte_diab_5, by = "SUBJID") %>% 
  # subjects with missing (NA) for generic drug count are those 
  # that did not have a diabetes-related Rx event and/or 
  # their only diabetes-related Rx events are insulins
  # these NA's converted to zeros here
  mutate(
    n = ifelse(is.na(n), 0, n),
    n_cat = as.character(n)
  ) 

# final summary table ---- 

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
      
      INSULIN_1yr_PRE_SURV_FLAG = factor(INSULIN_1yr_PRE_SURV_FLAG, 
                                         levels = c(1, 0), 
                                         labels = c("Yes", "No")
      )
    )
  
  var_label(tb1_df) <- list(
    SURVEY_AGE = "Survey age [Mean (SD)]", 
    FEMALE = "Female", 
    INSULIN_1yr_PRE_SURV_FLAG = "Insulin use during 1 yr pre-survey",
    n = "Number of distinct, non-insulin diabetes drugs used during 1 yr pre-survey", 
    n_cat = "Number of distinct, non-insulin diabetes drugs used during 1 yr pre-survey"
  )
  
  return(tb1_df)
}

pre_MI_tte_diab_5 <- t1_relabel(pre_MI_tte_diab_5)

Rx_tbl1 <- table1(
  ~ INSULIN_1yr_PRE_SURV_FLAG + n + n_cat | ETHNICITY_REV, 
  data = pre_MI_tte_diab_5, overall = NULL)

write.xlsx(
  Rx_tbl1, rowNames = TRUE,
  file = paste0(path_to_box,
                "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/Code/",
                "Cleaned_Scripts/diabetes_severity_descriptive_summary/",
                "Rx_tbl1.xlsx"))





