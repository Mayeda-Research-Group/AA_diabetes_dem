# # sensitivity analysis - Part 4
# added based on R1 revision
# main cox/aalen models on imputed dataset using 5+1yr criteria
# drop DM cases (Types 1 and 2) that developed 0-5 years pre-survey

# set up packages ----
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("tidyverse", "survival", "mice", "openxlsx", "mitools", "haven"
       # "magrittr", "foreign", "flextable", "huxtable", 
       # "survey", "tableone",  "miceadds", "mgcv",
       # "ggpubr",  "lmtest", "table1", "labelled", "gtsummary", "gt"
)

options(scipen = 999, digits = 8)

# set up paths and datasets ----

path_to_box <- "/Users/julietzhou/Library/CloudStorage/Box-Box/"
path_to_imputed <- "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/"
path_to_proj <- "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/Code/Cleaned_Scripts/"

imputed_tte_data <- readRDS(file = paste0(path_to_box, path_to_imputed, 
                                          "aa_adrd_diabetes_imputed_tte_data.rds"))

# prepare for analysis ----
# run the script that preps the data according to R1 revision
source(paste0(path_to_box, path_to_proj, "2.0.R1_data_prep.R"))

# additional step here to drop T1/2 DM cases in 0-5yrs pre-survey
imputed_tte_data_5 <- lapply(
  imputed_tte_data_5, 
  function(x) x %>% filter(DIAB_DX5YR_FLAG_3 != "0-5")
)

# load a function that fit Cox PH model on a given imputed dataset
# using given model formula and output model summary and combined HRs
source(file = paste0(path_to_box, 
                     "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/", 
                     "Code/Cleaned_Scripts/functions_analysis.R"))

# fit Cox PH models ----
## set up model formulas ----

surv_obj <- "Surv(SURVEY_AGE, MAIN_DEM_V1_END_AGE, MAIN_DEM_V1_END_DEM_FLAG) ~ "
race_ethn <- c("ASIAN", "ETHNICITY_REV")
diab <- "DIAB_DX5YR_FLAG"
covar <- c("FEMALE", "EDU_GE_COLLEGE", "USABORN_REV", "EHR_HT_MEDIAN")
interaction <- paste0(diab, "*", race_ethn)

model_formulas <- c(
  paste0(surv_obj, interaction[1], "+", covar[1]), 
  paste0(surv_obj, interaction[1], "+", paste(covar[1:2], collapse = "+")), 
  paste0(surv_obj, interaction[1], "+", paste(covar, collapse = "+")), 
  paste0(surv_obj, interaction[2], "+", covar[1]), 
  paste0(surv_obj, interaction[2], "+", paste(covar[1:2], collapse = "+")), 
  paste0(surv_obj, interaction[2], "+", paste(covar, collapse = "+")) 
)

model_formulas

## fit ASIAN aggregated models ----
cox_5_M1 <- cox_imp(imputed_tte_data_5, model_formulas[1], 
                    terms = list(c("DIAB_DX5YR_FLAG", "DIAB_DX5YR_FLAG:ASIAN")))
cox_5_M2 <- cox_imp(imputed_tte_data_5, model_formulas[2], 
                    terms = list(c("DIAB_DX5YR_FLAG", "DIAB_DX5YR_FLAG:ASIAN")))
cox_5_M3 <- cox_imp(imputed_tte_data_5, model_formulas[3], 
                    terms = list(c("DIAB_DX5YR_FLAG", "DIAB_DX5YR_FLAG:ASIAN")))
# take a look at some output
cox_5_M3$linear_summary
cox_5_M3$hr_summary
cox_5_M3$comb_hr

## fit subgroup models ----

# first specify the terms to combine HRs on
ethn_subgrps <- c("South Asian", "Chinese", "Japanese", "Filipino")
main_eff <- "DIAB_DX5YR_FLAG"
interactions <- paste0(main_eff, ":ETHNICITY_REV", ethn_subgrps)
terms <- lapply(interactions, function(x) c(main_eff, x))

cox_5_M4 <- cox_imp(imputed_tte_data_5, model_formulas[4], terms)
cox_5_M5 <- cox_imp(imputed_tte_data_5, model_formulas[5], terms)
cox_5_M6 <- cox_imp(imputed_tte_data_5, model_formulas[6], terms)
# take a look at some output
cox_5_M4$linear_summary
cox_5_M4$hr_summary
cox_5_M4$comb_hr

## save output ----
for (i in 1:6) {
  write.xlsx(
    get(paste0("cox_5_M", i)), 
    file = paste0(path_to_box, path_to_proj, "output/sensitivity_R1/",
                  paste0("drop_dm_cox_5yr_M", i, ".xlsx")))
}

# fit Aalen models ----
## set up model formulas ----

surv_obj <- "Surv(SURVEY_AGE, MAIN_DEM_V1_END_AGE, MAIN_DEM_V1_END_DEM_FLAG) ~ "
race_ethn <- c("ASIAN", "ETHNICITY_REV") %>% str_c("const(", ., ")")
diab <- str_c("const(", "DIAB_DX5YR_FLAG", ")") 
covar <- c("FEMALE", "EDU_GE_COLLEGE", "USABORN_REV", "EHR_HT_MEDIAN") %>% 
  str_c("const(", ., ")")
interaction <- paste0(diab, "*", race_ethn)

model_formulas <- c(
  paste0(surv_obj, interaction[1], "+", covar[1]), 
  paste0(surv_obj, interaction[1], "+", paste(covar[1:2], collapse = "+")), 
  paste0(surv_obj, interaction[1], "+", paste(covar, collapse = "+")), 
  paste0(surv_obj, interaction[2], "+", covar[1]), 
  paste0(surv_obj, interaction[2], "+", paste(covar[1:2], collapse = "+")), 
  paste0(surv_obj, interaction[2], "+", paste(covar, collapse = "+")) 
)

model_formulas

## fit ASIAN aggregated models ----
aalen_5_M1 <- aalen_imp(
  imputed_tte_data_5, model_formulas[1], py = 1000, 
  terms = list(c("const(DIAB_DX5YR_FLAG)", "const(DIAB_DX5YR_FLAG):const(ASIAN)")))
aalen_5_M2 <- aalen_imp(
  imputed_tte_data_5, model_formulas[2], py = 1000, 
  terms = list(c("const(DIAB_DX5YR_FLAG)", "const(DIAB_DX5YR_FLAG):const(ASIAN)")))
aalen_5_M3 <- aalen_imp(
  imputed_tte_data_5, model_formulas[3], py = 1000, 
  terms = list(c("const(DIAB_DX5YR_FLAG)", "const(DIAB_DX5YR_FLAG):const(ASIAN)")))
# take a look at some output
aalen_5_M1$aa_summary
aalen_5_M1$comb_ah

## fit subgroup models ----

# first specify the terms to combine HRs on
ethn_subgrps <- c("South Asian", "Chinese", "Japanese", "Filipino")
main_eff <- "const(DIAB_DX5YR_FLAG)"
interactions <- paste0(main_eff, ":const(ETHNICITY_REV)", ethn_subgrps)
terms <- lapply(interactions, function(x) c(main_eff, x))

aalen_5_M4 <- aalen_imp(
  imputed_tte_data_5, model_formulas[4], py = 1000, terms)
aalen_5_M5 <- aalen_imp(
  imputed_tte_data_5, model_formulas[5], py = 1000, terms)
aalen_5_M6 <- aalen_imp(
  imputed_tte_data_5, model_formulas[6], py = 1000, terms)

# take a look at some output
aalen_5_M4$aa_summary
aalen_5_M4$comb_ah

## save output ----
for (i in 1:6) {
  write.xlsx(
    get(paste0("aalen_5_M", i)), 
    file = paste0(path_to_box, path_to_proj, "/output/sensitivity_R1/",
                  paste0("drop_dm_aalen_5yr_M", i, ".xlsx")))
}
