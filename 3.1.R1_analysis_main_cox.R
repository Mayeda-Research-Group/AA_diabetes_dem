# main analysis 
# cox models on imputed dataset

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

# check dimension
dim(imputed_tte_data[[10]]) # n = 159477

# prepare for analysis ----
# run the script that preps the data according to R1 revision
source(paste0(path_to_box, path_to_proj, "2.0.R1_data_prep.R"))

# check dimension again 
dim(imputed_tte_data_5[[10]]) # n = 135939

# load a function that fit Cox PH model on a given imputed dataset
# using given model formula and output model summary and combined HRs
source(file = paste0(path_to_box, path_to_proj, "functions_analysis.R"))

# using 5+1yr membership criteria ----
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
cox_5_M1$linear_summary
cox_5_M1$hr_summary
cox_5_M1$comb_hr

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
    file = paste0(path_to_box, path_to_proj, "output/main_R1/",
                  paste0("main_cox_5yr_M", i, ".xlsx")))
}

# using 7+1yr membership criteria ----
## set up model formula ----
surv_obj <- "Surv(SURVEY_AGE, MAIN_DEM_V1_END_AGE, MAIN_DEM_V1_END_DEM_FLAG) ~ "
race_ethn <- c("ASIAN", "ETHNICITY_REV")
diab <- "DIAB_DX7YR_FLAG"
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
cox_7_M1 <- cox_imp(imputed_tte_data_7, model_formulas[1], 
                    terms = list(c("DIAB_DX7YR_FLAG", "DIAB_DX7YR_FLAG:ASIAN")))
cox_7_M2 <- cox_imp(imputed_tte_data_7, model_formulas[2], 
                    terms = list(c("DIAB_DX7YR_FLAG", "DIAB_DX7YR_FLAG:ASIAN")))
cox_7_M3 <- cox_imp(imputed_tte_data_7, model_formulas[3], 
                    terms = list(c("DIAB_DX7YR_FLAG", "DIAB_DX7YR_FLAG:ASIAN")))
# take a look at some output
cox_7_M1$linear_summary
cox_7_M1$hr_summary
cox_7_M1$comb_hr

## fit subgroup models ----

# first specify the terms to combine HRs on
ethn_subgrps <- c("South Asian", "Chinese", "Japanese", "Filipino")
main_eff <- "DIAB_DX7YR_FLAG"
interactions <- paste0(main_eff, ":ETHNICITY_REV", ethn_subgrps)
terms <- lapply(interactions, function(x) c(main_eff, x))

cox_7_M4 <- cox_imp(imputed_tte_data_7, model_formulas[4], terms)
cox_7_M5 <- cox_imp(imputed_tte_data_7, model_formulas[5], terms)
cox_7_M6 <- cox_imp(imputed_tte_data_7, model_formulas[6], terms)
# take a look at some output
cox_7_M4$linear_summary
cox_7_M4$hr_summary
cox_7_M4$comb_hr


## save output ----
for (i in 1:6) {
  write.xlsx(
    get(paste0("cox_7_M", i)), 
    file = paste0(path_to_box, path_to_proj, "output/main_R1/",
                  paste0("main_cox_7yr_M", i, ".xlsx")))
}

# using 9+1yr membership criteria ----
## set up model formula ----
surv_obj <- "Surv(SURVEY_AGE, MAIN_DEM_V1_END_AGE, MAIN_DEM_V1_END_DEM_FLAG) ~ "
race_ethn <- c("ASIAN", "ETHNICITY_REV")
diab <- "DIAB_DX9YR_FLAG"
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
cox_9_M1 <- cox_imp(imputed_tte_data_9, model_formulas[1], 
                    terms = list(c("DIAB_DX9YR_FLAG", "DIAB_DX9YR_FLAG:ASIAN")))
cox_9_M2 <- cox_imp(imputed_tte_data_9, model_formulas[2], 
                    terms = list(c("DIAB_DX9YR_FLAG", "DIAB_DX9YR_FLAG:ASIAN")))
cox_9_M3 <- cox_imp(imputed_tte_data_9, model_formulas[3], 
                    terms = list(c("DIAB_DX9YR_FLAG", "DIAB_DX9YR_FLAG:ASIAN")))
# take a look at some output
cox_9_M1$linear_summary
cox_9_M1$hr_summary
cox_9_M1$comb_hr

## fit subgroup models ----

# first specify the terms to combine HRs on
ethn_subgrps <- c("South Asian", "Chinese", "Japanese", "Filipino")
main_eff <- "DIAB_DX9YR_FLAG"
interactions <- paste0(main_eff, ":ETHNICITY_REV", ethn_subgrps)
terms <- lapply(interactions, function(x) c(main_eff, x))
terms

cox_9_M4 <- cox_imp(imputed_tte_data_9, model_formulas[4], terms)
cox_9_M5 <- cox_imp(imputed_tte_data_9, model_formulas[5], terms)
cox_9_M6 <- cox_imp(imputed_tte_data_9, model_formulas[6], terms)
# take a look at some output
cox_9_M4$linear_summary
cox_9_M4$hr_summary
cox_9_M4$comb_hr


## save output ----
for (i in 1:6) {
  write.xlsx(
    get(paste0("cox_9_M", i)), 
    file = paste0(path_to_box, path_to_proj, "output/main_R1/",
                  paste0("main_cox_9yr_M", i, ".xlsx")))
}


