# sensitivity analysis - Part 1
# main cox/aalen models on imputed dataset using 5+1yr criteria
# add all race/ethnicity x covariates interactions

# ---- set up packages ----
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("tidyverse", "survival", "mice", "openxlsx", "mitools", "timereg"
       # "magrittr", "foreign", "flextable", "huxtable", "haven", 
       # "survey", "tableone",  "miceadds", "mgcv",
       # "ggpubr",  "lmtest", "table1", "labelled", "gtsummary", "gt"
)

options(scipen = 999, digits=8)

# ---- set up paths and datasets ----

path_to_box <- "/Users/julietzhou/Library/CloudStorage/Box-Box/"
path_to_imputed <- "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/"
imputed_tte_data <- readRDS(file = paste0(path_to_box, path_to_imputed, 
                                          "aa_adrd_diabetes_imputed_tte_data.rds"))

# load functions that fit Cox PH / Aalen additive hazards model on a given 
# imputed dataset using given model formula and output model summary
# also output combined HRs/AHs if needed

source(file = paste0(path_to_box, 
                     "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/", 
                     "Code/Cleaned_Scripts/functions_analysis.R"))
# check dimension
dim(imputed_tte_data[[10]])

# add membership criteria (5+1 yrs pre-survey)
imputed_tte_data_5 <- imputed_tte_data

for (i in 1:20) {
  imputed_tte_data_5[[i]] <- imputed_tte_data[[i]] %>% 
    filter(PRESURV5YR_SAMPLE == 1)  
  # center height (important since we use it in interaction terms)
  imputed_tte_data_5[[i]]$HT_center <- scale(imputed_tte_data_5[[i]]$EHR_HT_MEDIAN,
                                             center = 66, scale = FALSE)
}

# ---- Cox PH Models ----
# set up model formulas ---- 
surv_obj <- "Surv(SURVEY_AGE, MAIN_DEM_V1_END_AGE, MAIN_DEM_V1_END_DEM_FLAG) ~ "
race_ethn <- c("ASIAN", "ETHNICITY_REV")
diab <- "DIAB_DX5YR_FLAG"
covar <- c(diab, "FEMALE", "EDU_GE_COLLEGE", "USABORN_REV", "HT_center")

interactions <- c(
  paste0(race_ethn[1], "*(", paste(covar[1:2], collapse = "+"), ")"),
  paste0(race_ethn[1], "*(", paste(covar[1:3], collapse = "+"), ")"),
  paste0(race_ethn[1], "*(", paste(covar, collapse = "+"), ")"),
  paste0(race_ethn[2], "*(", paste(covar[1:2], collapse = "+"), ")"),
  paste0(race_ethn[2], "*(", paste(covar[1:3], collapse = "+"), ")"),
  paste0(race_ethn[2], "*(", paste(covar, collapse = "+"), ")")
)

model_formulas <- paste0(surv_obj, interactions)
model_formulas

# fit ASIAN aggregated models ----
cox_M1 <- cox_imp(imputed_tte_data_5, model_formulas[1], 
                  terms = list(c("DIAB_DX5YR_FLAG", "ASIAN:DIAB_DX5YR_FLAG")))
cox_M2 <- cox_imp(imputed_tte_data_5, model_formulas[2], 
                  terms = list(c("DIAB_DX5YR_FLAG", "ASIAN:DIAB_DX5YR_FLAG")))
cox_M3 <- cox_imp(imputed_tte_data_5, model_formulas[3], 
                  terms = list(c("DIAB_DX5YR_FLAG", "ASIAN:DIAB_DX5YR_FLAG")))
# take a look at some output
cox_M3$linear_summary
cox_M3$hr_summary
cox_M3$comb_hr

# fit subgroup models ----

# first specify the terms to combine HRs on
ethn_subgrps <- c("South Asian", "Chinese", "Japanese", "Filipino")
main_eff <- "DIAB_DX5YR_FLAG"
interactions <- paste0("ETHNICITY_REV", ethn_subgrps, ":", main_eff)
terms <- vector(mode = "list", length = length(ethn_subgrps))
for (i in 1:length(terms)) {
  terms[[i]] <- c(main_eff, interactions[i])
}
terms

cox_M4 <- cox_imp(imputed_tte_data_5, model_formulas[4], terms)
cox_M5 <- cox_imp(imputed_tte_data_5, model_formulas[5], terms)
cox_M6 <- cox_imp(imputed_tte_data_5, model_formulas[6], terms)
# take a look at some output
cox_M4$linear_summary
cox_M4$hr_summary
cox_M4$comb_hr


# save output ----
for (i in 1:6) {
  write.xlsx(
    get(paste0("cox_M", i)), 
    file = paste0(path_to_box, 
                  "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/",
                  "Code/Cleaned_Scripts/output/sensitivity/",
                  paste0("sens_add_covar_int_cox_5yr_M", i, ".xlsx")))
}

# ---- Aalen Additive Hazards Models ----
# set up model formulas ---- 

surv_obj <- "Surv(SURVEY_AGE, MAIN_DEM_V1_END_AGE, MAIN_DEM_V1_END_DEM_FLAG) ~ "
race_ethn <- c("ASIAN", "ETHNICITY_REV") %>% str_c("const(", ., ")")
diab <- str_c("const(", "DIAB_DX5YR_FLAG", ")") 
covar <- c(diab, str_c("const(", "FEMALE", ")"), 
           str_c("const(", "EDU_GE_COLLEGE", ")"),
           str_c("const(", "USABORN_REV", ")"),
           str_c("const(", "HT_center", ")"))

interactions <- c(
  paste0(race_ethn[1], "*(", paste(covar[1:2], collapse = "+"), ")"),
  paste0(race_ethn[1], "*(", paste(covar[1:3], collapse = "+"), ")"),
  paste0(race_ethn[1], "*(", paste(covar, collapse = "+"), ")"),
  paste0(race_ethn[2], "*(", paste(covar[1:2], collapse = "+"), ")"),
  paste0(race_ethn[2], "*(", paste(covar[1:3], collapse = "+"), ")"),
  paste0(race_ethn[2], "*(", paste(covar, collapse = "+"), ")")
)

model_formulas <- paste0(surv_obj, interactions)
model_formulas

# fit ASIAN aggregated models ----
aalen_M1 <- aalen_imp(
  imputed_tte_data_5, model_formulas[1], py = 1000, 
  terms = list(c("const(DIAB_DX5YR_FLAG)", "const(ASIAN):const(DIAB_DX5YR_FLAG)")))
aalen_M2 <- aalen_imp(
  imputed_tte_data_5, model_formulas[2], py = 1000, 
  terms = list(c("const(DIAB_DX5YR_FLAG)", "const(ASIAN):const(DIAB_DX5YR_FLAG)")))
aalen_M3 <- aalen_imp(
  imputed_tte_data_5, model_formulas[3], py = 1000, 
  terms = list(c("const(DIAB_DX5YR_FLAG)", "const(ASIAN):const(DIAB_DX5YR_FLAG)")))
# take a look at some output
aalen_M1$aa_summary
aalen_M1$comb_ah

# fit subgroup models ----

# first specify the terms to combine HRs on
ethn_subgrps <- c("South Asian", "Chinese", "Japanese", "Filipino")
main_eff <- "const(DIAB_DX5YR_FLAG)"
interactions <- paste0("const(ETHNICITY_REV)", ethn_subgrps, ":", main_eff)
terms <- vector(mode = "list", length = length(ethn_subgrps))
for (i in 1:length(terms)) {
  terms[[i]] <- c(main_eff, interactions[i])
}
terms

aalen_M4 <- aalen_imp(
  imputed_tte_data_5, model_formulas[4], py = 1000, terms)
aalen_M5 <- aalen_imp(
  imputed_tte_data_5, model_formulas[5], py = 1000, terms)
aalen_M6 <- aalen_imp(
  imputed_tte_data_5, model_formulas[6], py = 1000, terms)

# take a look at some output
aalen_M4$aa_summary
aalen_M4$comb_ah

# save output ----
for (i in 1:6) {
  write.xlsx(
    get(paste0("aalen_M", i)), 
    file = paste0(path_to_box, 
                  "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/",
                  "Code/Cleaned_Scripts/output/sensitivity/",
                  paste0("sens_add_covar_int_aalen_5yr_M", i, ".xlsx")))
}
