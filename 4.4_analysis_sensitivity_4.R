# sensitivity analysis - Part 4
# main cox/aalen models on imputed dataset using 5+1yr criteria
# diabetes types

# ---- set up packages ----
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("tidyverse", "survival", "mice", "openxlsx", "mitools", "timereg"
       # "magrittr", "foreign", "flextable", "huxtable", "haven", 
       # "survey", "tableone",  "miceadds", "mgcv",
       # "ggpubr",  "lmtest", "table1", "labelled", "gtsummary", "gt"
)

options(scipen = 999, digits=8)

# load functions that fit Cox PH / Aalen additive hazards model on a given 
# imputed dataset using given model formula and output model summary
# also output combined HRs/AHs if needed

source(file = paste0(path_to_box, 
                     "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/", 
                     "Code/Cleaned_Scripts/functions.R"))

# ---- set up paths and datasets ----

path_to_box <- "/Users/julietzhou/Library/CloudStorage/Box-Box/"
path_to_imputed <- "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/"
imputed_tte_data <- readRDS(file = paste0(path_to_box, path_to_imputed, 
                                          "aa_adrd_diabetes_imputed_tte_data.rds"))

# check dimension
dim(imputed_tte_data[[10]])

# add membership criteria (5+1 yrs pre-survey)
imputed_tte_data_5 <- imputed_tte_data

for (i in 1:20) {
  imputed_tte_data_5[[i]] <- imputed_tte_data[[i]] %>% 
    filter(PRESURV5YR_SAMPLE == 1)
}