# set up packages ----
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("tidyverse", "table1", "labelled", "openxlsx", 
       # "magrittr", "foreign", "ggplot2", "haven", 
       # "survey", "tableone", "mice", "survival", "miceadds", "mgcv",
       # "ggpubr", "mitools", "lmtest", "huxtable", "kableExtra"
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

# note: we just want to use one copy from each defn (5/7/9+yrs) of diabetes
# for IR calculations 

# source the script for IR calculation
source(paste0(path_to_box, path_to_proj, "function_IR_calc.R"))

# standard population 
age_category <- c(59, 65, 70, 75, 80, 85, 120)
age_pop <- c(10805447, 9533545, 8857441, 7415813, 4945367, 4239587)
ethns <- c("White", "Asian", "Chinese", "Japanese", "Filipino", "South Asian")

# using 5+1yr membership criteria ----

# calculate person-years contributions
pys_5 <- calculate_pys(
  imputed_tte_data_5[[1]], age_cat = age_category, 
  fu_start = "SURVEY_AGE", fu_end = "MAIN_DEM_V1_END_AGE", 
  event_flag = "MAIN_DEM_V1_END_DEM_FLAG"
)

# calculate IR's by ethnicity and diabetes status
IR_5 <- lapply(
  c(0,1), 
  function(x) {
    calculate_ir(
      data = pys_5$data, 
      age_cat_labels = pys_5$age_cat_labels, 
      std_pop = age_pop,
      diab_exposure = diab_exposure <- c("DIAB_DX5YR_FLAG", x), 
      per_pys = 1000
    )
  }
)
names(IR_5) <- c("no_diab", "diab")


## format tables
## age specific rates ----

# function to collect age-specific rates into tables
collect_age_spec_ir <- function(IR_data) {
  no_diab_age_spec <- lapply(
    ethns,
    function(x) {
      IR_data$no_diab[[x]]$age_spec %>% 
        mutate(
          age_spec_ir = ifelse(
            unw_cases < 5, "< 5 events", 
            paste0(unw_adj_ir, " (", unw_adj_ir_CI_l, ", ", unw_adj_ir_CI_u, ")")
          )
        ) %>% 
        select(age_range, age_spec_ir) %>% 
        pivot_wider(names_from = age_range, values_from = age_spec_ir)
    }
  )
  
  no_diab_age_spec <- do.call(rbind, no_diab_age_spec) %>% 
    mutate(Ethnicity = ethns, Diabetes = "No diabetes") %>% 
    relocate(Diabetes, Ethnicity)
  
  diab_age_spec <- lapply(
    ethns,
    function(x) {
      IR_data$diab[[x]]$age_spec %>% 
        mutate(
          age_spec_ir = ifelse(
            unw_cases < 5, "< 5 events", 
            paste0(unw_adj_ir, " (", unw_adj_ir_CI_l, ", ", unw_adj_ir_CI_u, ")")
          )
        ) %>% 
        select(age_range, age_spec_ir) %>% 
        pivot_wider(names_from = age_range, values_from = age_spec_ir)
    }
  )
  
  diab_age_spec <- do.call(rbind, diab_age_spec) %>% 
    mutate(Ethnicity = ethns, Diabetes = "Diabetes") %>% 
    relocate(Diabetes, Ethnicity)
  
  return(rbind(no_diab_age_spec, diab_age_spec))
}

# save output
# collect_age_spec_ir(IR_5) %>%
#   write.xlsx(
#     .,
#     file = paste0(path_to_box, path_to_proj, "/output/",
#                   "age_spec_IR_ethn_x_diab_5yr.xlsx")
#   )

## age specific rates ----
# function to collect age-adjusted rates into tables
collect_age_adj_ir <- function(IR_data) {
  no_diab_age_spec <- lapply(
    ethns, function(x) {IR_data$no_diab[[x]]$age_adj}
  )
  no_diab_age_spec <- do.call(rbind, no_diab_age_spec) %>% 
    mutate(Ethnicity = ethns, Diabetes = "No diabetes") %>% 
    relocate(Diabetes, Ethnicity) %>% 
    mutate(age_adj_ir = paste0(adj_ir, " (", adj_ir_l, ", ", adj_ir_u, ")"))

  diab_age_spec <- lapply(
    ethns, function(x) {IR_data$diab[[x]]$age_adj}
  )
  
  diab_age_spec <- do.call(rbind, diab_age_spec) %>% 
    mutate(Ethnicity = ethns, Diabetes = "Diabetes") %>% 
    relocate(Diabetes, Ethnicity) %>% 
    mutate(age_adj_ir = paste0(adj_ir, " (", adj_ir_l, ", ", adj_ir_u, ")"))
  
  return(rbind(no_diab_age_spec, diab_age_spec))
}

# save the output
# collect_age_adj_ir(IR_5) %>% 
#     write.xlsx(
#       .,
#       file = paste0(path_to_box, path_to_proj, "/output/",
#                     "age_adj_IR_ethn_x_diab_5yr.xlsx")
#     )
