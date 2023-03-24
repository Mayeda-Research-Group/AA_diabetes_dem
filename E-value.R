# This scripts takes in Cox PH Model 3 

# set up packages ----
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("tidyverse", "openxlsx", "EValue"
       # "magrittr", "foreign", "ggplot2", "haven", "table1", "labelled",
       # "survey", "tableone", "mice", "survival", "miceadds", "mgcv",
       # "ggpubr", "mitools", "lmtest", "huxtable", "kableExtra"
)

options(scipen = 999, digits = 8)

# set up paths ----

path_to_box <- "/Users/julietzhou/Library/CloudStorage/Box-Box/"
path_to_output <- "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/Code/Cleaned_Scripts/output/"

# read in final Cox model results for 5+yr ----

cox_hr_M3 <- bind_rows(
  read.xlsx(
    paste0(path_to_box, path_to_output, "main_R1/main_cox_5yr_M3.xlsx"), 
    sheet = 2, 
    colNames = TRUE,
    rows = 1:2
  ),
  
  read.xlsx(
    paste0(path_to_box, path_to_output, "main_R1/main_cox_5yr_M3.xlsx"), 
    sheet = 3, 
    colNames = TRUE
  ) %>% 
    mutate(lowerCI = as.numeric(lowerCI),
           upperCI = as.numeric(upperCI)), 
  
  read.xlsx(
    paste0(path_to_box, path_to_output, "main_R1/main_cox_5yr_M6.xlsx"), 
    sheet = 3, 
    colNames = TRUE
  ) %>% 
    mutate(lowerCI = as.numeric(lowerCI),
           upperCI = as.numeric(upperCI)),
  
)

cox_hr_M3_intxn <- bind_rows(
  read.xlsx(
    paste0(path_to_box, path_to_output, "main_R1/main_cox_5yr_M3.xlsx"), 
    sheet = 2, 
    colNames = TRUE,
    rows = c(1, 8)
  ),
  
  read.xlsx(
    paste0(path_to_box, path_to_output, "main_R1/main_cox_5yr_M6.xlsx"), 
    sheet = 2, 
    colNames = TRUE, 
    rows = c(1, 11:14)
  ) %>% 
    mutate(lowerCI = as.numeric(lowerCI),
           upperCI = as.numeric(upperCI))
)


# calculate E-values for combined HRs ----

# rare disease assumption not true for white or japanese, and 
# true for others 

cox_hr_M3 <- cox_hr_M3 %>% 
  mutate(
    rare_disease = ifelse(
      term %in% c("DIAB_DX5YR_FLAG", "DIAB_DX5YR_FLAG:ETHNICITY_REVJapanese"), 
      FALSE, TRUE
    )
  )

evalues_NULL <- matrix(
  nrow = nrow(cox_hr_M3), ncol = 3, 
  dimnames = list(cox_hr_M3$term, c("point", "lower", "upper"))
)

for (i in 1:nrow(cox_hr_M3)) {
  temp <- evalues.HR(
    est = cox_hr_M3[i, 2], lo = cox_hr_M3[i, 3], hi = cox_hr_M3[i, 4], 
    rare = cox_hr_M3[i, 6], true = 1
  )
  evalues_NULL[i, ] <- temp[2, ]
}

# calculate E-values for intxn ----
# E-value required to explain heterogeneity in group versus non-Latino white participants

cox_hr_M3_intxn <- cox_hr_M3_intxn %>% 
  mutate(
    rare_disease = ifelse(
      term %in% c("DIAB_DX5YR_FLAG", "DIAB_DX5YR_FLAG:ETHNICITY_REVJapanese"), 
      FALSE, TRUE
    )
  )

evalues_intxn <- matrix(
  nrow = nrow(cox_hr_M3_intxn), ncol = 3, 
  dimnames = list(cox_hr_M3_intxn$term, c("point", "lower", "upper"))
)

for (i in 1:nrow(cox_hr_M3_intxn)) {
  temp <- evalues.HR(
    est = cox_hr_M3_intxn[i, 2], 
    lo = cox_hr_M3_intxn[i, 3], hi = cox_hr_M3_intxn[i, 4], 
    rare = cox_hr_M3_intxn[i, 6], true = 1
  )
  evalues_intxn[i, ] <- temp[2, ]
}



# formatt and save the results ----
ethn_level <- c("White", "Asian", "South Asian", "Chinese", "Japanese", "Filipino")

evalues_NULL <- evalues_NULL %>% 
  round(digits = 2) %>% 
  as_tibble(rownames = "term") %>% 
  mutate(Ethn = ethn_level, .before = point)

evalues_intxn <- evalues_intxn %>% 
  round(digits = 2) %>% 
  as_tibble(rownames = "term") %>% 
  mutate(Ethn = ethn_level[-1], .before = point)

write.xlsx(
  list("Null" = evalues_NULL, 
       "intxn" = evalues_intxn), 
  paste0(path_to_box, path_to_output, "e-value_Cox_M3.xlsx"),
  colNames = TRUE
)


