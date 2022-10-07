# multiple imputation 

# ---- set up packages ----
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "mice", "rlang"
       # "magrittr", "foreign", "tidyverse","ggplot2", "table1", "labelled"
       # "survey", "tableone", "openxlsx", "survival", "mgcv", "miceadds"
       # "openxlsx", "ggpubr", "mitools", "lmtest"
)


options(scipen = 999, digits = 8)

# ---- set up paths ----

path_to_box <- "/Users/julietzhou/Library/CloudStorage/Box-Box/"
# path_to_box <- "C:/Users/Staff/Box/"
data_path <- paste0(path_to_box,
                    "Asian_Americans_dementia_data/analysis_data_tables/",
                    "aa_adrd_cardiometabolic_tte.sas7bdat")

# ---- prepare pre-imputation dataset ----

data <- read_sas(data_path) # n = 184929

# sample size for manuscript
data %>% 
  filter(MAIN_DEM_V1_SAMPLE == 1, # n = 180394
         PRESURV5YR_SAMPLE == 1, # n = 154410
         ETHNICITY_REV %in% c(1, 2, 3, 5, 9) # n = 136180
         ) %>% 
  nrow()
  
tte_data_pre_mi <- data %>% 
  # exclude dementia cases pre-survey (prevalent dementia) 
  # and cases with follow-up time = 0
  filter(MAIN_DEM_V1_SAMPLE == 1) %>% 
  # select variables
  select(SUBJID, 
         MAIN_DEM_V1_SAMPLE, 
         SURVEY_AGE, FEMALE, 
         MAIN_DEM_V1_END_AGE, MAIN_DEM_V1_END_DEM_FLAG,
         MAIN_DEM_V1_DEM_AGE, MAIN_DEM_V1_FU_TIME, 
         MAIN_DEM_V1_END_TYPE,
         DIAB_DX5YR_FLAG, DIAB_DX7YR_FLAG, DIAB_DX9YR_FLAG,
         ETHNICITY_REV, ASIAN,
         USABORN_REV, EDUCATION_REV, EDU_GE_COLLEGE, 
         # income related
         SIZEOFHH, INCOME, INCOME_PP, 
         MARITALSTATUS, SMOKING_STATUS, USABORNFATHER_REV, USABORNMOTHER_REV,
         # general health and comorbidities to include in Table 1
         GENERALHEALTH,
         HTN_DX5YR_FLAG, HTN_DX7YR_FLAG, HTN_DX9YR_FLAG,
         FIRST_PREVSTROKE_FLAG, FIRST_INCSTROKE_FLAG,
         # membership criteria
         PRESURV5YR_SAMPLE, PRESURV7YR_SAMPLE, PRESURV9YR_SAMPLE,
         # add height variables
         SR_TOTAL_HEIGHT_IN, EHR_HT_MEDIAN
  ) %>%
  # include only South Asian, Chinese, Japanese, Filipino, and White
  filter(ETHNICITY_REV %in% c(1, 2, 3, 5, 9)) %>% 
  mutate(
    EDU_3 = case_when(
      EDUCATION_REV %in% c(1, 2) ~ "< HS",
      EDUCATION_REV %in% c(3, 4) ~ "HS + Some college",
      EDUCATION_REV %in% c(5, 6) ~ "College and above"),
    EDU_3 = factor(EDU_3, 
                   levels = c("< HS", "HS + Some college","College and above")))

# save the pre-MI dataset
# saveRDS(tte_data_pre_mi,
#         file = paste0(path_to_box,
#                       "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/",
#                       "aa_adrd_diabetes_pre_MI_tte_data.rds"))

# ---- check missingness ----

impute.var.list <- c(
  "SURVEY_AGE", "FEMALE",
  "ETHNICITY_REV", #"ASIAN",
  "USABORN_REV", "USABORNFATHER_REV", "USABORNMOTHER_REV",
  "EDUCATION_REV",
  "SIZEOFHH", "INCOME", "INCOME_PP",
  "MARITALSTATUS", "SMOKING_STATUS", 
  "SR_TOTAL_HEIGHT_IN", "EHR_HT_MEDIAN", 
  "GENERALHEALTH", "HTN_DX5YR_FLAG", "FIRST_PREVSTROKE_FLAG"
)

# Assess overall missingness in vars we want to impute / use for analysis
missingsummary <- data.frame(varname = impute.var.list, pctmiss = NA)
row.names(missingsummary) <- impute.var.list
for (i in impute.var.list){
  missingsummary[i, "pctmiss"] <- 100*sum(is.na(tte_data_pre_mi[, i]))/nrow(tte_data_pre_mi)
  print(i)
  print(table(tte_data_pre_mi[, i], exclude = NULL))
}

missingordered <- missingsummary[order(missingsummary$pctmiss), ]
missingordered

ordered.var.list <- c(paste(missingordered$varname))

# Assess missingness by race/ethnicity (Supplement)
# note that this is before subsetting on pre-survey membership criteria
# therefore total sample size is larger compared to Table 1

missing_x_ethn <- data.frame(
  varname = impute.var.list, 
  miss_w = NA, # White
  miss_a = NA, # Asian
  miss_c = NA, # Chinese
  miss_f = NA, # Filipino
  miss_j = NA, # Japanese
  miss_sa = NA # South Asian
)

col_ethn <- names(missing_x_ethn)[-1]
row.names(missing_x_ethn) <- impute.var.list

conds <- c(
  "ASIAN == 0", # White
  "ASIAN == 1", # Asian
  "ETHNICITY_REV ==  2", # Chinese
  "ETHNICITY_REV ==  5", # Filipino
  "ETHNICITY_REV ==  3", # Japanese
  "ETHNICITY_REV ==  1" # South Asian
)

for (i in impute.var.list){
  subset_data <- tte_data_pre_mi %>% 
    filter(PRESURV5YR_SAMPLE == 1) %>% 
    select(all_of(i), ETHNICITY_REV, ASIAN)
  for (j in 1:6) {
    missing_x_ethn[i, col_ethn[j]] <- subset_data %>% 
      filter(!!parse_expr(conds[j])) %>% 
      summarise(pctmiss = sum(is.na(get(i))) / n() * 100) %>% as.numeric()
  }
}

table(tte_data_pre_mi$ASIAN)
table(tte_data_pre_mi$ETHNICITY_REV)

missing_x_ethn
missing_x_ethn[order(missing_x_ethn$miss_a), ]
# output table 
# write.xlsx(missing_x_ethn, 
#            file = paste0(path_to_box, 
#                          "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/",
#                          "Code/Cleaned_Scripts/output/supplement/",
#                          "imp_summary_missing_x_ethn.xlsx"))

# prepare the dataset for imputation
# by dropping vars we don't need, ordering by missingness
impute.data <- tte_data_pre_mi[, ordered.var.list]

# set variable classes by type
str(impute.data)

#survey_age, income_pp, SR_TOTAL_HEIGHT_IN, EHR_HT_MEDIAN are continuous

# binary vars
impute.data$FEMALE <- as.factor(impute.data$FEMALE)
impute.data$USABORN_REV <- as.factor(impute.data$USABORN_REV)
impute.data$USABORNFATHER_REV <- as.factor(impute.data$USABORNFATHER_REV)
impute.data$USABORNMOTHER_REV <- as.factor(impute.data$USABORNMOTHER_REV)
impute.data$HTN_DX5YR_FLAG <- as.factor(impute.data$HTN_DX5YR_FLAG)
impute.data$FIRST_PREVSTROKE_FLAG <- as.factor(impute.data$FIRST_PREVSTROKE_FLAG)

# categorical vars
impute.data$ETHNICITY_REV <- factor(impute.data$ETHNICITY_REV, ordered = F)
impute.data$MARITALSTATUS <- factor(impute.data$MARITALSTATUS, ordered = F)
impute.data$GENERALHEALTH <- factor(impute.data$GENERALHEALTH, ordered = F)

# ordinal vars
impute.data$EDUCATION_REV <- factor(impute.data$EDUCATION_REV, ordered = T)
impute.data$INCOME <- factor(impute.data$INCOME, ordered = T)
impute.data$SIZEOFHH <- factor(impute.data$SIZEOFHH, ordered = T)
impute.data$SMOKING_STATUS <- factor(impute.data$SMOKING_STATUS, ordered = T)

# recheck classes
str(impute.data)

# ---- run single imputation to set up method ----
ini <- mice(impute.data, maxit = 0, 
            defaultMethod = c("pmm", "logreg", "polyreg", "polr"), seed = 12345)

ini$method
meth <- ini$method
meth

ini$predictorMatrix
pred <- ini$predictorMatrix

#change predictor matrix so income_pp doesn't predict income or sizeofhh
pred[c("INCOME", "SIZEOFHH"), "INCOME_PP"] <- 0
pred

# ---- run imputation ---- 
imp_fcs <- mice(impute.data, m = 20, maxit = 10, pred = pred, meth = meth, 
                defaultMethod = c("pmm", "logreg", "polyreg", "polr"), 
                seed = 12345)

# save(imp_fcs, 
#      file = paste0(path_to_box, "/Asian_Americans_dementia_data/diabetes_adrd/", 
#                    "Imputed_data/imp_fcs_full_03292022.R"))

# ---- post-imputation data processing ----

# load the pre-MI dataset
tte_data_pre_mi <- readRDS(
  file = paste0(path_to_box,
                "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/",
                "aa_adrd_diabetes_pre_MI_tte_data.rds"))

# load the mice object post MI
load(file = paste0(path_to_box, "/Asian_Americans_dementia_data/diabetes_adrd/", 
                   "Imputed_data/imp_fcs_full_03292022.R"))

# examine diagnostics
imp_fcs$loggedEvents

plot(imp_fcs)
densityplot(imp_fcs, ~INCOME)
densityplot(imp_fcs, ~SIZEOFHH)
densityplot(imp_fcs, ~INCOME_PP)
densityplot(imp_fcs, ~EDUCATION_REV)
densityplot(imp_fcs, ~USABORN_REV)

# ---- save stacked imputed dataset ----
# this is for generating table 1
impute.var.list <- c(
  "SURVEY_AGE", "FEMALE",
  "ETHNICITY_REV", #"ASIAN",
  "USABORN_REV", "USABORNFATHER_REV", "USABORNMOTHER_REV",
  "EDUCATION_REV",
  "SIZEOFHH", "INCOME", "INCOME_PP",
  "MARITALSTATUS", "SMOKING_STATUS", 
  "SR_TOTAL_HEIGHT_IN", "EHR_HT_MEDIAN", 
  "GENERALHEALTH", "HTN_DX5YR_FLAG", "FIRST_PREVSTROKE_FLAG"
)

imp.temp <- list()
for (i in 1:imp_fcs$m){
  imp.temp[[i]] <- complete(imp_fcs, action = i)
  imp.temp[[i]] <- cbind(tte_data_pre_mi$SUBJID, imp.temp[[i]][, c(impute.var.list)])
  imp.temp[[i]][, "imp"] <- i
}

imp_data_stacked <- do.call(rbind, imp.temp)
colnames(imp_data_stacked)
names(imp_data_stacked)[names(imp_data_stacked) == "tte_data_pre_mi$SUBJID"] <- "SUBJID"

# add other vars to dataset for table 1 ----

other.vars <- tte_data_pre_mi[, !names(tte_data_pre_mi) %in% 
                                c("SURVEY_AGE", "FEMALE", "ETHNICITY_REV", 
                                  "USABORN_REV", "USABORNFATHER_REV", "USABORNMOTHER_REV",
                                  # exclude all educations variables
                                  "EDUCATION_REV", "EDU_GE_COLLEGE", "EDU_3",
                                  "SIZEOFHH", "INCOME", "INCOME_PP",
                                  "MARITALSTATUS", "SMOKING_STATUS", 
                                  "SR_TOTAL_HEIGHT_IN", "EHR_HT_MEDIAN",
                                  "GENERALHEALTH", "HTN_DX5YR_FLAG", 
                                  "FIRST_PREVSTROKE_FLAG")]

# merge into stacked imputed dataset for table 1
diabetes_tbl1_data <- imp_data_stacked %>% 
  merge(x = ., y = other.vars, by = "SUBJID") %>% 
  mutate(
    EDU_GE_COLLEGE = ifelse(EDUCATION_REV %in% c(1,2,3,4), 0, 1),
    EDU_3 = case_when(
      EDUCATION_REV %in% c(1, 2) ~ "< HS",
      EDUCATION_REV %in% c(3, 4) ~ "HS + Some college",
      EDUCATION_REV %in% c(5, 6) ~ "College and above"),
    EDU_3 = factor(EDU_3, 
                   levels = c("< HS", "HS + Some college","College and above")))

# save the stacked imputed dataset for table 1
# saveRDS(diabetes_tbl1_data, 
#         file = paste0(path_to_box,
#                       "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/",
#                       "aa_adrd_diabetes_imputed_tbl1_data.rds"))

# ---- save imputed dataset as a mild object ----
# this is for running analysis
imp_data <- complete(imp_fcs, action = "all")

# merge into mild object
for (i in 1:imp_fcs$m) {
  imp_data[[i]] <- imp_data[[i]] %>% 
    cbind("SUBJID" = tte_data_pre_mi$SUBJID) %>% 
    merge(x = ., y = other.vars, by = "SUBJID") %>% 
    mutate(EDU_GE_COLLEGE = ifelse(EDUCATION_REV %in% c(1,2,3,4), 0, 1),
           EDU_3 = case_when(
             EDUCATION_REV %in% c(1, 2) ~ "< HS",
             EDUCATION_REV %in% c(3, 4) ~ "HS + Some college",
             EDUCATION_REV %in% c(5, 6) ~ "College and above"),
           EDU_3 = factor(EDU_3, 
                          levels = c("< HS", "HS + Some college",
                                     "College and above")), 
           ETHNICITY_REV = factor(ETHNICITY_REV, 
                                  levels = c(9, 1, 2, 3, 5),
                                  labels = c("White", "South Asian", "Chinese", 
                                             "Japanese", "Filipino")
           ), 
           ETHNICITY_REV = relevel(ETHNICITY_REV, ref = "White"))
}

# save the imputed dataset for analysis
# saveRDS(imp_data,
#         file = paste0(path_to_box,
#                       "Asian_Americans_dementia_data/diabetes_adrd/Imputed_data/",
#                       "aa_adrd_diabetes_imputed_tte_data.rds"))
