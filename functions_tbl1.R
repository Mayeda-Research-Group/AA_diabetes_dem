# functions used in table 1 scripts

require(table1)

# function to re-factor datasets
# and give meaningful labels for variables
t1_relabel <- function(df) {
  tb1_df <- df %>% 
    mutate(
      FEMALE = factor(FEMALE, levels = c(1, 0), labels = c("Yes", "No")),
      
      ASIAN = factor(ASIAN, levels = c(0, 1), labels = c("White", "Asian")), 
      
      ETHNICITY_REV = factor(ETHNICITY_REV, 
                             # levels = c(9, 1, 2, 3, 5),
                             # labels = c(9"White", 
                                        # 1"South Asian", 2"Chinese", 3"Japanese", 
                                        # 5"Filipino"
                             levels = c(9, 2, 3, 5, 1),
                             labels = c("White", "Chinese", "Japanese", 
                                        "Filipino", "South Asian")
                                        
      ),
      
      EDUCATION_REV = factor(EDUCATION_REV, levels = c(1:6), 
                             labels = c("Grade school", "Some high school", 
                                        "High school or GED", 
                                        "Technical/trade/some college", 
                                        "College", "Graduate school")
      ), 
      
      EDU_GE_COLLEGE = factor(EDU_GE_COLLEGE, levels = c(1, 0), 
                              labels = c("Yes", "No")),
      
      USABORN_REV = factor(USABORN_REV, levels = c(1, 0), 
                           labels = c("Born in the US", "Foreign born")),
      
      MARITALSTATUS = factor(MARITALSTATUS, levels = 1:4, 
                             labels = c("Never Married", 
                                        "Married or living as married",
                                        "Separated/Divorced", 
                                        "Widowed")),
      
      GENERALHEALTH = factor(GENERALHEALTH, levels = 1:5, 
                             labels = c("Excellent", "Very Good", "Good", 
                                        "Fair", "Poor")),
      
      DIAB_DX5YR_FLAG = factor(DIAB_DX5YR_FLAG, levels = c(1, 0),
                               labels = c("Yes", "No")),
      
      DIAB_DX7YR_FLAG = factor(DIAB_DX7YR_FLAG, levels = c(1, 0),
                               labels = c("Yes", "No")),
      
      DIAB_DX9YR_FLAG = factor(DIAB_DX9YR_FLAG, levels = c(1, 0),
                               labels = c("Yes", "No")),
      
      HTN_DX5YR_FLAG = factor(HTN_DX5YR_FLAG, levels = c(1, 0), 
                              labels = c("Yes", "No")),
      
      HTN_DX7YR_FLAG = factor(HTN_DX7YR_FLAG, levels = c(1, 0), 
                              labels = c("Yes", "No")),
      
      HTN_DX9YR_FLAG = factor(HTN_DX9YR_FLAG, levels = c(1, 0), 
                              labels = c("Yes", "No")),
      
      FIRST_PREVSTROKE_FLAG = factor(FIRST_PREVSTROKE_FLAG, levels = c(1, 0), 
                                     labels = c("Yes", "No")), 
      
      MAIN_DEM_V1_END_TYPE = factor(MAIN_DEM_V1_END_TYPE, 
                                    levels = c("DEMENTIA", "DEATH", 
                                               "ADMIN CENSORED", "END OF MEMBERSHIP", 
                                               "CENSORED 90+"),
                                    labels = c("Dementia", "Death", 
                                               "Administratively Censored", 
                                               "End of Membership", 
                                               "Censored 90+")), 
      # added for R1
      SMOKING_STATUS = factor(SMOKING_STATUS, levels = 1:3,
                              labels = c("Never", "Former", "Current"))
    )
  
  var_label(tb1_df) <- list(
    SURVEY_AGE = "Survey age, years [mean (SD)]", 
    FEMALE = "Female, n (%)", 
    EDUCATION_REV = "Education attainment, n (%)", 
    EDU_GE_COLLEGE = "College degree or more, n (%)",
    EDU_3 = "Education attainment: 3 categories, n (%)",
    USABORN_REV = "US born, n (%)",
    MARITALSTATUS = "Marital status, n (%)", 
    GENERALHEALTH = "General health, n (%)",
    INCOME_PP = "Household-adjusted income, dollars [mean (SD)]",
    MAIN_DEM_V1_END_TYPE = "End of follow up event, n (%)",
    MAIN_DEM_V1_FU_TIME = "Follow up time, years [mean (SD)]",
    DIAB_DX5YR_FLAG = "Diabetes exposure 5+ years pre-survey",
    DIAB_DX7YR_FLAG = "Diabetes exposure 7+ years pre-survey",
    DIAB_DX9YR_FLAG = "Diabetes exposure 9+ years pre-survey",
    HTN_DX5YR_FLAG = "Hypertension diagnosis 5+ years pre-survey, n (%)",
    HTN_DX7YR_FLAG = "Hypertension diagnosis 7+ years pre-survey, n (%)",
    HTN_DX9YR_FLAG = "Hypertension diagnosis 9+ years pre-survey, n (%)",
    FIRST_PREVSTROKE_FLAG = "History of stroke, n (%)", 
    EHR_HT_MEDIAN = "EHR height, in [mean (SD)]", 
    # added for R1
    SMOKING_STATUS = "Smoking status, n (%)"
  )
  
  return(tb1_df)
}

# variables in Table 1 added in R1
t1_relabel_add <- function(df) {
  tb1_df <- df %>% 
    mutate(
      SR_DEPRESS = factor(SR_DEPRESS, levels = c(1, 0), 
                          labels = c("Yes", "No/Missing"))
    )
  var_label(tb1_df) <- list(
    SR_BMI = "BMI [mean (SD)]", 
    SR_DEPRESS = "Self-rated depression, n (%)"
  )
  return(tb1_df)
}


# customized function for rendering continuous variables in table 1's
my.render.cont <- function(x) {
  with(
    stats.apply.rounding(stats.default(x), digits = 1,
                         rounding.fn = round_pad),
    c("", "Mean (SD)" = sprintf("%s (%s)", MEAN, SD))
  )
}

# customized function for rendering categorical variables in table 1's post-MI
my.render.cat_imp <- function(x, value.prefix = "") {
  N <- length(x) / 20
  freqs <- table(x) / 20
  pcts <- round(freqs / N * 100, 1)
  freqs_formatted <- paste0(round(freqs, 0), " (", as.numeric(pcts), "%)")
  names(freqs_formatted) <- paste0(value.prefix, names(freqs))
  
  out <- c("",freqs_formatted)
  return(out)
}

# function to apply Rubin's rule to imputed continuous variables

imp_cts_var_tbl1 <- function(df, cts_vars, grp) {
  # df <- tbl1_data_post_MI_5
  # cts_vars is a list that looks like:
  # cts_vars <- list("INCOME_PP" = 0, "EHR_HT_MEDIAN" = 1)
  # where the names of the items are variable names, and 
  # the numbers are rounding digits
  # grp <- c("ETHNICITY_REV", "DIAB_DX5YR_FLAG")
  
  out <- df %>% 
    group_by(across(all_of(grp))) %>% 
    group_keys() 
  
  for (var in names(cts_vars)) {
    temp <- df %>% 
      group_by(across(all_of(c("imp", grp)))) %>% 
      summarise(mean_imp = mean(get(var)), var_imp = var(get(var))) %>% 
      ungroup() %>% 
      group_by(across(all_of(grp))) %>% 
      summarise(mean = mean(mean_imp), 
                sd = mean(var_imp) %>% sqrt()) %>% 
      mutate(out = paste0(round_pad(mean, cts_vars[[var]]), 
                          " (", round_pad(sd, cts_vars[[var]]), ")"))
    out[var] <- temp$out
  }
  
  return(t(out))
}


# # function to apply Rubin's rule to imputed continuous variables
# # namely INCOME_PP and EHR_HT_MEDIAN
# 
# imp_cts_var_tbl1 <- function(df, DIAB_FLAG) {
#   cts_vars <- c("EHR_HT_MEDIAN", "INCOME_PP")
#   
#   # save grouping labels
#   race_ethn_diab <- df %>% 
#     filter(imp == 1) %>% 
#     select(SUBJID, ETHNICITY_REV, all_of(DIAB_FLAG))
#   # only work on related vars
#   cts_data <- df %>% 
#     select(SUBJID, imp, ETHNICITY_REV, all_of(DIAB_FLAG), all_of(cts_vars))
#   # set up output dataframe
#   out <- race_ethn_diab %>% 
#     group_by(ETHNICITY_REV, get(DIAB_FLAG)) %>% 
#     group_keys() %>% 
#     mutate(EHR_HT_MEDIAN = NA, INCOME_PP = NA) 
#   
#   # prepare wide data for each var
#   cts_data_HT <- cts_data %>% 
#     select(SUBJID, imp, EHR_HT_MEDIAN) %>% 
#     pivot_wider(id_cols = SUBJID, names_from = imp, values_from = EHR_HT_MEDIAN) %>% 
#     merge(race_ethn_diab, by = "SUBJID") %>% 
#     group_split(ETHNICITY_REV, get(DIAB_FLAG), .keep = FALSE)
#   cts_data_INCOME <- cts_data %>% 
#     select(SUBJID, imp, INCOME_PP) %>% 
#     pivot_wider(id_cols = SUBJID, names_from = imp, values_from = INCOME_PP) %>% 
#     merge(race_ethn_diab, by = "SUBJID") %>% 
#     group_split(ETHNICITY_REV, get(DIAB_FLAG), .keep = FALSE)
#   
#   for (i in 1:dim(out)[1]) {
#     # calculate mean and SD of height for each imputation
#     imp_mean_HT <- cts_data_HT[[i]][-22] %>% 
#       summarise(across(!SUBJID, mean)) %>% as.numeric()
#     imp_sd_HT <- cts_data_HT[[i]][-22] %>% 
#       summarise(across(!SUBJID, sd)) %>% as.numeric()
#     # combine means and SDs of height from 20 imputations
#     out[i, 3] <- paste0(round(mean(imp_mean_HT), 1), 
#                         " (", round(sqrt(mean(imp_sd_HT^2) + var(imp_mean_HT)), 1), ")")
#     # similar for income
#     imp_mean_INCOME <- cts_data_INCOME[[i]][-22] %>% 
#       summarise(across(!SUBJID, mean)) %>% as.numeric()
#     imp_sd_INCOME <- cts_data_INCOME[[i]][-22] %>% 
#       summarise(across(!SUBJID, sd)) %>% as.numeric()
#     
#     out[i, 4] <- paste0(round(mean(imp_mean_INCOME), 0), 
#                         " (", round(sqrt(mean(imp_sd_INCOME^2) + var(imp_mean_INCOME)), 0), ")")
#   }
#   
#   return(out)
# }




# # function to combine categorical summaries in table1 for imputed datasets
# # essentially takes the average of the summaries from across imputations
# 
# combine_tbl1_cat <- function(tbl1_data_post, m = 20, perc = FALSE, 
#                              grp, exposure) {
#   
#   tbl1_df_imp_post <- vector(mode = "list", length = m)
#   
#   for (i in 1:m) {
#     sample_in <- tbl1_data_post %>%
#       filter(imp == i) %>%
#       table1(
#         ~ EDUCATION_REV + EDU_GE_COLLEGE + EDU_3 + USABORN_REV + 
#           MARITALSTATUS + GENERALHEALTH
#         | get(grp) + get(exposure),
#         data = .,
#         overall = NULL
#       ) %>% as.data.frame()
#     
#     sample_out <-
#       matrix(nrow = dim(sample_in)[1],
#              ncol = dim(sample_in)[2])
#     
#     sample_out[1, ] <- sample_in[1, ] %>%
#       str_remove_all(pattern = "\\(N=") %>%
#       str_remove_all(pattern = "\\)")
#     
#     sample_out[, 1] <- sample_in[, 1]
#     sample_out[1, 1] <- "N"
#     
#     for (j in 2:dim(sample_in)[1]) {
#       if (!perc) {
#         sample_out[j, -1] <- sample_in[j, -1] %>%
#           str_remove_all(pattern = "\\s\\([0-9.]+%\\)")
#       } else {
#         sample_out[j, -1] <- sample_in[j, -1] %>%
#           str_extract(pattern = "\\s\\([0-9.]+%\\)") %>% 
#           str_remove_all(pattern = "\\(") %>% 
#           str_remove_all(pattern = "%\\)")
#       }
#     }
#     
#     tbl1_df_imp_post[[i]] <- sample_out %>%
#       as_tibble() %>%
#       mutate(imp = i)
#   }
#   
#   tbl1_df_imp_post <- bind_rows(tbl1_df_imp_post) %>%
#     mutate(
#       across(!V1, as.double)
#       # V2 = as.double(V2),
#       # V3 = as.double(V3),
#       # V4 = as.double(V4),
#       # V5 = as.double(V5),
#       # V6 = as.double(V6),
#       # V7 = as.double(V7)
#     )
#   
#   vars <- tbl1_df_imp_post$V1 %>% unique()
#   
#   tbl1_post_final <- matrix(nrow = length(vars),
#                             ncol = dim(tbl1_df_imp_post)[2]-2, 
#                             dimnames = list(vars))
#   
#   for (i in 1:length(vars)) {
#     tbl1_post_final[i, ] <- tbl1_df_imp_post %>%
#       filter(V1 == vars[i]) %>%
#       summarise(across(!V1 & !imp, ~ mean(.x, na.rm = TRUE))) %>%
#       as_vector() %>% 
#       round(digits = ifelse(perc, 1, 0))
#   }
#   
#   return(tbl1_post_final)
#   
# }


