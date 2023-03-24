# figures for
# (1) age-adjusted incidence rates
# (2) M3 results from Cox PH / Aalen models

# set up packages ----
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("tidyverse", "openxlsx", "gridExtra"
       # "magrittr", "foreign", "flextable", "huxtable", "haven", 
       # "survey", "tableone",  "miceadds", "mgcv", "survival", "mice", 
       # "ggpubr",  "lmtest", "table1", "labelled", "gtsummary", "gt"
       # "mitools", "timereg"
)

options(scipen = 999, digits = 8)

# set up paths ----

path_to_box <- "/Users/julietzhou/Library/CloudStorage/Box-Box/"
path_to_output <- paste0(
  path_to_box, 
  "/Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/Code/", 
  # "Cleaned_Scripts/output/main/"
  "Cleaned_Scripts/output/"
  )

# barplot for age-adjusted IRs (5+yr) ----

ethn_order <- c("White", "Asian", "South Asian", "Chinese", "Japanese", "Filipino")
ethn_level <- c("White", "Asian", "Chinese", "Filipino", "Japanese", "South Asian")

# race/ethnicity x diabetes 
# read in the IRs from 5yr criteria
ir_data <- read.xlsx(
  paste0(path_to_output, "age_adj_IR_ethn_x_diab_5yr.xlsx"),
  colNames = TRUE
) %>% 
  select(Diabetes, Ethnicity, adj_ir, adj_ir_l, adj_ir_u) %>% 
  mutate(
    Ethnicity = factor(Ethnicity, levels = ethn_level),
    Diabetes = factor(Diabetes, levels = c("No diabetes", "Diabetes")),
    plot_grp = ifelse(Ethnicity %in% c("White", "Asian"), "All", 
                           "Asian ethnic groups"),
         across(c(adj_ir, adj_ir_l, adj_ir_u), as.numeric))

# binary blue version
ggplot(ir_data, aes(x = Ethnicity, y = adj_ir, fill = Diabetes)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(
    stat = "identity",
    aes(ymin = adj_ir_l, ymax = adj_ir_u), width = .2,
    position = position_dodge(.9)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  #theme(axis.text.x = element_text(angle = 45,  vjust = 1, hjust = 1)) +
  theme_bw() +
  labs(
    # title = "Age-adjusted incidence rates by race/ethnicity and diabetes",
    x = "Race/Ethnicity", y = "Age-adjusted incidence rate per 1,000 person-years") +
  scale_fill_brewer(palette = "Paired") +
  facet_grid(. ~ plot_grp, scales = "free", space = "free")

# color coded version
ggplot(ir_data, aes(x = Ethnicity, y = adj_ir, 
                    fill = Ethnicity, alpha = Diabetes)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(
    stat = "identity",
    aes(ymin = adj_ir_l, ymax = adj_ir_u), width = .2,
    position = position_dodge(.9)) + 
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  scale_alpha_discrete(range = c(0.5, 1)) +
  #theme(axis.text.x = element_text(angle = 45,  vjust = 1, hjust = 1)) +
  theme_bw() + 
  theme(legend.position = "none") +
  labs(
    # title = "Age-adjusted incidence rates by race/ethnicity and diabetes", 
    x = "Race/Ethnicity", y = "Age-adjusted incidence rate per 1,000 person-years") +
  # scale_fill_brewer(palette = "Paired") +
  facet_grid(. ~ plot_grp, scales = "free", space = "free")

# ggsave(paste0(path_to_box,
#               "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/Code/",
#               "Cleaned_Scripts/figures/age_adj_IR_5yr_colored.png"))
# 
# ggsave(
#   paste0(path_to_box,
#          "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/Code/",
#          "Cleaned_Scripts/figures/age_adj_IR_5yr_colored_AJE.tiff"),
#   width = 7, height = 4, units = "in")

# M3 results from Cox PH models (5+yr) ----

cox_hr_M3 <- bind_rows(
  read.xlsx(
    paste0(path_to_output, "main_R1/main_cox_5yr_M3.xlsx"), 
    sheet = 2, 
    colNames = TRUE,
    rows = 1:2
    ),
  
  read.xlsx(
    paste0(path_to_output, "main_R1/main_cox_5yr_M3.xlsx"), 
    sheet = 3, 
    colNames = TRUE
    ) %>% 
    mutate(lowerCI = as.numeric(lowerCI),
           upperCI = as.numeric(upperCI)), 
  
  read.xlsx(
    paste0(path_to_output, "main_R1/main_cox_5yr_M6.xlsx"), 
    sheet = 3, 
    colNames = TRUE
  ) %>% 
    mutate(lowerCI = as.numeric(lowerCI),
           upperCI = as.numeric(upperCI)),
  
  ) %>% 
  mutate(
    ethn = factor(ethn_order, levels = ethn_level), 
    plot_grp = c(rep("All", 2), rep("Asian ethnic groups", 4))
  )

ggplot(cox_hr_M3, aes(x = ethn, y = estimate, color = ethn)) +
  geom_point(size = 2) + 
  geom_errorbar(
    stat = "identity",
    aes(ymin = lowerCI, ymax = upperCI), width = .2,
    position = position_dodge(.9)) + 
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 3.5, by = 0.5)) +
  labs(x = "Race/Ethnicity", 
       y = paste0("Hazard ratio for dementia incidence \n ", 
                  "(Diabetes vs no diabetes assessed 5 years before baseline)")) +
  scale_fill_brewer(palette="Blues") +
  facet_grid(. ~ plot_grp, scales = "free", space = "free")

 

# ---- M3 results from Aalen PH models (5+yr) ----

aalen_hd_M3 <- bind_rows(
  read.xlsx(
    paste0(path_to_output, "main_R1/main_aalen_5yr_M3.xlsx"), 
    sheet = 1, 
    colNames = TRUE,
    rows = 1:2
  ),
  
  read.xlsx(
    paste0(path_to_output, "main_R1/main_aalen_5yr_M3.xlsx"), 
    sheet = 2, 
    colNames = TRUE
  ) %>% 
    mutate(CI_lower = as.numeric(CI_lower),
           CI_upper = as.numeric(CI_upper)), 
  
  read.xlsx(
    paste0(path_to_output, "main_R1/main_aalen_5yr_M6.xlsx"), 
    sheet = 2, 
    colNames = TRUE
  ) %>% 
    mutate(CI_lower = as.numeric(CI_lower),
           CI_upper = as.numeric(CI_upper))
  
  ) %>% 
  mutate(
    ethn = factor(ethn_order, levels = ethn_level), 
    plot_grp = c(rep("All", 2), rep("Asian ethnic groups", 4))
  )

ggplot(aalen_hd_M3, aes(x = ethn, y = results, color = ethn)) +
  geom_point(size = 2) + 
  geom_errorbar(
    stat = "identity",
    aes(ymin = CI_lower, ymax = CI_upper), width = .2,
    position = position_dodge(.9)) + 
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  labs(x = "Race/Ethnicity", 
       y = paste0(
         "Hazard difference for dementia incidence \n", 
         "cases per 1,000 person-years \n", 
         "(Diabetes vs no diabetes assessed 5 years before baseline)")) +
  scale_fill_brewer(palette="Blues") +
  facet_grid(. ~ plot_grp, scales = "free", space = "free")

# ggsave(paste0(path_to_box,
#               "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/Code/",
#               "Cleaned_Scripts/figures/aalen_5_Model3.png"),
#        width = 7, height = 5, units = "in")
# 
# ggsave(paste0(path_to_box,
#               "Asian_Americans_dementia/Manuscripts/AA_ADRD_diabetes/Code/",
#               "Cleaned_Scripts/figures/aalen_5_Model3_AJE.tiff"),
#        width = 7, height = 5, units = "in")
