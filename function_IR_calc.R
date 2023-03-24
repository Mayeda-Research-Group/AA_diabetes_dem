# functions to calculate age adjusted IRs


# data being supplied should have
# ID: id variable
# fu_start: age at start of followup
# fu_end: age at end of followup regardless of event status
# event_flag: whether end of followup is due to event, 1 = event, 0 = others
# wt: optional weight variable

# age categories should be supplied as a vector
# e.g. age_cat <- c(59, 65, 70, 75, 80, 85, 120)

# standard population by age category should be supplied as a vector
# length being the same as the number of age categories of interest
# pop <- c(10805447, 9533545, 8857441, 7415813, 4945367, 4239587)

# function that calculates person-years and case contribution by age category 
calculate_pys <- function(data, age_cat, fu_start, fu_end, event_flag) {
  # testing
  # data <- rpgeh_dem
  # age_cat <- c(59, 65, 70, 75, 80, 85, 120)
  # fu_start <- "SURVEY_AGE"
  # fu_end = "MAIN_DEM_V1_END_AGE"
  # event_flag <- "MAIN_DEM_V1_END_DEM_FLAG"
  
  age_cat_labels <- c()
  
  for (i in 1:(length(age_cat) - 1)) {
    # testing 
    # i <- 1
    
    # initialize the age interval
    cat_start <- age_cat[i]
    cat_end <- age_cat[i+1]
    # create the variable names for py and case contribution
    age_int <- paste0(cat_start, "_", cat_end)
    vars <- paste0(c("py_", "case_"), age_int)
    py_var <- vars[1]
    case_var <- vars[2]
    
    # collect the age intervals for setting up the IR output table
    age_cat_labels <- c(age_cat_labels, age_int)
    
    # calculate py and case contribution by age category
    data <- data %>% 
      mutate(
        !! py_var := case_when(
          # start of fu after age category or end of fu before age category: 
          # contribute 0 pys
          get(fu_start) > cat_end | get(fu_end) <= cat_start ~ 0, 
          
          # start of fu before age category and end of fu during age category: 
          get(fu_start) <= cat_start & get(fu_end) <= cat_end ~ 
            get(fu_end) - cat_start, 
          
          # start of fu before age category and end of fu after age category: 
          get(fu_start) <= cat_start & get(fu_end) > cat_end ~ 
            cat_end - cat_start, 
          
          # start and end of fu during age category:
          get(fu_start) <= cat_end & get(fu_end) <= cat_end ~ 
            get(fu_end) - get(fu_start), 
          
          # start of fu during age category and end of fu after age cateogory:
          get(fu_start) <= cat_end & get(fu_end) > cat_end ~ 
            cat_end - get(fu_start), 
          
          TRUE ~ NA_real_),  
        
        # if end of fu is during age category, case contribution is the dem flag. 
        # otherwise, case contribution is zero
        !! case_var := ifelse(
          get(fu_end) <= cat_end & get(fu_end) > cat_start, 
          get(event_flag), 0
        )
      )
  }
  
  return(list(data = data, age_cat_labels = age_cat_labels))
}

# function that calculates age-adjusted incidence rates 
# given data with pys and cases (generated from calculate_pys())

calculate_ir <- function(data, age_cat_labels, std_pop, diab_exposure, 
                         per_pys = 1000) {
  # data <- pys_5$data
  # age_cat_labels <- pys_5$age_cat_labels
  # std_pop <- c(10805447, 9533545, 8857441, 7415813, 4945367, 4239587)
  # specify diabetes defn and for whom to calculate IR's (yes/no diabetes) 
  # diab_exposure <- c("DIAB_DX5YR_FLAG", 1)

  data <- data %>% 
    # filter by specification
    filter(get(diab_exposure[1]) == diab_exposure[2]) %>% 
    # split the data by ethn
    split(., .$ETHNICITY_REV)
  
  asian <- do.call(rbind, data[2:5])
  # append Asian into the data list
  data <- c(data, list("Asian" = asian)) 
  # order the race/ethnicities 
  data <- data[c("White", "Asian", "Chinese", "Japanese", "Filipino", "South Asian")]

  out <- lapply(data, function(x) {
    # using 2000 US census population as the standard population
    # set up age-specific IR table
    age_spec_ir <- tibble(
      age_range = age_cat_labels, 
      pop_count = std_pop, # std pop - count by age category
      pop_total = sum(pop_count), # std pop total 
      pop_prop = pop_count / pop_total, # std pop prop by age category
      unw_pys = NA, 
      unw_cases = NA
    )
    
    for (i in 1:length(age_cat_labels)) {
      # sum up pys and cases by age category
      cat <- age_cat_labels[i]
      age_spec_ir[i, "unw_pys"] <- sum(x[, paste0("py_", cat)]) 
      age_spec_ir[i, "unw_cases"] <- sum(x[, paste0("case_", cat)])
    }
    
    # age specific IRs
    age_spec_ir <- age_spec_ir %>% 
      mutate(
        unw_adj_ir = unw_cases / unw_pys,
        unw_adj_ir_se = sqrt(unw_cases / unw_pys^2),
        unw_adj_ir_CI_l = unw_adj_ir - 1.96 * unw_adj_ir_se,
        unw_adj_ir_CI_u = unw_adj_ir + 1.96 * unw_adj_ir_se
      ) 
    
    # age adjusted IR
    ir_out <- age_spec_ir %>% 
      mutate(
        # weight with standard pop prop
        unw_adj_ir_pop = unw_cases / unw_pys * pop_prop,
        unw_adj_ir_pop_var = unw_cases / unw_pys^2 * pop_prop^2
      ) %>% 
      summarise(
        across(c(unw_cases, unw_pys), sum), 
        adj_ir = sum(unw_adj_ir_pop),
        adj_ir_var = sum(unw_adj_ir_pop_var)) %>% 
      mutate(
        adj_ir_se = sqrt(adj_ir_var),
        adj_ir_l = adj_ir - 1.96 * adj_ir_se,
        adj_ir_u = adj_ir + 1.96 * adj_ir_se
      ) 
    
    # format the numbers to 2 decimal points and per 1000 pys
    out_1 <- age_spec_ir %>% 
      select(age_range, unw_cases, 
             unw_adj_ir, unw_adj_ir_CI_l, unw_adj_ir_CI_u) %>% 
      mutate(across(-c(age_range,unw_cases), 
                    function(x) round_pad(x*per_pys, 2)))
    
    out_2 <- ir_out %>% 
      select(unw_cases, unw_pys, adj_ir, adj_ir_l, adj_ir_u) %>% 
      mutate(across(c(adj_ir, adj_ir_l, adj_ir_u), 
                    function(x) round_pad(x*per_pys, 2)))
    
    return(list("age_spec" = out_1, "age_adj" = out_2))
  }
  )
  
  return(out)
  
}


