# functions used in analysis scripts

# functions fit a cox/aalen model on each imputed data (a mild object)
# given formula as a string
# further combines HRs/AHs from interaction to main effects

require(mitools)

cox_imp <- function(data, formula, terms = NULL) {
  
  # data <- imputed_tte_data_5
  # formula <- model_formulas[1]
  # terms = list(c("DIAB_DX5YR_FLAG", "DIAB_DX5YR_FLAG:ASIAN"))
  
  m <- data %>% length()
  sep_models <- vector(mode = "list", length = m)
  
  for(i in 1:m) {
    sep_models[[i]] <- coxph(as.formula(formula), data = data[[i]])
  }
  
  # pool the models to get a combined summary 
  linear_summary <- pool(sep_models) %>% summary()
  
  hr_summary <- linear_summary %>% 
    mutate(lowerCI = estimate - 1.96*std.error, 
           upperCI = estimate + 1.96*std.error) %>% 
    select(estimate, lowerCI, upperCI) %>% 
    exp() %>% 
    cbind(term = as.character(linear_summary$term), ., 
          p.value = linear_summary$p.value) %>% 
    mutate(p.value = ifelse(p.value < 0.001, 
                            paste("<", 0.001), round(p.value, 3)))
  
  # if terms are specified
  # calculate combined HR and CI given the terms to combine
  
  if (!is.null(terms)) {
    comb_hr <- data.frame(
      term = character(), estimate = numeric(),
      lowerCI = numeric(), upperCI = numeric())
    
    for (j in 1:length(terms)) {
      est <- vector(mode = "list", length = m)
      sd <- vector(mode = "list", length = m)
      var <- vector(mode = "list", length = m)
      
      index <- numeric(2)
      index[1] <- grep(paste0("^", terms[[j]][1], "$"), 
                       names(sep_models[[1]]$coefficients))
      index[2] <- grep(paste0("^", terms[[j]][2], "$"), 
                       names(sep_models[[1]]$coefficients))
      
      for (i in 1:m) {
        est[[i]] <- sep_models[[i]]$coefficients[index] %>% sum()
        var_mat <- sep_models[[i]]$var[index, index]
        sd[[i]] <- sqrt(var_mat[1,1] + var_mat[2,2] + 2*var_mat[1,2])
        var[[i]] <- var_mat[1,1] + var_mat[2,2] + 2*var_mat[1,2]
      }
      
      comb <- MIcombine(est, var)
      comb_hr_est <- exp(comb$coefficients)
      comb_hr_lower <- exp(comb$coefficients - 1.96*sqrt(comb$variance))
      comb_hr_upper <- exp(comb$coefficients + 1.96*sqrt(comb$variance))
      comb_hr <- add_row(comb_hr, 
                         term = terms[[j]][2], 
                         estimate = comb_hr_est, 
                         lowerCI = comb_hr_lower, 
                         upperCI = comb_hr_upper)
      
    }
    
    return(list(linear_summary = linear_summary, 
                hr_summary = hr_summary, 
                comb_hr = comb_hr))
  } else {
    
    return(list(linear_summary = linear_summary, hr_summary = hr_summary))
  }
  
}

####

aalen_imp <- function(data, formula, py = NULL, terms = NULL, seed = 12345) {
  
  set.seed(seed)
  m <- data %>% length()
  
  # fit models 
  sep_models <- data %>% 
    map(aalen, formula = as.formula(formula), start.time = 60, robust = 0)
  aaest <- vector(mode = "list", length = m)
  aavar <- vector(mode = "list", length = m)
  for (i in 1:m) {
    aaest[[i]] <- sep_models[[i]]$gamma %>% t() %>% c()
    aavar[[i]] <- sep_models[[i]]$var.gamma 
  }
  
  # return model summary
  aa_sum <- MIcombine(aaest, aavar) %>% summary() %>% 
    mutate(terms = rownames(sep_models[[1]]$gamma), .before = "results") %>% 
    mutate(t = results / se,
           p = ifelse(2*pnorm(-abs(t)) < 0.001, "<0.001", 
                      round(2*pnorm(-abs(t)), digits = 3))) %>% 
    rename(CI_lower = "(lower", CI_upper = "upper)") %>% 
    select(terms, results, CI_lower, CI_upper, p)
       
  # if terms are specified
  # calculate combined additive hazards and CI given the terms to combine
  
  if (!is.null(terms)) {
    # comb_ah <- tibble(ethnicity = character(), comb_ah = numeric(), 
    #                   CI_lower = numeric(), CI_upper = numeric())
    
    comb_ah <- data.frame(
      terms = character(), results = numeric(), 
      CI_lower = numeric(), CI_upper = numeric())
    
    for (j in 1:length(terms)) {
      target <- vector(length = 2)
      target[1] <- terms[[j]][1] %>% 
        gsub("\\(", "\\\\\\(", .) %>% 
        gsub("\\)", "\\\\\\)", .) %>% 
        paste0("^", ., "$")
      target[2] <- terms[[j]][2] %>% 
        gsub("\\(", "\\\\\\(", .) %>% 
        gsub("\\)", "\\\\\\)", .) %>% 
        paste0("^", ., "$")
      index <- numeric(2)
      index[1] <- grep(target[1], rownames(sep_models[[1]]$gamma))
      index[2] <- grep(target[2], rownames(sep_models[[1]]$gamma))
      
      est <- vector(mode = "list", length = m)
      var <- vector(mode = "list", length = m)
      
      for (i in 1:m) {
        est[[i]] <- sep_models[[i]]$gamma[index] %>% sum()
        var_mat <- sep_models[[i]]$var.gamma[index, index]
        var[[i]] <- var_mat[1,1] + var_mat[2,2] + 2*var_mat[1,2]
      }
      
      comb <- MIcombine(est, var)
      comb_ah_est <- comb$coefficients
      comb_ah_lower <- comb$coefficients - 1.96*sqrt(comb$variance) %>% as.double()
      comb_ah_upper <- comb$coefficients + 1.96*sqrt(comb$variance) %>% as.double()
      comb_ah <- add_row(comb_ah, 
                         terms = terms[[j]][2], 
                         results = comb_ah_est, 
                         CI_lower = comb_ah_lower, 
                         CI_upper = comb_ah_upper)
    }
    
  }
  
  # multiply by person-years if specified
  if (!is.null(py)) {   
    aa_sum <- aa_sum %>% mutate(
      results = results * py, 
      CI_lower = CI_lower * py, 
      CI_upper = CI_upper * py
    )
    
    if (!is.null(terms)) {
      comb_ah <- comb_ah %>% mutate(
        results = results * py,
        CI_lower = CI_lower * py,
        CI_upper = CI_upper * py
        )
    }
    # comb_ah <- comb_ah %>% mutate(
    #   comb_ah = comb_ah * py, 
    #   CI_lower = CI_lower * py, 
    #   CI_upper = CI_upper * py
    #   )
  }
  
  if (is.null(terms)) {
    return(list(aa_summary = aa_sum))
  } else {
    return(list(aa_summary = aa_sum, comb_ah = comb_ah))
  }
  # return(list(aa_summary = aa_sum, comb_ah = comb_ah))
  
}

####
# identical to the aalen_imp function 
# except for specifying start time = 0 in aalen()
aalen_imp_study_time <- function(data, formula, py = NULL, terms = NULL, 
                                 seed = 12345) {
  
  set.seed(seed)
  m <- data %>% length()
  
  # fit models 
  sep_models <- data %>% 
    map(aalen, formula = as.formula(formula), start.time = 0, robust = 0)
  aaest <- vector(mode = "list", length = m)
  aavar <- vector(mode = "list", length = m)
  for (i in 1:m) {
    aaest[[i]] <- sep_models[[i]]$gamma %>% t() %>% c()
    aavar[[i]] <- sep_models[[i]]$var.gamma 
  }
  
  # return model summary
  aa_sum <- MIcombine(aaest, aavar) %>% summary() %>% 
    mutate(terms = rownames(sep_models[[1]]$gamma), .before = "results") %>% 
    mutate(t = results / se,
           p = ifelse(2*pnorm(-abs(t)) < 0.001, "<0.001", 
                      round(2*pnorm(-abs(t)), digits = 3))) %>% 
    rename(CI_lower = "(lower", CI_upper = "upper)") %>% 
    select(terms, results, CI_lower, CI_upper, p)
  
  # if terms are specified
  # calculate combined additive hazards and CI given the terms to combine
  
  if (!is.null(terms)) {
    comb_ah <- tibble(ethnicity = character(), comb_ah = numeric(), 
                      CI_lower = numeric(), CI_upper = numeric())
    
    for (j in 1:length(terms)) {
      target <- vector(length = 2)
      target[1] <- terms[[j]][1] %>% 
        gsub("\\(", "\\\\\\(", .) %>% 
        gsub("\\)", "\\\\\\)", .) %>% 
        paste0("^", ., "$")
      target[2] <- terms[[j]][2] %>% 
        gsub("\\(", "\\\\\\(", .) %>% 
        gsub("\\)", "\\\\\\)", .) %>% 
        paste0("^", ., "$")
      index <- numeric(2)
      index[1] <- grep(target[1], rownames(sep_models[[1]]$gamma))
      index[2] <- grep(target[2], rownames(sep_models[[1]]$gamma))
      
      est <- vector(mode = "list", length = m)
      var <- vector(mode = "list", length = m)
      
      for (i in 1:m) {
        est[[i]] <- sep_models[[i]]$gamma[index] %>% sum()
        var_mat <- sep_models[[i]]$var.gamma[index, index]
        var[[i]] <- var_mat[1,1] + var_mat[2,2] + 2*var_mat[1,2]
      }
      
      comb <- MIcombine(est, var)
      comb_ah_est <- comb$coefficients
      comb_ah_lower <- comb$coefficients - 1.96*sqrt(comb$variance) %>% as.double()
      comb_ah_upper <- comb$coefficients + 1.96*sqrt(comb$variance) %>% as.double()
      comb_ah <- add_row(comb_ah, 
                         ethnicity = terms[[j]][2], 
                         comb_ah = comb_ah_est, 
                         CI_lower = comb_ah_lower, 
                         CI_upper = comb_ah_upper)
    }
    
  }
  
  # multiply by person-years if specified
  if (!is.null(py)) {   
    aa_sum <- aa_sum %>% mutate(
      results = results * py, 
      CI_lower = CI_lower * py, 
      CI_upper = CI_upper * py
    )
    
    comb_ah <- comb_ah %>% mutate(
      comb_ah = comb_ah * py, 
      CI_lower = CI_lower * py, 
      CI_upper = CI_upper * py
    )
  }
  
  return(list(aa_summary = aa_sum, comb_ah = comb_ah))
  
}


####
# OLD ----

# function fits a Cox PH model on imputed dataset 
# given model formula as a string
# output either a summary table of HRs from the model 
# or just output the model coefficients 

# cox_model_imp <- function(data, model, digits = 2, p = 0.001, summary = FALSE) {
#   sum <- data %>% 
#     map(coxph, formula = as.formula(model)) %>%
#     pool() %>% 
#     summary()
#   if(!summary) {
#   } else {
#     sum <- sum %>% 
#       mutate(lowerCI = estimate - 1.96*std.error, 
#              upperCI = estimate + 1.96*std.error) %>% 
#       select(estimate, lowerCI, upperCI) %>% 
#       exp() %>% round(digits) %>% 
#       cbind(term = as.character(sum$term), ., p.value = sum$p.value) %>% 
#       mutate(p.value = ifelse(p.value < p, paste("<", p), round(p.value, 3)))
#   }
#   
#   return(sum)
#   
# }

# calculate combined HR 

# calc_hr <- function(model, terms) {
#   terms[1] <- paste0("^", terms[1], "$")
#   terms[2] <- paste0("^", terms[2], "$")
#   index <- numeric(2)
#   index[1] <- grep(terms[1], names(model$coefficients))
#   index[2] <- grep(terms[2], names(model$coefficients))
#   est <- model$coefficients[index] %>% sum()
#   var_mat <- model$var[index, index]
#   sd <- sqrt(var_mat[1,1] + var_mat[2,2] + 2*var_mat[1,2])
#   hr <- exp(c(est, est - 1.96*sd, est + 1.96*sd))
#   names(hr) <- c("hr", "lower", "upper")
#   return(hr)
# }





# cox_imp <- function(data, formula, terms = NULL) {
#   
#   m <- data %>% length()
#   sep_models <- vector(mode = "list", length = m)
#   for(i in 1:m) {
#     sep_models[[i]] <- coxph(
#       as.formula(formula), data = data[[i]])
#   }
#   
#   # pool the models to get a combined summary 
#   linear_summary <- pool(sep_models) %>% summary()
#   
#   hr_summary <- linear_summary %>% 
#     mutate(lowerCI = estimate - 1.96*std.error, 
#            upperCI = estimate + 1.96*std.error) %>% 
#     select(estimate, lowerCI, upperCI) %>% 
#     exp() %>%  
#     cbind(term = as.character(linear_summary$term), ., 
#           p.value = linear_summary$p.value) %>% 
#     mutate(p.value = ifelse(p.value < 0.001, 
#                             paste("<", 0.001), round(p.value, 3)))
#   
#   # if terms are specified
#   # calculate combined HR and CI given the terms to combine
#   
#   if (!is.null(terms)) {
#     est <- vector(mode = "list", length = m)
#     sd <- vector(mode = "list", length = m)
#     var <- vector(mode = "list", length = m)
#     
#     terms[1] <- paste0("^", terms[1], "$")
#     terms[2] <- paste0("^", terms[2], "$")
#     index <- numeric(2)
#     index[1] <- grep(terms[1], names(sep_models[[1]]$coefficients))
#     index[2] <- grep(terms[2], names(sep_models[[1]]$coefficients))
#     
#     for (i in 1:m) {
#       est[[i]] <- sep_models[[i]]$coefficients[index] %>% sum()
#       var_mat <- sep_models[[i]]$var[index, index]
#       sd[[i]] <- sqrt(var_mat[1,1] + var_mat[2,2] + 2*var_mat[1,2])
#       var[[i]] <- var_mat[1,1] + var_mat[2,2] + 2*var_mat[1,2]
#     }
#     
#     comb <- MIcombine(est, var)
#     comb_hr_est <- exp(comb$coefficients)
#     comb_hr_lower <- exp(comb$coefficients - 1.96*sqrt(comb$variance))
#     comb_hr_upper <- exp(comb$coefficients + 1.96*sqrt(comb$variance))
#     comb_hr <- tibble(comb_hr_est, comb_hr_lower, comb_hr_upper)
#     
#     return(list(linear_summary = linear_summary, 
#                 hr_summary = hr_summary, 
#                 comb_hr = comb_hr))
#   } else {
#     
#     return(list(linear_summary = linear_summary, hr_summary = hr_summary))
#   }
#   
# }



