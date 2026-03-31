candidate_vars <- c("SeasonSWElog", "Mining_2Month", "Fire_2Month", 
                    "Road_2Month", "Needleleaf_2Month", "Veg_2Month")
base_formula <- "Death ~ 0 + Seasons + Seasons:SeasonSWElog + 
                     Mining_2Month + Fire_2Month + Road_2Month + 
                     Needleleaf_2Month + Veg_2Month + 
                     (1 | BioYear) + offset(log(EventTime))"
ctrl <- glmmTMBControl(optCtrl = list(iter.max = 2000, eval.max = 2000))
# ctrl <- glmmTMBControl(optimizer = optim,
#                        optArgs = list(method = "BFGS"))
results <- data.frame(
  Step = character(),
  Structure = character(),
  AIC = numeric(),
  Converged = logical(),
  Message = character(),
  stringsAsFactors = FALSE
)

#running 
print(">>> Running Base Model...")
m_base <- tryCatch({
  glmmTMB(as.formula(base_formula), 
          data = survival_data_no2025, 
          family = binomial(link = "cloglog"), 
          control = ctrl)
}, error = function(e) return(NULL))


print(">>> Step 1: Testing Single Random Slopes...")
for(var in candidate_vars) {
  random_term <- paste0("(0 + ", var, " | CaribouID)")
  current_formula <- paste(base_formula, "+", random_term)
  
  print(paste("Testing:", random_term))
  
  tryCatch({
    m <- glmmTMB(as.formula(current_formula), 
                 data = survival_data_no2025, 
                 family = binomial(link = "cloglog"), 
                 control = ctrl)
    
    results[nrow(results)+1, ] <- list(
      "Step 1", 
      random_term, 
      AIC(m), 
      m$sdr$pdHess, 
      "Single Slope"
    )
  }, error = function(e) {
    results[nrow(results)+1, ] <- list("Step 1", random_term, NA, FALSE, "Error/Did not converge")
  })
}

step1_results <- results %>% filter(Step == "Step 1", Converged == TRUE) %>% arrange(AIC)
best_step1 <- step1_results[1, ]
base_aic <- results[1, "AIC"]

print("--- Step 1 Result ---")
print(head(step1_results))



if(nrow(step1_results) > 0 && (base_aic - best_step1$AIC > 2)) {
  
  winner_var <- gsub(".*\\+ | \\|.*", "", best_step1$Structure)
  print(paste(">>> Step 1 Winner is:", winner_var, ". Proceeding to Step 2..."))

  remaining_vars <- setdiff(candidate_vars, winner_var)
  
  for(var in remaining_vars) {
    
    term_indep <- paste0(best_step1$Structure, " + (0 + ", var, " | CaribouID)")
    form_indep <- paste(base_formula, "+", term_indep)
    
    tryCatch({
      m <- glmmTMB(as.formula(form_indep), data = survival_data_no2025, family = binomial(link="cloglog"), control=ctrl)
      results[nrow(results)+1, ] <- list("Step 2", term_indep, AIC(m), m$sdr$pdHess, "Independent Structure")
    }, error = function(e) {})
    
    term_corr <- paste0("(0 + ", winner_var, " + ", var, " | CaribouID)")
    form_corr <- paste(base_formula, "+", term_corr)
    
    tryCatch({
      m <- glmmTMB(as.formula(form_corr), data = survival_data_no2025, family = binomial(link="cloglog"), control=ctrl)
      results[nrow(results)+1, ] <- list("Step 2", term_corr, AIC(m), m$sdr$pdHess, "Correlated Structure")
    }, error = function(e) {})
  }
  
} else {
  print(">>> No random slope improved the model significantly in Step 1. Stop.")
}

final_ranking <- results %>% 
  filter(Converged == TRUE) %>% 
  arrange(AIC)

print("=== FINAL MODEL RANKING ===")
print(final_ranking)
