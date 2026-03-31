library(glmmTMB)
library(pROC)  
library(dplyr) 
library(MuMIn)


realdata <- survival_data_no2025[!is.na(survival_data_no2025$Veg_2Month),]
m_global <- glmmTMB(Death ~ Seasons + Seasons:SeasonSWElog + 
                      Mining_2Month + Fire_2Month + Road_2Month + 
                      Needleleaf_2Month + Veg_2Month + 
                      (0 + SeasonSWElog | CaribouID) + 
                      (0 + Veg_2Month | CaribouID) +
                      (1|BioYear),
                    offset = log(EventTime), 
                    family = binomial(link = "cloglog"), 
                    data = realdata,
                    na.action = "na.fail")


get_AUC <- function(model) {
  preds <- predict(model, type = "response")
  obs <- model$frame$Death 
  r <- roc(obs, preds, quiet = TRUE)
  return(as.numeric(r$auc))
}


selection_table <- dredge(m_global, 
                          rank = "AIC", 
                          fixed = c("cond(Seasons)", "cond(Seasons:SeasonSWElog)"),
                          extra = c(AUC = get_AUC))





print(head(selection_table))


sorted_by_auc <- selection_table[order(selection_table$AUC, decreasing = TRUE), ]
print(head(sorted_by_auc))
