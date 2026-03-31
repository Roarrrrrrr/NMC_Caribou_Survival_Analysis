library(survival)
library(tidyverse)
library(coxme)
library(survminer)
library(broom)
library(lubridate)


coxme_model <- coxme(
  Surv(DayStart, DayStop, Death) ~ Seasons+
    Mining_2Month + 
    #Harv_2Month +
    # Anthro_3Month +
    Fire_2Month +
    Road_2Month + 
    #Lfother_3Month +
    Needleleaf_2Month +
    Veg_2Month+
    Seasons:SeasonSWElog +
    (1 |BioYear) +
    (0+SeasonSWElog|CaribouID)+
    (0+Veg_2Month|CaribouID),
  data = survival_data_no2025
)


windowcph <- coxph(
  Surv(DayStart, DayStop, Death) ~
    Mining_2Month + 
    #Harv_2Month +
    # Anthro_3Month +
    Fire_2Month +
    Road_2Month + 
    #Lfother_3Month +
    Needleleaf_2Month +
    Veg_2Month+
    Seasons +
    Seasons:SeasonSWElog +
    #SWElog+
    strata(BioYear),
  cluster = CaribouID,
  data = ssurvival_data_no2025
)

cphmodel <- windowcph

summary(cphmodel)
ggcoxdiagnostics(model)
surv.at.means <- survfit(model)
plot(surv.at.means, xlab="seasons", ylab="survival probability")
ggsurvplot(surv.at.means, data=survival_data, censor=F,
           conf.int = F,             
           conf.int.style = "ribbon")

survival_duration_data <- survival_data_no2025 %>%
  group_by(CaribouID, area) %>% # Group by both id and area
  summarise(
    # Find the collaring time for this specific individual
    collaring_time = min(tstart),
    # Find the last known time (death or last contact)
    last_known_time = max(tstop),
    event = sum(Event),
    # This tells summarise to drop the grouping structure after it's done
    .groups = 'drop'
  ) %>%
  # Now, calculate the survival duration in days for each individual
  mutate(
    duration_days = as.numeric(
      last_known_time-collaring_time)
    )
survival_duration_data <- survival_data %>%
  group_by(CaribouID, area) %>% # Group by both id and area
  summarise(
    event = sum(Event),
    duration_days = as.numeric(sum(EventTime)),
    # This tells summarise to drop the grouping structure after it's done
    .groups = 'drop'
  )



KM.area <- survfit(
  Surv(duration_days, event) ~ area,
  data = survival_duration_data
)

ggsurvplot(
  KM.area,
  data = survival_duration_data,
  #conf.int = TRUE,           # Show 95% confidence intervals
  #conf.int.style = "ribbon", # Use a shaded ribbon for CIs (clearer than lines)
  #risk.table = TRUE,         # Show a table of how many animals are at risk over time
  censor = FALSE,            # Hides the '+' marks for censored data for a cleaner look
  #palette = "jco",           # Use a nice color palette (from ggsci package)
  #ggtheme = theme_bw()       # Use a clean black and white theme
)

KM <- survfit(Surv(duration_days, event) ~ 1, data=survival_duration_data)
KM.tab <- tidy(KM) 
plot(KM, ylab="survival probability", xlab="days")
ggsurvplot(KM, conf.int=T)

KM <- survfit(Surv(tstart, tstop, Event) ~ 1, data=survival_data)
KM.tab <- tidy(KM) 
plot(KM, ylab="survival probability", xlab="days")
ggsurvplot(KM, conf.int=T)

assumption_test <- cox.zph(cox_model)
print(assumption_test)


# Create a binned variable for visualization
vis_data <- survival_data %>%
  mutate(lf_proximity = ifelse(dist_lf_other < 100, "Near Road", "Far From Road"))

# Fit a new model for visualization
vis_model <- survfit(
  Surv(tstop, event) ~ lf_proximity,
  data = vis_data,
  id = CaribouID # Specify id for correct curve calculation with repeated measures
)

# Plot the survival curves
plot(vis_model,
     col = c("blue", "red"),
     xlab = "Time (days)",
     ylab = "Survival Probability",
     main = "Caribou Survival Probability by Road Proximity")
legend("bottomleft",
       legend = c("Near Road", "Far From Road"),
       col = c("blue", "red"),
       lty = 1)


deathdata <- survival_data %>%
  group_by(CaribouID) %>%
  mutate(
    event = case_when(
    sum(event)==1~1,
    TRUE~0
  )) %>%
  ungroup()
deathdata <- deathdata[deathdata$event==1,]

survival_summary <- deathdata %>%
  group_by(CaribouID, area) %>% # Group by both id and area
  summarise(
    # Find the earliest start time (collaring time) for each id
    collaring_time = min(t1),
    
    # Find the latest end time (death time) for each id
    death_time = max(t2),
    
    # This tells summarise to keep the group structure, which is good practice
    .groups = 'drop'
  ) %>%
  # Now, calculate the difference between death and collaring
  mutate(
    survival_duration_days = as.numeric(
      difftime(death_time, collaring_time, units = "days")
    )
  ) %>%
  # Optional: Reorder columns for a nice, clean output
  select(CaribouID, area, collaring_time, death_time, survival_duration_days)


survival_data <- survival_data %>%
  mutate(
    TriSeasons = case_when(
      Seasons %in% c("1_Midwinter", "2_Latewinter", "6_Earlywinter") ~ "cold",
      Seasons %in% c("3_Spring", "5_Fall") ~ "transition",
      Seasons == "4_Summer" ~ "warm",
      TRUE ~ NA_character_
    )
  )

