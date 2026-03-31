library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(data.table)
library(broom.mixed)
library(pROC)
library(emmeans)
library(ggeffects)
library(ggplot2)
library(effects)
library(gridExtra)
library(cowplot)
#library(patchwork)


windowglmm <- glmmTMB(Death ~ Seasons +
                        Seasons:SeasonSWElog+
                        Mining_2Month +
                        #Anthro_2Month +
                        Fire_2Month +
                        Road_2Month +
                        #Lfother_2Month +
                        Needleleaf_2Month +
                        Veg_2Month +
                        (0+SeasonSWElog|CaribouID) +
                        (0+Veg_2Month|CaribouID)+
                        (1|BioYear) + 
                        offset(log(EventTime)),
                      data=survival_data_no2025, family = binomial(link="cloglog")
)

model <- windowglmm

seasonal_comparisons <- emmeans(model, specs = pairwise ~ next_Seasons)
print(seasonal_comparisons)
plot(seasonal_comparisons, ylab="Seasons")
emm <- emmeans(model, ~ Seasons)
emm_adj <- as.data.frame(confint(emm, adjust = "sidak"))
emm_adj$Seasons <- reorder(emm_adj$Seasons, emm_adj$emmean)
emm_adj <- emm_adj %>%
  mutate(
    Season_Label = factor(Seasons,
                          #levels = c("2_Spring", "3_Summer", "4_Fall", "1_Winter"),
                          levels = c("1_Winter","4_Fall", "3_Summer", "2_Spring"),
                          #labels = c("Spring", "Summer", "Fall", "Winter"),
                          labels = c("Winter", "Fall", "Summer", "Spring"))
  )
emmeans_adjusted_plot <- ggplot(emm_adj, aes(y = Season_Label, x = emmean)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+
  geom_pointrange(aes(xmin = asymp.LCL, xmax = asymp.UCL), linewidth = 2, color = "#B0B0F0", alpha=0.5) +
  geom_point(size = 2, color = "black") +
  labs(
    title = "Seasonal Mortality Risk",
    x = "Mortality Risk (Log-Odds Scale)",
    y = "" 
  ) +

  annotate(
    "text",
    x = -6.5, y = 4.2, # Position text to the right of the line
    label = "Higher\nMortality\nRisk",
    color = "darkorange",
    fontface = "bold",
    hjust = 0 # Left-justify the text
  ) +

  annotate(
    "text",
    x = -11, y = 4.2, # Position text to the left of the line
    label = "Lower\nMortality\nRisk",
    color = "deepskyblue4",
    fontface = "bold",
    hjust = 1 # Right-justify the text
  )

emmeans_adjusted_plot
ggsave(
  filename = "seasonal_emmeans_plot_adjusted.png",
  plot = emmeans_adjusted_plot,
  width = 7,
  height = 6,
  dpi = 500
)



model <- windowglmm
summary(model)
tidy(model)
res_mod <- simulateResiduals(fittedModel = model, plot=F)
plot(res_mod, quantreg = T, smoothScatter=F)
## this sentence for plotting residuals against single variable
clean_data <- survival_data %>%
  drop_na(Mining_3Month, Harv_3Month, Fire_3Month, Road_3Month, Lfother_3Month, Needleleaf_3Month, Veg_3Month)
plotResiduals(res_mod, form = clean_data$Mining_3Month, quantreg = T, smoothScatter=F, rank=T)
plotResiduals(res_mod, form = clean_data$Harv_3Month, quantreg = T, smoothScatter=F, rank=T)
plotResiduals(res_mod, form = clean_data$Fire_3Month, quantreg = T, smoothScatter=F, rank=T)
plotResiduals(res_mod, form = clean_data$Road_3Month, quantreg = T, smoothScatter=F, rank=T)
plotResiduals(res_mod, form = clean_data$Lfother_3Month, quantreg = T, smoothScatter=F, rank=T)
plotResiduals(res_mod, form = clean_data$Needleleaf_3Month, quantreg = T, smoothScatter=F, rank=T)
plotResiduals(res_mod, form = clean_data$Veg_3Month, quantreg = T, smoothScatter=F, rank=T)
## Some other evaluations
testDispersion(res_mod)
testZeroInflation(res_mod)
diagnose(fit)
testOutliers(res_mod, type = 'bootstrap')
performance::check_collinearity(model)
performance::check_singularity(model)
qqnorm(res_mod$scaledResiduals)
qqline(res_mod$scaledResiduals, col = "red")



outliers(res_mod, return = "logical")
clean_data <- survival_data %>%
  drop_na(Mining_3Month, Harv_3Month, Fire_3Month, Road_3Month, Lfother_3Month, Needleleaf_3Month, Veg_3Month)
new <- clean_data[!outliers(res_mod, return = "logical"),]

summary(betafit)
res <- simulateResiduals(fittedModel = betafit, plot=F)
plot(res, quantreg = T, smoothScatter=F)
testDispersion(res)
testZeroInflation(res)
diagnose(betafit)
testOutliers(res, type = 'bootstrap')

