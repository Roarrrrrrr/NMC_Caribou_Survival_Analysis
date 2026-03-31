library(scales)

raw_min <- min(survival_data_no2025$SeasonSWElog, na.rm=TRUE)
raw_max <- max(survival_data_no2025$SeasonSWElog, na.rm=TRUE)


limit <- min(abs(raw_min), abs(raw_max))

plot_limit <- limit 


swe_range <- seq(-plot_limit, plot_limit, length.out = 400)

new_data <- expand.grid(
  SeasonSWElog = swe_range,
  Seasons = unique(survival_data_no2025$Seasons)
)

new_data$Mining_60d <- 0
new_data$Fire_60d <- 0
new_data$Road_60d <- 0
new_data$Needleleaf_60d <- 0
new_data$Veg_60d <- 0
new_data$EventTime <- 1

preds <- predict(windowglmm, newdata = new_data, se.fit = TRUE, type = "link", re.form = NA)
new_data$fit_link <- preds$fit
new_data$se_link <- preds$se.fit

new_data$lower_link <- new_data$fit_link - 1.96 * new_data$se_link
new_data$upper_link <- new_data$fit_link + 1.96 * new_data$se_link

inv_cloglog <- function(x) { 1 - exp(-exp(x)) }
new_data$Risk <- inv_cloglog(new_data$fit_link)
new_data$Risk_Lower <- inv_cloglog(new_data$lower_link)
new_data$Risk_Upper <- inv_cloglog(new_data$upper_link)

new_data$Season_Simple <- case_when(
  grepl("Winter", new_data$Seasons) ~ "Winter",
  grepl("Spring", new_data$Seasons) ~ "Spring",
  grepl("Summer", new_data$Seasons) ~ "Summer",
  grepl("Fall", new_data$Seasons) ~ "Fall"
)
new_data$Season_Simple <- factor(new_data$Season_Simple, 
                                 levels = c("Summer", "Fall", "Winter", "Spring"))

cb_palette <- c(
  "Winter" = "#0072B2", "Spring" = "#009E73", 
  "Summer" = "#D55E00", "Fall"   = "#E69F00"
)


my_x_limit <- 3  
y_min <- 1e-7
y_max <- 0.05

p <- ggplot(new_data, aes(x = SeasonSWElog, y = Risk, color = Season_Simple, fill = Season_Simple)) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", size = 0.8) +
  
  geom_ribbon(aes(ymin = Risk_Lower, ymax = Risk_Upper), alpha = 0.12, color = NA) +
  geom_line(size = 1.5) +

  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x, n = 4),
    labels = trans_format("log10", math_format(10^.x)),
    expand = c(0, 0) 
  ) +
  
  scale_x_continuous(
    breaks = c(-2.5, 2.5), 
    labels = c("Shallow Snow", "Deep Snow")
  ) +
  
  scale_color_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +
  
  annotate("text", x = 0.1, y = 5e-3, label = "Seasonal Average\n(Baseline)", 
           hjust = 0, color = "grey50", size = 4, fontface = "italic") +
  
  labs(
       y = "Daily Probability of Mortality (log)",
       x = "Snow Depth", 
       color = "Season", fill = "Season") +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    
    axis.text.x = element_text(size = 12,color = "black", margin = margin(t = 5)),
    axis.text.y = element_text(size = 10,color = "black"),
    
    plot.title = element_text(face = "bold")
  ) +
  

  coord_cartesian(
    ylim = c(y_min, y_max), 
    xlim = c(-my_x_limit, my_x_limit) 
  )

print(p)
ggsave("Winter_Refuge_Interaction.jpeg", p, width = 9, height = 6, dpi = 300)
