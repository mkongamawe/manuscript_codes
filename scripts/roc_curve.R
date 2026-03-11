library(pROC)
library(ggtext)
### this script generates ROC curve for combined SBP and DBP measurements
## uses generalized linear model to combine SBP and DBP (can also modify to generate separate plots for SBP and DBP
#     for each measurment method )
## use updated_combined_data dataset. Remember to limit to individuals >18 years
# Prepare combined ROC data with AUCs and CIs

# Map measurement types to variable names
     bp_vars <- list(
         Clinic    = c("spot_bp_sys",       "spot_bp_dys"),
         `24hr`    = c("idaco_sbpbr_avg",   "idaco_dia_avg"),
         Daytime   = c("idaco_day_sbpbr",   "idaco_day_dia"),
         Nighttime = c("idaco_night_sbpbr", "idaco_night_dia")
     )

roc_data <- map_df(names(bp_vars), function(label) {
    vars <- bp_vars[[label]]
    df <- updated_combined_data %>%
        select(all_of(c(vars[1], vars[2], "vital_status"))) %>%
        filter(!is.na(!!sym(vars[1])), !is.na(!!sym(vars[2])))
    
    df$truth <- as.numeric(df$vital_status == "died")
    
    # Combined model (SBP + DBP)
    model <- glm(truth ~ get(vars[1]) + get(vars[2]), data = df, family = binomial)
    probs <- predict(model, type = "response")
    roc_obj <- roc(df$truth, probs, quiet = TRUE)
    
    # Get AUC with 95% CI
    auc_ci <- ci.auc(roc_obj)
    
    # Extract ROC coordinates and AUC/CI
    tibble(
        FPR = 1 - roc_obj$specificities,
        TPR = roc_obj$sensitivities,
        Measurement = label,
        AUC = round(auc(roc_obj), 3),
        AUC_text = sprintf("%.2f (%.2f,%.2f)", 
                           auc_ci[2], auc_ci[1], auc_ci[3])
    )
})

# Plot with CIs in legend
ggplot(roc_data, aes(x = FPR, y = TPR, 
                     color = paste0(Measurement, ": ", AUC_text))) +
    geom_line(linewidth = 1.2) +
    geom_abline(linetype = "dashed", color = "gray50") +
    labs(
        x = "1 - Specificity (FPR)",
        y = "Sensitivity (TPR)",
        color = "Measurement Type (AUC,95% CI)"
    ) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "white", colour = NA), # Use colour=NA for no border
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey70", linewidth = 0.4), # Use linewidth
      panel.grid.minor = element_line(colour = "grey70", linewidth = 0.3, linetype = "dotted"),
      axis.line = element_line(colour = "black", linewidth = 0.5),
      axis.text = element_markdown(colour = "black", size = 20), # Reduced size for combined plot
      axis.title = element_text(colour = "black", size = 30), # Reduced size
      legend.title = element_text(colour = "black", size = 18),
      legend.text = element_text(colour = "black", size = 15),
      legend.position.inside = c(1, 0.5),
      plot.title = element_text(face = "bold", colour = "black", size = 30, hjust = 0.5), # Style individual plot titles
      axis.title.x = element_text(margin = margin(t = 10), size = 30),
      axis.title.y = element_text(margin = margin(r = 10), size = 30),
      axis.ticks = element_line(colour = "grey20", linewidth = 0.2),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm") # Adjusted margins
    )

ggsave("roc_plot_CIs.png",
       width = 20, height = 12,
       path = "results/figures",
       dpi = 600)

