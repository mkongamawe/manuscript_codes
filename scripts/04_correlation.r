library(patchwork)
library(ggpubr)
library(ggcorrplot)
library(ggtext)
library(gt)
###############################################################################

correlation_result <- as.data.frame(
  (round(
    cor(
      updated_combined_data[,
        c(
          "spot_bp_sys",
          "idaco_sbpbr_avg",
          "idaco_day_sbpbr",
          "idaco_night_sbpbr",
          "spot_bp_dys",
          "idaco_dia_avg",
          "idaco_day_dia",
          "idaco_night_dia"
        )
      ],
      use = "complete.obs"
    ),
    2
  ))
)


correlation_result %>%
  gt(rownames_to_stub = TRUE) %>%
  text_case_match(
    "spot_bp_sys" ~ "Clinic SBP",
    "idaco_sbpbr_avg" ~ "24 - hour SBP",
    "idaco_day_sbpbr" ~ "Daytime SBP",
    "idaco_night_sbpbr" ~ "Nighttime SBP",
    "spot_bp_dys" ~ "Clinic DBP",
    "idaco_dia_avg" ~ "24 - hour DBP",
    "idaco_day_dia" ~ "Daytime DBP",
    "idaco_night_dia" ~ "Nighttime DBP",
    .locations = cells_stub()
  ) %>%
  cols_label(
    spot_bp_sys = "Clinic SBP",
    idaco_sbpbr_avg = "24 - hour SBP",
    idaco_day_sbpbr = "Daytime SBP",
    idaco_night_sbpbr = "Nighttime SBP",
    spot_bp_dys = "Clinic DBP",
    idaco_dia_avg = "24 - hour DBP",
    idaco_day_dia = "Daytime DBP",
    idaco_night_dia = "Nighttime DBP"
  ) %>%
  tab_header(
    title = md("**The Correlation of blood pressure indices**")
  ) %>%
  tab_footnote(
    footnote = "SBP - Systolic Blood pressure, DBP - Diastolic Blood Pressure",
    locations = cells_stub(rows = c(1, 5))
  )
##########################################################################################
#Correlation plot.

# #relaballing columns
custom_labels <- list(
  spot_bp_sys = "Clinic SBP",
  spot_bp_dys = "Clinic DBP",
  idaco_day_sbpbr = "Daytime SBP",
  idaco_day_dia = "Daytime DBP",
  idaco_night_sbpbr = "Nighttime SBP",
  idaco_night_dia = "Nighttime DBP",
  idaco_sbpbr_avg = "24h SBP",
  idaco_dia_avg = "24h DBP",
  sex = "Sex",
  on_med_htn = "Antihypertensive Drug",
  current_smoke = "Current Smoking Status",
  idaco_bp_phenotype = "Hypertension Phenotypes"
)

ggcorrplot(
  correlation_result,
  type = "upper",
  lab = TRUE, # Show the correlation values on the plot
  lab_size = 15,
  outline.color = "black",
  ggtheme = theme_minimal()
) +
  scale_fill_gradient2(
    low = "#00AFBB",
    high = "#FC4E07",
    mid = "white",
    midpoint = 0.5,
    limit = c(0, 1),
    space = "Lab",
  ) +
  scale_x_discrete(labels = custom_labels) +
  scale_y_discrete(labels = custom_labels) +
  theme(
    # Set overall plot appearance
    plot.background = element_rect(fill = "oldlace"),
    panel.background = element_rect(fill = "oldlace"),
    panel.grid.major = element_blank(), #colour = "grey70", size = 0.4
    panel.grid.minor = element_blank(),

    # Set axis appearance
    axis.line = element_line(colour = "black", size = 0.5),
    axis.text.x = element_markdown(colour = "black", size = 25),
    axis.text.y = element_markdown(colour = "black", size = 25),

    # Set legend appearance
    legend.background = element_rect(
      fill = "oldlace",
      linewidth = 1,
      colour = "#e7e7d8"
    ),
    legend.title = element_blank(), #nolint
    legend.text = element_text(colour = "black", size = 15),
    legend.key = element_blank(),
    legend.key.size = unit(1, "cm"),
    legend.position = c(1, 0.5),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),

    #Set strip text appearance
    strip.background = element_rect(fill = "oldlace"),
    strip.text = element_markdown(face = "bold", colour = "black", size = 18),

    # Set title appearance
    plot.title = element_markdown(
      face = "bold",
      size = 25, #nolint
      hjust = 0.5,
      vjust = -1
    ),
    plot.subtitle = element_markdown(
      face = "bold",
      size = 18, #nolint
      hjust = 0.5,
      vjust = -1
    ),
    plot.caption = element_markdown(
      face = "bold",
      size = 8, #nolint
      hjust = 1
    ),

    # Set plot margins
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  labs(
    title = "Correlation of blood pressure indices."
  )

ggsave(
  "Correlation_matrix.png",
  path = "results/figures",
  height = 20,
  width = 20,
  dpi = 400
)

################################
# -- Compare correlation after residualisation
###############################
# Define common theme to avoid repetition
common_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "grey70", linewidth = 0.4),
    panel.grid.minor = element_line(
      colour = "grey70",
      linewidth = 0.3,
      linetype = "dotted"
    ),
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.text = element_markdown(colour = "black", size = 20),
    axis.title = element_text(colour = "black", size = 30),
    legend.title = element_text(colour = "black", size = 18),
    legend.text = element_text(colour = "black", size = 15),
    legend.position.inside = c(1, 0.5),
    plot.title = element_text(
      face = "bold",
      colour = "black",
      size = 30,
      hjust = 0.5
    ),
    plot.caption = element_markdown(
      colour = "black",
      size = 20,
      hjust = 1,
      vjust = 0.9
    ),
    axis.title.x = element_text(margin = margin(t = 10), size = 30),
    axis.title.y = element_text(margin = margin(r = 10), size = 30),
    axis.ticks = element_line(colour = "grey20", linewidth = 0.2),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

# Define the pairs of variables to plot
var_pairs <- list(
  list(
    y_var = "idaco_sbpbr_avg",
    res_var = "idaco_sbpbr_avg_res1",
    title = "Correlation between Clinic SBP and 24h SBP",
    y_label = "24h SBP (mmHg)",
    res_title = "Residuals from regression of 24h SBP on Clinic SBP",
    res_y_label = "Residuals of 24h SBP"
  ),
  list(
    y_var = "idaco_day_sbpbr",
    res_var = "idaco_day_sbpbr_res1",
    title = "Correlation between Clinic SBP and Daytime SBP",
    y_label = "Daytime SBP (mmHg)",
    res_title = "Residuals from regression of Daytime SBP on Clinic SBP",
    res_y_label = "Residuals of Daytime SBP"
  ),
  list(
    y_var = "idaco_night_sbpbr",
    res_var = "idaco_night_sbpbr_res1",
    title = "Correlation between Clinic SBP and Nighttime SBP",
    y_label = "Nighttime SBP (mmHg)",
    res_title = "Residuals from regression of Nighttime SBP on Clinic SBP",
    res_y_label = "Residuals of Nighttime SBP"
  )
)

# Initialize a list to store plots
plot_list <- list()

# Loop through each variable pair
for (i in seq_along(var_pairs)) {
  # Extract variables and labels
  y_var <- var_pairs[[i]]$y_var
  res_var <- var_pairs[[i]]$res_var
  title <- var_pairs[[i]]$title
  y_label <- var_pairs[[i]]$y_label
  res_title <- var_pairs[[i]]$res_title
  res_y_label <- var_pairs[[i]]$res_y_label

  # Calculate Pearson correlation for main plot
  cor_coefs <- cor.test(
    updated_combined_data$spot_bp_sys,
    updated_combined_data[[y_var]],
    method = "pearson"
  )

  # Create main scatter plot
  p <- updated_combined_data %>%
    ggplot(aes(x = spot_bp_sys, y = .data[[y_var]])) +
    geom_point(colour = "blue") +
    geom_smooth(method = "lm", color = "#d70505") +
    labs(
      title = title,
      caption = paste(
        "Pearson Correlation: r =",
        round(cor_coefs$estimate, 2),
        "\nP-value:",
        format.pval(cor_coefs$p.value, eps = 0.01, digits = 2)
      ),
      x = "Clinic SBP (mmHg)",
      y = y_label
    ) +
    common_theme

  # Calculate Pearson correlation for residual plot
  cor_coefs_res <- cor.test(
    updated_combined_data$spot_bp_dys,
    updated_combined_data[[res_var]],
    method = "pearson"
  )

  # Create residual scatter plot
  q <- updated_combined_data %>%
    ggplot(aes(x = spot_bp_dys, y = .data[[res_var]])) +
    geom_point(colour = "blue") +
    geom_smooth(method = "lm", color = "#d70505") +
    labs(
      title = res_title,
      caption = paste(
        "Pearson Correlation: r =",
        round(cor_coefs_res$estimate, 2),
        "\nP-value:",
        format.pval(cor_coefs_res$p.value, eps = 0.01, digits = 2)
      ),
      x = "Clinic SBP (mmHg)",
      y = res_y_label
    ) +
    common_theme

  # Store the pair of plots
  plot_list[[paste0("p_", i)]] <- p
  plot_list[[paste0("q_", i)]] <- q
}

# Combine plots using patchwork: each pair side by side, pairs stacked vertically
combined_plot <- (plot_list$p_1 + plot_list$q_1) /
  (plot_list$p_2 + plot_list$q_2) /
  (plot_list$p_3 + plot_list$q_3)

# Display the combined plot
# combined_plot

# Optionally, save the combined plot
ggsave(
  "correlation_SBP_plots.png",
  path = "results/figures",
  combined_plot,
  width = 30,
  height = 40,
  dpi = 300
)
