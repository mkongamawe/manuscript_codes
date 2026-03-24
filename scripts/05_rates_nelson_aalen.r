library(tidyverse)
library(gt)
library(survival)
library(survminer)
library(ggpubr)
library(patchwork)
library(ggplotify)
library(gridExtra)

# Define BP categorization functions for each definition
bp_definitions <- list(
  "ESC" = function(data) {
    data %>%
      mutate(
        clinic_cat = case_when(
          spot_bp_sys < 120 & spot_bp_dys < 70 ~ "Normal",
          spot_bp_sys >= 120 & spot_bp_sys < 140 | spot_bp_dys >= 70 & spot_bp_dys < 90 ~ "Elevated BP",
          spot_bp_sys >= 140 | spot_bp_dys >= 90 ~ "Hypertension"
        ),
        amb_cat = case_when(
          idaco_sbpbr_avg < 115 & idaco_dia_avg < 65 |
            idaco_day_sbpbr < 120 & idaco_day_dia < 70 |
            idaco_night_sbpbr < 110 & idaco_night_dia < 60 ~ "Normal",
          idaco_sbpbr_avg >= 115 & idaco_sbpbr_avg < 130 |
            idaco_dia_avg >= 65 & idaco_dia_avg < 80 |
            idaco_day_sbpbr >= 120 & idaco_day_sbpbr < 135 |
            idaco_day_dia >= 70 & idaco_day_dia < 85 |
            idaco_night_sbpbr >= 110 & idaco_night_sbpbr < 120 |
            idaco_night_dia >= 60 & idaco_night_dia < 70 ~ "Elevated BP",
          idaco_sbpbr_avg >= 130 | idaco_dia_avg >= 80 |
            idaco_day_sbpbr >= 135 | idaco_day_dia >= 85 |
            idaco_night_sbpbr >= 120 | idaco_night_dia >= 70 ~ "Hypertension"
        )
      )
  },
  "ACC/AHA" = function(data) {
    data %>%
      mutate(
        clinic_cat = case_when(
          spot_bp_sys < 120 & spot_bp_dys < 80 ~ "Normal",
          spot_bp_sys >= 120 & spot_bp_sys < 130 & spot_bp_dys < 80 ~ "Elevated BP",
          spot_bp_sys >= 130 | spot_bp_dys >= 80 ~ "Hypertension"
        ),
        amb_cat = case_when(
          idaco_sbpbr_avg < 115 & idaco_dia_avg < 75 |
            idaco_day_sbpbr < 120 & idaco_day_dia < 80 |
            idaco_night_sbpbr < 100 & idaco_night_dia < 65 ~ "Normal",
          idaco_sbpbr_avg >= 115 & idaco_sbpbr_avg < 125 & idaco_dia_avg < 75 |
            idaco_day_sbpbr >= 120 & idaco_day_sbpbr < 130 & idaco_day_dia < 80 |
            idaco_night_sbpbr >= 100 & idaco_night_sbpbr < 110 & idaco_night_dia < 65 ~ "Elevated BP",
          idaco_sbpbr_avg >= 125 | idaco_dia_avg >= 75 |
            idaco_day_sbpbr >= 130 | idaco_day_dia >= 80 |
            idaco_night_sbpbr >= 110 | idaco_night_dia >= 65 ~ "Hypertension"
        )
      )
  },
  "ESH & ISH" = function(data) {
    data %>%
      mutate(
        clinic_cat = case_when(
          spot_bp_sys < 130 & spot_bp_dys < 85 ~ "Normal",
          spot_bp_sys >= 130 & spot_bp_sys < 140 | spot_bp_dys >= 85 & spot_bp_dys < 90 ~ "Elevated BP",
          spot_bp_sys >= 140 | spot_bp_dys >= 90 ~ "Hypertension"
        ),
        amb_cat = case_when(
          idaco_sbpbr_avg < 130 & idaco_dia_avg < 80 |
            idaco_day_sbpbr < 135 & idaco_day_dia < 85 |
            idaco_night_sbpbr < 120 & idaco_night_dia < 70 ~ "Normal",
          idaco_sbpbr_avg >= 130 | idaco_dia_avg >= 80 |
            idaco_day_sbpbr >= 135 | idaco_day_dia >= 85 |
            idaco_night_sbpbr >= 120 | idaco_night_dia >= 70 ~ "Hypertension"
        )
      )
  }
)

# Use the same bp_definitions from Part 1
# Initialize a list to store plots
plots <- list()

# Define custom color palette
custom_colours <- c("Normal" = "#00FF00",      # Green
                    "Elevated BP" = "#FFA500",    # Amber
                    "Hypertension" = "#FF0000"#, # Red
                    )

# Loop through BP definitions and categories
for (def in names(bp_definitions)) {
  # Apply the BP definition function to categorize the data
  categorized_data <- bp_definitions[[def]](updated_combined_data)
  
  # Define BP categories
  bp_categories <- list(
    clinic_cat = "Clinic BP",
    amb_cat = "Ambulatory BP"
  )

  for (bp_cat in names(bp_categories)) {
    # Create the survival formula dynamically
    formula_str <- paste("Surv(time_to_event, mortality_event) ~", bp_cat)
    fit <- survfit(as.formula(formula_str), data = categorized_data)
    
    # Create strata levels
    strata_levels <- levels(factor(categorized_data[[bp_cat]]))
  
    # 2. SUBSET your master palette to *just* those names:
    this_palette <- custom_colours[strata_levels]

    # Generate the survival plot with a cumulative hazard function
    p <- ggsurvplot(
      fit,
      data = categorized_data,
      fun = "event",                            # Nelson-Aalen cumulative hazard
      surv.scale = "percent",                  # Scale to percentage
      title = paste(def, "Definition -", bp_categories[[bp_cat]]),
      xlab = "Time (years)",
      ylab = "Cumulative Mortality (%)",
      ylim = c(0, 0.2),
      legend.title = "",    # Set legend title (e.g., "Ambulatory BP")
      legend.labs = strata_levels,            # Legend labels
      risk.table = TRUE,                         # Include risk table
      #risk.table.col = "strata",                # Color by strata
      risk.table.y.text.col = FALSE,             # Do not use colored text for risk table labels
      tables.height = 0.2,                       # Adjust table height
      tables.theme = theme_cleantable(),         # Clean table style
      palette = this_palette,                # Custom color palette
      xscale = 365.25,                     # Scale x-axis to years
      break.x.by = 365.25 * 2                # Break x-axis by 2 years
    )
    
    # Customize legend labels to show only the category name (e.g., "Elevated")
    p$plot <- p$plot + 
      scale_y_continuous(
        labels = function(x) format(x * 100, scientific = FALSE, trim = TRUE),
        limits = c(0, 0.2)
      )
    
    p$table <- p$table +
      theme(
        legend.position = "none"
      )

    # Combine plot and risk table into a single plot
    combined <- arrangeGrob(
      p$plot, 
      p$table, 
      ncol = 1,
      heights = c(3, 1)  # Adjust height ratio between plot and table
    )

    # Store only the main plot in the list
    plots[[paste(def, bp_cat, sep = "_")]] <- combined
  }
}

# Arrange all plots in a grid
n_plots <- length(plots)
n_cols <- 2  # Number of columns (approximate square layout)
n_rows <- 3  # Number of rows
combined_plot <- wrap_plots(plots, ncol = n_cols, nrow = n_rows)

# Save the combined plot as a PNG
ggsave(
  filename = "combined_nelson_aalen_main.png",
  plot = combined_plot,
  path = "results/figures",
  width = 15,
  height = 20
)
