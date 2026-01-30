library(ggplot2)
library(patchwork)

updated_combined_data$age_at_death <- floor(as.numeric(updated_combined_data$age + (updated_combined_data$time_to_event / 365.25)))

death_data <- updated_combined_data |>
  filter(!is.na(death_date))

death_summary <- death_data |>
  group_by(age_at_death, sex) |>
  summarise(
    count = n()
  )

# Proportion of those that died <70 and >=70
total_deaths <- nrow(death_data)
deaths_below_70 <- death_data %>%
  mutate(age_at_death = floor(age + (time_to_event / 365.25))) %>%
  filter(age_at_death < 70) %>%
  nrow()
deaths_70_and_above <- death_data %>%
  mutate(age_at_death = floor(age + (time_to_event / 365.25))) %>%
  filter(age_at_death >= 70) %>%
  nrow()

prop_below_70 <- deaths_below_70 / total_deaths
prop_70_and_above <- deaths_70_and_above / total_deaths

death_plot <- death_summary |>
  mutate(count_for_plot = ifelse(sex == "male", -count, count))

# Calculate the mean age at death for annotation (optional).
mean_age_men <- death_data %>%
  filter(sex == "male") %>%
  summarise(mean_age = mean(age_at_death))

mean_age_women <- death_data %>%
  filter(sex == "female") %>%
  summarise(mean_age = mean(age_at_death))

# Creating BP definitions
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
  "ESH" = function(data) {
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

# --- 2. Plot Generation Loop ---
plot_list <- list()

# Define a consistent color palette for the BP categories
bp_colors <- c("Normal" = "#3B9AB2", "Elevated BP" = "#F2B134", "Hypertension" = "#E15759")

# Loop through each BP definition to create and print a plot
for (definition_name in names(bp_definitions)) {
  
  # Apply the corresponding categorization function
  categorized_data <- bp_definitions[[definition_name]](updated_combined_data)
  
  # Prepare the data for plotting
  death_plot_data <- categorized_data |>
    filter(!is.na(death_date)) |>
    # Calculate age at death within the wrangling pipe
    mutate(age_at_death = floor(age + (time_to_event / 365.25))) |>
    # Group by the new category as well
    group_by(age_at_death, sex, clinic_cat) |>
    summarise(count = n(), .groups = 'drop') |>
    # Handle cases where a category might be missing after filtering
    filter(!is.na(clinic_cat)) |>
    # Create the count for the pyramid structure
    mutate(count_for_plot = ifelse(sex == "male", -count, count)) |>
    # Set the order for stacking. "Normal" will be at the base of the bar.
    mutate(clinic_cat = factor(clinic_cat, levels = c("Normal", "Elevated BP", "Hypertension")))
  
  # Find the maximum count for symmetrical x-axis limits
  max_count <- ceiling(max(abs(death_plot_data$count_for_plot)))
  
  # Create the plot
  p <- ggplot(death_plot_data, aes(x = count_for_plot, y = as.factor(age_at_death), fill = clinic_cat)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = 0, color = "grey20") +
    
    # Use the new color scale for BP categories
    scale_fill_manual(values = bp_colors, name = "BP Category") +
    
    # Customize axes
    scale_x_continuous(
      labels = abs,
      limits = c(-7, 7),
      #limits = c(-max_count, max_count),
      breaks = scales::breaks_pretty(n = 6)
    ) +
    scale_y_discrete(breaks = seq(0, 100, by = 10)) +
    
    # Add titles and labels
    labs(
      title = paste("Population Pyramid of Deaths by", definition_name, "Definition"),
      x = "Number of Deaths",
      y = "Age at Death"
    ) +
    
    # Add annotations for "Male" and "Female" labels
    annotate("text", x = -max_count / 2, y = 3, label = "Male", fontface = "bold", color = "grey30") +
    annotate("text", x = max_count / 2, y = 3, label = "Female", fontface = "bold", color = "grey30") +
    
    # Apply a clean theme
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
  
  plot_list[[definition_name]] <- p
}

# Combine the plots from the list into one column
combined_plot <- wrap_plots(plot_list, ncol = 1, axis = "collect_x") # :cite[2]

output_dir <- file.path("..", "Manuscript", "final_manuscript_results", "Figures")

# Save the combined plot as a PNG
ggsave(
  filename = "age_at_death.png",
  plot = combined_plot,
  path = file.path(cwd, output_dir),
  width = 20,
  height = 30
)