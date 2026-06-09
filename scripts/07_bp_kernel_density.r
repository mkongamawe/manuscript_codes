# Load required libraries
library(tidyverse)
library(ggtext)
library(ggpattern)
library(aod)

# Long format for all participants
long_data_all <- updated_combined_data |>
  #filter(mortality_event == 0) |>
  pivot_longer(
    cols = c(spot_bp_sys, idaco_day_sbpbr, idaco_night_sbpbr, idaco_sbpbr_avg),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(group = "All")

# Long format for participants with mortality_event == 1
long_data_mortality <- updated_combined_data |>
  filter(mortality_event == 1) |>
  pivot_longer(
    cols = c(spot_bp_sys, idaco_day_sbpbr, idaco_night_sbpbr, idaco_sbpbr_avg),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(group = "Dead")

# Combine the datasets
combined_data <- bind_rows(long_data_all, long_data_mortality)

# Define descriptive labels for the BP measurements
measure_labels <- c(
  spot_bp_sys = "Clinic SBP",
  idaco_day_sbpbr = "Daytime SBP",
  idaco_night_sbpbr = "Nighttime SBP",
  idaco_sbpbr_avg = "24h SBP"
)

# Calculate density for clinic values
clinic_values <- long_data_mortality |>
  filter(measure == "spot_bp_sys") |>
  pull(value)

dens_clinic <- density(clinic_values)
df_clinic <- data.frame(
  x = dens_clinic$x,
  y = dens_clinic$y,
  group = "Clinic SBP"
)

pct_missed_clinic <- sum(clinic_values < 140, na.rm = TRUE) /
  length(na.omit(clinic_values))
label_clinic <- sprintf("%.0f%% missed\n(clinic)", pct_missed_clinic * 100)

# Calculate density for nighttime values
night_values <- long_data_mortality |>
  filter(measure == "idaco_night_sbpbr") |>
  pull(value)

dens_night <- density(night_values)
df_night <- data.frame(
  x = dens_night$x,
  y = dens_night$y,
  group = "Nighttime SBP"
)

pct_missed_night <- sum(night_values < 120, na.rm = TRUE) /
  length(na.omit(night_values))
label_night <- sprintf("%.0f%% missed\n(nighttime)", pct_missed_night * 100)

# combine the density data frames
df_all <- bind_rows(df_clinic, df_night)

# Define threshhold for shedding
t_clinic <- 140
t_night <- 120

# Create subset for shedded area
shade_clinic <- df_clinic |> filter(x < t_clinic)
shade_night <- df_night |> filter(x < t_night)

# Calculate y max for threshold lines
y_max <- max(c(df_clinic$y, df_night$y)) * 1.05

# Create the plot
p <- ggplot() +
  geom_area(
    data = df_clinic,
    aes(x = x, y = y),
    fill = "#fec44f",
    alpha = 0.2
  ) +
  geom_area(data = df_night, aes(x = x, y = y), fill = "#a6cee3", alpha = 0.3) +

  geom_area_pattern(
    data = shade_clinic,
    aes(x = x, y = y),
    fill = "#fec44f",
    alpha = 0.5,
    pattern = "stripe",
    pattern_angle = 45,
    pattern_density = 0.05,
    pattern_spacing = 0.02,
    pattern_color = "#d95f02",
    pattern_fill = NA
  ) +
  geom_area_pattern(
    data = shade_night,
    aes(x = x, y = y),
    fill = "#a6cee3",
    alpha = 0.6,
    pattern = "stripe",
    pattern_angle = -45, # Negative angle flips the stripes #nolint
    pattern_density = 0.05,
    pattern_spacing = 0.02,
    pattern_color = "#1f78b4",
    pattern_fill = NA
  ) +
  # Add the main density outline curves on top
  geom_line(
    data = df_all,
    aes(x = x, y = y, color = group, linetype = group),
    linewidth = 1.2
  ) +

  geom_segment(
    aes(
      x = t_clinic,
      xend = t_clinic,
      y = 0,
      yend = y_max,
      color = "Clinic threshold >= 140 mmHg",
      linetype = "Clinic threshold >= 140 mmHg"
    ),
    linewidth = 1
  ) +
  geom_segment(
    aes(
      x = t_night,
      xend = t_night,
      y = 0,
      yend = y_max,
      color = "Nighttime threshold >= 120 mmHg",
      linetype = "Nighttime threshold >= 120 mmHg"
    ),
    linewidth = 1
  ) +

  annotate(
    "text",
    x = 100,
    y = 0.009,
    label = label_night,
    color = "#1f78b4",
    fontface = "italic",
    size = 4.5,
    hjust = 0.5
  ) +
  annotate(
    "curve",
    x = 105,
    y = 0.010,
    xend = 118,
    yend = 0.0115, # Adjust X/Y coordinates as needed based on your actual data
    curvature = -0.2,
    arrow = arrow(length = unit(0.2, "cm")),
    color = "#1f78b4"
  ) +

  # Clinic text and curved arrow
  annotate(
    "text",
    x = 150,
    y = 0.0075,
    label = label_clinic,
    color = "#d95f02",
    fontface = "italic",
    size = 4.5,
    hjust = 0.5
  ) +
  annotate(
    "curve",
    x = 155,
    y = 0.0085,
    xend = 142,
    yend = 0.0105, # Adjust X/Y coordinates as needed based on your actual data
    curvature = 0.2,
    arrow = arrow(length = unit(0.2, "cm")),
    color = "#d95f02"
  ) +

  scale_x_continuous(breaks = seq(80, 250, by = 20)) +
  scale_y_continuous(breaks = seq(0, 0.015, by = 0.002)) +

  scale_color_manual(
    name = "",
    values = c(
      "Clinic SBP" = "#d95f02",
      "Nighttime SBP" = "#1f78b4",
      "Clinic threshold >= 140 mmHg" = "#fec44f", # Lighter orange for the dotted line in your image
      "Nighttime threshold >= 120 mmHg" = "#1f78b4"
    )
  ) +

  scale_linetype_manual(
    name = "",
    values = c(
      "Clinic SBP" = "solid",
      "Nighttime SBP" = "solid",
      "Clinic threshold >= 140 mmHg" = "dashed",
      "Nighttime threshold >= 120 mmHg" = "dashed"
    )
  ) +

  # Aesthetics, Colors, and Theme matching your image
  theme_classic() +
  labs(
    title = "Deaths (n=118)",
    x = "Systolic Blood Pressure (mmHg)",
    y = "Density",
    color = ""
  ) +
  theme(
    legend.position = c(0.8, 0.9), # Moves legend inside the plot
    legend.background = element_rect(color = "gray"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

ggsave("bp_density_dist.png", plot = p, dpi = 600, path = "results/figures")
