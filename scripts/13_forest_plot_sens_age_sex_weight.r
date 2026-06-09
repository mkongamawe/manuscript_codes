#################################
# Subgroup Analysis - Version 2
#################################
library(survival)
library(dplyr)
library(tidyr)
library(broom)
library(forestplot)
library(ggplot2)
library(patchwork)
library(grid)

# Define BP columns and labels for both systolic and diastolic
bp_columns_sys <- list(
  spot_bp_sys = "Clinic SBP",
  idaco_day_sbpbr = "Daytime SBP",
  idaco_night_sbpbr = "Nighttime SBP",
  idaco_sbpbr_avg = "24h SBP"
)

bp_columns_dia <- list(
  spot_bp_dys = "Clinic DBP",
  idaco_day_dia = "Daytime DBP",
  idaco_night_dia = "Nighttime DBP",
  idaco_dia_avg = "24h DBP"
)

# User prompts
#include_diastolic <- readline(prompt = "Include diastolic BP analysis? (y/n): ") == "y"
#include_cardio <- readline(prompt = "Include cardiovascular events analysis? (y/n): ") == "y"

include_diastolic <- TRUE
include_cardio <- FALSE
# Combine BP columns based on user choice
if (include_diastolic) {
  bp_columns <- c(bp_columns_sys, bp_columns_dia)
} else {
  bp_columns <- bp_columns_sys
}

# Add Age and Weight categories to the data
updated_combined_data <- updated_combined_data |>
  mutate(
    Age = case_when(
      age < 45 ~ "18 - 44",
      age >= 45 & age < 70 ~ "45 - 69",
      age >= 70 ~ "70+"
    ),
    Weight = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi > 24.9 ~ "Overweight",
      TRUE ~ "Normal"
    )
  )

updated_combined_data$Weight <- factor(
  updated_combined_data$Weight,
  levels = c("Normal", "Underweight", "Overweight")
)

# Enhanced function to compute overall HRs with N and n
compute_overall_hrs <- function(data, event_var, time_var) {
  overall_results <- list()

  for (bp_col in names(bp_columns)) {
    data_temp <- data |>
      filter(!is.na(!!sym(bp_col))) |>
      mutate(bp_scaled = c(2 * scale(!!sym(bp_col))))

    # Calculate N and n
    N_total <- nrow(data_temp)
    n_events <- sum(data_temp[[event_var]], na.rm = TRUE)

    model <- coxph(
      as.formula(paste(
        "Surv(",
        time_var,
        ", ",
        event_var,
        ") ~ bp_scaled + age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"
      )),
      data = data_temp
    )

    tidy_res <- tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
      mutate(
        subgroup = "Overall",
        bp_label = bp_columns[[bp_col]],
        hr_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
        p_value_str = format.pval(p.value, eps = 0.001, digits = 1),
        N = N_total,
        n = n_events
      )
    tidy_res <- tidy_res[1, ]
    overall_results[[bp_col]] <- tidy_res
  }
  bind_rows(overall_results)
  #view(overall_results)
}

# Enhanced function to compute subgroup HRs with N and n
compute_subgroup_hrs <- function(
  data,
  subgroup_var,
  subgroup_levels,
  formula_base,
  event_var,
  time_var
) {
  subgroup_results <- list()

  for (sg in subgroup_levels) {
    data_sub <- data |> filter(!!sym(subgroup_var) == sg)

    for (bp_col in names(bp_columns)) {
      data_temp <- data_sub |>
        filter(!is.na(!!sym(bp_col))) |>
        mutate(bp_scaled = c(2 * scale(!!sym(bp_col))))

      # Calculate N and n for this subgroup
      N_total <- nrow(data_temp)
      n_events <- sum(data_temp[[event_var]], na.rm = TRUE)

      model_formula <- as.formula(paste(
        "Surv(",
        time_var,
        ", ",
        event_var,
        ") ~ bp_scaled +",
        formula_base
      ))
      model <- coxph(model_formula, data = data_temp)

      tidy_res <- tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
        filter(term == "bp_scaled") |>
        mutate(
          subgroup = sg,
          bp_label = bp_columns[[bp_col]],
          hr_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
          p_value_str = format.pval(p.value, eps = 0.001, digits = 1),
          N = N_total,
          n = n_events
        )
      subgroup_results[[paste(sg, bp_col)]] <- tidy_res
    }
  }
  bind_rows(subgroup_results)
}

# Enhanced function to prepare data for forestplot with N and n columns
prepare_forest_data <- function(combined_df) {
  # Define the desired order: systolic first, then diastolic
  bp_order <- c(
    "Clinic SBP",
    "Daytime SBP",
    "Nighttime SBP",
    "24h SBP",
    "Clinic DBP",
    "Daytime DBP",
    "Nighttime DBP",
    "24h DBP"
  )

  wide_data <- combined_df |>
    select(
      subgroup,
      bp_label,
      estimate,
      conf.low,
      conf.high,
      p_value_str,
      N,
      n
    ) |>
    pivot_wider(
      id_cols = c(bp_label),
      names_from = subgroup,
      values_from = c(estimate, conf.low, conf.high, p_value_str, N, n)
    )

  out_data <- wide_data |>
    mutate(across(everything(), as.character)) |>
    pivot_longer(cols = -bp_label) |>
    mutate(
      group = sapply(strsplit(name, "_"), function(x) tail(x, 1)),
      name = sapply(strsplit(name, "_"), function(x) {
        paste(x[1:(length(x) - 1)], collapse = "_")
      })
    ) |>
    pivot_wider(names_from = name, values_from = value) |>
    mutate(
      across(c(estimate, conf.low, conf.high, N, n), as.double),
      # Convert bp_label to factor with desired order
      bp_label = factor(bp_label, levels = bp_order)
    ) |>
    arrange(bp_label, desc(group == "Overall"), group) |> # This now uses the factor order
    group_by(bp_label) |>
    mutate(
      # Use bp_label as the main label, and indent the group
      `BP Measurement` = if_else(
        row_number() == 1,
        as.character(bp_label),
        paste0("   ", group)
      ),
      "HR (95% CI)" = sprintf(
        "%.2f (%.2f-%.2f)",
        estimate,
        conf.low,
        conf.high
      ),
      "p value" = p_value_str,
      "N" = N,
      "Events" = n,
      is_summary_row = (group == "Overall") # <-- ADD: Identify summary rows
    ) |>
    ungroup() # <-- MODIFY: Remove final group_by(group)
  # --- NEW SHAPE LOGIC ---
  # Assign distinct shapes dynamically to each subgroup (non-Overall)
  unique_groups <- unique(out_data$group[out_data$group != "Overall"])
  available_shapes <- c(15, 16, 17, 0, 1, 2, 5, 6) # up to 8 distinct shapes
  shape_map <- setNames(
    available_shapes[seq_along(unique_groups)],
    unique_groups
  )

  out_data <- out_data |>
    mutate(
      plot_pch = if_else(group == "Overall", 18, shape_map[group])
    )
  #browser()

  out_data
}

# Enhanced function to create forest plot with N and Events columns
create_forest_plot <- function(out_data, title) {
  # Define shapes list
  # 'Overall' rows get a diamond (pch=18)
  # 'data' rows get the vector we created in prepare_forest_data
  plot_shapes <- list(
    summary = 18,
    data = out_data$plot_pch[!out_data$is_summary_row]
  )

  # Define colors list (e.g., blue for male, red for female, black for summary)
  # This is dynamic based on the 'group' column
  plot_colors <- fpColors(
    box = case_when(
      out_data$group == "female" ~ "red",
      TRUE ~ "blue" # Default blue for male, age, weight
    ),
    line = case_when(
      out_data$group == "female" ~ "red",
      TRUE ~ "blue"
    ),
    summary = "black" # 'Overall' rows will be black
  )

  out_data |>
    forestplot(
      mean = estimate,
      lower = conf.low,
      upper = conf.high,
      labeltext = c(`BP Measurement`, "N", "Events", "HR (95% CI)", "p value"),
      is.summary = is_summary_row, # <-- ADD: Use our summary identifier
      #group = bp_label,           # <-- ADD: Group by BP measurement
      pch = plot_shapes, # <-- ADD: Use our custom shapes
      xlog = TRUE,
      boxsize = 0.25,
      clip = c(0.85, 2.0),
      col = plot_colors, # <-- MODIFY: Use our custom colors
      title = title,
      xticks = c(0.85, 1, 1.5, 2.0),
      graph.pos = 4,
      txt_gp = fpTxtGp(
        label = list(gpar(fontface = "bold"), gpar(cex = 1.2)),
        xlab = gpar(cex = 1.3),
        ticks = gpar(cex = 1.2)
      ),
      hrzl_lines = TRUE, # <-- MODIFY: Let 'group' arg handle lines
      xlab = "Hazard Ratio (HR)"
    ) |>
    # fp_set_style(...) |> # <-- REMOVE this redundant block
    fp_add_header("BP Measurement", "N", "Events", "HR (95% CI)", "p value") |>
    fp_set_zebra_style("#EFEFEF")
}

# Function to convert forestplot to grob
forestplot_to_grob <- function(fp) {
  grid::grid.grabExpr(print(fp))
}

# Function to run complete analysis for a given outcome
run_analysis <- function(event_var, time_var, outcome_name) {
  cat("Running analysis for:", outcome_name, "\n")

  # 1. Forest plot for Age subgroups
  overall_age <- compute_overall_hrs(updated_combined_data, event_var, time_var)
  subgroups_age <- compute_subgroup_hrs(
    updated_combined_data,
    subgroup_var = "Age",
    subgroup_levels = c("18 - 44", "45 - 69", "70+"),
    formula_base = "sex + bmi + current_smoke + a1c_imp + sod_pot_imp", # Removed those on hypertension meds
    event_var = event_var,
    time_var = time_var
  )
  combined_age <- bind_rows(overall_age, subgroups_age)
  out_data_age <- prepare_forest_data(combined_age)
  age_plot <- create_forest_plot(
    out_data_age,
    paste(outcome_name, "- Age Subgroups")
  )

  # 2. Forest plot for Weight subgroups
  overall_weight <- compute_overall_hrs(
    updated_combined_data,
    event_var,
    time_var
  )
  subgroups_weight <- compute_subgroup_hrs(
    updated_combined_data,
    subgroup_var = "Weight",
    subgroup_levels = c("Underweight", "Normal", "Overweight"),
    formula_base = "age + sex + on_med_htn+ current_smoke + a1c_imp + sod_pot_imp",
    event_var = event_var,
    time_var = time_var
  )
  combined_weight <- bind_rows(overall_weight, subgroups_weight)
  out_data_weight <- prepare_forest_data(combined_weight)
  weight_plot <- create_forest_plot(
    out_data_weight,
    paste(outcome_name, "- Weight Subgroups")
  )

  # 3. Forest plot for Sex subgroups
  overall_sex <- compute_overall_hrs(updated_combined_data, event_var, time_var)
  subgroups_sex <- compute_subgroup_hrs(
    updated_combined_data,
    subgroup_var = "sex",
    subgroup_levels = c("male", "female"),
    formula_base = "age + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp",
    event_var = event_var,
    time_var = time_var
  )
  combined_sex <- bind_rows(overall_sex, subgroups_sex)
  out_data_sex <- prepare_forest_data(combined_sex)
  sex_plot <- create_forest_plot(
    out_data_sex,
    paste(outcome_name, "- Sex Subgroups")
  )

  return(list(
    age_plot = age_plot,
    weight_plot = weight_plot,
    sex_plot = sex_plot
  ))
}

# Run mortality analysis
mortality_results <- run_analysis(
  "mortality_event",
  "time_to_event",
  "All Mortality"
)

# Run cardiovascular analysis if requested
if (include_cardio) {
  cardio_results <- run_analysis(
    "all_cardio_event",
    "time_to_all_cardio",
    "Cardiovascular Events"
  )
}

# Create output directory
output_dir <- file.path("results", "figures")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save individual plots and combined plots if cardiovascular analysis was run
if (include_cardio) {
  # Save combined plots side by side
  combined_age <- wrap_plots(
    forestplot_to_grob(mortality_results$age_plot),
    forestplot_to_grob(cardio_results$age_plot),
    ncol = 2
  )

  combined_weight <- wrap_plots(
    forestplot_to_grob(mortality_results$weight_plot),
    forestplot_to_grob(cardio_results$weight_plot),
    ncol = 2
  )

  combined_sex <- wrap_plots(
    forestplot_to_grob(mortality_results$sex_plot),
    forestplot_to_grob(cardio_results$sex_plot),
    ncol = 2
  )

  # Save combined plots
  ggsave(
    filename = "combined_age_subgroup.png",
    plot = combined_age,
    path = output_dir,
    width = 24,
    height = 30
  )

  ggsave(
    filename = "combined_weight_subgroup.png",
    plot = combined_weight,
    path = output_dir,
    width = 24,
    height = 30
  )

  ggsave(
    filename = "combined_sex_subgroup.png",
    plot = combined_sex,
    path = output_dir,
    width = 24,
    height = 30
  )

  cat("Combined mortality and cardiovascular events plots saved.\n")
} else {
  # Save individual mortality plots only
  ggsave(
    filename = "age_subgroup_mortality.png",
    plot = forestplot_to_grob(mortality_results$age_plot),
    path = output_dir,
    width = 15,
    height = 15
  )

  ggsave(
    filename = "weight_subgroup_mortality.png",
    plot = forestplot_to_grob(mortality_results$weight_plot),
    path = output_dir,
    width = 15,
    height = 15
  )

  ggsave(
    filename = "sex_subgroup_mortality.png",
    plot = forestplot_to_grob(mortality_results$sex_plot),
    path = output_dir,
    width = 15,
    height = 15
  )

  cat("Mortality subgroup plots saved.\n")
}

cat("Analysis completed successfully!\n")
