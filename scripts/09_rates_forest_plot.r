library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(survival)
library(broom)
library(ggplot2)
library(broom)
library(gtsummary)
library(patchwork)
library(labelled)
library(forestplot)
library(broom)
library(ggplotify)

# Define BP categorization functions for each definition
bp_definitions <- list(
  "ESC" = function(data) {
    data |>
      mutate(
        clinic_cat = case_when(
          spot_bp_sys < 120 & spot_bp_dys < 70 ~ "Normal",
          (spot_bp_sys >= 120 & spot_bp_sys < 140) |
            (spot_bp_dys >= 70 & spot_bp_dys < 90) ~ "Elevated BP",
          spot_bp_sys >= 140 | spot_bp_dys >= 90 ~ "Hypertension",
          TRUE ~ NA_character_
        ),
        amb_cat = case_when(
          (idaco_sbpbr_avg < 115 & idaco_dia_avg < 65) |
            (idaco_day_sbpbr < 120 & idaco_day_dia < 70) |
            (idaco_night_sbpbr < 110 & idaco_night_dia < 60) ~ "Normal",
          (idaco_sbpbr_avg >= 115 & idaco_sbpbr_avg < 130) |
            (idaco_dia_avg >= 65 & idaco_dia_avg < 80) |
            (idaco_day_sbpbr >= 120 & idaco_day_sbpbr < 135) |
            (idaco_day_dia >= 70 & idaco_day_dia < 85) |
            (idaco_night_sbpbr >= 110 & idaco_night_sbpbr < 120) |
            (idaco_night_dia >= 60 & idaco_night_dia < 70) ~ "Elevated BP",
          idaco_sbpbr_avg >= 130 |
            idaco_dia_avg >= 80 |
            idaco_day_sbpbr >= 135 |
            idaco_day_dia >= 85 |
            idaco_night_sbpbr >= 120 |
            idaco_night_dia >= 70 ~ "Hypertension",
          TRUE ~ NA_character_
        )
      )
  },
  "ACC/AHA" = function(data) {
    data |>
      mutate(
        clinic_cat = case_when(
          spot_bp_sys < 120 & spot_bp_dys < 80 ~ "Normal",
          spot_bp_sys >= 120 &
            spot_bp_sys < 130 &
            spot_bp_dys < 80 ~ "Elevated BP",
          spot_bp_sys >= 130 | spot_bp_dys >= 80 ~ "Hypertension",
          TRUE ~ NA_character_
        ),
        amb_cat = case_when(
          (idaco_sbpbr_avg < 115 & idaco_dia_avg < 75) |
            (idaco_day_sbpbr < 120 & idaco_day_dia < 80) |
            (idaco_night_sbpbr < 100 & idaco_night_dia < 65) ~ "Normal",
          (idaco_sbpbr_avg >= 115 &
            idaco_sbpbr_avg < 125 &
            idaco_dia_avg < 75) |
            (idaco_day_sbpbr >= 120 &
              idaco_day_sbpbr < 130 &
              idaco_day_dia < 80) |
            (idaco_night_sbpbr >= 100 &
              idaco_night_sbpbr < 110 &
              idaco_night_dia < 65) ~ "Elevated BP",
          idaco_sbpbr_avg >= 125 |
            idaco_dia_avg >= 75 |
            idaco_day_sbpbr >= 130 |
            idaco_day_dia >= 80 |
            idaco_night_sbpbr >= 110 |
            idaco_night_dia >= 65 ~ "Hypertension",
          TRUE ~ NA_character_
        )
      )
  },
  "ESH/ISH" = function(data) {
    data |>
      mutate(
        clinic_cat = case_when(
          spot_bp_sys < 130 & spot_bp_dys < 85 ~ "Normal",
          (spot_bp_sys >= 130 & spot_bp_sys < 140) |
            (spot_bp_dys >= 85 & spot_bp_dys < 90) ~ "Elevated BP",
          spot_bp_sys >= 140 | spot_bp_dys >= 90 ~ "Hypertension",
          TRUE ~ NA_character_
        ),
        amb_cat = case_when(
          (idaco_sbpbr_avg < 130 & idaco_dia_avg < 80) |
            (idaco_day_sbpbr < 135 & idaco_day_dia < 85) |
            (idaco_night_sbpbr < 120 & idaco_night_dia < 70) ~ "Normal",
          idaco_sbpbr_avg >= 130 |
            idaco_dia_avg >= 80 |
            idaco_day_sbpbr >= 135 |
            idaco_day_dia >= 85 |
            idaco_night_sbpbr >= 120 |
            idaco_night_dia >= 70 ~ "Hypertension",
          TRUE ~ NA_character_ # ESH/ISH amb_cat often doesn't have 'Elevated BP'
        )
      )
  }
)

# Age standardization using WHO standard population
who_std <- tibble(
  age = 0:90, # Corresponds to 91 values in std_pop
  std_pop = c(
    17917,
    17802,
    17701,
    17613,
    17536,
    17470,
    17414,
    17366,
    17327,
    17293,
    17263,
    17233,
    17199,
    17160,
    17115,
    17069,
    17018,
    16952,
    16866,
    16765,
    16656,
    16546,
    16434,
    16323,
    16212,
    16094,
    15971,
    15851,
    15735,
    15621,
    15504,
    15376,
    15233,
    15070,
    14890,
    14702,
    14510,
    14307,
    14091,
    13865,
    13629,
    13391,
    13163,
    12949,
    12745,
    12538,
    12320,
    12091,
    11845,
    11585,
    11320,
    11048,
    10757,
    10443,
    10113,
    9774,
    9435,
    9095,
    8757,
    8423,
    8087,
    7750,
    7425,
    7113,
    6812,
    6517,
    6221,
    5923,
    5618,
    5311,
    5005,
    4706,
    4412,
    4125,
    3844,
    3569,
    3300,
    3035,
    2773,
    2518,
    2271,
    2033,
    1807,
    1594,
    1392,
    1203,
    1025,
    863,
    718,
    589,
    1950 # This last value for age 90. If it's for 85+, adjust age range. Assuming it's for age 90.
    # The SEER link usually has 85+ as the last group for single ages.
    # If this 1950 is for "90+", the age vector should be 0:89, and the last category 60-89 or 60+
    # For this code, I'll assume age 0:90 and the last value is for age 90.
    # The user's agecat3 for 60+ goes up to 98, which is fine if std_pop covers that.
    # The provided std_pop has 91 values, matching age 0 to 90.
  )
) |>
  mutate(
    agecat3 = case_when(
      age >= 18 & age <= 29 ~ "18-29",
      age >= 30 & age <= 59 ~ "30-59",
      age >= 60 & age <= 90 ~ "60+", # Aligning with max age in who_std (0-90)
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(agecat3)) |> # Important: filter out NA agecat3 before summarising
  group_by(agecat3) |>
  summarise(std_pop_grp = sum(std_pop, na.rm = TRUE), .groups = "drop") |> # Renamed to avoid conflict
  mutate(std_prop = std_pop_grp / sum(std_pop_grp, na.rm = TRUE))

# Confounders for adjustment
confounders <- c(
  "age",
  "sex",
  "bmi",
  "on_med_htn",
  "current_smoke",
  "a1c_imp",
  "sod_pot_imp"
)

# Initialize an empty list to store detailed results
all_results_list <- list()

# Main processing loop
for (def_name in names(bp_definitions)) {
  message(paste("Processing Definition:", def_name))

  # Apply BP definition function
  categorized_data_for_def <- bp_definitions[[def_name]](updated_combined_data)

  bp_measurement_types_map <- list(
    clinic_cat = "Clinic BP",
    amb_cat = "Ambulatory BP"
  )

  for (bp_measure_col_name in names(bp_measurement_types_map)) {
    bp_measure_label <- bp_measurement_types_map[[bp_measure_col_name]]
    message(paste("  Measure Type:", bp_measure_label))

    # --- 1. Age-Standardized Rates ---
    data_for_rates <- categorized_data_for_def |>
      filter(!is.na(.data[[bp_measure_col_name]])) # Filter out NA BP categories

    # Proceed only if there's data for this category
    if (nrow(data_for_rates) == 0) {
      message(paste(
        "    Skipping rates for",
        def_name,
        "-",
        bp_measure_label,
        "due to no data after NA removal for BP category."
      ))
      next
    }

    # Get unique BP levels actually present in the data for this definition and measure
    # These will form the rows of our table for this section
    actual_bp_levels_in_data <- sort(unique(data_for_rates[[
      bp_measure_col_name
    ]]))

    # Calculate rates for All-Cause Mortality
    mortality_age_specific <- data_for_rates |>
      group_by(!!sym(bp_measure_col_name), agecat3) |>
      summarise(
        total_person_years = sum(time_to_event, na.rm = TRUE) / 365.25,
        total_events = sum(mortality_event, na.rm = TRUE),
        total_N = n(),
        .groups = "drop"
      ) |>
      left_join(who_std, by = "agecat3") |>
      mutate(
        # Handle cases where std_prop might be NA if agecat3 doesn't match (should not happen with pre-filtering)
        std_prop = ifelse(is.na(std_prop), 0, std_prop),
        age_specific_rate = ifelse(
          total_person_years > 0,
          (total_events / total_person_years) * 1000,
          0
        ),
        std_rate_component = age_specific_rate * std_prop
      )

    mortality_summary_rates <- mortality_age_specific |>
      group_by(!!sym(bp_measure_col_name)) |>
      summarise(
        mort_crude_rate = sum(age_specific_rate, na.rm = TRUE),
        mort_rate = sum(std_rate_component, na.rm = TRUE),
        mort_n = sum(total_events, na.rm = TRUE),
        mort_N = sum(total_N, na.rm = TRUE),
        .groups = "drop"
      ) |>
      rename(bp_level = !!sym(bp_measure_col_name))

    # Calculate rates for All Cardiovascular Events
    cardio_age_specific <- data_for_rates |>
      group_by(!!sym(bp_measure_col_name), agecat3) |>
      summarise(
        total_person_years = sum(time_to_all_cardio, na.rm = TRUE) / 365.25,
        total_events = sum(all_cardio_event, na.rm = TRUE),
        total_N = n(),
        .groups = "drop"
      ) |>
      left_join(who_std, by = "agecat3") |>
      mutate(
        std_prop = ifelse(is.na(std_prop), 0, std_prop),
        age_specific_rate = ifelse(
          total_person_years > 0,
          (total_events / total_person_years) * 1000,
          0
        ),
        std_rate_component = age_specific_rate * std_prop
      )

    cardio_summary_rates <- cardio_age_specific |>
      group_by(!!sym(bp_measure_col_name)) |>
      summarise(
        cvd_crude_rate = sum(age_specific_rate, na.rm = TRUE),
        cvd_rate = sum(std_rate_component, na.rm = TRUE),
        cvd_n = sum(total_events, na.rm = TRUE),
        cvd_N = sum(total_N, na.rm = TRUE),
        .groups = "drop"
      ) |>
      rename(bp_level = !!sym(bp_measure_col_name))

    # Combine rates: Ensure all actual_bp_levels_in_data are present
    rates_summary <- tibble(bp_level = actual_bp_levels_in_data) |>
      left_join(mortality_summary_rates, by = "bp_level") |>
      left_join(cardio_summary_rates, by = "bp_level")

    # --- 2. Adjusted Hazard Ratios ---
    data_for_hr <- categorized_data_for_def |>
      filter(!is.na(.data[[bp_measure_col_name]]))

    tidy_mort_hr <- tibble(
      bp_level = character(),
      mort_hr = numeric(),
      mort_ci_low = numeric(),
      mort_ci_high = numeric(),
      mort_hr_ci = character()
    )
    tidy_cvd_hr <- tibble(
      bp_level = character(),
      cvd_hr = numeric(),
      cvd_ci_low = numeric(),
      cvd_ci_high = numeric(),
      cvd_hr_ci = character()
    )

    if (
      nrow(data_for_hr) > 0 &&
        n_distinct(data_for_hr[[bp_measure_col_name]]) > 0
    ) {
      data_for_hr[[bp_measure_col_name]] <- factor(data_for_hr[[
        bp_measure_col_name
      ]])

      ref_level <- "Normal"
      if (!ref_level %in% levels(data_for_hr[[bp_measure_col_name]])) {
        if (length(levels(data_for_hr[[bp_measure_col_name]])) > 0) {
          ref_level <- levels(data_for_hr[[bp_measure_col_name]])[1]
          message(paste(
            "    Warning for HRs ('",
            def_name,
            "', '",
            bp_measure_label,
            "'): 'Normal' category not found. Using '",
            ref_level,
            "' as reference.",
            sep = ""
          ))
        } else {
          ref_level <- NA
        }
      }

      if (!is.na(ref_level)) {
        data_for_hr[[bp_measure_col_name]] <- relevel(
          data_for_hr[[bp_measure_col_name]],
          ref = ref_level
        )

        # Mortality HR
        model_data_mort <- data_for_hr |>
          select(all_of(c(
            "time_to_event",
            "mortality_event",
            bp_measure_col_name,
            confounders
          ))) |>
          na.omit()
        if (
          nrow(model_data_mort) > 0 &&
            n_distinct(model_data_mort[[bp_measure_col_name]]) > 1
        ) {
          tryCatch(
            {
              model_mortality <- coxph(
                formula = as.formula(paste(
                  "Surv(time_to_event, mortality_event) ~",
                  bp_measure_col_name,
                  "+",
                  paste(confounders, collapse = " + ")
                )),
                data = model_data_mort
              )
              tidy_mort_hr <- broom::tidy(
                model_mortality,
                exponentiate = TRUE,
                conf.int = TRUE,
              ) |>
                filter(str_starts(term, bp_measure_col_name)) |>
                mutate(
                  bp_level = str_remove(
                    term,
                    fixed(paste0(bp_measure_col_name))
                  ),
                  mort_hr = estimate,
                  mort_ci_low = conf.low,
                  mort_ci_high = conf.high,
                  mort_hr_ci = sprintf(
                    "%.2f (%.2f-%.2f)",
                    estimate,
                    conf.low,
                    conf.high
                  ),
                  mort_p_val = format.pval(p.value, eps = 0.001, digits = 1)
                ) |>
                select(
                  bp_level,
                  mort_hr,
                  mort_ci_low,
                  mort_ci_high,
                  mort_hr_ci,
                  mort_p_val
                )
              tidy_mort_hr <- bind_rows(
                tibble(
                  bp_level = ref_level,
                  mort_hr = 1,
                  mort_ci_low = NA,
                  mort_ci_high = NA,
                  mort_hr_ci = "1.00 (Reference)",
                  mort_p_val = NA
                ),
                tidy_mort_hr
              )
            },
            error = function(e) {
              message(paste(
                "    Error fitting mortality Cox model for",
                def_name,
                "-",
                bp_measure_label,
                ":",
                e$message
              ))
              tidy_mort_hr <<- tibble(
                bp_level = levels(data_for_hr[[bp_measure_col_name]]),
                mort_hr = NA_real_,
                mort_ci_low = NA_real_,
                mort_ci_high = NA_real_,
                mort_hr_ci = NA_character_,
                mort_p_val = NA_character_
              )
            }
          )
        } else {
          tidy_mort_hr <- tibble(
            bp_level = levels(data_for_hr[[bp_measure_col_name]]),
            mort_hr = NA_real_,
            mort_ci_low = NA_real_,
            mort_ci_high = NA_real_,
            mort_hr_ci = NA_character_,
            mort_p_val = NA_character_
          )
        }

        # CVD HR
        model_data_cvd <- data_for_hr |>
          select(all_of(c(
            "time_to_all_cardio",
            "all_cardio_event",
            bp_measure_col_name,
            confounders
          ))) |>
          na.omit()
        if (
          nrow(model_data_cvd) > 0 &&
            n_distinct(model_data_cvd[[bp_measure_col_name]]) > 1
        ) {
          tryCatch(
            {
              model_cardio <- coxph(
                formula = as.formula(paste(
                  "Surv(time_to_all_cardio, all_cardio_event) ~",
                  bp_measure_col_name,
                  "+",
                  paste(confounders, collapse = " + ")
                )),
                data = model_data_cvd
              )
              tidy_cvd_hr <- broom::tidy(
                model_cardio,
                exponentiate = TRUE,
                conf.int = TRUE
              ) |>
                filter(str_starts(term, bp_measure_col_name)) |>
                mutate(
                  bp_level = str_remove(
                    term,
                    fixed(paste0(bp_measure_col_name))
                  ),
                  cvd_hr = estimate,
                  cvd_ci_low = conf.low,
                  cvd_ci_high = conf.high,
                  cvd_hr_ci = sprintf(
                    "%.2f (%.2f-%.2f)",
                    estimate,
                    conf.low,
                    conf.high
                  ),
                  cvd_p_val = format.pval(p.value, eps = 0.001, digits = 1)
                ) |>
                select(
                  bp_level,
                  cvd_hr,
                  cvd_ci_low,
                  cvd_ci_high,
                  cvd_hr_ci,
                  cvd_p_val
                )
              tidy_cvd_hr <- bind_rows(
                tibble(
                  bp_level = ref_level,
                  cvd_hr = 1,
                  cvd_ci_low = NA,
                  cvd_ci_high = NA,
                  cvd_hr_ci = "1.00 (Reference)",
                  cvd_p_val = NA
                ),
                tidy_cvd_hr
              )
            },
            error = function(e) {
              message(paste(
                "    Error fitting CVD Cox model for",
                def_name,
                "-",
                bp_measure_label,
                ":",
                e$message
              ))
              tidy_cvd_hr <<- tibble(
                bp_level = levels(data_for_hr[[bp_measure_col_name]]),
                cvd_hr = NA_real_,
                cvd_ci_low = NA_real_,
                cvd_ci_high = NA_real_,
                cvd_hr_ci = NA_character_,
                cvd_p_val = NA_character_
              )
            }
          )
        } else {
          tidy_cvd_hr <- tibble(
            bp_level = levels(data_for_hr[[bp_measure_col_name]]),
            cvd_hr = NA_real_,
            cvd_ci_low = NA_real_,
            cvd_ci_high = NA_real_,
            cvd_hr_ci = NA_character_,
            cvd_p_val = NA_character_
          )
        }
      } else {
        tidy_mort_hr <- tibble(
          bp_level = actual_bp_levels_in_data,
          mort_hr = NA_real_,
          mort_ci_low = NA_real_,
          mort_ci_high = NA_real_,
          mort_hr_ci = NA_character_,
          mort_p_val = NA_character_
        )
        tidy_cvd_hr <- tibble(
          bp_level = actual_bp_levels_in_data,
          cvd_hr = NA_real_,
          cvd_ci_low = NA_real_,
          cvd_ci_high = NA_real_,
          cvd_hr_ci = NA_character_,
          cvd_p_val = NA_character_
        )
      }
    } else {
      tidy_mort_hr <- tibble(
        bp_level = actual_bp_levels_in_data,
        mort_hr = NA_real_,
        mort_ci_low = NA_real_,
        mort_ci_high = NA_real_,
        mort_hr_ci = NA_character_,
        mort_p_val = NA_character_
      )
      tidy_cvd_hr <- tibble(
        bp_level = actual_bp_levels_in_data,
        cvd_hr = NA_real_,
        cvd_ci_low = NA_real_,
        cvd_ci_high = NA_real_,
        cvd_hr_ci = NA_character_,
        cvd_p_val = NA_character_
      )
    }

    # Combine HR summaries
    hr_summary <- full_join(tidy_mort_hr, tidy_cvd_hr, by = "bp_level")

    # --- 3. Combine rates and HRs ---
    rates_summary$bp_level <- as.character(rates_summary$bp_level)
    hr_summary$bp_level <- as.character(hr_summary$bp_level)

    combined_stats_for_measure <- full_join(
      rates_summary,
      hr_summary,
      by = "bp_level"
    ) |>
      mutate(
        definition = def_name,
        bp_measurement_label = bp_measure_label
      ) |>
      select(
        definition,
        bp_measurement_label,
        bp_level,
        mort_n,
        mort_N,
        mort_crude_rate,
        mort_rate,
        mort_hr,
        mort_ci_low,
        mort_ci_high,
        mort_hr_ci,
        mort_p_val,
        cvd_n,
        cvd_N,
        cvd_crude_rate,
        cvd_rate,
        cvd_hr,
        cvd_ci_low,
        cvd_ci_high,
        cvd_hr_ci,
        cvd_p_val
      )

    all_results_list[[paste(
      def_name,
      bp_measure_col_name,
      sep = "_"
    )]] <- combined_stats_for_measure
  }
}

# Combine all results into one tibble
final_combined_data <- bind_rows(all_results_list)

# Ensure consistent factor levels for bp_level
all_possible_bp_levels <- c("Normal", "Elevated BP", "Hypertension")
final_combined_data$bp_level <- factor(
  final_combined_data$bp_level,
  levels = all_possible_bp_levels
)

# Arrange data
final_combined_data <- final_combined_data |>
  arrange(definition, bp_measurement_label, bp_level) |>
  filter(!is.na(bp_level))

# --- Create Forest Plots ---

# Add combined label for plotting
final_combined_data <- final_combined_data |>
  mutate(
    label = paste(definition, bp_measurement_label, bp_level, sep = " - ")
  )

### Forest Plot for All-Cause Mortality

create_forest_plot <- function(data, definition, outcome = "mort") {
  #Change outcome to "cvd" for CVD plots
  # Filter data for the specific definition
  plot_data <- data |>
    filter(definition == !!definition) |>
    filter(!is.na(.data[[paste0(outcome, "_hr")]])) |>
    mutate(
      n_text = as.character(.data[[paste0(outcome, "_n")]]), # Number of events
      N_text = as.character(.data[[paste0(outcome, "_N")]]), # Total N
      crude_rate = if_else(
        is.na(.data[[paste0(outcome, "_crude_rate")]]),
        "-",
        sprintf("%.2f", .data[[paste0(outcome, "_crude_rate")]])
      ),
      std_rate = if_else(
        is.na(.data[[paste0(outcome, "_rate")]]),
        "-",
        sprintf("%.2f", .data[[paste0(outcome, "_rate")]])
      ),
      hr_text = if_else(
        is.na(.data[[paste0(outcome, "_hr")]]),
        "-",
        sprintf("%.2f", .data[[paste0(outcome, "_hr")]])
      ),
      ci_text = if_else(
        is.na(.data[[paste0(outcome, "_ci_low")]]),
        "Reference",
        sprintf(
          "(%.2f-%.2f)",
          .data[[paste0(outcome, "_ci_low")]],
          .data[[paste0(outcome, "_ci_high")]]
        )
      ),
      lower_plot = if_else(
        is.na(.data[[paste0(outcome, "_ci_low")]]),
        .data[[paste0(outcome, "_hr")]],
        .data[[paste0(outcome, "_ci_low")]]
      ),
      upper_plot = if_else(
        is.na(.data[[paste0(outcome, "_ci_high")]]),
        .data[[paste0(outcome, "_hr")]],
        .data[[paste0(outcome, "_ci_high")]]
      ),
      hr_ci = as.character(.data[[paste0(outcome, "_hr_ci")]]),
      pval_text = as.character(.data[[paste0(outcome, "_p_val")]]),
    ) |>
    arrange(bp_measurement_label, bp_level)

  # Add a new column for labels with indentation
  unique_measures <- unique(plot_data$bp_measurement_label)
  plot_data <- plot_data |>
    group_by(bp_measurement_label) |>
    mutate(
      label = if_else(
        row_number() == 1,
        paste0(bp_measurement_label, "-", bp_level),
        paste0("  ", bp_level)
      )
    ) |>
    ungroup()

  # Prepare label matrix
  fp_labeltext <- cbind(
    plot_data$label,
    plot_data$n_text,
    plot_data$N_text,
    plot_data$crude_rate,
    plot_data$std_rate,
    plot_data$hr_ci,
    plot_data$pval_text
  )

  # Add header
  header <- c(
    "BP Category",
    "n",
    "N",
    "Crude Rate",
    "Standard Rate",
    "HR (95% CI)",
    "p value"
  )
  fp_labeltext <- rbind(header, fp_labeltext)

  # Define summary rows (header and measurement types)
  is_summary <- c(TRUE, rep(FALSE, nrow(plot_data)))
  for (measure in unique_measures) {
    measure_idx <- which(fp_labeltext[, 1] == measure)
    is_summary[measure_idx] <- TRUE
  }

  # Create forest plot
  forestplot(
    labeltext = fp_labeltext,
    mean = c(NA, plot_data[[paste0(outcome, "_hr")]]),
    lower = c(NA, plot_data$lower_plot),
    upper = c(NA, plot_data$upper_plot),
    is.summary = is_summary,
    zero = 1,
    boxsize = 0.25,
    graph.pos = 6,
    lineheight = unit(1.5, "lines"),
    col = fpColors(box = "blue", line = "blue", summary = "blue"),
    xlab = "Hazard Ratio",
    clip = c(0.5, 3.0),
    title = paste(definition),
    txt_gp = fpTxtGp(
      label = list(gpar(fontface = "bold"), gpar(cex = 0.8)), # Header bold, BP levels normal
      ticks = gpar(cex = 0.8),
      xlab = gpar(cex = 1),
      summary = gpar(fontface = "bold", cex = 1)
    ),
    hrzl_lines = list("2" = gpar(lty = 2)), # Line under header
    align = c("l", "c", "c", "c", "c") # Left-align labels, center others
  ) |>
    fp_set_zebra_style("#EFEFEF")
}

# Define BP definitions
definitions <- c("ESC", "ACC/AHA", "ESH/ISH")

#----------------------------------#
# Initialize an empty list to store the plots for mort
plots <- list()

# Generate plots for
plots <- list()
for (def in definitions) {
  # Create the forest plot
  fp <- create_forest_plot(final_combined_data, def, "mort")

  # Print and convert to grob using ggplotify
  plots[[def]] <- grid2grob(print(fp))
}

# Combine with patchwork and add titles
combined_plot <- wrap_plots(
  lapply(names(plots), function(def) {
    plots[[def]]
  }),
  nrow = 3
)

# Optional: Save to file
output_dir <- file.path("results", "figures")

# Check if the directory exists and create it if not
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the plot
ggsave(
  file.path(output_dir, "mortality_rates_forest_plots.png"),
  combined_plot,
  width = 15,
  height = 10
)

#----------------------------------#
# Initialize an empty list to store the plots for cvd
plots <- list()

# Generate plots for
plots <- list()
for (def in definitions) {
  # Create the forest plot
  fp <- create_forest_plot(final_combined_data, def, "cvd")

  # Print and convert to grob using ggplotify
  plots[[def]] <- grid2grob(print(fp))
}

# Combine with patchwork and add titles
combined_plot <- wrap_plots(
  lapply(names(plots), function(def) {
    plots[[def]]
  }),
  nrow = 3
)

# Optional: Save to file
output_dir <- file.path("results", "figures")

# Check if the directory exists and create it if not
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the plot
ggsave(
  file.path(output_dir, "cardiovascular_rates_forest_plots.png"),
  combined_plot,
  width = 15,
  height = 10
)
