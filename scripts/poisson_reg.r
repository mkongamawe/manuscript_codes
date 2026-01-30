# Load the libraries
# Ensure all these are installed: install.packages(c("survival", "dplyr", "ggplot2", "broom", "labelled", "forestplot", "ggplotify", "ggpubr", "patchwork"))
library(survival) # Still needed for Surv object if time variables are complex, but not for glm person-time
library(dplyr)
library(ggplot2)
library(broom)
library(labelled)
library(forestplot)
library(ggplotify) # To convert forestplot to grob
library(ggpubr)    # For ggarrange if needed, though patchwork is used later
library(patchwork) # For combining plots

# Relabelling columns (from user code)
var_label(updated_combined_data) <- list(
  spot_bp_sys_res1 = "Clinic SBP",
  idaco_day_sbpbr_res1 = "Daytime SBP",
  idaco_night_sbpbr_res1 = "Nighttime SBP",
  idaco_sbpbr_avg_res1 = "24h SBP",
  spot_bp_dys_res1 = "Clinic DBP",
  idaco_day_dia_res1 = "Daytime DBP",
  idaco_night_dia_res1 = "Nighttime DBP",
  idaco_dia_avg_res1 = "24h DBP",
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
  current_smoke = "Current Smoking Status"
)

# Define confounders
confounders_list <- c("age", "sex", "bmi", "on_med_htn", "current_smoke", "a1c_imp", "sod_pot_imp")

# --- Function to run Poisson models and prepare forest data ---
run_poisson_and_prep_forest <- function(data, bp_cols_list, outcome_event_col, person_time_col, model_type, scaling_factor = 1, is_model2 = FALSE) {
  
  poisson_models_list <- list()
  
  for (bp_col_name in names(bp_cols_list)) {
    message(paste("  Fitting Poisson for:", bp_cols_list[[bp_col_name]], "- Model:", model_type))
    
    # Create a temporary dataset for modification within the loop
    data_temp <- data
    
    # Scale the primary BP column
    if (scaling_factor != 1) {
      data_temp <- data_temp %>%
        mutate(!!bp_col_name := .data[[bp_col_name]] / scaling_factor)
    }
    
    # Base formula parts
    formula_lhs <- outcome_event_col
    formula_rhs_base <- paste(bp_col_name, "+", paste(confounders_list, collapse = " + "))
    
    if (is_model2) {
      adjustment_var_name <- if (grepl("spot_bp_sys_res1", bp_col_name)) {
        "idaco_sbpbr_avg"
      } else if (grepl("spot_bp_dys_res1", bp_col_name)) {
        "idaco_dia_avg"
      } else if (grepl("sbpbr_res1", bp_col_name)) { # Generalizing for other _res1 SBP
        "spot_bp_sys"
      } else if (grepl("dia_res1", bp_col_name)) { # Generalizing for other _res1 DBP
        "spot_bp_dys"
      } else if (grepl("sbpbr_avg", bp_col_name)) { # For 24h SBP
        "spot_bp_sys"
      } else if (grepl("dia_avg", bp_col_name)) { # For 24h DBP
        "spot_bp_dys"
      } else {
        NA_character_ # Should not happen if bp_cols_list is correctly defined for model 2
      }
      
      if (is.na(adjustment_var_name)) {
        warning(paste("Could not determine adjustment variable for residualized BP:", bp_col_name))
        formula_rhs <- formula_rhs_base
      } else {
        # Note: adjustment_var is NOT scaled here, consistent with user's Cox example for model2
        formula_rhs <- paste(formula_rhs_base, "+", adjustment_var_name)
      }
    } else {
      formula_rhs <- formula_rhs_base
    }
    
    # Add offset for person-time
    model_formula_str <- paste(formula_lhs, "~", formula_rhs, "+ offset(log(", person_time_col, "))")
    
    # Fit the Poisson model
    # Using tryCatch to handle potential errors during model fitting
    poisson_model_fit <- tryCatch({
      glm(as.formula(model_formula_str), family = poisson(link = "log"), data = data_temp)
    }, error = function(e) {
      message(paste("    Error fitting Poisson model for", bp_col_name, ":", e$message))
      return(NULL) # Return NULL if model fails
    })
    
    poisson_models_list[[bp_col_name]] <- poisson_model_fit
  }
  
  # Prepare data for forest plot
  forest_plot_data <- names(bp_cols_list) %>%
    lapply(function(bp_col_name) {
      model_fit <- poisson_models_list[[bp_col_name]]
      if (is.null(model_fit)) { # If model fitting failed
        return(tibble(
          label = bp_cols_list[[bp_col_name]],
          irr_ci = NA_character_,
          estimate = NA_real_,
          conf.low = NA_real_,
          conf.high = NA_real_,
          p_value_str = NA_character_
        ))
      }
      tidy(model_fit, conf.int = TRUE, exponentiate = TRUE) %>%
        filter(term == bp_col_name) %>%
        mutate(
          label = bp_cols_list[[bp_col_name]],
          irr_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
          p_value_str = format.pval(p.value, eps = 0.001, digits = 2)
        )
    }) %>%
    bind_rows() %>%
    select(label, irr_ci, estimate, conf.low, conf.high, p_value_str)
  
  return(forest_plot_data)
}

# --- Function to create forest plot ---
create_custom_forestplot <- function(plot_data, model_legend_names, plot_title, clip_range, xticks_range) {
  # 'plot_data' (e.g., combined_data_cvd_sbp) is already processed externally and has:
  # label, group ("Model 1"/"Model 2"), estimate, conf.low, conf.high, p (p-value string),
  # `BP Measurement`, `IRR (95% CI)`, `p value` (text for plot)

  # Ensure correct data types for forestplot numeric inputs if they were all character
  # The external pipeline already does `mutate(across(c(estimate, conf.low, conf.high), as.double))`,
  # so this should be fine.
  
  # Ensure ordering for the plot. The external pipeline for combined_data_cvd_sbp
  # already does group_by(label) and then group_by(group).
  # An explicit arrange here ensures the order passed to forestplot is consistent.
  data_for_fp <- plot_data %>%
    arrange(label, group) # 'group' is "Model 1", "Model 2", etc.

  # Define colors for forestplot based on number of models
  model_colors_box <- c("royalblue", "gold") # Add more if more models
  model_colors_line <- c("darkblue", "orange")  # Add more if more models
  
  # Ensure num_models reflects the actual groups present in the current plot_data
  # This handles cases where a plot might unexpectedly have only one model.
  present_groups <- unique(data_for_fp$group)
  num_present_models <- length(present_groups)
  
  # Create a mapping from actual group names to colors to ensure consistency
  # if not all model_legend_names are present or if order differs.
  # For simplicity, this example assumes model_legend_names correspond to sorted unique groups.
  # A more robust approach might involve matching `present_groups` to `model_legend_names`.
  current_model_colors_box <- model_colors_box[1:num_present_models]
  current_model_colors_line <- model_colors_line[1:num_present_models]
  
  # Prepare labeltext for forestplot by selecting the correct columns
  # forestplot expects a matrix of character data for labeltext
  #label_text_matrix <- as.matrix(data_for_fp[, c("BP Measurement", "IRR (95% CI)", "p value")])

  fp <- data_for_fp %>%
    forestplot(
      mean = estimate,    # Directly use numeric columns from data_for_fp
      lower = conf.low,
      upper = conf.high,
      labeltext = c(`BP Measurement`, "IRR (95% CI)", "p value"),
      xlog = TRUE,
      boxsize = 0.25,
      legend = model_legend_names, # Legend names for the models
      clip = clip_range,
      graph.pos = 2, 
      xticks = xticks_range,
      txt_gp = fpTxtGp(label = gpar(cex = 1.0),
                       xlab = gpar(cex = 1.0),
                       ticks = gpar(cex = 0.9)),
      hrzl_lines = TRUE, # auto-adds lines, often based on first col of labeltext blanking
      xlab = "Incidence Rate Ratio (IRR)"
    ) %>%
    # Apply styles per model. fp_set_style recycles colors along the rows.
    # This should work given data is arranged by label, then group.
    fp_set_style(
      box = current_model_colors_box,
      line = current_model_colors_line,
      summary = current_model_colors_box # For summary diamonds if is.summary=TRUE is used elsewhere
    ) %>%
    fp_add_header(
      `BP Measurement` = "BP Measurement", # Header for the first text column
      `IRR (95% CI)` = "IRR (95% CI)",   # Header for the second text column
      `p value` = "p value"             # Header for the third text column
    ) %>%
    fp_set_zebra_style("#EFEFEF") # Apply zebra stripes

  return(fp)
}


# --- Cardiovascular Events ---
message("Processing: All Cardiovascular Events")

# SBP - Cardiovascular
bp_cols_sbp_cvd_m1 <- list(spot_bp_sys = "Clinic SBP", idaco_day_sbpbr = "Daytime SBP", idaco_night_sbpbr = "Nighttime SBP", idaco_sbpbr_avg = "24h SBP")
forest_data_cvd_sbp_m1 <- run_poisson_and_prep_forest(updated_combined_data, bp_cols_sbp_cvd_m1, "all_cardio_event", "time_to_all_cardio", "SBP Model 1", scaling_factor = 10)

bp_cols_sbp_cvd_m2 <- list(spot_bp_sys_res1 = "Clinic SBP", idaco_day_sbpbr_res1 = "Daytime SBP", idaco_night_sbpbr_res1 = "Nighttime SBP", idaco_sbpbr_avg_res1 = "24h SBP")
forest_data_cvd_sbp_m2 <- run_poisson_and_prep_forest(updated_combined_data, bp_cols_sbp_cvd_m2, "all_cardio_event", "time_to_all_cardio", "SBP Model 2", scaling_factor = 10, is_model2 = TRUE) # Assuming _res1 also scaled by 10

combined_data_cvd_sbp <- bind_rows(
  forest_data_cvd_sbp_m1 %>% mutate(group = "Model 1"),
  forest_data_cvd_sbp_m2 %>% mutate(group = "Model 2")
) %>%
  select(label, group, estimate, conf.low, conf.high, p_value_str) %>%
  pivot_wider(
    id_cols = label,
    names_from = group,
    values_from = c(estimate, conf.low, conf.high, p_value_str)
  )

combined_data_cvd_sbp <- combined_data_cvd_sbp |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(cols = everything() & -label) |>
  mutate(group = sapply(strsplit(name, "_"), function(x) tail(x, 1)),
         name = sapply(strsplit(name, "_"), function(x) head(x, 1))) |>
  pivot_wider() |>
  mutate(across(c(estimate, conf.low, conf.high), as.double)) |>
  group_by(label) |>
   mutate(`BP Measurement` = if_else(!duplicated(label), label, ""),
          "IRR (95% CI)" = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
          "p value" = p) |>
  group_by(group)

all_cardio_plot_sbp <- create_custom_forestplot(
  plot_data = combined_data_cvd_sbp,
  model_legend_names = c("Model 1", "Model 2"),
  plot_title = "SBP and All Cardiovascular Events",
  clip_range = c(0.5, 2.5), # Adjust as needed for IRR
  xticks_range = c(0.5, 1, 1.5, 2.0, 2.5) # Adjust as needed
)

# DBP - Cardiovascular
bp_cols_dbp_cvd_m1 <- list(spot_bp_dys = "Clinic DBP", idaco_day_dia = "Daytime DBP", idaco_night_dia = "Nighttime DBP", idaco_dia_avg = "24h DBP")
forest_data_cvd_dbp_m1 <- run_poisson_and_prep_forest(updated_combined_data, bp_cols_dbp_cvd_m1, "all_cardio_event", "time_to_all_cardio", "DBP Model 1", scaling_factor = 5)

bp_cols_dbp_cvd_m2 <- list(spot_bp_dys_res1 = "Clinic DBP", idaco_day_dia_res1 = "Daytime DBP", idaco_night_dia_res1 = "Nighttime DBP", idaco_dia_avg_res1 = "24h DBP")
forest_data_cvd_dbp_m2 <- run_poisson_and_prep_forest(updated_combined_data, bp_cols_dbp_cvd_m2, "all_cardio_event", "time_to_all_cardio", "DBP Model 2", scaling_factor = 5, is_model2 = TRUE) # Assuming _res1 also scaled by 5

combined_data_cvd_dbp <- bind_rows(
  forest_data_cvd_dbp_m1 %>% mutate(group = "Model 1"),
  forest_data_cvd_dbp_m2 %>% mutate(group = "Model 2")
) %>%
  select(label, group, estimate, conf.low, conf.high, p_value_str) %>%
  pivot_wider(
    id_cols = label,
    names_from = group,
    values_from = c(estimate, conf.low, conf.high, p_value_str)
  )

combined_data_cvd_dbp <- combined_data_cvd_dbp |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(cols = everything() & -label) |>
  mutate(group = sapply(strsplit(name, "_"), function(x) tail(x, 1)),
         name = sapply(strsplit(name, "_"), function(x) head(x, 1))) |>
  pivot_wider() |>
  mutate(across(c(estimate, conf.low, conf.high), as.double)) |>
  group_by(label) |>
   mutate(`BP Measurement` = if_else(!duplicated(label), label, ""),
          "IRR (95% CI)" = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
          "p value" = p) |>
  group_by(group)

all_cardio_plot_dbp <- create_custom_forestplot(
  plot_data = combined_data_cvd_dbp,
  model_legend_names = c("Model 1", "Model 2"),
  plot_title = "DBP and All Cardiovascular Events",
  clip_range = c(0.5, 2.5),
  xticks_range = c(0.5, 1, 1.5, 2.0, 2.5)
)

# Combine SBP & DBP Cardiovascular plots
# Convert forestplot objects to grobs then use patchwork
if (!is.null(all_cardio_plot_sbp) && !is.null(all_cardio_plot_dbp)) {
  grob_cvd_sbp <- grid2grob(print(all_cardio_plot_sbp))
  grob_cvd_dbp <- grid2grob(print(all_cardio_plot_dbp))
  
  combined_cvd_plots <- wrap_plots(grob_cvd_sbp, grob_cvd_dbp, nrow = 2) + 
    plot_annotation(title = "Poisson Model: All Cardiovascular Events N = 34",
                    theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))
  #print(combined_cvd_plots)
} else {
  message("One or both cardiovascular forest plots could not be generated.")
}


# --- All-Cause Mortality ---
message("Processing: All-Cause Mortality")

# SBP - Mortality
bp_cols_sbp_mort_m1 <- list(spot_bp_sys = "Clinic SBP", idaco_day_sbpbr = "Daytime SBP", idaco_night_sbpbr = "Nighttime SBP", idaco_sbpbr_avg = "24h SBP")
forest_data_mort_sbp_m1 <- run_poisson_and_prep_forest(updated_combined_data, bp_cols_sbp_mort_m1, "mortality_event", "time_to_event", "SBP Model 1", scaling_factor = 10)

bp_cols_sbp_mort_m2 <- list(spot_bp_sys_res1 = "Clinic SBP", idaco_day_sbpbr_res1 = "Daytime SBP", idaco_night_sbpbr_res1 = "Nighttime SBP", idaco_sbpbr_avg_res1 = "24h SBP")
forest_data_mort_sbp_m2 <- run_poisson_and_prep_forest(updated_combined_data, bp_cols_sbp_mort_m2, "mortality_event", "time_to_event", "SBP Model 2", scaling_factor = 10, is_model2 = TRUE)

combined_data_mort_sbp <- bind_rows(
  forest_data_mort_sbp_m1 %>% mutate(group = "Model 1"),
  forest_data_mort_sbp_m2 %>% mutate(group = "Model 2")
) %>%
  select(label, group, estimate, conf.low, conf.high, p_value_str) %>%
  pivot_wider(
    id_cols = label,
    names_from = group,
    values_from = c(estimate, conf.low, conf.high, p_value_str)
  )

combined_data_mort_sbp <- combined_data_mort_sbp |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(cols = everything() & -label) |>
  mutate(group = sapply(strsplit(name, "_"), function(x) tail(x, 1)),
         name = sapply(strsplit(name, "_"), function(x) head(x, 1))) |>
  pivot_wider() |>
  mutate(across(c(estimate, conf.low, conf.high), as.double)) |>
  group_by(label) |>
   mutate(`BP Measurement` = if_else(!duplicated(label), label, ""),
          "IRR (95% CI)" = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
          "p value" = p) |>
  group_by(group)

all_mortality_plot_sbp <- create_custom_forestplot(
  plot_data = combined_data_mort_sbp,
  model_legend_names = c("Model 1", "Model 2"),
  plot_title = "SBP and All-Cause Mortality",
  clip_range = c(0.7, 2.0), # Adjust as needed for IRR
  xticks_range = c(0.7, 1, 1.5, 2.0) # Adjust as needed
)

# DBP - Mortality
bp_cols_dbp_mort_m1 <- list(spot_bp_dys = "Clinic DBP", idaco_day_dia = "Daytime DBP", idaco_night_dia = "Nighttime DBP", idaco_dia_avg = "24h DBP")
forest_data_mort_dbp_m1 <- run_poisson_and_prep_forest(updated_combined_data, bp_cols_dbp_mort_m1, "mortality_event", "time_to_event", "DBP Model 1", scaling_factor = 5)

bp_cols_dbp_mort_m2 <- list(spot_bp_dys_res1 = "Clinic DBP", idaco_day_dia_res1 = "Daytime DBP", idaco_night_dia_res1 = "Nighttime DBP", idaco_dia_avg_res1 = "24h DBP")
forest_data_mort_dbp_m2 <- run_poisson_and_prep_forest(updated_combined_data, bp_cols_dbp_mort_m2, "mortality_event", "time_to_event", "DBP Model 2", scaling_factor = 5, is_model2 = TRUE)

combined_data_mort_dbp <- bind_rows(
  forest_data_mort_dbp_m1 %>% mutate(group = "Model 1"),
  forest_data_mort_dbp_m2 %>% mutate(group = "Model 2")
) %>%
  select(label, group, estimate, conf.low, conf.high, p_value_str) %>%
  pivot_wider(
    id_cols = label,
    names_from = group,
    values_from = c(estimate, conf.low, conf.high, p_value_str)
  )

combined_data_mort_dbp <- combined_data_mort_dbp |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(cols = everything() & -label) |>
  mutate(group = sapply(strsplit(name, "_"), function(x) tail(x, 1)),
         name = sapply(strsplit(name, "_"), function(x) head(x, 1))) |>
  pivot_wider() |>
  mutate(across(c(estimate, conf.low, conf.high), as.double)) |>
  group_by(label) |>
   mutate(`BP Measurement` = if_else(!duplicated(label), label, ""),
          "IRR (95% CI)" = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
          "p value" = p) |>
  group_by(group)

all_mortality_plot_dbp <- create_custom_forestplot(
  plot_data = combined_data_mort_dbp,
  model_legend_names = c("Model 1", "Model 2"),
  plot_title = "DBP and All-Cause Mortality",
  clip_range = c(0.7, 2.0),
  xticks_range = c(0.7, 1, 1.5, 2.0)
)

# Combine SBP & DBP Mortality plots
if (!is.null(all_mortality_plot_sbp) && !is.null(all_mortality_plot_dbp)) {
  grob_mort_sbp <- grid2grob(print(all_mortality_plot_sbp))
  grob_mort_dbp <- grid2grob(print(all_mortality_plot_dbp))

  combined_mort_plots <- wrap_plots(grob_mort_sbp, grob_mort_dbp, nrow = 2) + 
    plot_annotation(title = "Poisson Model: All-Cause Mortality N = 118",
                    theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))
  #print(combined_mort_plots)
} else {
  message("One or both mortality forest plots could not be generated.")
}


# --- Combine All Plots (Mortality and Cardiovascular) ---
if (exists("combined_mort_plots") && exists("combined_cvd_plots")) {
    if (!is.null(combined_mort_plots) && !is.null(combined_cvd_plots)) {
        all_final_plots <- ggarrange(combined_mort_plots, combined_cvd_plots, ncol = 2, common.legend = FALSE)
        # To add an overall title to the ggarrange object, you might need to use annotate_figure
        # or convert the ggarrange object itself if it supports plot_annotation directly.
        # For simplicity, titles are on the combined_mort_plots and combined_cvd_plots.
        # print(all_final_plots)
        
        # Example for saving:
        ggsave("all_poisson_forest_plots.png", 
               path = "C:\\Users\\cmwagwabi\\OneDrive - Kemri Wellcome Trust\\Documents - Anthony Etyang's files\\ABPM and HDSS events Data_Shared with Clement\\Project Folder - Clement\\Manuscript\\results_v7\\Figures",
               plot = all_final_plots, width = 18, height = 12, dpi = 300)

    } else {
        message("Could not combine all plots as some intermediate plots are missing.")
    }
} else {
    message("Could not combine all plots as combined_mort_plots or combined_cvd_plots does not exist.")
}
