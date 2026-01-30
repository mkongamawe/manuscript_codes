# --- 1. LOAD LIBRARIES ---
# install.packages(c("dplyr", "survival", "broom", "forestplot", "patchwork", "ggpubr", "stringr", "labelled"))

library(dplyr)
library(survival)
library(broom)
library(forestplot)
library(patchwork)
library(ggpubr)
library(stringr)
library(labelled) # For var_label
library(grid)     # For gpar
library(ggplotify)

# --- 2. THE MASTER ANALYSIS FUNCTION ---

#' Runs Cox models and generates a forest plot for a specific outcome.
#'
#' @param data The main dataframe (`updated_combined_data`).
#' @param outcome_config A list containing all settings for the analysis,
#'   including outcome name, event/time columns, and BP variable lists.
#' @return A forestplot object (a grob).

run_analysis_for_outcome <- function(data, outcome_config) {

  message(paste("\n--- Starting Analysis for:", outcome_config$title, "---"))

  # =================================================================
  # A. FIT MODELS FOR EACH BP TYPE (e.g., SBP and DBP)
  # =================================================================
  all_results <- list()

  for (bp_type in names(outcome_config$bp_groups)) {
    
    config_group <- outcome_config$bp_groups[[bp_type]]
    message(paste("  -> Processing", bp_type, "models..."))
    
    # --- Run Model 1 (Always runs) ---
    message("    - Fitting Model 1 (unadjusted)...")
    model1_data <- purrr::map_df(names(config_group$model1_vars), ~{
      bp_col <- .x
      # Calculate the standard deviation of the original BP column
      sd_value <- sd(data[[bp_col]], na.rm = TRUE)
      half_sd <- 0.5 * sd_value
      
      # Print the half SD value for this BP measurement
      message(paste("    - For", config_group$model1_vars[[bp_col]], ", 0.5 SD =", round(half_sd, 2), "mmHg"))

      # The scaling must be applied back to the original column name.
      data_scaled <- data %>% mutate(!!bp_col := 2 * as.numeric(scale(!!sym(bp_col))))
      
      model_formula <- as.formula(paste("Surv(time=", outcome_config$time_col, ", event=", outcome_config$event_col, ") ~",
                                        bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"))
      
      cox_model <- coxph(model_formula, data = data_scaled)
      
      tidy(cox_model, conf.int = TRUE, exponentiate = TRUE) %>%
        filter(term == bp_col) %>%
        mutate(label = config_group$model1_vars[[bp_col]])
    }) %>% mutate(group = "Model 1",
                  p.value = format.pval(p.value, eps = 0.001, digits = 2))

    # --- Run Model 2 (Interactive) ---
    run_model_2 <- NA
    while (is.na(run_model_2)) {
      response <- readline(prompt = paste0("    ? Do you want to run Model 2 (adjusted) for ", bp_type, "? (yes/no): "))
      response <- tolower(trimws(response))
      if (response %in% c("yes", "y")) {
        run_model_2 <- TRUE
      } else if (response %in% c("no", "n")) {
        run_model_2 <- FALSE
      } else {
        message("    ! Invalid input. Please enter 'yes' or 'no'.")
      }
    }

    if (run_model_2) {
      message("    - Fitting Model 2 (adjusted)...")
      model2_data <- purrr::map_df(names(config_group$model2_vars), ~{
        bp_col <- .x
        adj_var <- config_group$adj_vars[[bp_col]]
        # NOTE: Model 2 uses residualized variables, assuming they don't need further scaling.
        # If they did, you would add a mutate() step here as in Model 1.
        
        model_formula <- as.formula(paste("Surv(time=", outcome_config$time_col, ", event=", outcome_config$event_col, ") ~",
                                          bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp +", adj_var))
        
        cox_model <- coxph(model_formula, data = data)
        
        tidy(cox_model, conf.int = TRUE, exponentiate = TRUE) %>%
          filter(term == bp_col) %>%
          mutate(label = config_group$model2_vars[[bp_col]])
      }) %>% mutate(group = "Model 2",
                    p.value = format.pval(p.value, eps = 0.001, digits = 2))
      
      # Combine results for this BP type
      all_results[[bp_type]] <- bind_rows(model1_data, model2_data)
      
    } else {
      message("    - Skipping Model 2.")
      all_results[[bp_type]] <- model1_data
    }
  }
  
  # =================================================================
  # B. PREPARE DATA FOR FOREST PLOT
  # =================================================================
  combined_data_for_plot <- bind_rows(all_results) %>%
    select(label, group, estimate, conf.low, conf.high, p.value) %>%
    pivot_wider(
      id_cols = label,
      names_from = group,
      values_from = c(estimate, conf.low, conf.high, p.value)
    )
  
  #print(combined_data_for_plot)

  # Prepare the text table for the plot
  plot_table_data <- combined_data_for_plot %>%
    mutate(across(everything(), as.character)) |>
    pivot_longer(cols = everything() & -label) |>
    mutate(group = sapply(strsplit(name, "_"), function(x) tail(x, 1)),
           name = sapply(strsplit(name, "_"), function(x) head(x, 1))) |>
    pivot_wider() |>
    mutate(across(c(estimate, conf.low, conf.high), as.double)) |>
    group_by(label) |>
    mutate(`BP Measurement` = if_else(!duplicated(label), label, ""),
           "HR (95% CI)" = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
           "p value" = p.value) |>
    group_by(group)
    
    
  #=================================================================
  # C. CREATE THE DYNAMIC FOREST PLOT
  # =================================================================
  message("  -> Generating forest plot...")
  
  # Dynamic plot parameters
  has_model_2 <- "Model 2" %in% plot_table_data$group

  plot_table_data <- if(has_model_2) {
    combined_data_for_plot %>%
      mutate(across(everything(), as.character)) |>
      pivot_longer(cols = everything() & -label) |>
      mutate(group = sapply(strsplit(name, "_"), function(x) tail(x, 1)),
            name = sapply(strsplit(name, "_"), function(x) head(x, 1))) |>
      pivot_wider() |>
      mutate(across(c(estimate, conf.low, conf.high), as.double)) |>
      group_by(label) |>
      mutate(`BP Measurement` = if_else(!duplicated(label), label, ""),
              "HR (95% CI)" = sprintf("%.2f (%.2f-%.3f)", estimate, conf.low, conf.high),
              "p value" = p.value) |>
      group_by(group)
  } else {
    combined_data_for_plot %>%
      mutate(across(everything(), as.character)) |>
      pivot_longer(cols = everything() & -label) |>
      mutate(group = sapply(strsplit(name, "_"), function(x) tail(x, 1)),
             name = sapply(strsplit(name, "_"), function(x) head(x, 1))) |>
      pivot_wider() |>
      mutate(across(c(estimate, conf.low, conf.high), as.double)) |>
      group_by(label) |>
      mutate(`BP Measurement` = if_else(!duplicated(label), label, ""),
             "HR (95% CI)" = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
             "p value" = p.value) |>
      group_by(group) |>
      ungroup()
  }
  
  

  plot_legend <- if (has_model_2) c("Model 1", "Model 2") else "Model 1"
  box_colors <- if (has_model_2) c("royalblue", "gold") else "royalblue"
  line_colors <- if (has_model_2) c("darkblue", "orange") else "darkblue"
  
browser()

  # Create the forest plot
  final_plot <- plot_table_data |>
    forestplot(
      mean=estimate,
      lower=conf.low,
      upper=conf.high,
      labeltext = c(`BP Measurement`, "HR (95% CI)", "p value")
    )
  # final_plot <- plot_table_data %>%
  #   forestplot(mean = c(estimate),
  #            lower = c(conf.low),
  #            upper = c(conf.high),
  #            labeltext = c(`BP Measurement`, "HR (95% CI)", "p value"),
  #            xlog = TRUE,
  #            boxsize = 0.25,
  #            #legend = plot_legend,
  #            clip = outcome_config$plot_clip,
  #            col = fpColors(box = "#1c5c8f", line = "#1c5c8f"),
  #            #title = "All Cardiovascular Events (n = 33)",
  #            xticks = outcome_config$plot_xticks,
  #            graph.pos = 2,
  #            txt_gp = fpTxtGp(label = gpar(cex = 1.2),
  #                             xlab = gpar(cex = 1.2),
  #                             ticks = gpar(cex = 1.1)),
  #            hrzl_lines = list("2" = gpar(lwd = 1, col = "black")),
  #            xlab = "Hazard Ratio (HR)"#,
  #            #is.summary = c(TRUE, rep(FALSE, nrow(plot_table_data)))  # Header row is a summary ro
  #            ) |>
  #   fp_set_style(box = box_colors,
  #                line = line_colors) |>
  #   #fp_add_header("BP Measurement", "HR (95% CI)", "p value") |>
  #   fp_set_zebra_style("#EFEFEF")

  return(final_plot)
}

# --- 3. DEFINE CONFIGURATIONS AND RUN THE FULL ANALYSIS ---

# Assume 'updated_combined_data' is your fully prepared dataset
# variable labels
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
  current_smoke = "Current Smoking Status",
  idaco_bp_phenotype = "Hypertension Phenotypes"
)


# --- Define the master configuration for all analyses ---
master_config <- list(
  mortality = list(
    title = "All-cause mortality (n = 118)",
    event_col = "mortality_event",
    time_col = "time_to_event",
    plot_clip = c(0.85, 1.5),
    plot_xticks = c(0.85, 1, 1.5),
    bp_groups = list(
      "Systolic BP" = list(
        model1_vars = list(spot_bp_sys = "Clinic SBP", idaco_day_sbpbr = "Daytime SBP", idaco_night_sbpbr = "Nighttime SBP", idaco_sbpbr_avg = "24h SBP"),
        model2_vars = list(spot_bp_sys_res1 = "Clinic SBP", idaco_day_sbpbr_res1 = "Daytime SBP", idaco_night_sbpbr_res1 = "Nighttime SBP", idaco_sbpbr_avg_res1 = "24h SBP"),
        adj_vars = list(spot_bp_sys_res1 = "idaco_sbpbr_avg", idaco_day_sbpbr_res1 = "spot_bp_sys", idaco_night_sbpbr_res1 = "spot_bp_sys", idaco_sbpbr_avg_res1 = "spot_bp_sys")
      ),
      "Diastolic BP" = list(
        model1_vars = list(spot_bp_dys = "Clinic DBP", idaco_day_dia = "Daytime DBP", idaco_night_dia = "Nighttime DBP", idaco_dia_avg = "24h DBP"),
        model2_vars = list(spot_bp_dys_res1 = "Clinic DBP", idaco_day_dia_res1 = "Daytime DBP", idaco_night_dia_res1 = "Nighttime DBP", idaco_dia_avg_res1 = "24h DBP"),
        adj_vars = list(spot_bp_dys_res1 = "idaco_dia_avg", idaco_day_dia_res1 = "spot_bp_dys", idaco_night_dia_res1 = "spot_bp_dys", idaco_dia_avg_res1 = "spot_bp_dys")
      )
    )
  ),
  
  cardiovascular = list(
    title = "All cardiovascular events (n = 34)",
    event_col = "all_cardio_event",
    time_col = "time_to_all_cardio",
    plot_clip = c(0.85, 2.0),
    plot_xticks = c(0.85, 1, 1.5, 2.0),
    bp_groups = list(
      "Systolic BP" = list(
        model1_vars = list(spot_bp_sys = "Clinic SBP", idaco_day_sbpbr = "Daytime SBP", idaco_night_sbpbr = "Nighttime SBP", idaco_sbpbr_avg = "24h SBP"),
        model2_vars = list(spot_bp_sys_res1 = "Clinic SBP", idaco_day_sbpbr_res1 = "Daytime SBP", idaco_night_sbpbr_res1 = "Nighttime SBP", idaco_sbpbr_avg_res1 = "24h SBP"),
        adj_vars = list(spot_bp_sys_res1 = "idaco_sbpbr_avg", idaco_day_sbpbr_res1 = "spot_bp_sys", idaco_night_sbpbr_res1 = "spot_bp_sys", idaco_sbpbr_avg_res1 = "spot_bp_sys")
      ),
      "Diastolic BP" = list(
        model1_vars = list(spot_bp_dys = "Clinic DBP", idaco_day_dia = "Daytime DBP", idaco_night_dia = "Nighttime DBP", idaco_dia_avg = "24h DBP"),
        model2_vars = list(spot_bp_dys_res1 = "Clinic DBP", idaco_day_dia_res1 = "Daytime DBP", idaco_night_dia_res1 = "Nighttime DBP", idaco_dia_avg_res1 = "24h DBP"),
        adj_vars = list(spot_bp_dys_res1 = "idaco_dia_avg", idaco_day_dia_res1 = "spot_bp_dys", idaco_night_dia_res1 = "spot_bp_dys", idaco_dia_avg_res1 = "spot_bp_dys")
      )
    )
  )
)

# --- Loop through each analysis, run it, and store the plot ---
all_plots <- list()
for (analysis_name in names(master_config)) {
  config <- master_config[[analysis_name]]
  
  # Run the entire analysis and plotting for this outcome
  plot_grob <- run_analysis_for_outcome(data = updated_combined_data, outcome_config = config)
  plot_grob <- grid2grob(print(plot_grob))

  # Add a main title to the plot grob
  titled_plot <- plot_grob %>%
    fp_add_header(part = "top", .gp = gpar(cex = 1.5), "") #%>% # Placeholder for title alignment
#     garnish_forestplot(
#     grid.text(config$title, x = 0.5, y = 0.98, gp = gpar(cex = 1.8, fontface = "bold"))
#     )

  all_plots[[analysis_name]] <- titled_plot
}

message("\n--- All analyses complete. Combining plots. ---")

# --- 4. COMBINE AND SAVE FINAL PLOTS (INTERACTIVE) ---

# Check if any plots were successfully generated
if (length(all_plots) > 0) {
  
  # Combine all the generated plot objects into a single layout
  # This uses ggpubr's ggarrange, which handles grobs well
  combined_final_plot <- ggarrange(plotlist = all_plots, ncol = length(all_plots), nrow = 1)
  
  # Print the final combined plot to the RStudio Viewer
  print(combined_final_plot)
  
  message("\n--- Preparing to save the final plot ---")
  
  # --- Interactive Prompts for Saving ---
  
  # 1. Prompt for the base filename
  plot_filename <- readline(prompt = "Enter the desired filename (without extension, e.g., 'My_Analysis_Results'): ")
  
  # Use a default name if the user doesn't enter anything
  if (is.null(plot_filename) || trimws(plot_filename) == "") {
    plot_filename <- "Combined_Forest_Plots"
    message(paste("No filename entered. Using default:", plot_filename))
  }
  
  # 2. Prompt for the plot format (device) with validation
  plot_device <- ""
  while (!plot_device %in% c("png", "svg")) {
    plot_device <- readline(prompt = "Enter the plot format (png or svg): ")
    plot_device <- tolower(trimws(plot_device))
    
    if (!plot_device %in% c("png", "svg")) {
      message("! Invalid format. Please choose 'png' or 'svg'.")
    }
  }
  
  # 3. Construct the full filename with the chosen extension
  full_save_name <- paste0(plot_filename, ".", plot_device)
  
  # 4. Define the save path (this part is still set manually in the script)
  save_path <- "../Manuscript/final_manuscript_results/Figures" # <-- IMPORTANT: SET YOUR SAVE PATH
  
  # --- Save the Plot ---
  ggsave(full_save_name,
         plot = combined_final_plot,
         path = save_path,
         height = 20, # Adjust as needed
         width = 35,  # Adjust as needed
         units = "cm",
         dpi = 300,
         # The device is inferred from the extension, but can be specified explicitly
         device = plot_device
  )
         
  message(paste0("\n\U2705 Success! Plot saved as '", full_save_name, "' in your specified path."))

} else {
  message("\U26A0 No plots were generated, so nothing was saved.")
}