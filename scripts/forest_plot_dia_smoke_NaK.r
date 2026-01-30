#Load the libraries
library(survival)
library(dplyr)
library(ggplot2)
library(broom)
library(gtsummary)
library(patchwork)
remotes::install_local('/Users/mwagwabi/Library/CloudStorage/OneDrive-Personal/Documents/Clement R/forestmodel', force = TRUE)
library(forestmodel)
library(labelled)
library(forestplot)
library(broom)
library(dplyr)
library(ggplotify)
library(ggpubr)

###########################################
updated_combined_data_strat <- updated_combined_data |>
  mutate(diabetes = ifelse(a1c_imp >= 6.5, "Yes", "No"),
         diabetes = factor(diabetes, levels = c("No", "Yes"))) 

#relaballing columns
var_label(updated_combined_data_strat) <- list(
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
  current_smoke = "Smoking Status",
  diabetes = "Diabetes Status",
  idaco_bp_phenotype = "Hypertension Phenotypes",
  sod_pot_imp = "Urine Na to K ratio"
)

#Plotting panels
panels_1 <- list(
  list(width = 0.01),
  list(width = 0.05, display = ~variable, fontface = "bold", heading = "BP Measurement"),
  list(width = 0.05, hjust = 0.3, display = ~level),
  list(width = 0.03, display = ~n_events , hjust = 0.3, heading = "n"),
  list(width = 0.05, display = ~n, hjust = 1, heading = "N"),
  list(width = 0.03, item = "vline", hjust = 0.5),
  list(
    width = 0.35, item = "forest", hjust = 0.5, heading = "Systolic Blood Pressure", linetype = "dashed",
    line_x = 0
  ),
  list(width = 0.03, item = "vline", hjust = 0.5),
  list(width = 0.10, display = ~ ifelse(reference, "Reference", sprintf(
    "%0.2f (%0.2f, %0.2f)",
    trans(estimate), trans(conf.low), trans(conf.high)
  )), display_na = NA, heading = "HR(95% CI)"),
  list(
    width = 0.05,
    display = ~ ifelse(reference, "", format.pval(p.value, digits = 1, eps = 0.001)),
    display_na = NA, hjust = 1, heading = "p value"
  ),
  list(width = 0.01)
)

panels_2 <- list(
  list(width = 0.01),
  list(width = 0.05, display = ~variable, fontface = "bold", heading = "BP Measurement"),
  list(width = 0.05, hjust = 0.3, display = ~level),
  list(width = 0.03, display = ~n_events , hjust = 0.3, heading = "n"),
  list(width = 0.05, display = ~n, hjust = 1, heading = "N"),
  list(width = 0.03, item = "vline", hjust = 0.5),
  list(
    width = 0.35, item = "forest", hjust = 0.5, heading = " Diastolic Blood Pressure", linetype = "dashed",
    line_x = 0
  ),
  list(width = 0.03, item = "vline", hjust = 0.5),
  list(width = 0.10, display = ~ ifelse(reference, "Reference", sprintf(
    "%0.2f (%0.2f, %0.2f)",
    trans(estimate), trans(conf.low), trans(conf.high)
  )), display_na = NA, heading = "HR(95% CI)"),
  list(
    width = 0.05,
    display = ~ ifelse(reference, "", format.pval(p.value, digits = 1, eps = 0.001)),
    display_na = NA, hjust = 1, heading = "p value"
  ),
  list(width = 0.01)
)

format <- forest_model_format_options(
  colour = "black",
  shape = 15,
  text_size = 10,
  point_size = 5,
  banded = TRUE
)

#all_mortality systolic model 1
# List of blood pressure columns and their labels
bp_columns <- list(
  idaco_day_sbpbr = "24h SBP"

)

# Prepare a list to store the regression tables
cox_models <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
   # Calculate the standard deviation and round to 1 decimal place
  sd_value <- round(sd(updated_combined_data_strat[[bp_col]], na.rm = TRUE), 1)
  sd_values[[bp_columns[[bp_col]]]] <- sd_value
  
  # Scale the blood pressure column
 updated_combined_data_1 <-updated_combined_data_strat |>
    mutate(!!bp_col := c(2 * scale(!!sym(bp_col))))
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time =updated_combined_data_1$time_to_event, event =updated_combined_data_1$mortality_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~ ", `bp_col`, " + age + sex + bmi + on_med_htn + current_smoke + diabetes + sod_pot_imp"))

  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data =updated_combined_data_1)

  # Unname the xlevels in the fitted Cox model
  cox_model$xlevels <- unname(cox_model$xlevels)
  
  # Store the Cox model in the list with the original column name
  cox_models[[bp_col]] <- cox_model
}

# Create the forest plot with merge_models using model_list
forest_plot_1 <- forest_model(
  cox_model,
  covariates = c("24h_SBP", "diabetes", "current_smoke", "sod_pot_imp"),
  merge_models = TRUE,
  exponentiate = TRUE,
  format_options = format,
  panels = panels_1,
  recalculate_width = FALSE
)

# Optional: Save to file
output_dir <- file.path("..", "Manuscript", "final_manuscript_results", "Figures")

# Check if the directory exists and create it if not
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


ggsave(file.path(output_dir, "covariate.png"),
       height = 20, width = 40, units = "cm",
       dpi = 400)
