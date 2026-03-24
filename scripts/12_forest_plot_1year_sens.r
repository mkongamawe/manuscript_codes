#Load the libraries
#library(survminer)
library(survival)
library(dplyr)
library(ggplot2)
library(broom)
library(gtsummary)
library(patchwork)
library(forestmodel)
library(labelled)
#library(tidycmprsk)
library(forestplot)
library(broom)
library(dplyr)
library(ggplotify)
library(ggpubr)

###########################################
# Table to be used for sensitivity analysis
updated_combined_data_x <- updated_combined_data |>
  filter(!(time_to_event <= 365.25 | time_to_all_cardio <= 365.25))

#relaballing columns
var_label(updated_combined_data_x) <- list(
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


###########################################
#all_cardio model 1
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_sys = "Clinic SBP",
  idaco_day_sbpbr = "Daytime SBP",
  idaco_night_sbpbr = "Nighttime SBP",
  idaco_sbpbr_avg = "24h SBP"
)

# Prepare a list to store the regression tables
cox_models <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
   # Calculate the standard deviation and round to 1 decimal place
  sd_value <- round(sd(updated_combined_data_x[[bp_col]], na.rm = TRUE), 1)
  sd_values[[bp_columns[[bp_col]]]] <- sd_value
  
  # Scale the blood pressure column
 updated_combined_data_x_1 <-updated_combined_data_x %>%
    mutate(!!bp_col := c(!!sym(bp_col)/10))
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time =updated_combined_data_x_1$time_to_all_cardio, event =updated_combined_data_x_1$all_cardio_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"))

  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data =updated_combined_data_x_1)

  # Unname the xlevels in the fitted Cox model
  cox_model$xlevels <- unname(cox_model$xlevels)
  
  # Store the Cox model in the list with the original column name
  cox_models[[bp_col]] <- cox_model
}

# Prepare data for forest plot
forest_data <- names(bp_columns) %>% 
  lapply(function(bp_col) {
    tidy(cox_models[[bp_col]], conf.int = TRUE, exponentiate = TRUE) %>% 
      filter(term == bp_col) %>% 
      mutate(
        label = bp_columns[[bp_col]],
        hr_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
        p_value_str = format.pval(p.value, eps = 0.001, digits = 1)  # Format the p-value accordingly
      )
  }) %>% 
  bind_rows() %>% 
  select(label, hr_ci, estimate, conf.low, conf.high, p_value_str)

#############################################
# all_cardio model 2
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_sys_res1 = "Clinic SBP",
  idaco_day_sbpbr_res1 = "Daytime SBP",
  idaco_night_sbpbr_res1 = "Nighttime SBP",
  idaco_sbpbr_avg_res1 = "24h SBP"
)

# Prepare a list to store the regression tables
cox_models <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
   # Calculate the standard deviation and round to 1 decimal place
  sd_value <- round(sd(updated_combined_data_x[[bp_col]], na.rm = TRUE), 1)
  sd_values[[bp_columns[[bp_col]]]] <- sd_value
  
  # Determine the adjustment variable
  adjustment_var <- if (bp_col == "spot_bp_sys_res1") {
    "idaco_sbpbr_avg"
  } else {
    "spot_bp_sys"
  }

  # Scale the blood pressure column
 updated_combined_data_x_1 <-updated_combined_data_x %>%
    mutate(!!bp_col := c(!!sym(bp_col)/10))
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time =updated_combined_data_x_1$time_to_all_cardio, event =updated_combined_data_x_1$all_cardio_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp + get(adjustment_var)"))

  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data =updated_combined_data_x_1)

  # Unname the xlevels in the fitted Cox model
  cox_model$xlevels <- unname(cox_model$xlevels)
  
  # Store the Cox model in the list with the original column name
  cox_models[[bp_col]] <- cox_model
}

# Prepare data for forest plot
forest_data_2 <- names(bp_columns) %>% 
  lapply(function(bp_col) {
    tidy(cox_models[[bp_col]], conf.int = TRUE, exponentiate = TRUE) %>% 
      filter(term == bp_col) %>%
      mutate(
        label = bp_columns[[bp_col]],
        hr_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
        p_value_str = format.pval(p.value, eps = 0.001, digits = 1)  # Format the p-value accordingly
      )
  }) %>% 
  bind_rows() %>% 
  select(label, hr_ci, estimate, conf.low, conf.high, p_value_str)

#################################
# Combining all cardiovascular model1 & 2
# Combine datasets and add a group identifier
combined_data <- bind_rows(
  forest_data %>% mutate(group = "Model 1"),
  forest_data_2 %>% mutate(group = "Model 2")
) %>%
  select(label, group, estimate, conf.low, conf.high, p_value_str) %>%
  pivot_wider(
    id_cols = label,
    names_from = group,
    values_from = c(estimate, conf.low, conf.high, p_value_str)
  )

out_data <- combined_data |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(cols = everything() & -label) |>
  mutate(group = sapply(strsplit(name, "_"), function(x) tail(x, 1)),
         name = sapply(strsplit(name, "_"), function(x) head(x, 1))) |>
  pivot_wider() |>
  mutate(across(c(estimate, conf.low, conf.high), as.double)) |>
  group_by(label) |>
   mutate(`BP Measurement` = if_else(!duplicated(label), label, ""),
          "HR (95% CI)" = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
          "p value" = p) |>
  group_by(group)

all_cardio_plot <- out_data |>
  forestplot(mean = c(estimate),
             lower = c(conf.low),
             upper = c(conf.high),
             labeltext = c(`BP Measurement`, "HR (95% CI)", "p value"),
             xlog = TRUE,
             boxsize = 0.25,
             legend = c("Model 1", "Model 2"),
             clip = c(0.50, 2.5),
             col = fpColors(box = "#1c5c8f", line = "#1c5c8f"),
             #title = "All Cardiovascular Events (n = 33)",
             xticks = c(0.5, 1, 1.5, 2.0, 2.5),
             graph.pos = 2,
             txt_gp = fpTxtGp(label = gpar(cex = 1.2),
                              xlab = gpar(cex = 1.2),
                              ticks = gpar(cex = 1.1)),
             hrzl_lines = list("2" = gpar(lwd = 1, col = "black")),
             xlab = "Hazard Ratio (HR)"#,
             #is.summary = c(TRUE, rep(FALSE, nrow(out_data)))  # Header row is a summary ro
             ) |>
fp_set_style(box = c("royalblue", "gold"),
             line = c("darkblue", "orange"),
             summary = c("darkblue", "red")) |>
fp_add_header("BP Measurement", "HR (95% CI)", "p value") |>
fp_set_zebra_style("#EFEFEF")

###########################################
#all_cardio dys model 1
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_dys = "Clinic DBP",
  idaco_day_dia = "Daytime DBP",
  idaco_night_dia = "Nighttime DBP",
  idaco_dia_avg = "24h DBP"
)

# Prepare a list to store the regression tables
cox_models <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
   # Calculate the standard deviation and round to 1 decimal place
  sd_value <- round(sd(updated_combined_data_x[[bp_col]], na.rm = TRUE), 1)
  sd_values[[bp_columns[[bp_col]]]] <- sd_value
  
  # Scale the blood pressure column
 updated_combined_data_x_1 <-updated_combined_data_x %>%
    mutate(!!bp_col := c(!!sym(bp_col)/5))
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time =updated_combined_data_x_1$time_to_all_cardio, event =updated_combined_data_x_1$all_cardio_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"))

  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data =updated_combined_data_x_1)

  # Unname the xlevels in the fitted Cox model
  cox_model$xlevels <- unname(cox_model$xlevels)
  
  # Store the Cox model in the list with the original column name
  cox_models[[bp_col]] <- cox_model
}

# Prepare data for forest plot
forest_data <- names(bp_columns) %>% 
  lapply(function(bp_col) {
    tidy(cox_models[[bp_col]], conf.int = TRUE, exponentiate = TRUE) %>% 
      filter(term == bp_col) %>% 
      mutate(
        label = bp_columns[[bp_col]],
        hr_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
        p_value_str = format.pval(p.value, eps = 0.001, digits = 1)  # Format the p-value accordingly
      )
  }) %>% 
  bind_rows() %>% 
  select(label, hr_ci, estimate, conf.low, conf.high, p_value_str)

#############################################
#all_cardio model 2
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_dys_res1 = "Clinic DBP",
  idaco_day_dia_res1 = "Daytime DBP",
  idaco_night_dia_res1 = "Nighttime DBP",
  idaco_dia_avg_res1 = "24h DBP"
)

# Prepare a list to store the regression tables
cox_models <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
   # Calculate the standard deviation and round to 1 decimal place
  sd_value <- round(sd(updated_combined_data_x[[bp_col]], na.rm = TRUE), 1)
  sd_values[[bp_columns[[bp_col]]]] <- sd_value
  
  # Determine the adjustment variable
  adjustment_var <- if (bp_col == "spot_bp_dys_res1") {
    "idaco_dia_avg"
  } else {
    "spot_bp_dys"
  }

  # Scale the blood pressure column
 updated_combined_data_x_1 <-updated_combined_data_x %>%
    mutate(!!bp_col := c(!!sym(bp_col)/5))
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time =updated_combined_data_x_1$time_to_all_cardio, event =updated_combined_data_x_1$all_cardio_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp + get(adjustment_var)"))

  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data =updated_combined_data_x_1)

  # Unname the xlevels in the fitted Cox model
  cox_model$xlevels <- unname(cox_model$xlevels)
  
  # Store the Cox model in the list with the original column name
  cox_models[[bp_col]] <- cox_model
}
# Prepare data for forest plot
forest_data_2 <- names(bp_columns) %>% 
  lapply(function(bp_col) {
    tidy(cox_models[[bp_col]], conf.int = TRUE, exponentiate = TRUE) %>% 
      filter(term == bp_col) %>% 
      mutate(
        label = bp_columns[[bp_col]],
        hr_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
        p_value_str = format.pval(p.value, eps = 0.001, digits = 1)  # Format the p-value accordingly
      )
  }) %>% 
  bind_rows() %>% 
  select(label, hr_ci, estimate, conf.low, conf.high, p_value_str)

#################################
# Combining all cardiovascular model1, 2 & 3
# Combine datasets and add a group identifier
combined_data <- bind_rows(
  forest_data %>% mutate(group = "Model 1"),
  forest_data_2 %>% mutate(group = "Model 2")
) %>%
  select(label, group, estimate, conf.low, conf.high, p_value_str) %>%
  pivot_wider(
    id_cols = label,
    names_from = group,
    values_from = c(estimate, conf.low, conf.high, p_value_str)
  )

out_data <- combined_data |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(cols = everything() & -label) |>
  mutate(group = sapply(strsplit(name, "_"), function(x) tail(x, 1)),
         name = sapply(strsplit(name, "_"), function(x) head(x, 1))) |>
  pivot_wider() |>
  mutate(across(c(estimate, conf.low, conf.high), as.double)) |>
  group_by(label) |>
   mutate(`BP Measurement` = if_else(!duplicated(label), label, ""),
          "HR (95% CI)" = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
          "p value" = p) |>
  group_by(group)

all_cardio_plot_2 <- out_data |>
  forestplot(mean = c(estimate),
             lower = c(conf.low),
             upper = c(conf.high),
             labeltext = c(`BP Measurement`, "HR (95% CI)", "p value"),
             xlog = TRUE,
             boxsize = 0.25,
             legend = c("Model 1", "Model 2"),
             clip = c(0.5, 2.5),
             col = fpColors(box = "#1c5c8f", line = "#1c5c8f"),
             #title = "All Cardiovascular Events (n = 33)",
             xticks = c(0.5, 1, 1.5, 2.0, 2.5),
             graph.pos = 2,
             txt_gp = fpTxtGp(label = gpar(cex = 1.2),
                              xlab = gpar(cex = 1.2),
                              ticks = gpar(cex = 1.1)),
             hrzl_lines = list("2" = gpar(lwd = 1, col = "black")),
             xlab = "Hazard Ratio (HR)"#,
             #is.summary = c(TRUE, rep(FALSE, nrow(out_data)))  # Header row is a summary ro
             ) |>
fp_set_style(box = c("royalblue", "gold"),
             line = c("darkblue", "orange"),
             summary = c("darkblue", "red")) |>
fp_add_header("BP Measurement", "HR (95% CI)", "p value") |>
fp_set_zebra_style("#EFEFEF")

#All cardiovascular event plots SBP & DBP
p1 <- grid2grob(print(all_cardio_plot))
p2 <- grid2grob(print(all_cardio_plot_2))

all_all_cardio_dia <- wrap_plots(p1, p2, nrow = 2) +
    plot_annotation(title = "All cardiovascular events (n = 34)",
                    theme = theme(plot.title = element_text(size = 30, hjust = 0.5)))

#all_all_cardio_dia
####################################
####################################
#All all mortality
#all_mortality model 1
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_sys = "Clinic SBP",
  idaco_day_sbpbr = "Daytime SBP",
  idaco_night_sbpbr = "Nighttime SBP",
  idaco_sbpbr_avg = "24h SBP"
)

# Prepare a list to store the regression tables
cox_models <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
   # Calculate the standard deviation and round to 1 decimal place
  sd_value <- round(sd(updated_combined_data_x[[bp_col]], na.rm = TRUE), 1)
  sd_values[[bp_columns[[bp_col]]]] <- sd_value
  
  # Scale the blood pressure column
 updated_combined_data_x_1 <-updated_combined_data_x %>%
    mutate(!!bp_col := c(!!sym(bp_col)/10))
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time =updated_combined_data_x_1$time_to_event, event =updated_combined_data_x_1$mortality_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"))

  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data =updated_combined_data_x_1)

  # Unname the xlevels in the fitted Cox model
  cox_model$xlevels <- unname(cox_model$xlevels)
  
  # Store the Cox model in the list with the original column name
  cox_models[[bp_col]] <- cox_model
}

# Prepare data for forest plot
forest_data <- names(bp_columns) %>% 
  lapply(function(bp_col) {
    tidy(cox_models[[bp_col]], conf.int = TRUE, exponentiate = TRUE) %>% 
      filter(term == bp_col) %>% 
      mutate(
        label = bp_columns[[bp_col]],
        hr_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
        p_value_str = format.pval(p.value, eps = 0.001, digits = 1)  # Format the p-value accordingly
      )
  }) %>% 
  bind_rows() %>% 
  select(label, hr_ci, estimate, conf.low, conf.high, p_value_str)

#############################################
#all_mortality model 2
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_sys_res1 = "Clinic SBP",
  idaco_day_sbpbr_res1 = "Daytime SBP",
  idaco_night_sbpbr_res1 = "Nighttime SBP",
  idaco_sbpbr_avg_res1 = "24h SBP"
)

# Prepare a list to store the regression tables
cox_models <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
   # Calculate the standard deviation and round to 1 decimal place
  sd_value <- round(sd(updated_combined_data_x[[bp_col]], na.rm = TRUE), 1)
  sd_values[[bp_columns[[bp_col]]]] <- sd_value
  
  # Determine the adjustment variable
  adjustment_var <- if (bp_col == "spot_bp_sys_res1") {
    "idaco_sbpbr_avg"
  } else {
    "spot_bp_sys"
  }

  # Scale the blood pressure column
 updated_combined_data_x_1 <-updated_combined_data_x %>%
    mutate(!!bp_col := c(!!sym(bp_col)/10))
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time =updated_combined_data_x_1$time_to_event, event =updated_combined_data_x_1$mortality_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp + get(adjustment_var)"))

  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data =updated_combined_data_x_1)

  # Unname the xlevels in the fitted Cox model
  cox_model$xlevels <- unname(cox_model$xlevels)
  
  # Store the Cox model in the list with the original column name
  cox_models[[bp_col]] <- cox_model
}

# Prepare data for forest plot
forest_data_2 <- names(bp_columns) %>% 
  lapply(function(bp_col) {
    tidy(cox_models[[bp_col]], conf.int = TRUE, exponentiate = TRUE) %>% 
      filter(term == bp_col) %>% 
      mutate(
        label = bp_columns[[bp_col]],
        hr_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
        p_value_str = format.pval(p.value, eps = 0.001, digits = 1)  # Format the p-value accordingly
      )
  }) %>% 
  bind_rows() %>% 
  select(label, hr_ci, estimate, conf.low, conf.high, p_value_str)

#################################
# Combining all mortality systolic model1 & 2
# Combine datasets and add a group identifier
combined_data <- bind_rows(
  forest_data %>% mutate(group = "Model 1"),
  forest_data_2 %>% mutate(group = "Model 2")
) %>%
  select(label, group, estimate, conf.low, conf.high, p_value_str) %>%
  pivot_wider(
    id_cols = label,
    names_from = group,
    values_from = c(estimate, conf.low, conf.high, p_value_str)
  )

out_data <- combined_data |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(cols = everything() & -label) |>
  mutate(group = sapply(strsplit(name, "_"), function(x) tail(x, 1)),
         name = sapply(strsplit(name, "_"), function(x) head(x, 1))) |>
  pivot_wider() |>
  mutate(across(c(estimate, conf.low, conf.high), as.double)) |>
  group_by(label) |>
   mutate(`BP Measurement` = if_else(!duplicated(label), label, ""),
          "HR (95% CI)" = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
          "p value" = p) |>
  group_by(group)

all_mortality_plot <- out_data |>
  forestplot(mean = c(estimate),
             lower = c(conf.low),
             upper = c(conf.high),
             labeltext = c(`BP Measurement`, "HR (95% CI)", "p value"),
             xlog = TRUE,
             boxsize = 0.25,
             legend = c("Model 1", "Model 2"),
             clip = c(0.85, 2.0),
             col = fpColors(box = "#1c5c8f", line = "#1c5c8f"),
             #title = "All Mortality Events (n = 119)",
             xticks = c(0.85, 1, 1.5, 2.0),
             graph.pos = 2,
             txt_gp = fpTxtGp(label = gpar(cex = 1.2),
                              xlab = gpar(cex = 1.2),
                              ticks = gpar(cex = 1.1)),
             hrzl_lines = list("2" = gpar(lwd = 1, col = "black")),
             xlab = "Hazard Ratio (HR)"#,
             #is.summary = c(TRUE, rep(FALSE, nrow(out_data)))  # Header row is a summary ro
             ) |>
fp_set_style(box = c("royalblue", "gold"),
               line = c("darkblue", "orange"),
               summary = c("darkblue", "red")) |>
fp_add_header("BP Measurement", "HR (95% CI)", "p value") |>
fp_set_zebra_style("#EFEFEF")

####################################
#All all mortality
#all_mortality model 1
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_dys = "Clinic DBP",
  idaco_day_dia = "Daytime DBP",
  idaco_night_dia = "Nighttime DBP",
  idaco_dia_avg = "24h DBP"
)

# Prepare a list to store the regression tables
cox_models <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
   # Calculate the standard deviation and round to 1 decimal place
  sd_value <- round(sd(updated_combined_data_x[[bp_col]], na.rm = TRUE), 1)
  sd_values[[bp_columns[[bp_col]]]] <- sd_value
  
  # Scale the blood pressure column
 updated_combined_data_x_1 <-updated_combined_data_x %>%
    mutate(!!bp_col := c(!!sym(bp_col)/5))
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time =updated_combined_data_x_1$time_to_event, event =updated_combined_data_x_1$mortality_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"))

  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data =updated_combined_data_x_1)

  # Unname the xlevels in the fitted Cox model
  cox_model$xlevels <- unname(cox_model$xlevels)
  
  # Store the Cox model in the list with the original column name
  cox_models[[bp_col]] <- cox_model
}

# Prepare data for forest plot
forest_data <- names(bp_columns) %>% 
  lapply(function(bp_col) {
    tidy(cox_models[[bp_col]], conf.int = TRUE, exponentiate = TRUE) %>% 
      filter(term == bp_col) %>% 
      mutate(
        label = bp_columns[[bp_col]],
        hr_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
        p_value_str = format.pval(p.value, eps = 0.001, digits = 1)  # Format the p-value accordingly
      )
  }) %>% 
  bind_rows() %>% 
  select(label, hr_ci, estimate, conf.low, conf.high, p_value_str)

#############################################
#all_mortality model 2
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_dys_res1 = "Clinic DBP",
  idaco_day_dia_res1 = "Daytime DBP",
  idaco_night_dia_res1 = "Nighttime DBP",
  idaco_dia_avg_res1 = "24h DBP"
)

# Prepare a list to store the regression tables
cox_models <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
   # Calculate the standard deviation and round to 1 decimal place
  sd_value <- round(sd(updated_combined_data_x[[bp_col]], na.rm = TRUE), 1)
  sd_values[[bp_columns[[bp_col]]]] <- sd_value
  
  # Determine the adjustment variable
  adjustment_var <- if (bp_col == "spot_bp_dys_res1") {
    "idaco_dia_avg"
  } else {
    "spot_bp_dys"
  }

  # Scale the blood pressure column
 updated_combined_data_x_1 <-updated_combined_data_x %>%
    mutate(!!bp_col := c(!!sym(bp_col)/5))
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time =updated_combined_data_x_1$time_to_event, event =updated_combined_data_x_1$mortality_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp + get(adjustment_var)"))

  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data =updated_combined_data_x_1)

  # Unname the xlevels in the fitted Cox model
  cox_model$xlevels <- unname(cox_model$xlevels)
  
  # Store the Cox model in the list with the original column name
  cox_models[[bp_col]] <- cox_model
}
# Prepare data for forest plot
forest_data_2 <- names(bp_columns) %>% 
  lapply(function(bp_col) {
    tidy(cox_models[[bp_col]], conf.int = TRUE, exponentiate = TRUE) %>% 
      filter(term == bp_col) %>% 
      mutate(
        label = bp_columns[[bp_col]],
        hr_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
        p_value_str = format.pval(p.value, eps = 0.001, digits = 1)  # Format the p-value accordingly
      )
  }) %>% 
  bind_rows() %>% 
  select(label, hr_ci, estimate, conf.low, conf.high, p_value_str)

################################
# Combining all mortality diastolic model1 & 2
# Combine datasets and add a group identifier
combined_data <- bind_rows(
  forest_data %>% mutate(group = "Model 1"),
  forest_data_2 %>% mutate(group = "Model 2")
) %>%
  select(label, group, estimate, conf.low, conf.high, p_value_str) %>%
  pivot_wider(
    id_cols = label,
    names_from = group,
    values_from = c(estimate, conf.low, conf.high, p_value_str)
  )

out_data <- combined_data |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(cols = everything() & -label) |>
  mutate(group = sapply(strsplit(name, "_"), function(x) tail(x, 1)),
         name = sapply(strsplit(name, "_"), function(x) head(x, 1))) |>
  pivot_wider() |>
  mutate(across(c(estimate, conf.low, conf.high), as.double)) |>
  group_by(label) |>
   mutate(`BP Measurement` = if_else(!duplicated(label), label, ""),
          "HR (95% CI)" = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
          "p value" = p) |>
  group_by(group)

all_mortality_plot_2 <- out_data |>
  forestplot(mean = c(estimate),
             lower = c(conf.low),
             upper = c(conf.high),
             labeltext = c(`BP Measurement`, "HR (95% CI)", "p value"),
             xlog = TRUE,
             boxsize = 0.25,
             legend = c("Model 1", "Model 2"),
             clip = c(0.85, 2.0),
             col = fpColors(box = "#1c5c8f", line = "#1c5c8f"),
             #title = "All Mortality Events (n = 119)",
             xticks = c(0.85, 1, 1.5, 2.0),
             graph.pos = 2,
             txt_gp = fpTxtGp(label = gpar(cex = 1.2),
                              xlab = gpar(cex = 1.2),
                              ticks = gpar(cex = 1.1)),
             hrzl_lines = list("2" = gpar(lwd = 1, col = "black")),
             xlab = "Hazard Ratio (HR)"#,
             #is.summary = c(TRUE, rep(FALSE, nrow(out_data)))  # Header row is a summary ro
             ) |>
fp_set_style(box = c("royalblue", "gold"),
             line = c("darkblue", "orange"),
             summary = c("darkblue", "red")) |>
fp_add_header("BP Measurement", "HR (95% CI)", "p value") |>
fp_set_zebra_style("#EFEFEF")

#All all cardiovascular event plots
p1 <- grid2grob(print(all_mortality_plot))
p2 <- grid2grob(print(all_mortality_plot_2))

all_all_mortality_dia <- wrap_plots(p1, p2, nrow = 2) +
    plot_annotation(title = "All-cause mortality (n = 106)",
                    theme = theme(plot.title = element_text(size = 30, hjust = 0.5)))

#all_all_mortality_dia

################################
#
all_all_plots <- ggarrange(all_all_mortality_dia, all_all_cardio_dia, ncol = 2)

# Optional: Save to file
output_dir <- file.path("results", "figures")

# Check if the directory exists and create it if not
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save the plot
ggsave(file.path(output_dir, "New all mortality cardio 1yr sens.png"),
       height = 30, width = 60, units = "cm",
       dpi = 400)