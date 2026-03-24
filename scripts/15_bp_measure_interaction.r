#####################################
#Run after running Cox_regression
# Load required package
library(survival)
library(tidyverse)
library(readxl)
library(readr)
library(gt)
library(haven)
library(writexl)
library(gtsummary)
library(tidycmprsk)

###############################################
updated_combined_data <- updated_combined_data |>
  mutate(Age = case_when(
    age < 45 ~ "18 - 44",
    age >= 45 & age < 70 ~ "45 - 69",
    age >= 60 ~ "70+"
  ))

#Cox per SD systolic 1
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_sys = "Clinic SBP",
  idaco_day_sbpbr = "Daytime SBP",
  idaco_night_sbpbr = "Nighttime SBP",
  idaco_sbpbr_avg = "24h SBP"
)

# Prepare a list to store the regression tables
regression_tables <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
  
  # Scale the blood pressure column by 10 units
  updated_combined_data_1 <- updated_combined_data %>%
    mutate(scaled_bp = !!sym(bp_col) / 10)
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time = updated_combined_data_1$time_to_event, event = updated_combined_data_1$mortality_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "* Age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"))

  cox_model <- coxph(model_formula, data = updated_combined_data_1)
  
  # Create a regression table and rename the variable
  reg_table <- tbl_regression(
    cox_model,
    exponentiate = TRUE,
    label = list(
      scaled_bp = bp_columns[[bp_col]]
    )
  ) %>%
  add_nevent(location = "level") %>%
  # Remove the confounder rows
  modify_table_body(~ .x %>% 
                    filter(!variable %in% c("Age", "sex", "bmi", "on_med_htn", "current_smoke", "a1c_imp", "sod_pot_imp", "study", "study")))

  # Add the regression table to the list
  regression_tables[[bp_columns[[bp_col]]]] <- reg_table
}

# Display the regression tables
cox_sys_regression_table_1 <- tbl_stack(regression_tables) %>%
                   modify_table_styling(
                    column = estimate,
                    rows = !is.na(estimate),
                    cols_merge_pattern = "{estimate} ({conf.low}, {conf.high})"
                   ) %>%
                   modify_header(estimate ~ "**HR (95% CI)**") %>%
                   modify_header(label ~ "**BP Measurement**") %>%
                   modify_column_hide(c(conf.low)) %>%
                   modify_table_styling(
                    column = estimate,
                    footnote = "HR per 10mmHg in usual blood pressure"
                   )
# Display the combined regression table
cox_sys_regression_table_1


#All Cardio Cox per SD systolic 1
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_sys = "Clinic SBP",
  idaco_day_sbpbr = "Daytime SBP",
  idaco_night_sbpbr = "Nighttime SBP",
  idaco_sbpbr_avg = "24h SBP"
)

# Prepare a list to store the regression tables
regression_tables <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
  
  # Scale the blood pressure column by 10 units
  updated_combined_data_1 <- updated_combined_data %>%
    mutate(scaled_bp = !!sym(bp_col) / 10)
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time = updated_combined_data_1$time_to_event, event = updated_combined_data_1$all_cardio_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "* Age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"))

  cox_model <- coxph(model_formula, data = updated_combined_data_1)
  # Create a regression table and rename the variable
  reg_table <- tbl_regression(
    cox_model,
    exponentiate = TRUE,
    label = list(
      scaled_bp = bp_columns[[bp_col]]
    )
  ) %>%
  add_nevent(location = "level") %>%
  # Remove the confounder rows
  modify_table_body(~ .x %>% filter(!variable %in% c("Age", "sex", "bmi", "on_med_htn", "current_smoke", "a1c_imp", "sod_pot_imp", "study")))

  # Add the regression table to the list
  regression_tables[[bp_columns[[bp_col]]]] <- reg_table
}

# Display the regression tables
all_cardio_cox_sys_regression_table_1 <- tbl_stack(regression_tables) %>%
                   modify_table_styling(
                    column = estimate,
                    rows = !is.na(estimate),
                    cols_merge_pattern = "{estimate} ({conf.low}, {conf.high})"
                   ) %>%
                   modify_header(estimate ~ "**HR (95% CI)**") %>%
                   modify_header(label ~ "**BP Measurement**") %>%
                   modify_column_hide(c(conf.low)) %>%
                    modify_table_styling(
                    column = estimate,
                    footnote = "HR per 10mmHg in usual blood pressure"
                   )
# Display the combined regression table
all_cardio_cox_sys_regression_table_1



#Combining all cardiovascular systolic BP
all_cardio_systolic_cox <- tbl_merge(tbls = list(cox_systolic_combined, all_cardio_mortality),
                             tab_spanner = FALSE) %>%
                  as_gt() %>%
                  gt::tab_spanner(
                    label = gt::md("**All-cause Mortality**"), 
                    columns = c(estimate_1_1, ci_1_1, p.value_1_1, estimate_2_1, ci_2_1, p.value_2_1),
                    level = 2
                  ) %>%
                  gt::tab_spanner(
                    label = gt::md("**All Cardiovascular Events**"), 
                    columns = c(estimate_1_2, ci_1_2, p.value_1_2, estimate_2_2, ci_2_2, p.value_2_2),
                    level = 2
                  ) %>%
                  gt::gtsave(filename = "all_systolic_study_interaction.DOCX", path = "C:\\Users\\cmwagwabi\\OneDrive - Kemri Wellcome Trust\\Documents - Anthony Etyang's files\\ABPM and HDSS events Data_Shared with Clement\\Project Folder - Clement\\Manuscript\\results_v7")
all_cardio_systolic_cox

###############################################
#Cox per SD systolic 1
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_dys = "Clinic DBP",
  idaco_day_dia = "Daytime DBP",
  idaco_night_dia = "Nighttime DBP",
  idaco_dia_avg = "24h DBP"
)

# Prepare a list to store the regression tables
regression_tables <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
  
  # Scale the blood pressure column by 10 units
  updated_combined_data_1 <- updated_combined_data %>%
    mutate(scaled_bp = !!sym(bp_col) / 5)
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time = updated_combined_data_1$time_to_event, event = updated_combined_data_1$mortality_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "* Age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"))

  cox_model <- coxph(model_formula, data = updated_combined_data_1)
  # Create a regression table and rename the variable
  reg_table <- tbl_regression(
    cox_model,
    exponentiate = TRUE,
    label = list(
      scaled_bp = bp_columns[[bp_col]]
    )
  ) %>%
  add_nevent(location = "level") %>%
  # Remove the confounder rows
  modify_table_body(~ .x %>% 
                    filter(!variable %in% c("Age", "sex", "bmi", "on_med_htn", "current_smoke", "a1c_imp", "sod_pot_imp", "study", "study")))

  # Add the regression table to the list
  regression_tables[[bp_columns[[bp_col]]]] <- reg_table
}

# Display the regression tables
cox_dys_regression_table_1 <- tbl_stack(regression_tables) %>%
                   modify_table_styling(
                    column = estimate,
                    rows = !is.na(estimate),
                    cols_merge_pattern = "{estimate} ({conf.low}, {conf.high})"
                   ) %>%
                   modify_header(estimate ~ "**HR (95% CI)**") %>%
                   modify_header(label ~ "**BP Measurement**") %>%
                   modify_column_hide(c(conf.low)) %>%
                   modify_table_styling(
                    column = estimate,
                    footnote = "HR per 5mmHg in usual blood pressure"
                   )
# Display the combined regression table
cox_dys_regression_table_1


#All Cardio Cox per SD systolic 1
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_dys = "Clinic DBP",
  idaco_day_dia = "Daytime DBP",
  idaco_night_dia = "Nighttime DBP",
  idaco_dia_avg = "24h DBP"
)

# Prepare a list to store the regression tables
regression_tables <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
  
  # Scale the blood pressure column by 10 units
  updated_combined_data_1 <- updated_combined_data %>%
    mutate(scaled_bp = !!sym(bp_col) / 10)
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time = updated_combined_data_1$time_to_event, event = updated_combined_data_1$all_cardio_event)
  
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "* Age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"))

  cox_model <- coxph(model_formula, data = updated_combined_data_1)
  # Create a regression table and rename the variable
  reg_table <- tbl_regression(
    cox_model,
    exponentiate = TRUE,
    label = list(
      scaled_bp = bp_columns[[bp_col]]
    )
  ) %>%
  add_nevent(location = "level") %>%
  # Remove the confounder rows
  modify_table_body(~ .x %>% filter(!variable %in% c("Age", "sex", "bmi", "on_med_htn", "current_smoke", "a1c_imp", "sod_pot_imp", "study")))

  # Add the regression table to the list
  regression_tables[[bp_columns[[bp_col]]]] <- reg_table
}

# Display the regression tables
all_cardio_cox_dys_regression_table_1 <- tbl_stack(regression_tables) %>%
                   modify_table_styling(
                    column = estimate,
                    rows = !is.na(estimate),
                    cols_merge_pattern = "{estimate} ({conf.low}, {conf.high})"
                   ) %>%
                   modify_header(estimate ~ "**HR (95% CI)**") %>%
                   modify_header(label ~ "**BP Measurement**") %>%
                   modify_column_hide(c(conf.low)) %>%
                    modify_table_styling(
                    column = estimate,
                    footnote = "HR per 5mmHg in usual blood pressure"
                   )
# Display the combined regression table
all_cardio_cox_dys_regression_table_1

#Combining all cardiovascular systolic BP
all_cardio_diastolic_cox <- tbl_merge(tbls = list(cox_diastolic_combined, all_cardio_mortality_dia),
                             tab_spanner = FALSE) %>%
                  as_gt() %>%
                  gt::tab_spanner(
                    label = gt::md("**All-cause Mortality**"), 
                    columns = c(estimate_1_1, ci_1_1, p.value_1_1, estimate_2_1, ci_2_1, p.value_2_1),
                    level = 2
                  ) %>%
                  gt::tab_spanner(
                    label = gt::md("**All Cardiovascular Events**"), 
                    columns = c(estimate_1_2, ci_1_2, p.value_1_2, estimate_2_2, ci_2_2, p.value_2_2),
                    level = 2
                  ) %>%
                  gt::gtsave(filename = "all_diastolic_study_interaction.DOCX", path = "C:\\Users\\cmwagwabi\\OneDrive - Kemri Wellcome Trust\\Documents - Anthony Etyang's files\\ABPM and HDSS events Data_Shared with Clement\\Project Folder - Clement\\Manuscript\\results_v7")
all_cardio_diastolic_cox