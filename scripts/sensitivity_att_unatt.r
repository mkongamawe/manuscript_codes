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
#Cox per SD systolic 1
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_sys = "Clinic SBP"
)

# Prepare a list to store the regression tables
regression_tables <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
  
  # Scale the blood pressure column by 10 units
  updated_combined_data_1 <- updated_combined_data |>
  mutate(
    study = relevel(factor(study), ref = "Unattended"),
    !!bp_col := as.numeric(2 * scale(!!sym(bp_col)))
  )
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time = updated_combined_data_1$time_to_event, event = updated_combined_data_1$mortality_event)
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "* study + age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"))

  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data = updated_combined_data_1)
  # Create a regression table and rename the variable
  reg_table <- tbl_regression(
    cox_model,
    exponentiate = TRUE,
    label = setNames(
    list(bp_columns[[bp_col]]),
    bp_col
      )
    ) %>%
  # Remove the confounder rows
  modify_table_body(~ .x %>% filter(!variable %in% c("age", "sex", "bmi", "on_med_htn", "current_smoke", "a1c_imp", "sod_pot_imp"))) |>
  add_nevent(location = "level")
  # Add the regression table to the list
  regression_tables[[bp_columns[[bp_col]]]] <- reg_table
}

# Display the regression tables
cox_sys_regression_table_1 <- tbl_stack(regression_tables) %>%
                   modify_table_styling(
                    column = estimate,
                    rows = !is.na(estimate),
                    cols_merge_pattern = "{estimate}\n ({conf.low} - {conf.high})"
                   ) %>%
                   modify_header(estimate ~ "**HR (95% CI)**") %>%
                   modify_header(label ~ "**BP Measurement**") %>%
                   modify_column_hide(c(conf.low)) %>%
                   modify_table_styling(
                    column = estimate,
                    footnote_abbrev = "HR = Hazard Ratio, CI = Confidence Interval") %>%
                   modify_table_styling(
                    column = estimate,
                    footnote = "HR per 0.5 SD in blood pressure"
                   )
# Display the combined regression table
cox_sys_regression_table_1

#################################
#Cox per SD systolic 2
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_sys_res1 = "Clinic SBP"
)

# Prepare a list to store the regression tables
regression_tables <- list()

# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
  # Scale the blood pressure column by 10 units
  updated_combined_data_1 <- updated_combined_data |>
  mutate(
    study = relevel(factor(study), ref = "Unattended"),
    !!bp_col := as.numeric(2 * scale(!!sym(bp_col)))
  )
  
  # Determine the adjustment variable
  adjustment_var <- if (bp_col == "spot_bp_sys_res1") {
    "idaco_sbpbr_avg"
  } else {
    "spot_bp_sys"
  }
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time = updated_combined_data_1$time_to_event, event = updated_combined_data_1$mortality_event)
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "* study + age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp + get(adjustment_var)")) 
  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data = updated_combined_data_1)
  # Create a regression table and rename the variable
  reg_table <- tbl_regression(
    cox_model,
    exponentiate = TRUE,
    label = setNames(
    list(bp_columns[[bp_col]]),
    bp_col
      )
    ) %>%
  # Remove the confounder rows
  modify_table_body(~ .x %>% filter(!variable %in% c("age", "sex", "bmi", "on_med_htn", "current_smoke", "a1c_imp", "sod_pot_imp", "get(adjustment_var)"))) |>
  add_nevent(location = "level")


  # Add the regression table to the list
  regression_tables[[bp_columns[[bp_col]]]] <- reg_table
}

# Display the regression tables
cox_sys_regression_table_2 <- tbl_stack(regression_tables) %>%
                   modify_table_styling(
                    column = estimate,
                    rows = !is.na(estimate),
                    cols_merge_pattern = "{estimate}\n ({conf.low} - {conf.high})"
                   ) %>%
                   modify_header(estimate ~ "**HR (95% CI)**") %>%
                   modify_header(label ~ "**BP Measurement**") %>%
                   modify_column_hide(c(conf.low)) %>%
                   modify_table_styling(
                    column = estimate,
                    footnote_abbrev = "HR = Hazard Ratio, CI = Confidence Interval") %>%
                   modify_table_styling(
                    column = estimate,
                    footnote = "HR per 0.5 SD in blood pressure"
                   )
# Display the combined regression table
cox_sys_regression_table_2

#Cox per SD systolic combined
cox_systolic_combined <- tbl_merge(tbls = list(cox_sys_regression_table_1, cox_sys_regression_table_2),
                                        tab_spanner = c("**Confounder adjusted**", "**Additionally adjusted for 24-hour blood pressure**"))
                         #as_gt() |>
                         #gt::gtsave(filename = "all mortality systolic att unatt sens.DOCX", path = "C:\\Users\\cmwagwabi\\OneDrive - Kemri Wellcome Trust\\Documents - Anthony Etyang's files\\ABPM and HDSS events Data_Shared with Clement\\Project Folder - Clement\\Manuscript\\results_v2")


cox_systolic_combined

######################################################
#Cox per SD diastolic 1
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_dys = "Clinic DBP"
)

# Prepare a list to store the regression tables
regression_tables <- list()
sd_values <- list()
# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
  # Scale the blood pressure column by 10 units
  updated_combined_data_1 <- updated_combined_data |>
  mutate(
    study = relevel(factor(study), ref = "Unattended"),
    !!bp_col := as.numeric(2 * scale(!!sym(bp_col)))
  )
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time = updated_combined_data_1$time_to_event, event = updated_combined_data_1$mortality_event)
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "* study + age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"))

  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data = updated_combined_data_1)
  # Create a regression table and rename the variable
  reg_table <- tbl_regression(
    cox_model,
    exponentiate = TRUE,
    label = setNames(
    list(bp_columns[[bp_col]]),
    bp_col
      )
    ) %>%
  # Remove the confounder rows
  modify_table_body(~ .x %>% filter(!variable %in% c("age", "sex", "bmi", "on_med_htn", "current_smoke", "a1c_imp", "sod_pot_imp"))) |>
  add_nevent(location = "level")

  # Add the regression table to the list
  regression_tables[[bp_columns[[bp_col]]]] <- reg_table
}

# Display the regression tables
cox_dia_regression_table_1 <- tbl_stack(regression_tables) %>%
                   modify_table_styling(
                    column = estimate,
                    rows = !is.na(estimate),
                    cols_merge_pattern = "{estimate}\n ({conf.low} - {conf.high})"
                   ) %>%
                   modify_header(estimate ~ "**HR (95% CI)**") %>%
                   modify_header(label ~ "**BP Measurement**") %>%
                   modify_column_hide(c(conf.low)) %>%
                   modify_table_styling(
                    column = estimate,
                    footnote_abbrev = "HR = Hazard Ratio, CI = Confidence Interval") %>%
                   modify_table_styling(
                    column = estimate,
                    footnote = "HR per 0.5 SD in blood pressure"
                   )
# Display the combined regression table
cox_dia_regression_table_1

#Cox per SD diastolic 2
# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_dys_res1 = "Clinic DBP"
)

# Prepare a list to store the regression tables
regression_tables <- list()

# Loop through each blood pressure column and fit Cox model
for (bp_col in names(bp_columns)) {
  # Scale the blood pressure column by 10 units
  updated_combined_data_1 <- updated_combined_data |>
  mutate(
    study = relevel(factor(study), ref = "Unattended"),
    !!bp_col := as.numeric(2 * scale(!!sym(bp_col)))
  )
  
  # Determine the adjustment variable
  adjustment_var <- if (bp_col == "spot_bp_dys_res1") {
    "idaco_dia_avg"
  } else {
    "spot_bp_dys"
  }
  
  # Fit a Cox proportional hazards model adjusted for age, BMI, HypertensionMeds, and CurrentSmoker
  surv_object <- Surv(time = updated_combined_data_1$time_to_event, event = updated_combined_data_1$mortality_event)
  # Create a formula with the actual blood pressure column name
  model_formula <- as.formula(paste("surv_object ~", bp_col, "* study + age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp + get(adjustment_var)")) 

  # Fit the Cox model with the dynamic formula
  cox_model <- coxph(model_formula, data = updated_combined_data_1)
  # Create a regression table and rename the variable
  reg_table <- tbl_regression(
    cox_model,
    exponentiate = TRUE,
    label = setNames(
    list(bp_columns[[bp_col]]),
    bp_col
      )
    ) %>%
  # Remove the confounder rows
  modify_table_body(~ .x %>% filter(!variable %in% c("age", "sex", "bmi", "on_med_htn", "current_smoke", "a1c_imp", "sod_pot_imp", "get(adjustment_var)"))) |>
  add_nevent(location = "level")

  # Add the regression table to the list
  regression_tables[[bp_columns[[bp_col]]]] <- reg_table
}

# Display the regression tables
cox_dia_regression_table_2 <- tbl_stack(regression_tables) %>%
                   modify_table_styling(
                    column = estimate,
                    rows = !is.na(estimate),
                    cols_merge_pattern = "{estimate}\n ({conf.low} - {conf.high})"
                   ) %>%
                   modify_header(estimate ~ "**HR (95% CI)**") %>%
                   modify_header(label ~ "**BP Measurement**") %>%
                   modify_column_hide(c(conf.low)) %>%
                   modify_table_styling(
                    column = estimate,
                    footnote_abbrev = "HR = Hazard Ratio, CI = Confidence Interval") %>%
                   modify_table_styling(
                    column = estimate,
                    footnote = "HR per 0.5 SD in blood pressure"
                   )
# Display the combined regression table
cox_dia_regression_table_2

#Cox per SD systolic combined
cox_diastolic_combined <- tbl_merge(tbls = list(cox_dia_regression_table_1, cox_dia_regression_table_2),
                                        tab_spanner = c("**Confounder adjusted**", "**Additionally adjusted for 24-hour blood pressure**"))
                        #   as_gt() |>
                        #  gt::gtsave(filename = "all mortality diastolic att unatt sens.DOCX", path = "C:\\Users\\cmwagwabi\\OneDrive - Kemri Wellcome Trust\\Documents - Anthony Etyang's files\\ABPM and HDSS events Data_Shared with Clement\\Project Folder - Clement\\Manuscript\\results_v2")


cox_diastolic_combined

all_models_combined <- tbl_stack(tbls = list(cox_systolic_combined, cox_diastolic_combined))
all_models_combined |>
  as_gt() |>
  gt::gtsave(filename = "all_mortality_att_unatt_sens.DOCX",
             path = "results")
