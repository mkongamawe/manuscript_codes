# Meant to calculate post_hoc power calculation of my main cox regression model
library(survival)
library(powerSurvEpi)
library(labelled)
library(broom)
library(gtsummary)
################################
# ----------------------Only all cause mortality plots-------------------------
####################################
#relaballing columns
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

# Prepare lists to store results
cox_models <- list()
cox_power <- list()
regression_tables <- list()

# Loop through each BP column
for (bp_col in names(bp_columns)) {
  
  # Scale the BP variable
  updated_combined_data_1 <-updated_combined_data |>
        mutate(!!bp_col := c(2 * scale(!!sym(bp_col))))
  
  # Fit Cox model
  surv_object <- Surv(
    time = updated_combined_data_1$time_to_event,
    event = updated_combined_data_1$mortality_event
  )
  
  model_formula <- as.formula(
    paste("surv_object ~", bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp")
  )
  
  cox_model <- coxph(model_formula, data = updated_combined_data_1)
  
  # Store model
  cox_models[[bp_col]] <- cox_model
  
  # ---- Post-hoc power calculation ----
  n <- cox_model$n
  HR <- exp(coef(cox_model)[bp_col])
  
  # Formula for R^2 estimation (BP ~ covariates)
  r2_formula <- as.formula(
    paste(bp_col, "~ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp")
  )
  
  power_result <- powerEpiCont(
    formula = r2_formula,
    dat = updated_combined_data_1,
    var.X1 = bp_col,
    var.failureFlag = "mortality_event",
    n = n,
    theta = HR,
    alpha = 0.05
  )
  
  cox_power[[bp_col]] <- list(
    HR = HR,
    power = power_result$power,
    rho2 = power_result$rho2
  )
  
  # ---- Create gtsummary regression table ----
reg_table <- tbl_regression(
  cox_model,
  exponentiate = TRUE,
  label = setNames(
    list(bp_columns[[bp_col]]),
    bp_col
  )
) |>
  # Keep only the BP variable (remove confounders if desired)
  modify_table_body(~ .x |> filter(variable == bp_col)) |>
  # Add post-hoc power and rho² as new columns
  modify_table_body(
    ~ .x |> mutate(
        Power = round(cox_power[[bp_col]]$power, 2),
        Rho2 = round(cox_power[[bp_col]]$rho2, 2)
      )
  )

regression_tables[[bp_columns[[bp_col]]]] <- reg_table
}

# ---- Stack all BP tables into one combined table ----
mortality_sys <- tbl_stack(regression_tables) |>
  modify_header(
    estimate ~ "**HR (95% CI)**",
    label ~ "**BP Measurement**",
    Power ~ "**Post-hoc Power**",
    Rho2 ~ "**R² with Covariates**"
  ) |>
  modify_table_styling(
    column = estimate,
    rows = !is.na(estimate),
    cols_merge_pattern = "{estimate}\n({conf.low}-{conf.high})"#,
    #footnote_abbrev = "HR = Hazard Ratio, CI = Confidence Interval"
  ) |>
  modify_table_styling(
    column = Power,
    footnote = "Post-hoc power calculated using Hsieh & Lavori method"
  ) |>
  modify_table_styling(
    column = Rho2,
    footnote = "R² = proportion of BP variance explained by covariates"
  )

####################################
#All all mortality
#all_mortality model 2

# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_sys_res1 = "Clinic SBP",
  idaco_day_sbpbr_res1 = "Daytime SBP",
  idaco_night_sbpbr_res1 = "Nighttime SBP",
  idaco_sbpbr_avg_res1 = "24h SBP"
)

# Prepare lists to store results
cox_models <- list()
cox_power <- list()
regression_tables <- list()

# Loop through each BP column
for (bp_col in names(bp_columns)) {
  
  # Scale the BP variable
  updated_combined_data_1 <-updated_combined_data |>
        mutate(!!bp_col := c(2 * scale(!!sym(bp_col))))
  
  # Determine the adjustment variable
  adjustment_var <- if (bp_col == "spot_bp_sys_res1") {
    "idaco_sbpbr_avg"
  } else {
    "spot_bp_sys"
  }

  # Fit Cox model
  surv_object <- Surv(
    time = updated_combined_data_1$time_to_event,
    event = updated_combined_data_1$mortality_event
  )
  
  model_formula <- as.formula(
    paste("surv_object ~", bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp + get(adjustment_var)")
  )
  
  cox_model <- coxph(model_formula, data = updated_combined_data_1)
  
  # Store model
  cox_models[[bp_col]] <- cox_model
  
  # ---- Post-hoc power calculation ----
  n <- cox_model$n
  HR <- exp(coef(cox_model)[bp_col])
  
  # Formula for R^2 estimation (BP ~ covariates)
  r2_formula <- as.formula(
    paste(bp_col, "~ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp + get(adjustment_var)")
  )
  
  power_result <- powerEpiCont(
    formula = r2_formula,
    dat = updated_combined_data_1,
    var.X1 = bp_col,
    var.failureFlag = "mortality_event",
    n = n,
    theta = HR,
    alpha = 0.05
  )
  
  cox_power[[bp_col]] <- list(
    HR = HR,
    power = power_result$power,
    rho2 = power_result$rho2
  )
  
  # ---- Create gtsummary regression table ----
reg_table <- tbl_regression(
  cox_model,
  exponentiate = TRUE,
  label = setNames(
    list(bp_columns[[bp_col]]),
    bp_col
  )
) |>
  # Keep only the BP variable (remove confounders if desired)
  modify_table_body(~ .x |> filter(variable == bp_col)) |>
  # Add post-hoc power and rho² as new columns
  modify_table_body(
    ~ .x |> mutate(
        Power = round(cox_power[[bp_col]]$power, 2),
        Rho2 = round(cox_power[[bp_col]]$rho2, 2)
      )
  )

regression_tables[[bp_columns[[bp_col]]]] <- reg_table
}

# ---- Stack all BP tables into one combined table ----
mortality_sys_2 <- tbl_stack(regression_tables) |>
  modify_header(
    estimate ~ "**HR (95% CI)**",
    label ~ "**BP Measurement**",
    Power ~ "**Post-hoc Power**",
    Rho2 ~ "**R² with Covariates**"
  ) |>
  modify_table_styling(
    column = estimate,
    rows = !is.na(estimate),
    cols_merge_pattern = "{estimate}\n({conf.low}-{conf.high})"#,
    #footnote_abbrev = "HR = Hazard Ratio, CI = Confidence Interval"
  ) |>
  modify_table_styling(
    column = Power,
    footnote = "Post-hoc power calculated using Hsieh & Lavori method"
  ) |>
  modify_table_styling(
    column = Rho2,
    footnote = "R² = proportion of BP variance explained by covariates"
  )

#Cox per SD systolic combined
sys_combined <- tbl_merge(tbls = list(mortality_sys, mortality_sys_2),
                                        tab_spanner = c("**Confounder adjusted**", "**Additionally adjusted for 24-hour or clinic blood pressure**"))


####################################
#All all mortality dia
#all_mortality model 1

# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_dys = "Clinic DBP",
  idaco_day_dia = "Daytime DBP",
  idaco_night_dia = "Nighttime DBP",
  idaco_dia_avg = "24h DBP"
)

# Prepare lists to store results
cox_models <- list()
cox_power <- list()
regression_tables <- list()

# Loop through each BP column
for (bp_col in names(bp_columns)) {
  
  # Scale the BP variable
  updated_combined_data_1 <-updated_combined_data |>
        mutate(!!bp_col := c(2 * scale(!!sym(bp_col))))
  
  # Fit Cox model
  surv_object <- Surv(
    time = updated_combined_data_1$time_to_event,
    event = updated_combined_data_1$mortality_event
  )
  
  model_formula <- as.formula(
    paste("surv_object ~", bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp")
  )
  
  cox_model <- coxph(model_formula, data = updated_combined_data_1)
  
  # Store model
  cox_models[[bp_col]] <- cox_model
  
  # ---- Post-hoc power calculation ----
  n <- cox_model$n
  HR <- exp(coef(cox_model)[bp_col])
  
  # Formula for R^2 estimation (BP ~ covariates)
  r2_formula <- as.formula(
    paste(bp_col, "~ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp")
  )
  
  power_result <- powerEpiCont(
    formula = r2_formula,
    dat = updated_combined_data_1,
    var.X1 = bp_col,
    var.failureFlag = "mortality_event",
    n = n,
    theta = HR,
    alpha = 0.05
  )
  
  cox_power[[bp_col]] <- list(
    HR = HR,
    power = power_result$power,
    rho2 = power_result$rho2
  )
  
  # ---- Create gtsummary regression table ----
reg_table <- tbl_regression(
  cox_model,
  exponentiate = TRUE,
  label = setNames(
    list(bp_columns[[bp_col]]),
    bp_col
  )
) |>
  # Keep only the BP variable (remove confounders if desired)
  modify_table_body(~ .x |> filter(variable == bp_col)) |>
  # Add post-hoc power and rho² as new columns
  modify_table_body(
    ~ .x |> mutate(
        Power = round(cox_power[[bp_col]]$power, 2),
        Rho2 = round(cox_power[[bp_col]]$rho2, 2)
      )
  )

regression_tables[[bp_columns[[bp_col]]]] <- reg_table
}

# ---- Stack all BP tables into one combined table ----
mortality_dia <- tbl_stack(regression_tables) |>
  modify_header(
    estimate ~ "**HR (95% CI)**",
    label ~ "**BP Measurement**",
    Power ~ "**Post-hoc Power**",
    Rho2 ~ "**R² with Covariates**"
  ) |>
  modify_table_styling(
    column = estimate,
    rows = !is.na(estimate),
    cols_merge_pattern = "{estimate}\n({conf.low}-{conf.high})"#,
    #footnote_abbrev = "HR = Hazard Ratio, CI = Confidence Interval"
  ) |>
  modify_table_styling(
    column = Power,
    footnote = "Post-hoc power calculated using Hsieh & Lavori method"
  ) |>
  modify_table_styling(
    column = Rho2,
    footnote = "R² = proportion of BP variance explained by covariates"
  )

####################################
#All all mortality
#all_mortality model 2

# List of blood pressure columns and their labels
bp_columns <- list(
  spot_bp_dys_res1 = "Clinic DBP",
  idaco_day_dia_res1 = "Daytime DBP",
  idaco_night_dia_res1 = "Nighttime DBP",
  idaco_dia_avg_res1 = "24h DBP"
)

# Prepare lists to store results
cox_models <- list()
cox_power <- list()
regression_tables <- list()

# Loop through each BP column
for (bp_col in names(bp_columns)) {
  
  # Scale the BP variable
  updated_combined_data_1 <-updated_combined_data |>
        mutate(!!bp_col := c(2 * scale(!!sym(bp_col))))
  
  # Determine the adjustment variable
  adjustment_var <- if (bp_col == "spot_bp_dys_res1") {
    "idaco_dia_avg"
  } else {
    "spot_bp_dys"
  }

  # Fit Cox model
  surv_object <- Surv(
    time = updated_combined_data_1$time_to_event,
    event = updated_combined_data_1$mortality_event
  )
  
  model_formula <- as.formula(
    paste("surv_object ~", bp_col, "+ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp + get(adjustment_var)")
  )
  
  cox_model <- coxph(model_formula, data = updated_combined_data_1)
  
  # Store model
  cox_models[[bp_col]] <- cox_model
  
  # ---- Post-hoc power calculation ----
  n <- cox_model$n
  HR <- exp(coef(cox_model)[bp_col])
  
  # Formula for R^2 estimation (BP ~ covariates)
  r2_formula <- as.formula(
    paste(bp_col, "~ age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp + get(adjustment_var)")
  )
  
  power_result <- powerEpiCont(
    formula = r2_formula,
    dat = updated_combined_data_1,
    var.X1 = bp_col,
    var.failureFlag = "mortality_event",
    n = n,
    theta = HR,
    alpha = 0.05
  )
  
  cox_power[[bp_col]] <- list(
    HR = HR,
    power = power_result$power,
    rho2 = power_result$rho2
  )
  
  # ---- Create gtsummary regression table ----
reg_table <- tbl_regression(
  cox_model,
  exponentiate = TRUE,
  label = setNames(
    list(bp_columns[[bp_col]]),
    bp_col
  )
) |>
  # Keep only the BP variable (remove confounders if desired)
  modify_table_body(~ .x |> filter(variable == bp_col)) |>
  # Add post-hoc power and rho² as new columns
  modify_table_body(
    ~ .x |> mutate(
        Power = round(cox_power[[bp_col]]$power, 2),
        Rho2 = round(cox_power[[bp_col]]$rho2, 2)
      )
  )

regression_tables[[bp_columns[[bp_col]]]] <- reg_table
}

# ---- Stack all BP tables into one combined table ----
mortality_dia_2 <- tbl_stack(regression_tables) |>
  modify_header(
    estimate ~ "**HR (95% CI)**",
    label ~ "**BP Measurement**",
    Power ~ "**Post-hoc Power**",
    Rho2 ~ "**R² with Covariates**"
  ) |>
  modify_table_styling(
    column = estimate,
    rows = !is.na(estimate),
    cols_merge_pattern = "{estimate}\n({conf.low}-{conf.high})"#,
    #footnote_abbrev = "HR = Hazard Ratio, CI = Confidence Interval"
  ) |>
  modify_table_styling(
    column = Power,
    footnote = "Post-hoc power calculated using Hsieh & Lavori method"
  ) |>
  modify_table_styling(
    column = Rho2,
    footnote = "R² = proportion of BP variance explained by covariates"
  )

#Cox per SD systolic combined
dia_combined <- tbl_merge(tbls = list(mortality_dia, mortality_dia_2),
                                        tab_spanner = c("**Confounder adjusted**", "**Additionally adjusted for 24-hour or clinic blood pressure**"))

all_combined <- tbl_stack(tbls = list(sys_combined, dia_combined)) |>
  as_gt() |>
  gt::gtsave(filename = "post_hoc_power_calc.DOCX",
             path = "results")
