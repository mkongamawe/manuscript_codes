library(tidyverse)
library(tidyr)
library(ggtext)
library(aod)
library(gt)
#######
# Define the confounders
confounders <- "age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"

# List of measures
measures <- c("idaco_night_sbpbr", "idaco_day_sbpbr", "idaco_sbpbr_avg", "spot_bp_sys")

# Initialize a list to store the results
results <- list()

# Loop through each measure and calculate the Wald test statistic
for (measure in measures) {
  # Create the formula
  formula <- as.formula(paste("mortality_event ~", measure, "+", confounders))
  
  # Fit the model
  model <- glm(formula, data = updated_combined_data, family = binomial)
  
  # Perform Wald test
  wald_result <- wald.test(b = coef(model), Sigma = vcov(model), Terms = 2)
  
  # Extract the chi-square statistic
  chi_square <- wald_result$result$chi2[1]

  # Store the result
  results[[measure]] <- chi_square
}

# Calculate informativeness for each measure
spot_bp_sys_chi2 <- results[["spot_bp_sys"]]
informativeness <- sapply(results, function(x) round((x / spot_bp_sys_chi2) * 100))

# Create a dataframe with the results
informative1_sys <- data.frame(
  measure = names(results),
  confounder_adjusted_chi2_statistic = round(unlist(results), 1),
  informativeness = paste0(informativeness, "%")
)

####
# Define the confounders
confounders <- "age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"

# List of measures
measures <- c("idaco_night_dia", "idaco_day_dia", "idaco_dia_avg", "spot_bp_dys")

# Initialize a list to store the results
results <- list()

# Loop through each measure and calculate the Wald test statistic
for (measure in measures) {
  # Create the formula
  formula <- as.formula(paste("mortality_event ~", measure, "+", confounders))
  
  # Fit the model
  model <- glm(formula, data = updated_combined_data, family = binomial)
  
  # Perform Wald test
  wald_result <- wald.test(b = coef(model), Sigma = vcov(model), Terms = 2)
  
  # Extract the chi-square statistic
  chi_square <- wald_result$result$chi2[1]
  
  # Store the result
  results[[measure]] <- chi_square
}

# Calculate informativeness for each measure
spot_bp_dys_chi2 <- results[["spot_bp_dys"]]
informativeness <- sapply(results, function(x) round((x / spot_bp_dys_chi2) * 100))

# Create a dataframe with the results
informative1_dys <- data.frame(
  measure = names(results),
  confounder_adjusted_chi2_statistic = round(unlist(results), 1),
  informativeness = paste0(informativeness, "%")
)


#####################
# Define the confounders
confounders <- "age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"

# List of measures
measures <- c("idaco_night_sbpbr", "idaco_day_sbpbr", "idaco_sbpbr_avg", "spot_bp_sys")

# Initialize a list to store the results
results <- list()

# Loop through each measure and calculate the Wald test statistic
for (measure in measures) {
  # Create the formula
  formula <- as.formula(paste("all_cardio_event ~", measure, "+", confounders))
  
  # Fit the model
  model <- glm(formula, data = updated_combined_data, family = binomial)
  
  # Perform Wald test
  wald_result <- wald.test(b = coef(model), Sigma = vcov(model), Terms = 2)
  
  # Extract the chi-square statistic
  chi_square <- wald_result$result$chi2[1]
  
  # Store the result
  results[[measure]] <- chi_square
}

# Calculate informativeness for each measure
spot_bp_sys_chi2 <- results[["spot_bp_sys"]]
informativeness <- sapply(results, function(x) round((x / spot_bp_sys_chi2) * 100))

# Create a dataframe with the results
informative2_sys <- data.frame(
  measure = names(results),
  confounder_adjusted_chi2_statistic = round(unlist(results), 1),
  informativeness = paste0(informativeness, "%")
)

####################
# Define the confounders
confounders <- "age + sex + bmi + on_med_htn + current_smoke + a1c_imp + sod_pot_imp"

# List of measures
measures <- c("idaco_night_dia", "idaco_day_dia", "idaco_dia_avg", "spot_bp_dys")

# Initialize a list to store the results
results <- list()

# Loop through each measure and calculate the Wald test statistic
for (measure in measures) {
  # Create the formula
  formula <- as.formula(paste("all_cardio_event ~", measure, "+", confounders))
  
  # Fit the model
  model <- glm(formula, data = updated_combined_data, family = binomial)
  
  # Perform Wald test
  wald_result <- wald.test(b = coef(model), Sigma = vcov(model), Terms = 2)
  
  # Extract the chi-square statistic
  chi_square <- wald_result$result$chi2[1]
  
  # Store the result
  results[[measure]] <- chi_square
}

# Calculate informativeness for each measure
spot_bp_dys_chi2 <- results[["spot_bp_dys"]]
informativeness <- sapply(results, function(x) round((x / spot_bp_dys_chi2) * 100))

# Create a dataframe with the results
informative2_dys <- data.frame(
  measure = names(results),
  confounder_adjusted_chi2_statistic = round(unlist(results), 1),
  informativeness = paste0(informativeness, "%")
)

################################
systolic_informativenes <- full_join(informative1_sys, informative2_sys, by = "measure")

systolic_informativenes |>
  gt() |>
  text_case_match(
    "idaco_night_sbpbr" ~ "Nighttime systolic",
    "idaco_day_sbpbr" ~ "Daytime systolic",
    "idaco_sbpbr_avg" ~ "24-h systolic",
    "spot_bp_sys" ~ "Clinic systolic"
  ) |>
  cols_label(
    measure = md(""),
    confounder_adjusted_chi2_statistic.x = md("Confounder-adjusted _&chi;<sup>2</sup>_ statistic "),
    informativeness.x = md("Informativeness "),
    confounder_adjusted_chi2_statistic.y = md("Confounder-adjusted _&chi;<sup>2</sup>_ statistic"),
    informativeness.y = md("Informativeness")
  ) |>
  tab_spanner(
    label = md("**All-cause Mortality**"),
    columns = c(confounder_adjusted_chi2_statistic.x, informativeness.x)
  ) |>
  tab_spanner(
    label = md("**Cardiovascular Events**"),
    columns = c(confounder_adjusted_chi2_statistic.y, informativeness.y)
  ) |>
tab_footnote(
  footnote = "The model is adjusted for age, sex, BMI, smoking status, use of hypertension medication and Diabetes Status",
  locations = cells_column_labels(c(confounder_adjusted_chi2_statistic.x, confounder_adjusted_chi2_statistic.y))
) |>
tab_footnote(
  footnote = md("Informativeness of the given measure(as indicated by the confounder-adjusted &chi;<sup>2</sup> statistic)
                 as a percentage of the informativeness of the clinic systolic blood pressure"),
  locations = cells_column_labels(c(informativeness.x, informativeness.y))
) |>
tab_caption(
  caption = md("Relative informativeness of different systolic blood pressure indices for all-cause mortality and all cardiovascular events")
) |>
gt::gtsave(filename = "informativeness_systolic.docx", path = "results/tables")

############################################################################
diastolic_informativeness <- full_join(informative1_dys, informative2_dys, by = "measure")

diastolic_informativeness |>
  gt() |>
  text_case_match(
    "idaco_night_dia" ~ "Nighttime diastolic",
    "idaco_day_dia" ~ "Daytime diastolic",
    "idaco_dia_avg" ~ "24-h diastolic",
    "spot_bp_dys" ~ "Clinic diastolic"
  ) |>
  cols_label(
    measure = md(""),
    confounder_adjusted_chi2_statistic.x = md("Confounder-adjusted _&chi;<sup>2</sup>_ statistic "),
    informativeness.x = md("Informativeness "),
    confounder_adjusted_chi2_statistic.y = md("Confounder-adjusted _&chi;<sup>2</sup>_ statistic"),
    informativeness.y = md("Informativeness")
  ) |>
  tab_spanner(
    label = md("**All-cause Mortality**"),
    columns = c(confounder_adjusted_chi2_statistic.x, informativeness.x)
  ) |>
  tab_spanner(
    label = md("**Cardiovascular Events**"),
    columns = c(confounder_adjusted_chi2_statistic.y, informativeness.y)
  ) |>
  tab_footnote(
  footnote = "The model is adjusted for age, sex, BMI, smoking status, use of hypertension medication and diabetes status",
  locations = cells_column_labels(c(confounder_adjusted_chi2_statistic.x, confounder_adjusted_chi2_statistic.y))
) |>
tab_footnote(
  footnote = md("Informativeness of the given measure(as indicated by the confounder-adjusted &chi;<sup>2</sup> statistic)
                 as a percentage of the informativeness of the clinic systolic blood pressure"),
  locations = cells_column_labels(c(informativeness.x, informativeness.y))
) |>
tab_caption(
  caption = md("Relative informativeness of different diastolic blood pressure indices for all-cause mortality and all cardiovascular events")
) |>
gt::gtsave(filename = "informativeness_diastolic.docx",
           path = "results/tables")
