############################################################################
library(gtsummary)
library(gt)

#view(project_table_1)
project_table_1 <- updated_combined_data %>%
  mutate(vital_status = as.factor(vital_status),
    weight = case_when(bmi < 18.5 ~ "Underweight",
                       bmi > 24.9 ~ "Overweight",
                       TRUE ~ "Normal"),
    diabetes_status = case_when(
      a1c_imp >= 6.5 ~ "yes",
      TRUE ~ "no"),
    agecat = case_when(
        age < 45 ~ "<45",
        age >= 45 & age < 70 ~ "45-69",
        age >= 60 ~ "70+"
    ),
    clin_hyp = if_else(spot_bp_sys >= 134 | spot_bp_dys >= 87, "yes", "no"),
    amb_hyp = if_else(idaco_sbpbr_avg >= 124 | idaco_dia_avg >= 70, "yes", "no"),
    iso_clin_sys_hyp = if_else(spot_bp_sys >= 134 & spot_bp_dys < 87, "yes", "no"),
    iso_amb_sys_hyp = if_else(idaco_sbpbr_avg >= 124 & idaco_dia_avg < 70, "yes", "no"),
    iso_clin_dia_hyp = if_else(spot_bp_sys < 134 & spot_bp_dys >= 87, "yes", "no"),
    iso_amb_dia_hyp = if_else(idaco_sbpbr_avg < 124 & idaco_dia_avg >= 70, "yes", "no")
    ) |>
  select(sex, age, agecat, weight, on_med_htn,
         current_smoke, diabetes_status, sod_pot_imp, idaco_day_sbpbr, idaco_day_dia,
         idaco_night_sbpbr, idaco_night_dia, spot_bp_sys,
         spot_bp_dys, idaco_sbpbr_avg,
         idaco_dia_avg, clin_hyp, amb_hyp,
         iso_clin_sys_hyp, iso_amb_sys_hyp, iso_clin_dia_hyp, iso_amb_dia_hyp)

project_table_1 %>%
    tbl_summary(
        #by = sex,
        missing = "ifany",
        statistic = list(all_continuous() ~ "{mean} ({sd})",
                         age ~ "{median} ({p25}, {p75})",
                         sod_pot_imp ~ "{median} ({p25}, {p75})"),
        label = list(
            age = "Age", agecat = "Age Categories", weight = "BMI",on_med_htn = "On Hypertension Medicine",
            current_smoke = "Current Smoker", diabetes_status = "Diabetes status", sod_pot_imp = "Urine Na to K ratio",
            idaco_day_sbpbr = "Daytime Systolic", idaco_day_dia = "Daytime Diastolic",
            idaco_night_sbpbr = "Nighttime Systolic", idaco_night_dia = "Nighttime Diastolic",
            spot_bp_sys = "Clinical Systolic", spot_bp_dys = "Clinical Diastolic",
            idaco_sbpbr_avg = "24-h Systolic", idaco_dia_avg = "24-h Diastolic" ,
            clin_hyp = "Clinic", amb_hyp = "Ambulatory",
            iso_clin_sys_hyp = "Clinic", iso_amb_sys_hyp = "Ambulatory",
            iso_clin_dia_hyp = "Clinic", iso_amb_dia_hyp = "Ambulatory"
        )
    ) %>%
    #add_overall(col_label = "All Participants") %>%
    #add_p(
        #test = list(
    #all_continuous() ~ "aov", # ANOVA for continuous variables
    #all_categorical() ~ "chisq.test") # Chi-squared test for categorical variables
    #) %>%
    add_variable_grouping(
        "Risk Factors" = c("weight", "on_med_htn", "current_smoke", "diabetes_status", "sod_pot_imp")
    ) %>%
    add_variable_grouping(
        "Blood Pressure" = c("idaco_day_sbpbr","idaco_night_sbpbr", "idaco_night_dia",
        "idaco_day_dia", "spot_bp_sys", "spot_bp_dys", "idaco_sbpbr_avg", "idaco_dia_avg")
    ) %>%
    add_variable_grouping(
        "Hypertension" = c("clin_hyp", "amb_hyp")
    ) %>%
    add_variable_grouping(
        "Isolated Systolic Hypertension" = c("iso_clin_sys_hyp", "iso_amb_sys_hyp")
    ) %>%
    add_variable_grouping(
        "Isolated Diastolic Hypertension" = c("iso_clin_dia_hyp", "iso_amb_dia_hyp")
    ) %>%
    as_gt() %>%
    tab_header(
        title = md("**Characteristics of the participants**")
    ) %>%
   gt::gtsave(filename = "Table_1.docx", path = "..\\Manuscript\\final_manuscript_results")


project_table_1 <- updated_combined_data %>%
  mutate(vital_status = as.factor(vital_status),
    weight = case_when(bmi < 18.5 ~ "Underweight",
                       bmi > 24.9 ~ "Overweight",
                       TRUE ~ "Normal"),
    diabetes_status = case_when(
      a1c_imp >= 6.5 ~ "yes",
      TRUE ~ "no"),
    agecat = case_when(
        age < 45 ~ "<45",
        age >= 45 & age < 70 ~ "45-69",
        age >= 60 ~ "70+"
    ),
    clin_hyp = if_else(spot_bp_sys >= 134 | spot_bp_dys >= 87, "yes", "no"),
    amb_hyp = if_else(idaco_sbpbr_avg >= 124 | idaco_dia_avg >= 70, "yes", "no"),
    iso_clin_sys_hyp = if_else(spot_bp_sys >= 134 & spot_bp_dys < 87, "yes", "no"),
    iso_amb_sys_hyp = if_else(idaco_sbpbr_avg >= 124 & idaco_dia_avg < 70, "yes", "no"),
    iso_clin_dia_hyp = if_else(spot_bp_sys < 134 & spot_bp_dys >= 87, "yes", "no"),
    iso_amb_dia_hyp = if_else(idaco_sbpbr_avg < 124 & idaco_dia_avg >= 70, "yes", "no")
    ) |>
  select(vital_status, sex, age, agecat, weight, on_med_htn,
         current_smoke, diabetes_status, sod_pot_imp, idaco_day_sbpbr, idaco_day_dia,
         idaco_night_sbpbr, idaco_night_dia, spot_bp_sys,
         spot_bp_dys, idaco_sbpbr_avg,
         idaco_dia_avg, clin_hyp, amb_hyp,
         iso_clin_sys_hyp, iso_amb_sys_hyp, iso_clin_dia_hyp, iso_amb_dia_hyp)

project_table_1 %>%
    tbl_summary(
        by = vital_status,
        missing = "ifany",
        statistic = list(all_continuous() ~ "{mean} ({sd})",
                         age ~ "{median} ({p25}, {p75})",
                         sod_pot_imp ~ "{median} ({p25}, {p75})"),
        label = list(
            age = "Age", agecat = "Age Categories", weight = "BMI",on_med_htn = "On Hypertension Medicine",
            current_smoke = "Current Smoker", diabetes_status = "Diabetes status", sod_pot_imp = "Urine Na to K ratio",
            idaco_day_sbpbr = "Daytime Systolic", idaco_day_dia = "Daytime Diastolic",
            idaco_night_sbpbr = "Nighttime Systolic", idaco_night_dia = "Nighttime Diastolic",
            spot_bp_sys = "Clinical Systolic", spot_bp_dys = "Clinical Diastolic",
            idaco_sbpbr_avg = "24-h Systolic", idaco_dia_avg = "24-h Diastolic" ,
            clin_hyp = "Clinic", amb_hyp = "Ambulatory",
            iso_clin_sys_hyp = "Clinic", iso_amb_sys_hyp = "Ambulatory",
            iso_clin_dia_hyp = "Clinic", iso_amb_dia_hyp = "Ambulatory"
        )
    ) %>%
    add_overall(col_label = "All Participants") %>%
    #add_p(
        #test = list(
    #all_continuous() ~ "aov", # ANOVA for continuous variables
    #all_categorical() ~ "chisq.test") # Chi-squared test for categorical variables
    #) %>%
    add_variable_grouping(
        "Risk Factors" = c("weight", "on_med_htn", "current_smoke", "diabetes_status", "sod_pot_imp")
    ) %>%
    add_variable_grouping(
        "Blood Pressure" = c("idaco_day_sbpbr","idaco_night_sbpbr", "idaco_night_dia",
        "idaco_day_dia", "spot_bp_sys", "spot_bp_dys", "idaco_sbpbr_avg", "idaco_dia_avg")
    ) %>%
    add_variable_grouping(
        "Hypertension" = c("clin_hyp", "amb_hyp")
    ) %>%
    add_variable_grouping(
        "Isolated Systolic Hypertension" = c("iso_clin_sys_hyp", "iso_amb_sys_hyp")
    ) %>%
    add_variable_grouping(
        "Isolated Diastolic Hypertension" = c("iso_clin_dia_hyp", "iso_amb_dia_hyp")
    ) %>%
    as_gt() %>%
    tab_header(
        title = md("**Characteristics of the participants**")
    ) %>%
    gt::gtsave(filename = "Table_1_vital_status_strat.docx", path = "..\\Manuscript\\final_manuscript_results")

############################################
drop_table_1 <- filtered_out_3 %>%
    select(sex, age, agecat3, bmi, on_med_htn,
         current_smoke, a1c, sod_pot, idaco_day_sbpbr, idaco_day_dia,
         idaco_night_sbpbr, idaco_night_dia, spot_bp_sys,
         spot_bp_dys, idaco_sbpbr_avg,
         idaco_dia_avg) %>%
  mutate(#vital_status = as.factor(vital_status),
         weight = case_when(bmi < 18.5 ~ "Underweight",
                            bmi > 24.9 ~ "Overweight",
                            TRUE ~ "Normal"),
         diabetes_status = case_when(
           a1c >= 6.5 ~ "yes",
           TRUE ~ "no"),
         agecat = case_when(
           age < 45 ~ "<45",
           age >= 45 & age < 70 ~ "45-69",
           age >= 60 ~ "70+"
         )) |>
  select(sex, age, agecat, weight, on_med_htn,
         current_smoke, diabetes_status, sod_pot, idaco_day_sbpbr, idaco_day_dia,
         idaco_night_sbpbr, idaco_night_dia, spot_bp_sys,
         spot_bp_dys, idaco_sbpbr_avg,
         idaco_dia_avg)

drop_table_1 %>%
    tbl_summary(
        #by = sex,
        missing = "ifany",
        statistic = list(all_continuous() ~ "{mean} ({sd})",
                         age ~ "{median} ({p25}, {p75})",
                         sod_pot ~ "{median} ({p25}, {p75})"),
        label = list(
            age = "Age", agecat = "Age Categories", weight = "BMI",on_med_htn = "On Hypertension Medicine", current_smoke = "Current Smoker", diabetes_status = "Diabetes status", sod_pot = "Urine Na to K ratio",
            idaco_day_sbpbr = "Daytime Systolic", idaco_night_sbpbr = "Nighttime Systolic",
            idaco_day_dia = "Daytime Diastolic", idaco_night_dia = "Nighttime Diastolic",
            spot_bp_sys = "Clinical Systolic", spot_bp_dys = "Clinical Diastolic",
            idaco_sbpbr_avg = "24-h Systolic", idaco_dia_avg = "24-h Diastolic",
            idaco_bp_phenotype = "Hypertension Phenotypes"
        )
    ) %>%
    #add_overall(col_label = "All Participants") %>%
    add_variable_grouping(
        "Risk Factors" = c("weight", "on_med_htn", "current_smoke", "diabetes_status", "sod_pot")
    ) %>%
    add_variable_grouping(
        "Blood Pressure" = c("idaco_day_sbpbr","idaco_night_sbpbr", "idaco_night_dia",
        "idaco_day_dia", "spot_bp_sys", "spot_bp_dys", "idaco_sbpbr_avg", "idaco_dia_avg")
    ) %>%
    as_gt() %>%
    tab_header(
        title = md("**Characteristics of excluded participants**")
    ) %>%
   gt::gtsave(filename = "drop_table_1.docx", path = "../Manuscript/final_manuscript_results")
