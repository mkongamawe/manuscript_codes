################################################
#Collecting the filtered out participants
filtered_out <- project_master_1 %>%
  select(
    "pk_person",
    "updated_time",
    "study_no",
    "sex",
    "age",
    "agegrp",
    "height",
    "weight", #Delete updated time from this.
    "bmi",
    "muac",
    starts_with("idaco"),
    "spot_bp_sys",
    "spot_bp_dys",
    "spot_pulse",
    "prev_diag_htn",
    "on_med_htn",
    "current_smoke",
    "a1c",
    "self_dm",
    "dob",
    "agecat5",
    "agecat3",
    "study",
    "Urine_sodium_",
    "Urine_potassium"
  ) %>%
  rename("sodium" = Urine_sodium_, "potassium" = Urine_potassium) %>%
  mutate(
    sod_pot = as.numeric(sodium) / as.numeric(potassium),
    age = ifelse(
      is.na(age),
      trunc(interval(dob, updated_time) / years(1)),
      age
    ),
    bmi = ifelse(is.na(bmi), round((weight) / ((height / 100)^2), 2), bmi)
  ) %>%
  filter(!is.na(study_no)) %>%
  filter(
    is.na(age) |
      is.na(bmi) |
      is.na(on_med_htn) |
      is.na(current_smoke) |
      idaco_good != 1 |
      is.na(idaco_night_sbpbr) |
      is.na(idaco_night_dia) |
      is.na(idaco_day_sbpbr) |
      is.na(idaco_day_dia) |
      is.na(spot_bp_dys) |
      is.na(spot_bp_sys)
  )

####################################
# Filter rows where pk_person has duplicates and resolve conflicts
resolved_duplicates <- filtered_out %>%
  group_by(pk_person) %>%
  filter(n() > 1) %>%
  arrange(pk_person, study, row_number()) %>% # Sort to prioritize "shinda" and then by row order
  mutate(
    keep_flag = case_when(
      study == "shinda" ~ 1, # Assign priority to "shinda"
      study == "nahenda" ~ 2, # Assign second priority to "Nahenda"
      TRUE ~ 3 # Assign lowest priority for other cases
    )
  ) %>%
  arrange(keep_flag, .by_group = TRUE) %>% # Sort by priority within groups
  slice_head(n = 1) %>% # Keep the first row per group
  ungroup() # Remove grouping

# Add resolved duplicates back to the full dataset
filtered_out_1 <- filtered_out %>%
  anti_join(resolved_duplicates, by = "pk_person") %>%
  bind_rows(resolved_duplicates)

############################################################
################################################################################
#Run Shinda2_partiipants and nahenda_part before
#After table cleaning

filtered_out_1$current_smoke <- as.factor(filtered_out_1$current_smoke)
filtered_out_1$on_med_htn <- as.factor(filtered_out_1$on_med_htn)

day_weight <- 20 / (20 + 40)
night_weight <- 40 / (20 + 40)

filtered_out_1$idaco_sbpbr_avg <- round(
  (filtered_out_1$idaco_day_sbpbr * day_weight) +
    (filtered_out_1$idaco_night_sbpbr * night_weight)
)
filtered_out_1$idaco_dia_avg <- round(
  (filtered_out_1$idaco_day_dia * day_weight) +
    (filtered_out_1$idaco_night_dia * night_weight)
)


filtered_out_2 <- filtered_out_1 %>%
  mutate(
    idaco_bp_phenotype = case_when(
      idaco_good == 1 &
        spot_bp_sys < 140 &
        spot_bp_dys < 90 &
        idaco_sbpbr_avg < 130 &
        idaco_dia_avg < 80 ~ "Normotensive",
      idaco_good == 1 &
        (spot_bp_sys >= 140 | spot_bp_dys >= 90) &
        (idaco_sbpbr_avg < 130 &
          idaco_dia_avg < 80) ~ "White Coat Hypertensive",
      idaco_good == 1 &
        spot_bp_sys < 140 &
        spot_bp_dys < 90 &
        (idaco_sbpbr_avg >= 130 | idaco_dia_avg >= 80) ~ "Masked Hypertensive",
      idaco_good == 1 &
        (spot_bp_sys >= 140 | spot_bp_dys >= 90) &
        (idaco_sbpbr_avg >= 130 |
          idaco_dia_avg >= 80) ~ "Sustained Hypertensive",
      TRUE ~ NA_character_
    )
  )

filtered_out_2 <- filtered_out_2 %>%
  mutate(
    diabetis_status = case_when(
      a1c >= 6.5 ~ "yes",
      a1c >= 5.7 & a1c <= 6.4 ~ "pre",
      TRUE ~ "no"
    )
  )

levels(filtered_out_2$prev_diag_htn)[
  levels(filtered_out_2$prev_diag_htn) == ""
] <- "No"
levels(filtered_out_2$on_med_htn)[
  levels(filtered_out_2$on_med_htn) == ""
] <- "No"
levels(filtered_out_2$current_smoke)[
  levels(filtered_out_2$current_smoke) == ""
] <- "No"

################################################################################
#creating a table with complete data.
# project_master_esh <- filtered_out_1 %>%
#     filter(!is.na(sex), esh_good == 1)

drop_master_idaco <- filtered_out_1 %>%
  filter(!is.na(sex), idaco_good == 1)


unique(filtered_out_2$current_smoke)

################################################################################
# Remove exact duplicate rows
filtered_out_2 <- filtered_out_2[!duplicated(filtered_out_2), ]

#identify duplicates.
n_occur <- data.frame(table(nahenda_master_3$pk_person))
n_occur[n_occur$Freq > 1, ]
shinda2_master_1[
  shinda2_master_1$pk_person %in% n_occur$var1[n_occur$Freq > 1],
]

# Filter rows where pk_person has duplicates and resolve conflicts
resolved_duplicates <- filtered_out_2 %>%
  group_by(pk_person) %>%
  filter(n() > 1) %>%
  arrange(pk_person, study, row_number()) %>% # Sort to prioritize "shinda" and then by row order
  mutate(
    keep_flag = case_when(
      study == "shinda" ~ 1, # Assign priority to "shinda"
      study == "nahenda" ~ 2, # Assign second priority to "Nahenda"
      TRUE ~ 3 # Assign lowest priority for other cases
    )
  ) %>%
  arrange(keep_flag, .by_group = TRUE) %>% # Sort by priority within groups
  slice_head(n = 1) %>% # Keep the first row per group
  ungroup() # Remove grouping

# Add resolved duplicates back to the full dataset
filtered_out_2 <- filtered_out_2 %>%
  anti_join(resolved_duplicates, by = "pk_person") %>%
  bind_rows(resolved_duplicates)

# Remove those that are below 18 years
x <- filtered_out_2 %>%
  filter(age >= 18)

set.seed(26)
filtered_out_2 <- x %>%
  sample_n(503)

filtered_out_3 <- filtered_out_2 |>
  mutate(
    agecat3 = case_when(
      age >= 18 & age <= 29 ~ "18-29",
      age >= 30 & age <= 59 ~ "30-59",
      TRUE ~ "60+"
    )
  )

# Description about the filtered out participants
filt_desc <- filtered_out_3 |>
  summarise(
    bp_incomplete = sum(idaco_good != 1 | is.na(idaco_good)),
    spot_incomplete = sum(
      idaco_good == 1 & (is.na(spot_bp_sys) | is.na(spot_bp_dys)),
      na.rm = TRUE
    )
  )

filtered_out_4 <- project_master_4 |>
  filter(age < 18)

common <- intersect(names(filtered_out_3), names(filtered_out_4))
filtered_out_3 <- filtered_out_3 %>%
  select(all_of(common))

filtered_out_4 <- filtered_out_4 %>%
  select(all_of(common))

filtered_out_5 <- rbind(filtered_out_3, filtered_out_4) |>
  mutate(
    dia_col = case_when(
      self_dm == "yes" | a1c >= 6.5 ~ "yes",
      self_dm == "no" | a1c < 6.5 ~ "no",
      TRUE ~ NA
    )
  )

############################################
drop_table_1 <- filtered_out_3 %>%
  select(
    sex,
    age,
    agecat3,
    bmi,
    on_med_htn,
    current_smoke,
    a1c,
    sod_pot,
    idaco_day_sbpbr,
    idaco_day_dia,
    idaco_night_sbpbr,
    idaco_night_dia,
    spot_bp_sys,
    spot_bp_dys,
    idaco_sbpbr_avg,
    idaco_dia_avg
  ) %>%
  mutate(
    #vital_status = as.factor(vital_status),
    weight = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi > 24.9 ~ "Overweight",
      TRUE ~ "Normal"
    ),
    diabetes_status = case_when(
      a1c >= 6.5 ~ "yes",
      TRUE ~ "no"
    ),
    agecat = case_when(
      age < 45 ~ "<45",
      age >= 45 & age < 70 ~ "45-69",
      age >= 60 ~ "70+"
    )
  ) |>
  select(
    sex,
    age,
    agecat,
    weight,
    on_med_htn,
    current_smoke,
    diabetes_status,
    sod_pot,
    idaco_day_sbpbr,
    idaco_day_dia,
    idaco_night_sbpbr,
    idaco_night_dia,
    spot_bp_sys,
    spot_bp_dys,
    idaco_sbpbr_avg,
    idaco_dia_avg
  )

drop_table_1 %>%
  tbl_summary(
    #by = sex,
    missing = "ifany",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      age ~ "{median} ({p25}, {p75})",
      sod_pot ~ "{median} ({p25}, {p75})"
    ),
    label = list(
      age = "Age",
      agecat = "Age Categories",
      weight = "BMI",
      on_med_htn = "On Hypertension Medicine",
      current_smoke = "Current Smoker",
      diabetes_status = "Diabetes status",
      sod_pot = "Urine Na to K ratio",
      idaco_day_sbpbr = "Daytime Systolic",
      idaco_night_sbpbr = "Nighttime Systolic",
      idaco_day_dia = "Daytime Diastolic",
      idaco_night_dia = "Nighttime Diastolic",
      spot_bp_sys = "Clinical Systolic",
      spot_bp_dys = "Clinical Diastolic",
      idaco_sbpbr_avg = "24-h Systolic",
      idaco_dia_avg = "24-h Diastolic",
      idaco_bp_phenotype = "Hypertension Phenotypes"
    )
  ) %>%
  #add_overall(col_label = "All Participants") %>%
  add_variable_grouping(
    "Risk Factors" = c(
      "weight",
      "on_med_htn",
      "current_smoke",
      "diabetes_status",
      "sod_pot"
    )
  ) %>%
  add_variable_grouping(
    "Blood Pressure" = c(
      "idaco_day_sbpbr",
      "idaco_night_sbpbr",
      "idaco_night_dia",
      "idaco_day_dia",
      "spot_bp_sys",
      "spot_bp_dys",
      "idaco_sbpbr_avg",
      "idaco_dia_avg"
    )
  ) %>%
  as_gt() %>%
  tab_header(
    title = md("**Characteristics of excluded participants**")
  ) %>%
  gt::gtsave(filename = "drop_table_1.docx", path = "results/tables")
