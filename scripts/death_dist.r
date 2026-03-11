library(gtsummary)
library(gt)

decadent_data <- updated_combined_data |>
  filter(mortality_event == 1) |>
  select(cause1)

decadent_table <- decadent_data |>
  tbl_summary(missing = "ifany") |>
  modify_header(label = "Cause of Death all") |>
  as_gt() |>
  gtsave("results/death_dist_all.docx")

decadent_data_night_hyp <- updated_combined_data |>
  filter(mortality_event == 1 &
           (idaco_night_sbpbr >= 120 | idaco_night_dia >= 70)) |>
  select(cause1)

decadent_table_night_hyp <- decadent_data_night_hyp |>
  tbl_summary(missing = "ifany") |>
  modify_header(label = "Cause of Death (Night Hypertension)") |>
  as_gt() |>
  gtsave("results/death_dist_night_hyp.docx")

decadent_data_clinic_hyp <- updated_combined_data |>
  filter(mortality_event == 1 &
           (spot_bp_sys >= 140 | spot_bp_dys >= 90)) |>
  select(cause1)

decadent_table_clinic_hyp <- decadent_data_clinic_hyp |>
  tbl_summary(missing = "ifany") |>
  modify_header(label = "Cause of Death (Clinic Hypertension)") |>
  as_gt() |>
  gtsave("results/death_dist_clinic_hyp.docx")
