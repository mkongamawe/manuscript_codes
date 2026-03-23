library(tidyverse)

#Calculating residuals for 24 hr BP components.
model1 <- lm(idaco_sbpbr_avg ~ spot_bp_sys, data = updated_combined_data)
model2 <- lm(idaco_day_sbpbr ~ spot_bp_sys, data = updated_combined_data)
model3 <- lm(idaco_night_sbpbr ~ spot_bp_sys, data = updated_combined_data)
model4 <- lm(spot_bp_sys ~ idaco_sbpbr_avg, data = updated_combined_data)
model5 <- lm(idaco_dia_avg ~ spot_bp_dys, data = updated_combined_data)
model6 <- lm(idaco_day_dia ~ spot_bp_dys, data = updated_combined_data)
model7 <- lm(idaco_night_dia ~ spot_bp_dys, data = updated_combined_data)
model8 <- lm(spot_bp_dys ~ idaco_dia_avg, data = updated_combined_data)

# Calculate raw residuals (in the original scale, e.g., mmHg)
idaco_sbpbr_avg_res1 <- residuals(model1)
idaco_day_sbpbr_res1 <- residuals(model2)
idaco_night_sbpbr_res1 <- residuals(model3)
spot_bp_sys_res1 <- residuals(model4)
idaco_dia_avg_res1 <- residuals(model5)
idaco_day_dia_res1 <- residuals(model6)
idaco_night_dia_res1 <- residuals(model7)
spot_bp_dys_res1 <- residuals(model8)


updated_combined_data <- cbind(updated_combined_data, idaco_sbpbr_avg_res1, idaco_day_sbpbr_res1,
                               idaco_night_sbpbr_res1, spot_bp_sys_res1, idaco_dia_avg_res1,
                               idaco_day_dia_res1, idaco_night_dia_res1, spot_bp_dys_res1)

# Calculate the z-scores for the bp measurements
updated_combined_data <- updated_combined_data %>%
  mutate(
    idaco_sbpbr_avg_z = scale(idaco_sbpbr_avg, center = TRUE, scale = TRUE),
    idaco_day_sbpbr_z = scale(idaco_day_sbpbr, center = TRUE, scale = TRUE),
    idaco_night_sbpbr_z = scale(idaco_night_sbpbr, center = TRUE, scale = TRUE),
    spot_bp_sys_z = scale(spot_bp_sys, center = TRUE, scale = TRUE),
    idaco_dia_avg_z = scale(idaco_dia_avg, center = TRUE, scale = TRUE),
    idaco_day_dia_z = scale(idaco_day_dia, center = TRUE, scale = TRUE),
    idaco_night_dia_z = scale(idaco_night_dia, center = TRUE, scale = TRUE),
    spot_bp_dys_z = scale(spot_bp_dys, center = TRUE, scale = TRUE)
  )