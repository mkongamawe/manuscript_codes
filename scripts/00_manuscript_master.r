# The master manuscript script.
# Created by C. N. Mwagwabi on 2025-06-27
# Last modified by C. N. Mwagwabi on 2025-06-27
# Individual R scripts are sourced in this script to create the master manuscript.

# Initialize an r environment
# renv::init(bare = TRUE)

# Load the data
updated_combined_data <- readr::read_csv(
  "./data/manuscript_data.csv",
)[, -1]

# Add the residuals and z-scores
source("scripts/01_residual_calc.r")

# Create table 1 and table for those not analysed
source("scripts/02_table1.r")

# Create a file for the dropped participants
source("scripts/03_dropped_participants.r")

# Visualise the correlation of BP measurements
source("scripts/04_correlation.r")

# Creating Nelson-Aalen plots
source("scripts/05_rates_nelson_aalen.r")

# Prognostic sensitivity results
source("scripts/06_prognostic_sensitivity_table_graph.r")
source("scripts/07_bp_kernel_density.r")

# Mortality proportions per BP category
source("scripts/08_mortality_proportions.r")

# Rates by BP categories
source("scripts/09_rates_forest_plot.r")

# Creating forestplots
# Association of BP with mortality and CV events (Cox regression) - per 1/2 SD
source("scripts/10_forest_plot_per_half_SD_increment.r")

# Association of BP with mortality and CV events per 10/5 mmHg
source("scripts/11_forest_plots_10_5.r")

# Association of BP with mortality and CV events (Cox regression) - 1 year sensitivity
source("scripts/12_forest_plot_1year_sens.r")

# Association of BP with mortality - Age/Sex/Weight sensitivities
source("scripts/13_forest_plot_sens_age_sex_weight.r")

# Checking the AUCs of BP measurements
source("scripts/14_roc_curve.r")

# Rates forest plots
source("scripts/rates_forest.r")

# To determine the interaction between attended and unattended
# Clinic blood pressure methods
source("scripts/sensitivity_att_unatt.r")

# To determine the post-hoc power of the models
source("scripts/post_hoc_power_cox.r")
