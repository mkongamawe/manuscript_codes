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

# Creating Nelson-Aalen plots
source("scripts/04_rates_nelson_aalen.r")

# Prognostic sensitivity results
source("scripts/05_prognostic_sensitivity_table_graph.r")
source("scripts/06_bp_kernel_density.r")

# Mortality proportions per BP category
source("scripts/07_mortality_proportions.r")

# Creating forestplots
source("scripts/07_forest_plot_per_half_SD_increment.r") # To continue from here

# Association of BP with mortality and CV events (Cox regression)
source("scripts/new_forest_plot_residual.r")

# Association of BP with mortality and CV events (Cox regression) - 1 year sensitivity
source("scripts/new_forest_plot_sensitivity_1year.r")

# Association of BP with mortality and CV events (Cox regression) - per 1/2 SD
source("scripts/new_forest_plot_half_SD_increment.r")

# Association of BP with mortality and CV events (Poisson regression)
source("scripts/poisson_reg.r")

# Association of BP with mortality and CV events (Poisson regression) - no interpolation
# Ensure to run the project master file with no imputation and change labels correctly
source("scripts/poisson_reg.r")

# Rates forest plots
source("scripts/rates_forest.r")

# To determine the interaction between attended and unattended
# Clinic blood pressure methods
source("scripts/sensitivity_att_unatt.r")

# To determine the post-hoc power of the models
source("scripts/post_hoc_power_cox.r")
