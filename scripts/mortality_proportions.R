# ---- Load required libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(patchwork)
library(cowplot)
library(grid)

# Optional: Rename for consistency
analysis_data <- updated_combined_data

# ---- Optional: Check required variables exist ----
required_vars <- c("spot_bp_sys", "spot_bp_dys", "idaco_sbpbr_avg", "idaco_dia_avg", 
                   "idaco_day_sbpbr", "idaco_day_dia", "idaco_night_sbpbr", 
                   "idaco_night_dia", "vital_status")

missing_vars <- setdiff(required_vars, names(analysis_data))
if (length(missing_vars) > 0) {
    stop("Missing variables in the dataset: ", paste(missing_vars, collapse = ", "))
}

# ---- Global y-axis limit for consistent plots ----
global_ylim <- c(0, 0.20)  # e.g., up to 20% mortality

# ---- Define BP categorization functions by guideline ----
# Define BP categorization functions for each definition
bp_definitions <- list(
  "ESC" = function(data) {
    data %>%
      mutate(
        clinic_cat = case_when(
          spot_bp_sys < 120 & spot_bp_dys < 70 ~ "Normal",
          spot_bp_sys >= 120 & spot_bp_sys < 140 | spot_bp_dys >= 70 & spot_bp_dys < 90 ~ "Elevated BP",
          spot_bp_sys >= 140 | spot_bp_dys >= 90 ~ "Hypertension"
        ),
        amb_cat = case_when(
          idaco_sbpbr_avg < 115 & idaco_dia_avg < 65 |
            idaco_day_sbpbr < 120 & idaco_day_dia < 70 |
            idaco_night_sbpbr < 110 & idaco_night_dia < 60 ~ "Normal",
          idaco_sbpbr_avg >= 115 & idaco_sbpbr_avg < 130 |
            idaco_dia_avg >= 65 & idaco_dia_avg < 80 |
            idaco_day_sbpbr >= 120 & idaco_day_sbpbr < 135 |
            idaco_day_dia >= 70 & idaco_day_dia < 85 |
            idaco_night_sbpbr >= 110 & idaco_night_sbpbr < 120 |
            idaco_night_dia >= 60 & idaco_night_dia < 70 ~ "Elevated BP",
          idaco_sbpbr_avg >= 130 | idaco_dia_avg >= 80 |
            idaco_day_sbpbr >= 135 | idaco_day_dia >= 85 |
            idaco_night_sbpbr >= 120 | idaco_night_dia >= 70 ~ "Hypertension"
        )
      )
  },
  "ACC/AHA" = function(data) {
    data %>%
      mutate(
        clinic_cat = case_when(
          spot_bp_sys < 120 & spot_bp_dys < 80 ~ "Normal",
          spot_bp_sys >= 120 & spot_bp_sys < 130 & spot_bp_dys < 80 ~ "Elevated BP",
          spot_bp_sys >= 130 | spot_bp_dys >= 80 ~ "Hypertension"
        ),
        amb_cat = case_when(
          idaco_sbpbr_avg < 115 & idaco_dia_avg < 75 |
            idaco_day_sbpbr < 120 & idaco_day_dia < 80 |
            idaco_night_sbpbr < 100 & idaco_night_dia < 65 ~ "Normal",
          idaco_sbpbr_avg >= 115 & idaco_sbpbr_avg < 125 & idaco_dia_avg < 75 |
            idaco_day_sbpbr >= 120 & idaco_day_sbpbr < 130 & idaco_day_dia < 80 |
            idaco_night_sbpbr >= 100 & idaco_night_sbpbr < 110 & idaco_night_dia < 65 ~ "Elevated BP",
          idaco_sbpbr_avg >= 125 | idaco_dia_avg >= 75 |
            idaco_day_sbpbr >= 130 | idaco_day_dia >= 80 |
            idaco_night_sbpbr >= 110 | idaco_night_dia >= 65 ~ "Hypertension"
        )
      )
  },
  "ESH & ISH" = function(data) {
    data %>%
      mutate(
        clinic_cat = case_when(
          spot_bp_sys < 130 & spot_bp_dys < 85 ~ "Normal",
          spot_bp_sys >= 130 & spot_bp_sys < 140 | spot_bp_dys >= 85 & spot_bp_dys < 90 ~ "Elevated BP",
          spot_bp_sys >= 140 | spot_bp_dys >= 90 ~ "Hypertension"
        ),
        amb_cat = case_when(
          idaco_sbpbr_avg < 130 & idaco_dia_avg < 80 |
            idaco_day_sbpbr < 135 & idaco_day_dia < 85 |
            idaco_night_sbpbr < 120 & idaco_night_dia < 70 ~ "Normal",
          idaco_sbpbr_avg >= 130 | idaco_dia_avg >= 80 |
            idaco_day_sbpbr >= 135 | idaco_day_dia >= 85 |
            idaco_night_sbpbr >= 120 | idaco_night_dia >= 70 ~ "Hypertension"
        )
      )
  }
)

# ---- Function to create mortality plot per guideline ----
create_guideline_plot <- function(guideline) {
    categorized_data <- bp_definitions[[guideline]](analysis_data)
    
    plot_data <- categorized_data %>%
        mutate(
            died = vital_status == "died"
        ) %>%
        select(died, clinic_cat, amb_cat) %>%
        pivot_longer(cols = c(clinic_cat, amb_cat),
                     names_to = "type",
                     values_to = "bp_category") %>%
        filter(!is.na(bp_category)) %>%
        group_by(type, bp_category) %>%
        summarise(
            mortality_rate = mean(died, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        mutate(
            label = sprintf("%.1f", mortality_rate * 100),
            bp_category = factor(bp_category, levels = c("Normal", "Elevated BP", "Hypertension")),
            type = recode(type,
                          clinic_cat = "Clinic",
                          amb_cat = "Ambulatory")
        )
    
    bp_colors <- c(
        "Hypertension" = "#e41a1c",
        "Elevated BP" = "#ff7f00",
        "Normal" = "#4daf4a"
          )
    
    p <- ggplot(plot_data, aes(x = type, y = mortality_rate, fill = bp_category)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.6, alpha = 0.9) +
        geom_text(aes(label = label), position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
        scale_fill_manual(values = bp_colors, name = "BP Category") +
        scale_y_continuous(
            labels = function(x) x * 100,
            limits = global_ylim,
            expand = expansion(mult = c(0, 0.05))
        ) +
        labs(
            title = guideline,
            x = NULL,
            y = "Cumulative Mortality (%)"
        ) +
        theme_minimal() +
        theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 0, hjust = 0.5)
        )
    
    return(p)
}


# 3.
create_guideline_plot_with_legend <- function(guideline) {
    p <- create_guideline_plot(guideline)
    p + theme(legend.position = "bottom",
              legend.title = element_blank())
}

# ---- Generate and view plots ----




# ---- Generate plots for each guideline ----
plot_list <- map(unique(names(bp_definitions)), create_guideline_plot)

#  Display plots in sequence
print(plot_list[[1]])  # ESC
print(plot_list[[2]])  # ACC/AHA
print(plot_list[[3]])  # ESH & ISH

p1 <- plot_list[[1]] # ESC
p2 <- plot_list[[2]] # ACC/AHA
p3 <- plot_list[[3]] # ESH & ISH 


# Add legend only to the 4th plot (adjust index if needed)
plot_list[[5]] <- create_guideline_plot_with_legend(names(bp_definitions)[3])
p6 <- create_guideline_plot_with_legend(names(bp_definitions)[3])

# Extract the legend from one plot 
legend_plot <- patchwork::wrap_elements(get_legend(p6))

# Use the same color scheme
bp_colors <- c(
    "Normal" = "#4daf4a",
    "Elevated BP" = "#ff7f00",
    "Hypertension" = "#e41a1c"
)

# Create a dummy plot just for legend
legend_only_plot <- ggplot(data.frame(
    type = factor(c("Normal", "Elevated BP", "Hypertension"), 
                  levels = c("Normal", "Elevated BP", "Hypertension"))
), aes(x = type, fill = type)) +
    geom_bar() +
    scale_fill_manual(values = bp_colors, name = "BP Category") +
    theme_void() +
    theme(legend.position = "bottom")

# Extract just the legend as a grob
legend_grob <- get_legend(legend_only_plot)

grid.draw(legend_grob)
grid::grid.draw(legend_grob)


print(legend_grob)
# Optional: Arrange all plots together if needed
# library(patchwork)
# wrap_plots(plot_list)


# --- 7. Combine plots and add overall title and theme ---

combined_plot <- wrap_plots(p1,p2,p3,legend_plot,ncol = 3) +
    plot_annotation(
        title = "Mortality by hypertension status by measurement method across different guideline definitions",
        theme = theme(
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
        )
    )

# --- 8. Print and save combined plot ---

print(combined_plot)  # Ensure plot displays in scripts/non-interactive sessions

ggsave(
    filename = "..\\Manuscript\\final_manuscript_results\\Figures",
    plot = combined_plot,
    device = "pdf",
    width = 15,
    height = 12
)
