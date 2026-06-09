# prognostic accuracy tables and figures
#################test##########################
library(tidyverse)
library(flextable)
library(officer)
library(ggtext)
library(patchwork)
library(grid)
library(ggbreak)

# Results without KILIFI and JHS
# Define thresholds
bp_thresholds <- list(
    `ESH/ISH/ESC` = list(
        Clinic = c(140, 90),
        `24hr` = c(130, 80),
        Daytime = c(135, 85),
        Nighttime = c(120, 70)
    ),
    `ACC/AHA` = list(
        Clinic = c(130, 80),
        `24hr` = c(125, 75),
        Daytime = c(130, 80),
        Nighttime = c(110, 65)
    )
)

# Map measurement types to variable names
bp_vars <- list(
    Clinic = c("spot_bp_sys", "spot_bp_dys"),
    `24hr` = c("idaco_sbpbr_avg", "idaco_dia_avg"),
    Daytime = c("idaco_day_sbpbr", "idaco_day_dia"),
    Nighttime = c("idaco_night_sbpbr", "idaco_night_dia")
)

# Function to calculate and format sensitivity/specificity
get_sens_spec <- function(data, sys_var, dia_var, sys_cut, dia_cut) {
    test_pos <- data[[sys_var]] >= sys_cut | data[[dia_var]] >= dia_cut
    true_pos <- data$vital_status == "died"

    pred <- ifelse(test_pos, 1, 0)
    actual <- ifelse(true_pos, 1, 0)

    TP <- sum(pred == 1 & actual == 1, na.rm = TRUE)
    FP <- sum(pred == 1 & actual == 0, na.rm = TRUE)
    TN <- sum(pred == 0 & actual == 0, na.rm = TRUE)
    FN <- sum(pred == 0 & actual == 1, na.rm = TRUE)

    sens <- TP / (TP + FN)
    sens_ci <- binom.test(TP, TP + FN)$conf.int
    sens_fmt <- sprintf(
        "%d(%d, %d)",
        round(100 * sens),
        round(100 * sens_ci[1]),
        round(100 * sens_ci[2])
    )

    spec <- TN / (TN + FP)
    spec_ci <- binom.test(TN, TN + FP)$conf.int
    spec_fmt <- sprintf(
        "%d(%d, %d)",
        round(100 * spec),
        round(100 * spec_ci[1]),
        round(100 * spec_ci[2])
    )

    return(list(sens = sens_fmt, spec = spec_fmt))
}

# Initialize output tables
sensitivity_tbl <- tibble()
specificity_tbl <- tibble()

# Loop over definitions and measurement types
for (def in names(bp_thresholds)) {
    sens_row <- list(Definition = def)
    spec_row <- list(Definition = def)

    for (type in names(bp_vars)) {
        vars <- bp_vars[[type]]
        cuts <- bp_thresholds[[def]][[type]]

        result <- get_sens_spec(
            updated_combined_data,
            vars[1],
            vars[2],
            cuts[1],
            cuts[2]
        )

        sens_row[[type]] <- result$sens
        spec_row[[type]] <- result$spec
    }

    sensitivity_tbl <- bind_rows(sensitivity_tbl, sens_row)
    specificity_tbl <- bind_rows(specificity_tbl, spec_row)
}

# Function to build flextable with spacer columns
make_section_table <- function(df, title) {
    # Add spacer columns
    df$spA <- ""
    df$sp0 <- ""
    df$sp1 <- ""
    df$sp2 <- ""
    df$sp3 <- ""

    # Reorder columns
    df <- df[, c(
        "Definition",
        "spA",
        "24hr",
        "sp0",
        "Nighttime",
        "sp1",
        "Daytime",
        "sp2",
        "Clinic",
        "sp3"
    )]

    # Create flextable
    ft <- flextable(df) |>
        set_header_labels(
            Definition = "HTN Definition",
            `24hr` = "24hr",
            Nighttime = "Nighttime",
            Daytime = "Daytime",
            Clinic = "Clinic",
            spA = "",
            sp0 = "",
            sp1 = "",
            sp2 = "",
            sp3 = "" # spacer columns have no labels
        ) |>
        add_header_row(values = title, colwidths = 10) |>
        merge_h(part = "header") |>
        align(align = "center", part = "all") |>
        width(j = c("spA", "sp0", "sp1", "sp2", "sp3"), width = 0.03) |>

        # Remove all borders first
        border_remove() |>

        # Add border under the title row
        hline(i = 1, border = fp_border(width = 1), part = "header") |>
        hline(i = 2, border = fp_border(width = 1), part = "header") |>

        # Add bottom border to last row in body
        hline_bottom(border = fp_border(width = 1), part = "body") |>

        # Ensure spacer columns have no visible borders
        border(
            j = c("spA", "sp0", "sp1", "sp2", "sp3"),
            border = fp_border(color = "white"),
            part = "all"
        ) |>

        # Add bottom border to spacer columns  on the second row of the body
        hline(
            i = 1,
            j = c("spA", "sp0", "sp1", "sp2", "sp3"),
            border = fp_border(width = 1),
            part = "header"
        ) |>

        # Add bottom border to spacer columns  on the last row of the body
        hline(
            i = nrow(df),
            j = c("spA", "sp0", "sp1", "sp2", "sp3"),
            border = fp_border(width = 1),
            part = "body"
        ) |>

        autofit()

    return(ft)
}


# Create flextables
ft_sens <- make_section_table(sensitivity_tbl, "Prognostic Sensitivity")
ft_spec <- make_section_table(specificity_tbl, "Prognostic Specificity")

# Export to Word
doc <- read_docx() |>
    # body_add_par("Hypertension Definition Validity Metrics", style = "heading 1") |>
    body_add_flextable(ft_sens) |>
    body_add_par("", style = "Normal") |>
    body_add_flextable(ft_spec)

print(doc, target = "results/tables/prognostic_accuracy_tables.docx")


##### generate graph for sensitivities and specificities
# Reshape the sensitivity table to long format by definition
sensitivity_long <- sensitivity_tbl |>
    pivot_longer(
        cols = -Definition,
        names_to = "Measurement",
        values_to = "Sensitivity_CI"
    ) |>
    mutate(
        Sensitivity = as.numeric(str_extract(Sensitivity_CI, "^[0-9]+")),
        CI_Lower = as.numeric(str_extract(Sensitivity_CI, "(?<=\\().+?(?=,)")),
        CI_Upper = as.numeric(str_extract(Sensitivity_CI, "(?<=, ).+?(?=\\))"))
    )

# Reorder Measurement factor levels: 24hr, nighttime, daytime, clinic
sensitivity_long$Measurement <- factor(
    sensitivity_long$Measurement,
    levels = c("24hr", "Nighttime", "Daytime", "Clinic")
)

sensitivity_long$Definition <- factor(
    sensitivity_long$Definition,
    levels = c("ESH/ISH/ESC", "ACC/AHA")
)

custom_colors <- c(
    "24hr" = "#0072B2",
    "Nighttime" = "#e31a1c",
    "Daytime" = "#33a02c",
    "Clinic" = "#ff7f00"
)

p <- ggplot(
    sensitivity_long,
    aes(x = Measurement, y = Sensitivity, color = Measurement)
) +
    geom_point(size = 3, shape = 15) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.1) +
    facet_wrap(~Definition, ncol = 2) +
    labs(
        title = "Prognostic Sensitivity by Measurement Method",
        x = "Measurement Method",
        y = "Sensitivity (%)"
    ) +
    scale_colour_manual(values = custom_colors) +
    theme_minimal(base_size = 13) +
    theme(
        # Set overall plot appearance
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "white", size = 0.4),
        panel.grid.minor = element_blank(),

        axis.line = element_line(color = "black"),
        #Set strip text appearance
        strip.background = element_rect(fill = "oldlace"),
        strip.text = element_markdown(
            face = "bold",
            colour = "black",
            size = 15
        ),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 0.5) # center the title
    )

# Reshape the specificit table to long format by definition
specificity_long <- specificity_tbl |>
    pivot_longer(
        cols = -Definition,
        names_to = "Measurement",
        values_to = "Specificity_CI"
    ) |>
    mutate(
        Specificity = as.numeric(str_extract(Specificity_CI, "^[0-9]+")),
        CI_Lower = as.numeric(str_extract(Specificity_CI, "(?<=\\().+?(?=,)")),
        CI_Upper = as.numeric(str_extract(Specificity_CI, "(?<=, ).+?(?=\\))"))
    )

# Reorder Measurement factor levels: 24hr, nighttime, daytime, clinic
specificity_long$Measurement <- factor(
    specificity_long$Measurement,
    levels = c("24hr", "Nighttime", "Daytime", "Clinic")
)

specificity_long$Definition <- factor(
    specificity_long$Definition,
    levels = c("ESH/ISH/ESC", "ACC/AHA")
)

custom_colors <- c(
    "24hr" = "#0072B2",
    "Nighttime" = "#e31a1c",
    "Daytime" = "#33a02c",
    "Clinic" = "#ff7f00"
)

# # Code is commented out because it plots specificity
# q <- ggplot(specificity_long, aes(x = Measurement, y = Specificity, color = Measurement)) +
#         geom_point(size = 3,shape=15) +
#         geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.1) +
#         facet_wrap(~Definition, ncol = 2) +
#         labs(
#             title = "Prognostic Specificity by Measurement Method",
#             x = "Measurement Method", y = "Specificity (%)"
#         ) +
#         scale_colour_manual(values = custom_colors) +
#         theme_minimal(base_size = 13) +
#         theme(# Set overall plot appearance
#                 plot.background = element_rect(fill = "white"),
#                 panel.background = element_rect(fill = "white"),
#                 panel.grid.major = element_line(colour = "white", size = 0.4),
#                 panel.grid.minor = element_blank(),

#             axis.line = element_line(color = "black"),
#             #Set strip text appearance
#             strip.background = element_rect(fill = "oldlace"),
#             strip.text = element_markdown(face = "bold", colour = "black",
#                                         size = 15),
#             legend.position = "none",
#             plot.title = element_text(hjust = 0.5,vjust = 0.5)  # center the title
#         )

# comb <- wrap_plots(p, q, nrow = 2) +
#         plot_layout(guides = "collect")

ggsave(
    "prognostic_sensitivity_plot_main.png",
    height = 8,
    width = 10,
    path = "results/figures"
)
