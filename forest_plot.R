#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggridges)
  library(patchwork)
  library(cowplot)
})

# ---------------------------------------------------------------------------- #
# Font handling
# ---------------------------------------------------------------------------- #
font_family <- "sans"
if (requireNamespace("extrafont", quietly = TRUE)) {
  tryCatch({
    extrafont::loadfonts(device = "win", quiet = TRUE)
    if ("Times New Roman" %in% extrafont::fonts()) {
      font_family <- "Times New Roman"
    }
  }, error = function(e) {
    message("Failed to load extrafont fonts, falling back to system defaults.")
  })
}

# ---------------------------------------------------------------------------- #
# Forest plot data
# ---------------------------------------------------------------------------- #
forest_data <- tribble(
  ~Category,             ~Outcome,                   ~K,  ~n_bfr, ~n_it,
  "Anaerobic capacity",  "VO₂max",                 23,   249,   209,
  "Anaerobic capacity",  "Maximal power",            13,   132,   112,
  "Anaerobic capacity",  "Peak power",                7,    62,    62,
  "Anaerobic capacity",  "Mean power",                8,    72,    70,
  "Muscle fitness",      "Muscle strength",          17,   179,   164,
  "Muscle fitness",      "Muscle thickness",          6,    83,    83,
  "Muscle fitness",      "Muscle endurance",          6,    77,    76,
  "Endurance performance","Time trial performance",    5,    57,    53,
  "Endurance performance","Time to fatigue",           5,    51,    50,
  "Sprint performance",   "Peak speed",                2,    18,    18,
  "Sprint performance",   "Sprint time",               3,    32,    30,
  "Sprint performance",   "Repeated sprint ability",    4,    45,    43
) %>%
  mutate(
    Outcome = factor(Outcome, levels = Outcome),
    SMD = c(0.63, 0.18, 0.19, 0.70, 0.88, 0.55, 0.43, -0.16, 1.26, 0.74, -0.42, 0.01),
    CI_lower = c(0.28, -0.11, -0.42, 0.11, 0.46, -0.12, 0.01, -0.69, 0.01, 0.06, -1.17, -0.68),
    CI_upper = c(0.97, 0.46, 0.80, 1.29, 1.30, 1.22, 0.86, 0.38, 2.50, 1.41, 0.33, 0.69),
    p_value = c("<0.01", "0.20", "0.19", "0.03", "<0.01", "0.08", "0.04", "0.45", "0.04", "0.03", "0.27", "0.97"),
    I_squared = c("63%", "0%", "44%", "47%", "64%", "61%", "0%", "0%", "86%", "0%", "53%", "0%"),
    PI_lower = c(-0.73, -0.11, -1.05, -0.58, -0.56, -0.90, 0.01, -0.69, -2.78, 0.06, -3.10, -0.68),
    PI_upper = c(1.98, 0.46, 1.42, 1.97, 2.31, 2.00, 0.86, 0.38, 5.29, 1.41, 2.26, 0.69),
    Power = c("98%", "25%", "15%", "87%", "99%", "59%", "67%", "11%", "73%", "48%", "27%", "4%"),
    GRADE_raw = c("\u2295\u2295\u2295\u25cb", "\u2295\u2295\u2295\u25cb", "\u2295\u25cb\u25cb\u25cb", "\u2295\u2295\u2295\u25cb",
                  "\u2295\u2295\u2295\u25cb", "\u2295\u25cb\u25cb\u25cb", "\u2295\u2295\u2295\u2295", "\u2295\u2295\u25cb\u25cb",
                  "\u2295\u2295\u25cb\u25cb", "\u2295\u2295\u2295\u25cb", "\u2295\u25cb\u25cb\u25cb", "\u2295\u25cb\u25cb\u25cb")
  )

y_positions <- seq(30, 8, by = -2)
forest_data <- forest_data %>% mutate(y_position = y_positions[seq_len(n())])

forest_data <- forest_data %>%
  mutate(
    GRADE = map_chr(GRADE_raw, ~{
      checks <- nchar(gsub("\u25cb", "", .x))
      paste(rep("\u2713", checks), collapse = "")
    })
  )

# ---------------------------------------------------------------------------- #
# Ridgeline data (simulated)
# ---------------------------------------------------------------------------- #
set.seed(123)
ridgeline_data <- forest_data %>%
  mutate(sim_data = pmap(
    list(Outcome, SMD, K, y_position),
    ~ tibble(
      Outcome = ..1,
      SMD = rnorm(..3, mean = ..2, sd = 0.4),
      y_position = ..4
    )
  )) %>%
  pull(sim_data) %>%
  list_rbind() %>%
  group_by(Outcome, y_position) %>%
  mutate(mean_smd = mean(SMD)) %>%
  ungroup()

# ---------------------------------------------------------------------------- #
# Common layout constants
# ---------------------------------------------------------------------------- #
header_y <- 33.0
title_y <- 35.0
top_line_y <- 35.7
header_line_y <- 32.0
axis_line_y <- 6.0
bottom_limit <- 2.5
total_height <- 37.0

convert_to_npc <- function(value) {
  (value - bottom_limit) / (total_height - bottom_limit)
}

line_positions_npc <- c(
  top = convert_to_npc(top_line_y),
  header = convert_to_npc(header_line_y),
  footer = convert_to_npc(axis_line_y + 3.0)
)

# ---------------------------------------------------------------------------- #
# Left table
# ---------------------------------------------------------------------------- #
left_col_x <- c(outcome = 0.1, k = 4.0, bfr = 5.2, it = 6.4)

category_labels <- tribble(
  ~label,                 ~y,
  "Anaerobic capacity",   31.0,
  "Anaerobic capacity",   27.0,
  "Muscle fitness",       23.0,
  "Endurance performance",17.0,
  "Sprint performance",   13.0
)

left_plot <- ggplot(forest_data, aes(y = y_position)) +
  geom_text(
    data = category_labels,
    aes(x = 0, label = label),
    hjust = 0, size = 3.5, fontface = "bold", color = "darkblue",
    family = font_family
  ) +
  geom_text(aes(x = left_col_x["outcome"], label = Outcome),
            hjust = 0, size = 3.2, family = font_family) +
  geom_text(aes(x = left_col_x["k"], label = K),
            hjust = 0.5, size = 3, family = font_family) +
  geom_text(aes(x = left_col_x["bfr"], label = n_bfr),
            hjust = 0.5, size = 3, family = font_family) +
  geom_text(aes(x = left_col_x["it"], label = n_it),
            hjust = 0.5, size = 3, family = font_family) +
  annotate("text", x = left_col_x["outcome"], y = header_y, label = "Outcome",
           hjust = 0, size = 3.2, fontface = "bold", family = font_family) +
  annotate("text", x = left_col_x["k"], y = header_y, label = "K",
           hjust = 0.5, size = 3.2, fontface = "bold", family = font_family) +
  annotate("text", x = left_col_x["bfr"], y = header_y, label = "IT+BFR",
           hjust = 0.5, size = 3.2, fontface = "bold", family = font_family) +
  annotate("text", x = left_col_x["it"], y = header_y, label = "IT",
           hjust = 0.5, size = 3.2, fontface = "bold", family = font_family) +
  annotate("text", x = mean(range(left_col_x)), y = title_y, label = "Characteristics",
           hjust = 0.5, size = 3.5, fontface = "bold", family = font_family) +
  geom_segment(x = -0.2, xend = left_col_x["it"], y = title_y - 1.0, yend = title_y - 1.0,
               linewidth = 0.8, color = "black") +
  coord_cartesian(xlim = c(-0.2, 7.0), ylim = c(bottom_limit, total_height), clip = "off") +
  theme_void(base_family = font_family) +
  theme(plot.margin = margin(0, 0, 0, 10),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA))

# ---------------------------------------------------------------------------- #
# Centre forest plot
# ---------------------------------------------------------------------------- #
new_colors <- c("#BBE6FA", "#89CAEA", "#4596CD", "#0B75B3", "#015696", "#012A61")

center_plot <- ggplot(ridgeline_data, aes(x = SMD, y = y_position)) +
  geom_segment(x = 0, xend = 0, y = axis_line_y + 0.8, yend = header_line_y - 0.1,
               linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_segment(data = tibble(x = seq(-2, 2)),
               aes(x = x, xend = x, y = axis_line_y + 0.3, yend = axis_line_y + 0.8),
               linewidth = 0.5, colour = "black", inherit.aes = FALSE) +
  geom_density_ridges(
    aes(fill = mean_smd, y = y_position + 0.3, group = y_position),
    jittered_points = TRUE,
    point_color = "black",
    point_size = 0.9,
    point_shape = 19,
    point_alpha = 0.7,
    alpha = 0.8,
    scale = 0.55,
    bandwidth = 0.25,
    rel_min_height = 0.01
  ) +
  geom_segment(data = forest_data,
               aes(x = CI_lower, xend = CI_upper, y = y_position - 0.15, yend = y_position - 0.15),
               linewidth = 0.8, colour = "black", alpha = 0.8) +
  geom_point(data = forest_data,
             aes(x = SMD, y = y_position - 0.15),
             colour = "#1E88A8", fill = "#1E88A8", size = 2, shape = 22, stroke = 1) +
  scale_fill_gradientn(
    colours = new_colors,
    name = "Hedge's g",
    breaks = seq(-2, 3),
    limits = c(-2, 3),
    guide = "none"
  ) +
  annotate("text", x = 0, y = title_y, label = "Forest plot",
           hjust = 0.5, size = 3.5, fontface = "bold", family = font_family) +
  annotate("text", x = 0, y = header_y, label = "Random effects model",
           hjust = 0.5, size = 3.2, fontface = "bold", family = font_family) +
  geom_segment(x = -1.8, xend = 1.8, y = title_y - 1.0, yend = title_y - 1.0,
               linewidth = 0.8, color = "black") +
  annotate("text", x = -2, y = axis_line_y - 0.5, label = "-2", size = 3, family = font_family) +
  annotate("text", x = -1, y = axis_line_y - 0.5, label = "-1", size = 3, family = font_family) +
  annotate("text", x = 0,  y = axis_line_y - 0.5, label = "0",  size = 3, family = font_family) +
  annotate("text", x = 1,  y = axis_line_y - 0.5, label = "1",  size = 3, family = font_family) +
  annotate("text", x = 2,  y = axis_line_y - 0.5, label = "2",  size = 3, family = font_family) +
  labs(x = "Hedge's g (95%CI)", y = NULL) +
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  coord_cartesian(ylim = c(bottom_limit, total_height), clip = "off") +
  theme_minimal(base_family = font_family) +
  theme(
    axis.text = element_blank(),
    axis.title.x = element_text(size = 11, face = "bold", vjust = 1),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(0, 0, 0, 0)
  )

# ---------------------------------------------------------------------------- #
# Right table
# ---------------------------------------------------------------------------- #
right_table <- forest_data %>%
  mutate(
    Hedges_g = sprintf("%.2f (%.2f, %.2f)", SMD, CI_lower, CI_upper),
    Prediction_interval = sprintf("(%.2f, %.2f)", PI_lower, PI_upper)
  )

right_col_x <- c(hedges = 1.0, pvalue = 3.2, i2 = 4.4, pi = 6.2, power = 8.4, grade = 9.8)

right_plot <- ggplot(right_table, aes(y = y_position)) +
  geom_text(aes(x = right_col_x["hedges"], label = Hedges_g),
            hjust = 0.5, size = 3, family = font_family) +
  geom_text(aes(x = right_col_x["pvalue"], label = p_value),
            hjust = 0.5, size = 3, family = font_family) +
  geom_text(aes(x = right_col_x["i2"], label = I_squared),
            hjust = 0.5, size = 3, family = font_family) +
  geom_text(aes(x = right_col_x["pi"], label = Prediction_interval),
            hjust = 0.5, size = 3, family = font_family) +
  geom_text(aes(x = right_col_x["power"], label = Power),
            hjust = 0.5, size = 3, family = font_family) +
  geom_text(aes(x = right_col_x["grade"], label = GRADE),
            hjust = 0.5, size = 4, family = font_family, fontface = "bold") +
  annotate("text", x = right_col_x["hedges"], y = header_y, label = "Hedge's g (95%CI)",
           hjust = 0.5, size = 3.2, fontface = "bold", family = font_family) +
  annotate("text", x = right_col_x["pvalue"], y = header_y, label = "p-value",
           hjust = 0.5, size = 3.2, fontface = "bold", family = font_family) +
  annotate("text", x = right_col_x["i2"], y = header_y, label = "I²",
           hjust = 0.5, size = 3.2, fontface = "bold", family = font_family) +
  annotate("text", x = right_col_x["pi"], y = header_y, label = "Prediction\ninterval",
           hjust = 0.5, size = 3.2, fontface = "bold", family = font_family, lineheight = 0.8) +
  annotate("text", x = right_col_x["power"], y = header_y, label = "Power",
           hjust = 0.5, size = 3.2, fontface = "bold", family = font_family) +
  annotate("text", x = right_col_x["grade"], y = header_y, label = "GRADE",
           hjust = 0.5, size = 3.2, fontface = "bold", family = font_family) +
  annotate("text", x = mean(range(right_col_x["hedges"], right_col_x["grade"])), y = title_y,
           label = "Summary of findings", hjust = 0.5, size = 3.5, fontface = "bold", family = font_family) +
  geom_segment(x = right_col_x["hedges"], xend = 10.7, y = title_y - 1.0, yend = title_y - 1.0,
               linewidth = 0.8, colour = "black") +
  coord_cartesian(xlim = c(-0.3, 11), ylim = c(bottom_limit, total_height), clip = "off") +
  theme_void(base_family = font_family) +
  theme(plot.margin = margin(0, 10, 0, 0),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA))

# ---------------------------------------------------------------------------- #
# Combine panels and add shared lines
# ---------------------------------------------------------------------------- #
combined_plot <- left_plot | center_plot | right_plot

final_plot <- combined_plot +
  plot_layout(widths = c(1.2, 0.9, 2.2)) +
  plot_annotation(
    theme = theme(
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

line_x <- c(0.02, 0.98)
final_plot_with_lines <- ggdraw(final_plot) +
  draw_line(x = line_x, y = rep(line_positions_npc["top"], 2), size = 1.2) +
  draw_line(x = line_x, y = rep(line_positions_npc["header"], 2), size = 1.2) +
  draw_line(x = line_x, y = rep(line_positions_npc["footer"], 2), size = 1.2)

print(final_plot_with_lines)
