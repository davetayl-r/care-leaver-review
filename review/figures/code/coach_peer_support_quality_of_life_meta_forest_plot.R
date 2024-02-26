## META ANALYSIS PLOTS: TRANSITIONS REVIEW

## INTERVENTION: COACHING AND PEER SUPPORT PROGRAMS
## OUTCOME: QUALITY OF LIFE

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(ggplot2)
library(gt)
library(patchwork)
library(cowplot)

## READ DATA

coach_peer_support_quality_of_life_meta_plot_data_location <- "./review/figures/plot_data/my_life_quality_of_life_meta_data.RDS"
raw_coach_peer_support_quality_of_life_meta_plot_data <- readRDS(coach_peer_support_quality_of_life_meta_plot_data_location)

## PREPARE FOREST PLOT DATA

coach_peer_support_quality_of_life_meta_plot_data <- raw_coach_peer_support_quality_of_life_meta_plot_data %>%
  mutate(author = case_when(
    is.na(author) ~ "Overall effect",
    TRUE ~ author
  )) %>%
  mutate(significant_95_per_cent = case_when(
    overall_effect_p_value < 0.05 ~ "Yes",
    overall_effect_p_value >= 0.05 ~ "No")
  ) %>%
  mutate(
    overall_effect_p_value = round(overall_effect_p_value, 3),
    i_squared_p = round(i_squared_p, 3)
  )

## CREATE META DIAMOND

coach_peer_support_quality_of_life_diamond <- data.frame(
  author = "Overall effect",
  x = c(
    coach_peer_support_quality_of_life_meta_plot_data$overall_effect[3], coach_peer_support_quality_of_life_meta_plot_data$overall_effect_lower[3], 
    coach_peer_support_quality_of_life_meta_plot_data$overall_effect_lower[3], coach_peer_support_quality_of_life_meta_plot_data$overall_effect[3], 
    coach_peer_support_quality_of_life_meta_plot_data$overall_effect[3], coach_peer_support_quality_of_life_meta_plot_data$overall_effect_upper[3],
    coach_peer_support_quality_of_life_meta_plot_data$overall_effect_upper[3], coach_peer_support_quality_of_life_meta_plot_data$overall_effect[3]),
  y = c(
    1 + 0.1, 1,
    1, 1 - 0.1, 
    1 + 0.1, 1,
    1, 1 - 0.1)
)

## PREPARE HETEROGENEITY DATA

i_squared_heterogeneity <- coach_peer_support_quality_of_life_meta_plot_data %>%
  select(
    i_squared,
    i_squared_p
  ) %>%
  mutate(
    i_squared = i_squared * 100,
    i_squared = round(i_squared, 1)) %>%
  slice(max(row_number()))

## PREPARE PLOT ANNOTATIONS

coach_peer_support_quality_of_life_meta_results_plot_data <- coach_peer_support_quality_of_life_meta_plot_data %>%
  mutate(TE = case_when(
    study == "Overall effect" ~ overall_effect,
    TRUE ~ TE)
  ) %>%
  mutate(lower = case_when(
    study == "Overall effect" ~ overall_effect_lower,
    TRUE ~ lower)
  ) %>%
  mutate(upper = case_when(
    study == "Overall effect" ~ overall_effect_upper,
    TRUE ~ upper)
  ) %>%
  mutate(
    total_w_random = sum(coach_peer_support_quality_of_life_meta_plot_data$w.random, na.rm = TRUE),
    w.random = case_when(
      study == "Overall effect" ~ sum(coach_peer_support_quality_of_life_meta_plot_data$w.random, na.rm = TRUE),
      TRUE ~ w.random),
    per_cent_w_random = (w.random/total_w_random)*100,
    per_cent_w_random = round(per_cent_w_random, 1),
    per_cent_w_random = paste(per_cent_w_random, "%", sep = "")) %>%
  select(
    y_axis_order,
    author,
    TE,
    lower,
    upper,
    per_cent_w_random,
    significant_95_per_cent
  ) %>%
  mutate(
    TE = round(TE, 2),
    lower = round(lower, 2),
    upper = round(upper, 2),
    con_inf = paste(
      "[",
      lower,
      ", ",
      upper,
      "]",
      sep = ""),
    TE = as.character(TE)
  ) %>%
  mutate(
    author_wrap = str_wrap(
      author,
      width = 100) 
  ) %>%
  arrange(
    desc(y_axis_order)
  ) %>%
  rename(
    SMD = TE
  ) %>%
  select(
    -author,
    -lower,
    -upper
  )

## CREATE FOREST PLOT

coach_peer_support_quality_of_life_forest_plot <- coach_peer_support_quality_of_life_meta_plot_data %>%
  ggplot() +
  aes(
    y = y_axis_order,
    x = TE
  ) +
  geom_vline(
    aes(
      xintercept = overall_effect[3],
    ),
    linetype = "dotdash"
  ) +
  geom_point(
    shape = 15, 
    colour = "#bcbcbc",
    aes(
      size = w.random)
  ) +
  geom_point(
    shape = 3, 
    colour = "#000000",
    aes(
      size = 1)
  ) +
  geom_linerange(
    colour = "#000000",
    aes(
      xmin = lower, 
      xmax = upper)
  ) +
  geom_vline(
    xintercept = 0) +
  annotate(
    geom = "polygon",
    x = coach_peer_support_quality_of_life_diamond$x,
    y = coach_peer_support_quality_of_life_diamond$y,
    colour = ifelse(
      coach_peer_support_quality_of_life_meta_results_plot_data$significant_95_per_cent == "Yes", "#008744", "#d62d20")
    [dim(coach_peer_support_quality_of_life_meta_results_plot_data)[1]],
    fill = ifelse(
      coach_peer_support_quality_of_life_meta_results_plot_data$significant_95_per_cent == "Yes", "#008744", "#d62d20")
    [dim(coach_peer_support_quality_of_life_meta_results_plot_data)[1]],
  ) +
  labs(
    title = "",
    subtitle = paste("Favours comparison", "Favours intervention", sep = "          "),
    x = "Standardised Mean Difference"
  ) +  
  coord_cartesian(
    xlim = c(
      -1.5, 
      1.5
    ),
    ylim = c(
      min(coach_peer_support_quality_of_life_meta_plot_data$y_axis_order)-0.25,
      max(coach_peer_support_quality_of_life_meta_plot_data$y_axis_order)+0.25
    ),
    clip = "off"
  ) +
  theme(
    plot.title = element_text(
      vjust = 2,
      hjust = 0,
      face = "bold",
      size = 14,
      margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10,
      margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")
    ),
    axis.text = element_text(
      colour = "#000000",
      size = 11
    ),
    axis.title.x = element_text(
      colour = "#000000",
      size = 12,
      hjust = 0.5,
      vjust = 0.5,
      margin = margin(t = 0, r = 0, b = -5, l = 0, unit = "pt")
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(colour = "#000000"),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(
      fill = "transparent", 
      colour = NA),
    panel.background = element_rect(
      fill = "transparent", 
      colour = NA), 
    legend.position = "none"
  )

coach_peer_support_quality_of_life_left_plot_annotations <- coach_peer_support_quality_of_life_meta_results_plot_data %>%
  ggplot() +
  aes(
    y = y_axis_order
  ) +
  geom_text(
    aes(
      x = 0, 
      label = author_wrap),
    fontface = ifelse(coach_peer_support_quality_of_life_meta_results_plot_data$author_wrap == "Overall effect", "bold", "plain"),
    size = 4, 
    hjust = 0) +
  labs(
    title = "     Effect of C&PSP on quality of life 16-24 months from baseline",
    subtitle = "Study",
    x = bquote("Heterogeneity:"~I^2 == .(paste(i_squared_heterogeneity$i_squared, "%, ", sep = ""))~p == .(i_squared_heterogeneity$i_squared_p))             
  ) +
  scale_x_continuous(
    limits = c(0,1),
    expand = c(0,0)
  ) +  
  coord_cartesian(
    ylim = c(
      min(coach_peer_support_quality_of_life_meta_plot_data$y_axis_order)-0.25,
      max(coach_peer_support_quality_of_life_meta_plot_data$y_axis_order)+0.25
    ),
    clip = "off"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      vjust = 2,
      hjust = 0,
      face = "bold",
      size = 14,
      margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      face = "bold",
      size = 12,
      margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")
    ),
    axis.text = element_text(
      colour = "#000000",
      size = 11
    ),
    axis.title.x = element_text(
      colour = "#000000",
      size = 12,
      hjust = 0,
      vjust = 0.5,
      margin = margin(t = 0, r = 0, b = -5, l = 0, unit = "pt")
    ),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

coach_peer_support_quality_of_life_right_plot_annotations <- coach_peer_support_quality_of_life_meta_results_plot_data %>%
  mutate(
    SMD = paste(
      SMD, 
      con_inf,
      sep = " ")
  ) %>%
  ggplot() +
  aes(
    y = y_axis_order
  ) +
  geom_text(
    aes(
      x = 0, 
      label = SMD), 
    hjust = 0,
    vjust = 0.5,
    fontface = ifelse(coach_peer_support_quality_of_life_meta_results_plot_data$y_axis_order == "1", "bold", "plain")) +
  labs(
    title = "",
    subtitle = "SMD [95% CI]",
    x = ""
  ) +
  scale_x_continuous(
    limits = c(0,1),
    expand = c(0,0)
  ) + 
  coord_cartesian(
    ylim = c(
      min(coach_peer_support_quality_of_life_meta_plot_data$y_axis_order)-0.25,
      max(coach_peer_support_quality_of_life_meta_plot_data$y_axis_order)+0.25
    ),
    clip = "off"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      vjust = 2,
      hjust = 0,
      face = "bold",
      size = 14,
      margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      face = "bold",
      size = 12,
      margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")
    ),
    axis.text = element_text(
      colour = "#000000",
      size = 11
    ),
    axis.title.x = element_text(
      colour = "#000000",
      size = 12,
      vjust = 0.5,
      margin = margin(t = 0, r = 0, b = -5, l = 0, unit = "pt")
    ),
    axis.text.x = element_blank(),
    axis.text.y = element_blank())

## COMBINE THREE PLOTS

combined_coach_peer_support_quality_of_life_meta_plot <- plot_grid(
  coach_peer_support_quality_of_life_left_plot_annotations, 
  coach_peer_support_quality_of_life_forest_plot, 
  coach_peer_support_quality_of_life_right_plot_annotations,
  nrow = 1,
  ncol = 3,
  align = "hv", 
  rel_widths = c(1.2, 1.6, 0.45)
  ) 

## EXPORT PLOT

ggsave(filename = "./review/figures/output/coach_peer_support_quality_of_life_meta.png",
       plot = combined_coach_peer_support_quality_of_life_meta_plot,
       height = 1.5,
       width = 10)

## EXPORT GGPLOT OBJECT

saveRDS(
  combined_coach_peer_support_quality_of_life_meta_plot,
  file = "./review/figures/plot_data/coach_peer_support_quality_of_life_meta_plot_object.RDS")
