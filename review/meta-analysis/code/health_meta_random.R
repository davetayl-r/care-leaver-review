## TRANSITIONS REVIEW: META ANALYSIS
## OUTCOME: HEALTH

## THIS CODE DOES THE FOLLOWING:
## 1. IMPORTS PRE-TRANSFORMED ES
## 2. POOLS THE ES IN A FIXED OR RAMDOM EFFECTS MODEL
## 3. EXPORTS A FOREST PLOT OF THE OUTPUT

library(readxl)
library(tidyverse)
library(meta)

## READ DATA

health_es_data_location <- "./review/es-transformation/output/health_es_data.rds"
health_es_data <- readRDS(health_es_data_location)

## SUBSET DATA FOR STUDIES TO INCLUDE IN META

my_life_qol_es <- health_es_data %>%
  filter(study %in% c("TAKE CHARGE", "Better Futures"))

## MY LIFE POOL EFFECTS: QUALITY OF LIFE

my_life_qol_meta <- metagen(
  es,
  se,
  data = my_life_qol_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD") 

## INSPECT RESULTS

my_life_qol_meta %>%
  summary()

my_life_qol_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  my_life_qol_meta,
  file = "./review/meta-analysis/output/meta/my_life_qol_meta.RDS")

saveRDS(
  my_life_qol_meta,
  file = "./supplementary-material/inputs/my_life_qol_meta.RDS")

## EXPORT QOL META DATA FOR PLOTTING

my_life_qol_results <- bind_cols(
  study = "Overall effect",
  overall_effect = my_life_qol_meta$TE.random,
  overall_effect_upper = my_life_qol_meta$upper.random,
  overall_effect_lower = my_life_qol_meta$lower.random,
  overall_effect_p_value = my_life_qol_meta$pval.random,
  i_squared = my_life_qol_meta$I2,
  i_squared_p = my_life_qol_meta$pval.Q
)

my_life_qol_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(my_life_qol_results) %>%
  left_join(my_life_qol_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "TAKE CHARGE" ~ 3,
      study == "Better Futures" ~ 2,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/my_life_quality_of_life_meta_data.RDS")
