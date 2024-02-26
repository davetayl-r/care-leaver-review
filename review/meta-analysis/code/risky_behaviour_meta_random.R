## TRANSITIONS REVIEW: META ANALYSIS
## OUTCOME: RISKY BEHAVIOUR

## THIS CODE DOES THE FOLLOWING:
## 1. IMPORTS PRE-TRANSFORMED ES
## 2. POOLS THE ES IN A FIXED OR RAMDOM EFFECTS MODEL
## 3. EXPORTS A FOREST PLOT OF THE OUTPUT

library(tidyverse)
library(readxl)
library(meta)

## READ DATA

risky_behaviour_es_data_location <- "./review/es-transformation/output/risky_behaviour_es_data.rds"
raw_risky_behaviour_es_data <- readRDS(risky_behaviour_es_data_location)

## SUBSET DATA FOR STUDIES TO INCLUDE IN META

independent_living_program_risky_behaviour_es <- raw_risky_behaviour_es_data %>%
  filter(
    study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" |
    study == "Life Skills Training Program: Los Angeles County" |
    study == "Independent Living – Employment Services Program, Kern County, CA")

## POOL EFFECTS: INDEPENDENT LIVING PROGRAM POOL EFFECTS: BECOME PREGNANT

independent_living_program_pregnant_meta <- metagen(
   es,
   se,
   data = independent_living_program_risky_behaviour_es,
   studlab = study,
   comb.random = TRUE,
   prediction = TRUE,
   sm = "SMD") 

## INSPECT RESULTS

independent_living_program_pregnant_meta %>%
  summary()

independent_living_program_pregnant_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_pregnant_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_pregnant_meta.RDS")

saveRDS(
  independent_living_program_pregnant_meta,
  file = "./supplementary-material/inputs/independent_living_program_pregnant_meta.RDS")

## EXPORT META DATA FOR PLOTTING

independent_living_program_pregnant_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_pregnant_meta$TE.random,
  overall_effect_upper = independent_living_program_pregnant_meta$upper.random,
  overall_effect_lower = independent_living_program_pregnant_meta$lower.random,
  overall_effect_p_value = independent_living_program_pregnant_meta$pval.random,
  i_squared = independent_living_program_pregnant_meta$I2,
  i_squared_p = independent_living_program_pregnant_meta$pval.Q
)

independent_living_program_pregnant_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_pregnant_meta_results) %>%
  left_join(independent_living_program_risky_behaviour_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 4,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 2,
      study == "Life Skills Training Program: Los Angeles County" ~ 3,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_pregnant_meta_data.RDS")
