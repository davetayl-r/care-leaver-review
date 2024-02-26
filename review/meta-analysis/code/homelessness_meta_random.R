## TRANSITIONS REVIEW: META ANALYSIS
## OUTCOME: HOMELESSNESS

## THIS CODE DOES THE FOLLOWING:
## 1. IMPORTS PRE-TRANSFORMED ES
## 2. POOLS THE ES IN A FIXED OR RAMDOM EFFECTS MODEL
## 3. EXPORTS A FOREST PLOT OF THE OUTPUT

library(tidyverse)
library(readxl)
library(meta)

## READ DATA

homelessness_es_data_location <- "./review/es-transformation/output/homelessness_es_data.rds"
raw_homelessness_es_data <- readRDS(homelessness_es_data_location)

## SUBSET DATA FOR STUDIES TO INCLUDE IN META

homelessness_es_data <- raw_homelessness_es_data %>%
  filter(
    study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" |
    study == "Life Skills Training Program: Los Angeles County" |
    study == "Independent Living – Employment Services Program, Kern County, CA")

## SPLIT INTO OUTCOMES

independent_living_program_homelessness_es <- homelessness_es_data %>%
  filter(outcome == "Homelessness") 

independent_living_program_residential_moves_es <- homelessness_es_data %>%
  filter(outcome == "Number of residential moves")

## POOL EFFECTS: HOMELESSNESS

independent_living_program_homelessness_meta <- metagen(
  TE = es,
  seTE = se,
  data = independent_living_program_homelessness_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD")

## INSPECT RESULTS

independent_living_program_homelessness_meta %>%
  summary()

independent_living_program_homelessness_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_homelessness_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_homelessness_meta.RDS")

saveRDS(
  independent_living_program_homelessness_meta,
  file = "./supplementary-material/inputs/independent_living_program_homelessness_meta.RDS")

## POOL EFFECTS: RESIDENTIAL MOVES

independent_living_program_residential_moves_meta <- metagen(
  TE = es,
  seTE = se,
  data = independent_living_program_residential_moves_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD") 

## INSPECT RESULTS

independent_living_program_residential_moves_meta %>%
  summary()

independent_living_program_residential_moves_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_residential_moves_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_residential_moves_meta.RDS")

saveRDS(
  independent_living_program_residential_moves_meta,
  file = "./supplementary-material/inputs/independent_living_program_residential_moves_meta.RDS")

## EXPORT HOMELESSNESS META DATA FOR PLOTTING

independent_living_program_homelessness_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_homelessness_meta$TE.random,
  overall_effect_upper = independent_living_program_homelessness_meta$upper.random,
  overall_effect_lower = independent_living_program_homelessness_meta$lower.random,
  overall_effect_p_value = independent_living_program_homelessness_meta$pval.random,
  i_squared = independent_living_program_homelessness_meta$I2,
  i_squared_p = independent_living_program_homelessness_meta$pval.Q
)

independent_living_program_homelessness_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_homelessness_meta_results) %>%
  left_join(independent_living_program_homelessness_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 3,
      study == "Life Skills Training Program: Los Angeles County" ~ 2,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 4,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review//figures/plot_data/independent_living_program_homelessness_meta_data.RDS")

## EXPORT RESIDENTIAL MOVES META DATA FOR PLOTTING

independent_living_program_residential_moves_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_residential_moves_meta$TE.random,
  overall_effect_upper = independent_living_program_residential_moves_meta$upper.random,
  overall_effect_lower = independent_living_program_residential_moves_meta$lower.random,
  overall_effect_p_value = independent_living_program_residential_moves_meta$pval.random,
  i_squared = independent_living_program_residential_moves_meta$I2,
  i_squared_p = independent_living_program_residential_moves_meta$pval.Q
)

independent_living_program_residential_moves_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_residential_moves_meta_results) %>%
  left_join(independent_living_program_residential_moves_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 3,
      study == "Life Skills Training Program: Los Angeles County" ~ 4,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 2,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review//figures/plot_data/independent_living_program_residential_moves_meta_data.RDS")
