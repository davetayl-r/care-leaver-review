## TRANSITIONS REVIEW: META ANALYSIS
## OUTCOME: EXPOSURE TO VIOLENCE

## THIS CODE DOES THE FOLLOWING:
## 1. IMPORTS PRE-TRANSFORMED ES
## 2. POOLS THE ES IN A FIXED OR RAMDOM EFFECTS MODEL
## 3. EXPORTS A FOREST PLOT OF THE OUTPUT

library(tidyverse)
library(readxl)
library(meta)

## READ DATA

exp_violence_es_data_location <- "./review/es-transformation/output/exp_violence_es_data.rds"
raw_exp_violence_es_data <- readRDS(exp_violence_es_data_location)

## SPLIT INTO OUTCOMES

independent_living_program_delinquent_behaviours_es <- raw_exp_violence_es_data %>%
  filter(
    study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" |
    study == "Life Skills Training Program: Los Angeles County" |
    study == "Independent Living – Employment Services Program, Kern County, CA") %>%
  filter(outcome == "Number of delinquent behaviours")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: # OF DELINQUENT BEHAVIOURS

independent_living_program_delinquent_behaviours_meta <- metagen(
   es,
   se,
   data = independent_living_program_delinquent_behaviours_es,
   studlab = study,
   comb.random = TRUE,
   prediction = TRUE,
   sm = "SMD") 

## INSPECT RESULTS

independent_living_program_delinquent_behaviours_meta %>%
  summary()

independent_living_program_delinquent_behaviours_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_delinquent_behaviours_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_delinquent_behaviours_meta.RDS")

saveRDS(
  independent_living_program_delinquent_behaviours_meta,
  file = "./supplementary-material/inputs/independent_living_program_delinquent_behaviours_meta.RDS")

## EXPORT META DATA FOR PLOTTING

independent_living_program_delinquent_behaviours_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_delinquent_behaviours_meta$TE.random,
  overall_effect_upper = independent_living_program_delinquent_behaviours_meta$upper.random,
  overall_effect_lower = independent_living_program_delinquent_behaviours_meta$lower.random,
  overall_effect_p_value = independent_living_program_delinquent_behaviours_meta$pval.random,
  i_squared = independent_living_program_delinquent_behaviours_meta$I2,
  i_squared_p = independent_living_program_delinquent_behaviours_meta$pval.Q
)

independent_living_program_delinquent_behaviours_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_delinquent_behaviours_meta_results) %>%
  left_join(independent_living_program_delinquent_behaviours_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 4,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 2,
      study == "Life Skills Training Program: Los Angeles County" ~ 3,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_delinquent_behaviours_meta_data.RDS")
