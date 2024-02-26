## TRANSITIONS REVIEW: META ANALYSIS
## OUTCOME: EDUCATION

## THIS CODE DOES THE FOLLOWING:
## 1. IMPORTS PRE-TRANSFORMED ES
## 2. POOLS THE ES IN A FIXED OR RAMDOM EFFECTS MODEL
## 3. EXPORTS A FOREST PLOT OF THE OUTPUT

library(tidyverse)
library(readxl)
library(meta)

## READ DATA

education_es_data_location <- "./review/es-transformation/output/education_es_data.rds"
raw_education_es_data <- readRDS(education_es_data_location)

## SPLIT INTO INTERVENTIONS

my_life_education_domain_es <- raw_education_es_data %>%
  filter(study %in% c("TAKE CHARGE", "Better Futures"))

independent_living_program_education_domain_es <- raw_education_es_data %>%
  filter(
    study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" |
    study == "Life Skills Training Program: Los Angeles County" |
    study == "Independent Living – Employment Services Program, Kern County, CA")

## SPLIT INTO OUTCOMES

independent_living_program_hs_ged_es <- independent_living_program_education_domain_es %>%
  filter(outcome == "High school diploma or GED")

independent_living_program_attended_university_es <- independent_living_program_education_domain_es %>%
  filter(outcome == "Attended college")

my_life_hs_ged_es <- my_life_education_domain_es %>%
  filter(outcome == "High school diploma or GED")

my_life_post_secondary_ed_es <- my_life_education_domain_es %>%
  filter(outcome == "Undertook postsecondary education")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: COMPLETE HIGH SCHOOL

independent_living_program_hs_ged_meta <- metagen(
   es,
   se,
   data = independent_living_program_hs_ged_es,
   studlab = study,
   comb.random = TRUE,
   prediction = TRUE,
   sm = "SMD") 

## INSPECT RESULTS

independent_living_program_hs_ged_meta %>%
  summary()

independent_living_program_hs_ged_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_hs_ged_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_hs_ged_meta.RDS")

saveRDS(
  independent_living_program_hs_ged_meta,
  file = "./supplementary-material/inputs/independent_living_program_hs_ged_meta.RDS")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: ATTENDED UNIVERSITY

independent_living_program_attended_university_meta <- metagen(
  es,
  se,
  data = independent_living_program_attended_university_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD") 

## INSPECT RESULTS

independent_living_program_attended_university_meta %>%
  summary()

independent_living_program_attended_university_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_attended_university_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_attended_university_meta.RDS")

saveRDS(
  independent_living_program_attended_university_meta,
  file = "./supplementary-material/inputs/independent_living_program_attended_university_meta.RDS")

## MY LIFE POOL EFFECTS: HIGH SCHOOL EDUCATION OR EQUIV.

my_life_hs_ged_meta <- metagen(
  es,
  se,
  data = my_life_hs_ged_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD") 

## INSPECT RESULTS

my_life_hs_ged_meta %>%
  summary()

my_life_hs_ged_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  my_life_hs_ged_meta,
  file = "./review/meta-analysis/output/meta/my_life_hs_ged_meta.RDS")

saveRDS(
  my_life_hs_ged_meta,
  file = "./supplementary-material/inputs/my_life_hs_ged_meta.RDS")

## MY LIFE POOL EFFECTS: POST SECONDARY EDUCATION

my_life_post_secondary_ed_meta <- metagen(
  es,
  se,
  data = my_life_post_secondary_ed_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD")

## INSPECT RESULTS

my_life_post_secondary_ed_meta %>%
  summary()

my_life_post_secondary_ed_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  my_life_post_secondary_ed_meta,
  file = "./review/meta-analysis/output/meta/my_life_post_secondary_ed_meta.RDS")

saveRDS(
  my_life_post_secondary_ed_meta,
  file = "./supplementary-material/inputs/my_life_post_secondary_ed_meta.RDS")

## EXPORT META DATA FOR PLOTTING

independent_living_program_hs_ged_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_hs_ged_meta$TE.random,
  overall_effect_upper = independent_living_program_hs_ged_meta$upper.random,
  overall_effect_lower = independent_living_program_hs_ged_meta$lower.random,
  overall_effect_p_value = independent_living_program_hs_ged_meta$pval.random,
  i_squared = independent_living_program_hs_ged_meta$I2,
  i_squared_p = independent_living_program_hs_ged_meta$pval.Q
)

independent_living_program_hs_ged_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_hs_ged_meta_results) %>%
  left_join(independent_living_program_hs_ged_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 4,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 2,
      study == "Life Skills Training Program: Los Angeles County" ~ 3,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_hs_ged_meta_data.RDS")

independent_living_program_attended_university_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_attended_university_meta$TE.random,
  overall_effect_upper = independent_living_program_attended_university_meta$upper.random,
  overall_effect_lower = independent_living_program_attended_university_meta$lower.random,
  overall_effect_p_value = independent_living_program_attended_university_meta$pval.random,
  i_squared = independent_living_program_attended_university_meta$I2,
  i_squared_p = independent_living_program_attended_university_meta$pval.Q
)

independent_living_program_attended_university_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_attended_university_meta_results) %>%
  left_join(independent_living_program_attended_university_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 4,
      study == "Life Skills Training Program: Los Angeles County" ~ 2,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 3,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_attended_university_meta_data.RDS")

my_life_hs_ged_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = my_life_hs_ged_meta$TE.random,
  overall_effect_upper = my_life_hs_ged_meta$upper.random,
  overall_effect_lower = my_life_hs_ged_meta$lower.random,
  overall_effect_p_value = my_life_hs_ged_meta$pval.random,
  i_squared = my_life_hs_ged_meta$I2,
  i_squared_p = my_life_hs_ged_meta$pval.Q
)

my_life_hs_ged_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(my_life_hs_ged_meta_results) %>%
  left_join(my_life_hs_ged_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "TAKE CHARGE" ~ 3,
      study == "Better Futures" ~ 2,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/my_life_hs_ged_meta_data.RDS")

my_life_post_secondary_ed_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = my_life_post_secondary_ed_meta$TE.random,
  overall_effect_upper = my_life_post_secondary_ed_meta$upper.random,
  overall_effect_lower = my_life_post_secondary_ed_meta$lower.random,
  overall_effect_p_value = my_life_post_secondary_ed_meta$pval.random,
  i_squared = my_life_post_secondary_ed_meta$I2,
  i_squared_p = my_life_post_secondary_ed_meta$pval.Q
)

my_life_post_secondary_ed_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(my_life_post_secondary_ed_meta_results) %>%
  left_join(my_life_post_secondary_ed_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "TAKE CHARGE" ~ 2,
      study == "Better Futures" ~ 3,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/my_life_post_secondary_ed_meta_data.RDS")