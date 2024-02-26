## TRANSITIONS REVIEW: META ANALYSIS
## OUTCOME: LIFE SKILLS

## THIS CODE DOES THE FOLLOWING:
## 1. IMPORTS PRE-TRANSFORMED ES
## 2. POOLS THE ES IN A FIXED OR RAMDOM EFFECTS MODEL
## 3. EXPORTS A FOREST PLOT OF THE OUTPUT

library(tidyverse)
library(readxl)
library(meta)

## READ DATA

life_skills_es_data_location <- "./review/es-transformation/output/life_skills_es_data.rds"
raw_life_skills_es_data <- readRDS(life_skills_es_data_location)

## SUBSET DATA FOR STUDIES TO INCLUDE IN META

independent_living_program_life_skills_es <- raw_life_skills_es_data %>%
  filter(
    study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" |
    study == "Life Skills Training Program: Los Angeles County" |
    study == "Independent Living – Employment Services Program, Kern County, CA")

## SPLIT INTO OUTCOMES

independent_living_program_prep_overall_es <- independent_living_program_life_skills_es %>%
  filter(outcome == "Preparedness (overall)")

independent_living_program_prep_job_es <- independent_living_program_life_skills_es %>%
  filter(outcome == "Preparedness (job-related)")

independent_living_program_fin_account_es <- independent_living_program_life_skills_es %>%
  filter(outcome == "Financial accounts (any)")

independent_living_program_social_sec_es <- independent_living_program_life_skills_es %>%
  filter(outcome == "Social security card")

independent_living_program_birth_cert_es <- independent_living_program_life_skills_es %>%
  filter(outcome == "Birth certificate")

independent_living_program_drivers_lic_es <- independent_living_program_life_skills_es %>%
  filter(outcome == "Drivers license")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: OVERALL PREPAREDNESS

independent_living_program_prep_overall_meta <- metagen(
   es,
   se,
   data = independent_living_program_prep_overall_es,
   studlab = study,
   comb.random = TRUE,
   prediction = TRUE,
   sm = "SMD") 

## INSPECT RESULTS

independent_living_program_prep_overall_meta %>%
  summary()

independent_living_program_prep_overall_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_prep_overall_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_prep_overall_meta.RDS")

saveRDS(
  independent_living_program_prep_overall_meta,
  file = "./supplementary-material/inputs/independent_living_program_prep_overall_meta.RDS")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: JOB-RELATED PREPAREDNESS

independent_living_program_prep_job_meta <- metagen(
  es,
  se,
  data = independent_living_program_prep_job_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD") 

## INSPECT RESULTS

independent_living_program_prep_job_meta %>%
  summary()

independent_living_program_prep_job_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_prep_job_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_prep_job_meta.RDS")

saveRDS(
  independent_living_program_prep_job_meta,
  file = "./supplementary-material/inputs/independent_living_program_prep_job_meta.RDS")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: FINANCIAL ACCOUNTS

independent_living_program_fin_account_meta <- metagen(
  es,
  se,
  data = independent_living_program_fin_account_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD") 

## INSPECT RESULTS

independent_living_program_fin_account_meta %>%
  summary()

independent_living_program_fin_account_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_fin_account_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_fin_account_meta.RDS")

saveRDS(
  independent_living_program_fin_account_meta,
  file = "./supplementary-material/inputs/independent_living_program_fin_account_meta.RDS")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: SOCIAL SECURITY CARD

independent_living_program_social_sec_meta <- metagen(
  es,
  se,
  data = independent_living_program_social_sec_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD") 

## INSPECT RESULTS

independent_living_program_social_sec_meta %>%
  summary()

independent_living_program_social_sec_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_social_sec_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_social_sec_meta.RDS")

saveRDS(
  independent_living_program_social_sec_meta,
  file = "./supplementary-material/inputs/independent_living_program_social_sec_meta.RDS")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: HAS BIRTH CERTIFICATE

independent_living_program_birth_cert_meta <- metagen(
  es,
  se,
  data = independent_living_program_birth_cert_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD")

## INSPECT RESULTS

independent_living_program_birth_cert_meta %>%
  summary()

independent_living_program_birth_cert_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_birth_cert_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_birth_cert_meta.RDS")

saveRDS(
  independent_living_program_birth_cert_meta,
  file = "./supplementary-material/inputs/independent_living_program_birth_cert_meta.RDS")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: HAS DRIVERS LICENCE

independent_living_program_drivers_lic_meta <- metagen(
  es,
  se,
  data = independent_living_program_drivers_lic_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD")  

## INSPECT RESULTS

independent_living_program_drivers_lic_meta %>%
  summary()

independent_living_program_drivers_lic_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_drivers_lic_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_drivers_lic_meta.RDS")

saveRDS(
  independent_living_program_drivers_lic_meta,
  file = "./supplementary-material/inputs/independent_living_program_drivers_lic_meta.RDS")

## EXPORT META DATA FOR PLOTTING

independent_living_program_prep_overall_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_prep_overall_meta$TE.random,
  overall_effect_upper = independent_living_program_prep_overall_meta$upper.random,
  overall_effect_lower = independent_living_program_prep_overall_meta$lower.random,
  overall_effect_p_value = independent_living_program_prep_overall_meta$pval.random,
  i_squared = independent_living_program_prep_overall_meta$I2,
  i_squared_p = independent_living_program_prep_overall_meta$pval.Q
)

independent_living_program_prep_overall_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_prep_overall_meta_results) %>%
  left_join(independent_living_program_prep_overall_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 4,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 2,
      study == "Life Skills Training Program: Los Angeles County" ~ 3,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_prep_overall_meta_data.RDS")

independent_living_program_prep_job_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_prep_job_meta$TE.random,
  overall_effect_upper = independent_living_program_prep_job_meta$upper.random,
  overall_effect_lower = independent_living_program_prep_job_meta$lower.random,
  overall_effect_p_value = independent_living_program_prep_job_meta$pval.random,
  i_squared = independent_living_program_prep_job_meta$I2,
  i_squared_p = independent_living_program_prep_job_meta$pval.Q
)

independent_living_program_prep_job_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_prep_job_meta_results) %>%
  left_join(independent_living_program_prep_job_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 4,
      study == "Life Skills Training Program: Los Angeles County" ~ 2,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 3,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_prep_job_meta_data.RDS")

independent_living_program_fin_account_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_fin_account_meta$TE.random,
  overall_effect_upper = independent_living_program_fin_account_meta$upper.random,
  overall_effect_lower = independent_living_program_fin_account_meta$lower.random,
  overall_effect_p_value = independent_living_program_fin_account_meta$pval.random,
  i_squared = independent_living_program_fin_account_meta$I2,
  i_squared_p = independent_living_program_fin_account_meta$pval.Q
)

independent_living_program_fin_account_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_fin_account_meta_results) %>%
  left_join(independent_living_program_fin_account_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 4,
      study == "Life Skills Training Program: Los Angeles County" ~ 2,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 3,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_fin_account_meta_data.RDS")

independent_living_program_social_sec_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_social_sec_meta$TE.random,
  overall_effect_upper = independent_living_program_social_sec_meta$upper.random,
  overall_effect_lower = independent_living_program_social_sec_meta$lower.random,
  overall_effect_p_value = independent_living_program_social_sec_meta$pval.random,
  i_squared = independent_living_program_social_sec_meta$I2,
  i_squared_p = independent_living_program_social_sec_meta$pval.Q
)

independent_living_program_social_sec_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_social_sec_meta_results) %>%
  left_join(independent_living_program_social_sec_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 4,
      study == "Life Skills Training Program: Los Angeles County" ~ 2,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 3,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_social_sec_meta_data.RDS")

independent_living_program_birth_cert_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_birth_cert_meta$TE.random,
  overall_effect_upper = independent_living_program_birth_cert_meta$upper.random,
  overall_effect_lower = independent_living_program_birth_cert_meta$lower.random,
  overall_effect_p_value = independent_living_program_birth_cert_meta$pval.random,
  i_squared = independent_living_program_birth_cert_meta$I2,
  i_squared_p = independent_living_program_birth_cert_meta$pval.Q
)

independent_living_program_birth_cert_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_birth_cert_meta_results) %>%
  left_join(independent_living_program_birth_cert_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 4,
      study == "Life Skills Training Program: Los Angeles County" ~ 2,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 3,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_birth_cert_meta_data.RDS")

independent_living_program_drivers_lic_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_drivers_lic_meta$TE.random,
  overall_effect_upper = independent_living_program_drivers_lic_meta$upper.random,
  overall_effect_lower = independent_living_program_drivers_lic_meta$lower.random,
  overall_effect_p_value = independent_living_program_drivers_lic_meta$pval.random,
  i_squared = independent_living_program_drivers_lic_meta$I2,
  i_squared_p = independent_living_program_drivers_lic_meta$pval.Q
)

independent_living_program_drivers_lic_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_drivers_lic_meta_results) %>%
  left_join(independent_living_program_drivers_lic_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 4,
      study == "Life Skills Training Program: Los Angeles County" ~ 2,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 3,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_drivers_lic_meta_data.RDS")