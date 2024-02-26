## TRANSITIONS REVIEW: META ANALYSIS
## OUTCOME: EMPLOYMENT

## THIS CODE DOES THE FOLLOWING:
## 1. IMPORTS PRE-TRANSFORMED ES
## 2. POOLS THE ES IN A FIXED OR RAMDOM EFFECTS MODEL
## 3. EXPORTS A FOREST PLOT OF THE OUTPUT

library(tidyverse)
library(readxl)
library(meta)

## READ DATA

employment_es_data_location <- "./review/es-transformation/output/employment_es_data.rds"
raw_employment_es_data <- readRDS(employment_es_data_location)

## SPLIT INTO INTERVENTIONS

my_life_employment_es <- raw_employment_es_data %>%
  filter(study %in% c("TAKE CHARGE", "Better Futures"))

independent_living_program_employment_domain_es <- raw_employment_es_data %>%
  filter(
    study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" |
    study == "Life Skills Training Program: Los Angeles County" |
    study == "Independent Living – Employment Services Program, Kern County, CA")

## SPLIT INTO OUTCOMES

independent_living_program_employment_es <- independent_living_program_employment_domain_es %>%
  filter(outcome == "Currently employed")

independent_living_program_earnings_es <- independent_living_program_employment_domain_es %>%
  filter(outcome == "Earnings is past 12 months")

independent_living_program_net_worth_es <- independent_living_program_employment_domain_es %>%
  filter(outcome == "Net worth")

independent_living_program_fin_assist_es <- independent_living_program_employment_domain_es %>%
  filter(outcome == "Received any financial assistance")

## MY LIFE POOL EFFECTS: EMPLOYMENT

my_life_employment_meta <- metagen(
  es,
  se,
  data = my_life_employment_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD") 

## INSPECT RESULTS

my_life_employment_meta %>%
  summary()

my_life_employment_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  my_life_employment_meta,
  file = "./review/meta-analysis/output/meta/my_life_employment_meta.RDS")

saveRDS(
  my_life_employment_meta,
  file = "./supplementary-material/inputs/my_life_employment_meta.RDS")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: CURRENT EMPLOYMENT

independent_living_program_employment_meta <- metagen(
   es,
   se,
   data = independent_living_program_employment_es,
   studlab = study,
   comb.random = TRUE,
   prediction = TRUE,
   sm = "SMD") 

## INSPECT RESULTS

independent_living_program_employment_meta %>%
  summary()

independent_living_program_employment_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_employment_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_employment_meta.RDS")

saveRDS(
  independent_living_program_employment_meta,
  file = "./supplementary-material/inputs/independent_living_program_employment_meta.RDS")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: EARNINGS

independent_living_program_earnings_meta <- metagen(
  es,
  se,
  data = independent_living_program_earnings_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD") 

## INSPECT RESULTS

independent_living_program_earnings_meta %>%
  summary()

independent_living_program_earnings_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_earnings_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_earnings_meta.RDS")

saveRDS(
  independent_living_program_earnings_meta,
  file = "./supplementary-material/inputs/independent_living_program_earnings_meta.RDS")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: NET WORTH

independent_living_program_net_worth_meta <- metagen(
  es,
  se,
  data = independent_living_program_net_worth_es,
  studlab = study,
  comb.random = TRUE,
  prediction = TRUE,
  sm = "SMD") 

## INSPECT RESULTS

independent_living_program_net_worth_meta %>%
  summary()

independent_living_program_net_worth_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_net_worth_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_net_worth_meta.RDS")

saveRDS(
  independent_living_program_net_worth_meta,
  file = "./supplementary-material/inputs/independent_living_program_net_worth_meta.RDS")

## INDEPENDENT LIVING PROGRAM POOL EFFECTS: FINANCIAL ASSISTANCE

independent_living_program_fin_assist_meta <- metagen(
   es,
   se,
   data = independent_living_program_fin_assist_es,
   studlab = study,
   comb.random = TRUE,
   prediction = TRUE,
   sm = "SMD") 

## INSPECT RESULTS

independent_living_program_fin_assist_meta %>%
  summary()

independent_living_program_fin_assist_meta %>%
  forest()

## EXPORT META OBJECT

saveRDS(
  independent_living_program_fin_assist_meta,
  file = "./review/meta-analysis/output/meta/independent_living_program_fin_assist_meta.RDS")

saveRDS(
  independent_living_program_fin_assist_meta,
  file = "./supplementary-material/inputs/independent_living_program_fin_assist_meta.RDS")

## EXPORT META DATA FOR PLOTTING

my_life_employment_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = my_life_employment_meta$TE.random,
  overall_effect_upper = my_life_employment_meta$upper.random,
  overall_effect_lower = my_life_employment_meta$lower.random,
  overall_effect_p_value = my_life_employment_meta$pval.random,
  i_squared = my_life_employment_meta$I2,
  i_squared_p = my_life_employment_meta$pval.Q
)

my_life_employment_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(my_life_employment_meta_results) %>%
  left_join(my_life_employment_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "TAKE CHARGE" ~ 3,
      study == "Better Futures" ~ 2,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/my_life_employment_meta_data.RDS")

independent_living_program_employment_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_employment_meta$TE.random,
  overall_effect_upper = independent_living_program_employment_meta$upper.random,
  overall_effect_lower = independent_living_program_employment_meta$lower.random,
  overall_effect_p_value = independent_living_program_employment_meta$pval.random,
  i_squared = independent_living_program_employment_meta$I2,
  i_squared_p = independent_living_program_employment_meta$pval.Q
)

independent_living_program_employment_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_employment_meta_results) %>%
  left_join(independent_living_program_employment_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 2,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 4,
      study == "Life Skills Training Program: Los Angeles County" ~ 3,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_employment_meta_data.RDS")

independent_living_program_earnings_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_earnings_meta$TE.random,
  overall_effect_upper = independent_living_program_earnings_meta$upper.random,
  overall_effect_lower = independent_living_program_earnings_meta$lower.random,
  overall_effect_p_value = independent_living_program_earnings_meta$pval.random,
  i_squared = independent_living_program_earnings_meta$I2,
  i_squared_p = independent_living_program_earnings_meta$pval.Q
)

independent_living_program_earnings_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_earnings_meta_results) %>%
  left_join(independent_living_program_earnings_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 4,
      study == "Life Skills Training Program: Los Angeles County" ~ 3,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 2,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_earnings_meta_data.RDS")


independent_living_program_net_worth_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_net_worth_meta$TE.random,
  overall_effect_upper = independent_living_program_net_worth_meta$upper.random,
  overall_effect_lower = independent_living_program_net_worth_meta$lower.random,
  overall_effect_p_value = independent_living_program_net_worth_meta$pval.random,
  i_squared = independent_living_program_net_worth_meta$I2,
  i_squared_p = independent_living_program_net_worth_meta$pval.Q
)

independent_living_program_net_worth_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_net_worth_meta_results) %>%
  left_join(independent_living_program_net_worth_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 4,
      study == "Life Skills Training Program: Los Angeles County" ~ 3,
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 2,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_net_worth_meta_data.RDS")


independent_living_program_fin_assist_meta_results <- bind_cols(
  study = "Overall effect",
  overall_effect = independent_living_program_fin_assist_meta$TE.random,
  overall_effect_upper = independent_living_program_fin_assist_meta$upper.random,
  overall_effect_lower = independent_living_program_fin_assist_meta$lower.random,
  overall_effect_p_value = independent_living_program_fin_assist_meta$pval.random,
  i_squared = independent_living_program_fin_assist_meta$I2,
  i_squared_p = independent_living_program_fin_assist_meta$pval.Q
)

independent_living_program_fin_assist_meta %>%
  as.data.frame() %>%
  rename(study = studlab) %>%
  bind_rows(independent_living_program_fin_assist_meta_results) %>%
  left_join(independent_living_program_fin_assist_es, by = "study") %>%
  mutate(
    y_axis_order = case_when(
      study == "Life Skills Training Program: Los Angeles County" ~ 2,
      study == "Independent Living – Employment Services Program, Kern County, CA" ~ 3,
      study == "Massachusetts Adolescent Outreach Program for Youths in Intensive Foster Care" ~ 4,
      study == "Overall effect" ~ 1)
  ) %>%
  saveRDS(
    "./review/figures/plot_data/independent_living_program_fin_assist_meta_data.RDS")
