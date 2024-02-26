## OUTCOME: HOMELESSNESS
## EFFECT SIZE TRANSFORMATION

## LOAD REQUIRED PACKAGES

library(readxl)
library(tidyverse)
library(esc)

## READ DATA

raw_data_location <- "./review/es-transformation/data/es_input.xlsx"

raw_homelessness_es_data <- read_excel(
  path = raw_data_location,
  sheet = "homelessness"
)

## CLEAN ES DATA

clean_homelessness_es_data <- raw_homelessness_es_data %>%
  mutate(
    grp1n = round(grp1n, 0),
    grp2n = round(grp2n, 0),
    outcome_id = row_number())

## CONVERT TO COMMON EFFECT SIZE

homelessness_binary_proportions_results <- clean_homelessness_es_data %>%
  filter(esc_type == "binary_proportions") %>%
  effect_sizes(
    study = outcome_id,
    fun = "esc_bin_prop",                                              
    prop1event = prop1event,
    grp1n = grp1n,
    prop2event = prop2event,
    grp2n = grp2n,
    es.type = "g") %>%
  select(
    study,
    measure,
    es,
    ci.lo,
    ci.hi,
    sample.size,
    se,
    var
  )

homelessness_mean_sd_results <- clean_homelessness_es_data %>%
  filter(esc_type == "esc_mean_sd") %>%
  effect_sizes(
    study = outcome_id,
    fun = "esc_mean_sd",                                              
    grp1m = grp1m,
    grp1sd = grp1sd, 
    grp2m = grp2m, 
    grp2sd = grp2sd, 
    grp1n = grp1n,
    grp2n = grp2n,
    es.type = "g") %>%
  select(
    study,
    measure,
    es,
    ci.lo,
    ci.hi,
    sample.size,
    se,
    var
  )

homelessness_mean_se_results <- clean_homelessness_es_data %>%
  filter(esc_type == "esc_mean_se") %>%
  effect_sizes(
    study = outcome_id,
    fun = "esc_mean_se",                                              
    grp1m = grp1m,
    grp1se = grp1se, 
    grp2m = grp2m, 
    grp2se = grp2se, 
    grp1n = grp1n,
    grp2n = grp2n,
    es.type = "g") %>%
  select(
    study,
    measure,
    es,
    ci.lo,
    ci.hi,
    sample.size,
    se,
    var
  )

homelessness_esc_t_results <- clean_homelessness_es_data %>%
  filter(esc_type == "esc_t") %>%
  effect_sizes(
    study = outcome_id,
    fun = "esc_t",                                              
    grp1m = grp1m,
    grp2m = grp2m, 
    p = t_pvalue,
    grp1n = grp1n,
    grp2n = grp2n,
    es.type = "g") %>%
  select(
    study,
    measure,
    es,
    ci.lo,
    ci.hi,
    sample.size,
    se,
    var
  ) %>%
  # change direction of effect to align with findings
  mutate(
    es = case_when(
      study == 7 ~ es * -1),
    ci.lo_new = case_when(
      study == 7 ~ ci.hi * -1),
    ci.hi_new = case_when(
      study == 7 ~ ci.lo * -1)
  ) %>%
  select(
    -ci.hi,
    -ci.lo
  ) %>%
  rename(
    ci.hi = ci.hi_new,
    ci.lo = ci.lo_new
  )

homelessness_odds_ratio_data_one <- clean_homelessness_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[1],
    or = .$or[1],
    se = .$se[1],
    totaln = .$totaln[1],
    es.type = "g") %>%
  as.data.frame()

homelessness_odds_ratio_data_two <- clean_homelessness_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[2],
    or = .$or[2],
    se = .$se[2],
    totaln = .$totaln[2],
    es.type = "g") %>%
  as.data.frame()

homelessness_odds_ratio_data_three <- clean_homelessness_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[3],
    or = .$or[3],
    se = .$se[3],
    totaln = .$totaln[3],
    es.type = "g") %>%
  as.data.frame()

homelessness_odds_ratio_results <- bind_rows(
  homelessness_odds_ratio_data_one,
  homelessness_odds_ratio_data_two,
  homelessness_odds_ratio_data_three) %>%
  select(
    study,
    measure,
    es,
    ci.lo,
    ci.hi,
    sample.size,
    se,
    var
  )

## MERGE ES DATA

merged_homelessness_es_data <- bind_rows(
  homelessness_binary_proportions_results,
  homelessness_mean_sd_results,
  homelessness_mean_se_results,
  homelessness_esc_t_results,
  homelessness_odds_ratio_results) %>%
  rename(
    outcome_id = study) %>%
  arrange(
    outcome_id)

## MERGE WITH CLEAN DATA TO STUDY INFORMATION

informative_homelessness_es_data <- clean_homelessness_es_data %>%
  mutate(
    outcome_domain = "Homelessness",
  ) %>%
  select(
    outcome_id,
    study,
    author,
    outcome_domain,
    outcome,
    outcome_timing)

export_homelessness_es_data <- informative_homelessness_es_data %>% 
  left_join(merged_homelessness_es_data, by = "outcome_id") %>%
  select(
   -outcome_id 
  ) %>%
  mutate(
    paste_es = paste(round(es,2), " [", round(ci.lo, 2), ", ", round(ci.hi, 2), "]", sep = "")
  )

## EXPORT DATA

saveRDS(
  object = export_homelessness_es_data,
  file = "./review/es-transformation/output/homelessness_es_data.rds")

write_csv(
  export_homelessness_es_data,
  file = "./review/es-transformation/output/homelessness_es_data.csv")

