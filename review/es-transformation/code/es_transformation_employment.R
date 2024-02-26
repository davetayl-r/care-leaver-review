## OUTCOME: EMPLOYMENT
## EFFECT SIZE TRANSFORMATION

## LOAD REQUIRED PACKAGES

library(readxl)
library(tidyverse)
library(esc)

## READ DATA

raw_data_location <- "./review/es-transformation/data/es_input.xlsx"

raw_employment_es_data <- read_excel(
  path = raw_data_location,
  sheet = "employment"
)

## CLEAN ES DATA

clean_employment_es_data <- raw_employment_es_data %>%
  mutate(
    grp1n = round(grp1n, 0),
    grp2n = round(grp2n, 0),
    outcome_id = row_number()
  )

## CONVERT TO COMMON EFFECT SIZE

employment_binary_proportions_results <- clean_employment_es_data %>%
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

employment_mean_sd_results <- clean_employment_es_data %>%
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

employment_mean_se_results <- clean_employment_es_data %>%
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
    es.type = "g"
  ) %>%
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

employment_esc_t_results <- clean_employment_es_data %>%
  filter(esc_type == "esc_t") %>%
  effect_sizes(
    study = outcome_id,
    fun = "esc_t",
    grp1m = grp1m,
    grp2m = grp2m,
    p = t_pvalue,
    grp1n = grp1n,
    grp2n = grp2n,
    es.type = "g"
  ) %>%
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

employment_odds_ratio_results <- clean_employment_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[1],
    or = .$or[1],
    se = .$se[1],
    totaln = .$totaln[1],
    es.type = "g") %>%
  as.data.frame() %>%
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

merged_employment_es_data <- bind_rows(
  employment_binary_proportions_results,
  employment_mean_sd_results,
  employment_mean_se_results,
  employment_esc_t_results,
  employment_odds_ratio_results) %>%
  rename(
    outcome_id = study) %>%
  arrange(
    outcome_id)

## MERGE WITH CLEAN DATA TO STUDY INFORMATION

informative_employment_es_data <- clean_employment_es_data %>%
  mutate(
    outcome_domain = "employment",
  ) %>%
  select(
    outcome_id,
    study,
    author,
    outcome_domain,
    outcome,
    outcome_timing)

export_employment_es_data <- informative_employment_es_data %>% 
  left_join(merged_employment_es_data, by = "outcome_id") %>%
  select(
   -outcome_id 
  ) %>%
  mutate(
    paste_es = paste(round(es,2), " [", round(ci.lo, 2), ", ", round(ci.hi, 2), "]", sep = "")
  )

## EXPORT DATA

saveRDS(
  object = export_employment_es_data,
  file = "./review/es-transformation/output/employment_es_data.rds")

write_csv(
  export_employment_es_data,
  file = "./review/es-transformation/output/employment_es_data.csv")