## OUTCOME: RELATIONSHIPS
## EFFECT SIZE TRANSFORMATION

## LOAD REQUIRED PACKAGES

library(readxl)
library(tidyverse)
library(esc)

## READ DATA

raw_data_location <- "./review/es-transformation/data/es_input.xlsx"

raw_relationships_es_data <- read_excel(
  path = raw_data_location,
  sheet = "relationships"
)

## CLEAN ES DATA

clean_relationships_es_data <- raw_relationships_es_data %>%
  mutate(
    grp1n = round(grp1n, 0),
    grp2n = round(grp2n, 0),
    outcome_id = row_number()
  )

## CONVERT TO COMMON EFFECT SIZE

relationships_binary_proportions_results <- clean_relationships_es_data %>%
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

relationships_mean_sd_results <- clean_relationships_es_data %>%
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

relationships_esc_t_results <- clean_relationships_es_data %>%
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
  )

## MERGE ES DATA

merged_relationships_es_data <- bind_rows(
  relationships_binary_proportions_results,
  relationships_mean_sd_results,
  relationships_esc_t_results) %>%
  rename(
    outcome_id = study) %>%
  arrange(
    outcome_id)

## MERGE WITH CLEAN DATA TO STUDY INFORMATION

informative_relationships_es_data <- clean_relationships_es_data %>%
  mutate(
    outcome_domain = "relationships",
  ) %>%
  select(
    outcome_id,
    study,
    author,
    outcome_domain,
    outcome,
    outcome_timing)

export_relationships_es_data <- informative_relationships_es_data %>% 
  left_join(merged_relationships_es_data, by = "outcome_id") %>%
  select(
   -outcome_id 
  ) %>%
  mutate(
    paste_es = paste(round(es,2), " [", round(ci.lo, 2), ", ", round(ci.hi, 2), "]", sep = "")
  )

## EXPORT DATA

saveRDS(
  object = export_relationships_es_data,
  file = "./review/es-transformation/output/relationships_es_data.rds")

write_csv(
  export_relationships_es_data,
  file = "./review/es-transformation/output/relationships_es_data.csv")

