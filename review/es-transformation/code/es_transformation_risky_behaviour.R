## OUTCOME: RISKY BEHAVIOUR
## EFFECT SIZE TRANSFORMATION

## LOAD REQUIRED PACKAGES

library(readxl)
library(tidyverse)
library(esc)

## READ DATA

raw_data_location <- "./review/es-transformation/data/es_input.xlsx"

raw_risky_behaviour_es_data <- read_excel(
  path = raw_data_location,
  sheet = "risky_behaviour"
)

## CLEAN ES DATA

clean_risky_behaviour_es_data <- raw_risky_behaviour_es_data %>%
  mutate(
    grp1n = round(grp1n, 0),
    grp2n = round(grp2n, 0),
    outcome_id = row_number()
  )

## CONVERT TO COMMON EFFECT SIZE

risky_behaviour_binary_proportions_results <- clean_risky_behaviour_es_data %>%
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

risky_behaviour_esc_b_results <- clean_risky_behaviour_es_data %>%
  filter(esc_type == "esc_B") %>%
  effect_sizes(
    study = outcome_id,
    fun = "esc_B", 
    b = beta,
    sdy = sdy,
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

risky_behaviour_esc_t_results <- clean_risky_behaviour_es_data %>%
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
  # specify direction of effect consitent with study findings
  mutate(
    es = case_when(
      study == 4 ~ es * -1),
    ci.lo_new = case_when(
      study == 4 ~ ci.hi * -1),
    ci.hi_new = case_when(
      study == 4 ~ ci.lo * -1)
  ) %>%
  select(
    -ci.hi,
    -ci.lo
  ) %>%
  rename(
    ci.hi = ci.hi_new,
    ci.lo = ci.lo_new
  )

## MERGE ES DATA

merged_risky_behaviour_es_data <- bind_rows(
  risky_behaviour_binary_proportions_results,
  risky_behaviour_esc_b_results,
  risky_behaviour_esc_t_results) %>%
  rename(
    outcome_id = study) %>%
  arrange(
    outcome_id)

## MERGE WITH CLEAN DATA TO STUDY INFORMATION

informative_risky_behaviour_es_data <- clean_risky_behaviour_es_data %>%
  mutate(
    outcome_domain = "risky_behaviour",
  ) %>%
  select(
    outcome_id,
    study,
    author,
    outcome_domain,
    outcome,
    outcome_timing)

export_risky_behaviour_es_data <- informative_risky_behaviour_es_data %>% 
  left_join(merged_risky_behaviour_es_data, by = "outcome_id") %>%
  select(
   -outcome_id 
  ) %>%
  mutate(
    paste_es = paste(round(es,2), " [", round(ci.lo, 2), ", ", round(ci.hi, 2), "]", sep = "")
  )

## EXPORT DATA

saveRDS(
  object = export_risky_behaviour_es_data,
  file = "./review/es-transformation/output/risky_behaviour_es_data.rds")

write_csv(
  export_risky_behaviour_es_data,
  file = "./review/es-transformation/output/risky_behaviour_es_data.csv")

