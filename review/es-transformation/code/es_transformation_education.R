## OUTCOME: EDUCATION
## EFFECT SIZE TRANSFORMATION

## LOAD REQUIRED PACKAGES

library(readxl)
library(tidyverse)
library(esc)

## READ DATA

raw_data_location <- "./review/es-transformation/data/es_input.xlsx"

raw_education_es_data <- read_excel(
  path = raw_data_location,
  sheet = "education"
)

## CLEAN ES DATA

clean_education_es_data <- raw_education_es_data %>%
  mutate(
    grp1n = round(grp1n, 0),
    grp2n = round(grp2n, 0),
    outcome_id = row_number()
  )

## CONVERT TO COMMON EFFECT SIZE

education_binary_proportions_results <- clean_education_es_data %>%
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

education_odds_ratio_data_one <- clean_education_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[1],
    or = .$or[1],
    se = .$se[1],
    totaln = .$totaln[1],
    es.type = "g") %>%
  as.data.frame()

education_odds_ratio_data_two <- clean_education_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[2],
    or = .$or[2],
    se = .$se[2],
    totaln = .$totaln[2],
    es.type = "g") %>%
  as.data.frame()

education_odds_ratio_data_three <- clean_education_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[3],
    or = .$or[3],
    se = .$se[3],
    totaln = .$totaln[3],
    es.type = "g") %>%
  as.data.frame()

education_odds_ratio_data_four <- clean_education_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[4],
    or = .$or[4],
    se = .$se[4],
    totaln = .$totaln[4],
    es.type = "g") %>%
  as.data.frame()

education_odds_ratio_data_five <- clean_education_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[5],
    or = .$or[5],
    se = .$se[5],
    totaln = .$totaln[5],
    es.type = "g") %>%
  as.data.frame()

education_odds_ratio_data_six <- clean_education_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[6],
    or = .$or[6],
    se = .$se[6],
    totaln = .$totaln[6],
    es.type = "g") %>%
  as.data.frame()

education_odds_ratio_data_seven <- clean_education_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[7],
    or = .$or[7],
    se = .$se[7],
    totaln = .$totaln[7],
    es.type = "g") %>%
  as.data.frame()

education_odds_ratio_results <- bind_rows(
  education_odds_ratio_data_one,
  education_odds_ratio_data_two,
  education_odds_ratio_data_three,
  education_odds_ratio_data_four,
  education_odds_ratio_data_five,
  education_odds_ratio_data_six,
  education_odds_ratio_data_seven) %>%
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

merged_education_es_data <- bind_rows(
  education_binary_proportions_results,
  education_odds_ratio_results) %>%
  rename(
    outcome_id = study) %>%
  arrange(
    outcome_id)

## MERGE WITH CLEAN DATA TO STUDY INFORMATION

informative_education_es_data <- clean_education_es_data %>%
  mutate(
    outcome_domain = "education",
  ) %>%
  select(
    outcome_id,
    study,
    author,
    outcome_domain,
    outcome,
    outcome_timing)

export_education_es_data <- informative_education_es_data %>% 
  left_join(merged_education_es_data, by = "outcome_id") %>%
  select(
   -outcome_id 
  ) %>%
  mutate(
    paste_es = paste(round(es,2), " [", round(ci.lo, 2), ", ", round(ci.hi, 2), "]", sep = "")
  )

## EXPORT DATA

saveRDS(
  object = export_education_es_data,
  file = "./review/es-transformation/output/education_es_data.rds")

write_csv(
  export_education_es_data,
  file = "./review/es-transformation/output/education_es_data.csv")
