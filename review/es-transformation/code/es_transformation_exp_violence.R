## OUTCOME: EXPOSURE TO VIOLENCE
## EFFECT SIZE TRANSFORMATION

## LOAD REQUIRED PACKAGES

library(readxl)
library(tidyverse)
library(esc)

## READ DATA

raw_data_location <- "./review/es-transformation/data/es_input.xlsx"

raw_exp_violence_es_data <- read_excel(
  path = raw_data_location,
  sheet = "exp_violence"
)

## CLEAN ES DATA

clean_exp_violence_es_data <- raw_exp_violence_es_data %>%
  mutate(
    grp1n = round(grp1n, 0),
    grp2n = round(grp2n, 0),
    outcome_id = row_number()
  )

## CONVERT TO COMMON EFFECT SIZE

exp_violence_binary_proportions_results <- clean_exp_violence_es_data %>%
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

exp_violence_mean_sd_results <- clean_exp_violence_es_data %>%
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

exp_violence_esc_t_results <- clean_exp_violence_es_data %>%
  filter(esc_type == "esc_t") %>%
  effect_sizes(
    study = outcome_id,
    fun = "esc_t",                                              
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

exp_violence_odds_ratio_data_one <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[1],
    or = .$or[1],
    se = .$se[1],
    totaln = .$totaln[1],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_two <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[2],
    or = .$or[2],
    se = .$se[2],
    totaln = .$totaln[2],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_three <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[3],
    or = .$or[3],
    se = .$se[3],
    totaln = .$totaln[3],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_four <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[4],
    or = .$or[4],
    se = .$se[4],
    totaln = .$totaln[4],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_five <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[5],
    or = .$or[5],
    se = .$se[5],
    totaln = .$totaln[5],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_six <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[6],
    or = .$or[6],
    se = .$se[6],
    totaln = .$totaln[6],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_seven <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[7],
    or = .$or[7],
    se = .$se[7],
    totaln = .$totaln[7],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_eight <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[8],
    or = .$or[8],
    se = .$se[8],
    totaln = .$totaln[8],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_nine <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[9],
    or = .$or[9],
    se = .$se[9],
    totaln = .$totaln[9],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_ten <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[10],
    or = .$or[10],
    se = .$se[10],
    totaln = .$totaln[10],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_eleven <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[11],
    or = .$or[11],
    se = .$se[11],
    totaln = .$totaln[11],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_twelve <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[12],
    or = .$or[12],
    se = .$se[12],
    totaln = .$totaln[12],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_thirteen <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[13],
    or = .$or[13],
    se = .$se[13],
    totaln = .$totaln[13],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_data_fourteen <- clean_exp_violence_es_data %>%
  filter(esc_type == "convert_or2d") %>%
  convert_or2d(
    study = .$outcome_id[14],
    or = .$or[14],
    se = .$se[14],
    totaln = .$totaln[14],
    es.type = "g") %>%
  as.data.frame()

exp_violence_odds_ratio_results <- bind_rows(
  exp_violence_odds_ratio_data_one,
  exp_violence_odds_ratio_data_two,
  exp_violence_odds_ratio_data_three,
  exp_violence_odds_ratio_data_four,
  exp_violence_odds_ratio_data_five,
  exp_violence_odds_ratio_data_six,
  exp_violence_odds_ratio_data_seven,
  exp_violence_odds_ratio_data_eight,
  exp_violence_odds_ratio_data_nine,
  exp_violence_odds_ratio_data_ten,
  exp_violence_odds_ratio_data_eleven,
  exp_violence_odds_ratio_data_twelve,
  exp_violence_odds_ratio_data_thirteen,
  exp_violence_odds_ratio_data_fourteen) %>%
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

merged_exp_violence_es_data <- bind_rows(
  exp_violence_binary_proportions_results,
  exp_violence_mean_sd_results,
  exp_violence_esc_t_results,
  exp_violence_odds_ratio_results) %>%
  rename(
    outcome_id = study) %>%
  arrange(
    outcome_id)

## MERGE WITH CLEAN DATA TO STUDY INFORMATION

informative_exp_violence_es_data <- clean_exp_violence_es_data %>%
  mutate(
    outcome_domain = "exp_violence",
  ) %>%
  select(
    outcome_id,
    study,
    author,
    outcome_domain,
    outcome,
    outcome_timing)

export_exp_violence_es_data <- informative_exp_violence_es_data %>% 
  left_join(merged_exp_violence_es_data, by = "outcome_id") %>%
  select(
   -outcome_id 
  ) %>%
  mutate(
    paste_es = paste(round(es,2), " [", round(ci.lo, 2), ", ", round(ci.hi, 2), "]", sep = "")
  )

## EXPORT DATA

saveRDS(
  object = export_exp_violence_es_data,
  file = "./review/es-transformation/output/exp_violence_es_data.rds")

write_csv(
  export_exp_violence_es_data,
  file = "./review/es-transformation/output/exp_violence_es_data.csv")

