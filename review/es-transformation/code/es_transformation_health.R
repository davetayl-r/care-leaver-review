## OUTCOME: HEALTH
## EFFECT SIZE TRANSFORMATION

## LOAD REQUIRED PACKAGES

library(readxl)
library(tidyverse)
library(esc)
library(countES)
#devtools::install_github("stefanycoxe/countES")

## READ DATA

raw_data_location <- "./review/es-transformation/data/es_input.xlsx"

raw_health_es_data <- read_excel(
  path = raw_data_location,
  sheet = "health"
)

## CLEAN ES DATA

clean_health_es_data <- raw_health_es_data %>%
  mutate(
    grp1n = round(grp1n, 0),
    grp2n = round(grp2n, 0),
    outcome_id = row_number()
  )

## CONVERT TO COMMON EFFECT SIZE

health_binary_proportions_results <- clean_health_es_data %>%
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

health_mean_sd_results <- clean_health_es_data %>%
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

health_mean_se_results <- clean_health_es_data %>%
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

health_esc_t_results <- clean_health_es_data %>%
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

health_neg_binom_es_data_input_one <- clean_health_es_data %>%
  filter(
    esc_type == "count_es_neg_binom_without_zero",
    outcome_id == 1)

countES(
  int = health_neg_binom_es_data_input_one$intercept,
  int_se = health_neg_binom_es_data_input_one$intercept_se,
  slope = health_neg_binom_es_data_input_one$beta,
  slope_se = health_neg_binom_es_data_input_one$beta_se,
  disp = health_neg_binom_es_data_input_one$disp,
  mtype = "negative binomial",
  reps = 10000,
  CI_level = 95,
  randseed = 1000,
  eff_plot = TRUE,
  dist_plot = TRUE) 

## manually add results to data frame

health_neg_binom_es_data_output_one <- data.frame(
  study = health_neg_binom_es_data_input_one$outcome_id[1],
  measure = "g",
  es = 1.339738,
  ci.lo = -0.5079577,
  ci.hi = 10.97353,
  sample.size = health_neg_binom_es_data_input_one$grp1n[1] + health_neg_binom_es_data_input_one$grp2n[1],
  se = NA_real_,
  var = NA_real_
) 

health_neg_binom_es_data_input_two <- clean_health_es_data %>%
  filter(
    esc_type == "count_es_neg_binom_without_zero",
    outcome_id == 3)

countES(
  int = health_neg_binom_es_data_input_two$intercept,
  int_se = health_neg_binom_es_data_input_two$intercept_se,
  slope = health_neg_binom_es_data_input_two$beta,
  slope_se = health_neg_binom_es_data_input_two$beta_se,
  disp = health_neg_binom_es_data_input_two$disp,
  mtype = "negative binomial",
  reps = 10000,
  CI_level = 95,
  randseed = 1000,
  eff_plot = TRUE,
  dist_plot = TRUE) 

## manually add results to data frame

health_neg_binom_es_data_output_two <- data.frame(
  study = health_neg_binom_es_data_input_two$outcome_id[1],
  measure = "g",
  es = 3.33628,
  ci.lo = -1.1730762,
  ci.hi = 58.79016,
  sample.size = health_neg_binom_es_data_input_two$grp1n[1] + health_neg_binom_es_data_input_two$grp2n[1],
  se = NA_real_,
  var = NA_real_
) 

health_poisson_es_data_three_input <- clean_health_es_data %>%
  filter(esc_type == "count_es_poisson_without_zero")

countES(
  int = health_poisson_es_data_three_input$intercept,
  int_se = health_poisson_es_data_three_input$intercept_se,
  slope = health_poisson_es_data_three_input$beta,
  slope_se = health_poisson_es_data_three_input$beta_se,
  mtype = "poisson",
  reps = 10000,
  CI_level = 95,
  randseed = 1000,
  eff_plot = TRUE,
  dist_plot = TRUE) 

## manually add results to data frame

health_poisson_es_data_output_three <- data.frame(
  study = health_poisson_es_data_three_input$outcome_id[1],
  measure = "g",
  es = 5.108321,
  ci.lo = -10.7670653,
  ci.hi = 111.882011,
  sample.size = health_poisson_es_data_three_input$grp1n[1] + health_poisson_es_data_three_input$grp2n[1],
  se = NA_real_,
  var = NA_real_
  ) 

health_count_es_results <- bind_rows(
  health_neg_binom_es_data_output_one,
  health_neg_binom_es_data_output_two,
  health_poisson_es_data_output_three
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

## MERGE ES DATA

merged_health_es_data <- bind_rows(
  health_binary_proportions_results,
  health_mean_sd_results,
  health_mean_se_results,
  health_esc_t_results,
  health_count_es_results) %>%
  rename(
    outcome_id = study) %>%
  arrange(
    outcome_id)

## MERGE WITH CLEAN DATA TO STUDY INFORMATION

informative_health_es_data <- clean_health_es_data %>%
  mutate(
    outcome_domain = "health",
  ) %>%
  select(
    outcome_id,
    study,
    author,
    outcome_domain,
    outcome,
    outcome_timing)

export_health_es_data <- informative_health_es_data %>% 
  left_join(merged_health_es_data, by = "outcome_id") %>%
  select(
   -outcome_id 
  ) %>%
  mutate(
    paste_es = paste(round(es,2), " [", round(ci.lo, 2), ", ", round(ci.hi, 2), "]", sep = "")
  )

## EXPORT DATA

saveRDS(
  object = export_health_es_data,
  file = "./review/es-transformation/output/health_es_data.rds")

write_csv(
  export_health_es_data,
  file = "./review/es-transformation/output/health_es_data.csv")

