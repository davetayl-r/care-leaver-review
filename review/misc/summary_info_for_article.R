## TRANSITIONS REVIEW
## SUPPLEMENTARY DATA FOR REVIEW

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(readxl)

## COUNT NUMBER OF INCLUDED OUTCOMES AND PROPORTION THAT WERE NULL

# Set the path to the folder containing the RDS files
folder_with_es_output <- "./review/es-transformation/output"

# List all the RDS files in the directory with the specific pattern "_es_data.RDS"
es_output <- list.files(
  path = folder_with_es_output,
  pattern = "es_data\\.rds$",
  full.names = TRUE)

# Read and combine all the RDS files into one dataframe
combined_es_data <- es_output %>%
  map_df(readRDS)

# Select relevant data
select_combined_es_data <- combined_es_data %>%
  select(
    study,
    outcome,
    es,
    ci.lo,
    ci.hi
  )

# Declare function to check if CI crosses the line of no effect
check_es_null <- function(
    data, 
    es_col = "es", 
    ci_lo_col = "ci.lo", 
    ci_hi_col = "ci.hi") {

    result <- data %>%
      rowwise() %>%
      mutate(
        is_null = if_else(
          (get(ci_lo_col) < 0 & get(ci_hi_col) > 0) | (get(ci_lo_col) > 0 & get(ci_hi_col) < 0), 
          TRUE, 
          FALSE)
      ) %>%
      ungroup()

return(result)
}

# Run function
checked_data <- check_es_null(select_combined_es_data)

# Summarise
summarised_information <- checked_data %>%
  summarise(
    total_outcomes = n(),
    null_outcomes = sum(is_null),
    proportion_null = mean(is_null)
  )