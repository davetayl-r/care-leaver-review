## PLOT FINAL FOREST PLOT FOR PUBLICATION

## LOAD REQUIRED PACKAGES

library(tidyverse)
library(ggplot2)
library(cowplot)

## READ IN PLOTS

independent_living_program_homelessness_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_homelessness_meta_plot_object.RDS"  
independent_living_program_homelessness_meta_plot_object <- readRDS(independent_living_program_homelessness_meta_plot_object_location)

independent_living_program_residential_moves_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_residential_moves_meta_plot_object.RDS"   
independent_living_program_residential_moves_meta_plot_object <- readRDS(independent_living_program_residential_moves_meta_plot_object_location) 

independent_living_program_hs_ged_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_hs_ged_meta_plot_object.RDS"  
independent_living_program_hs_ged_meta_plot_object <- readRDS(independent_living_program_hs_ged_meta_plot_object_location)

coach_peer_support_hs_ged_meta_plot_object_location <- "./review/figures/plot_data/coach_peer_support_hs_ged_meta_plot_object.RDS"     
coach_peer_support_hs_ged_meta_plot_object <- readRDS(coach_peer_support_hs_ged_meta_plot_object_location) 

coach_peer_support_post_secondary_ed_meta_plot_object_location <- "./review/figures/plot_data/coach_peer_support_post_secondary_ed_meta_plot_object.RDS"
coach_peer_support_post_secondary_ed_meta_plot_object <- readRDS(coach_peer_support_post_secondary_ed_meta_plot_object_location) 

independent_living_program_attended_university_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_attended_university_meta_plot_object.RDS"
independent_living_program_attended_university_meta_plot_object <- readRDS(independent_living_program_attended_university_meta_plot_object_location)

independent_living_program_earnings_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_earnings_meta_plot_object.RDS"   
independent_living_program_earnings_meta_plot_object <- readRDS(independent_living_program_earnings_meta_plot_object_location)

independent_living_program_employment_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_employment_meta_plot_object.RDS"   
independent_living_program_employment_meta_plot_object <- readRDS(independent_living_program_employment_meta_plot_object_location)

coach_peer_support_employment_meta_plot_object_location <- "./review/figures/plot_data/coach_peer_support_employment_meta_plot_object.RDS"   
coach_peer_support_employment_meta_plot_object <- readRDS(coach_peer_support_employment_meta_plot_object_location) 

coach_peer_support_quality_of_life_meta_plot_object_location <- "./review/figures/plot_data/coach_peer_support_quality_of_life_meta_plot_object.RDS"   
coach_peer_support_quality_of_life_meta_plot_object <- readRDS(coach_peer_support_quality_of_life_meta_plot_object_location) 

independent_living_program_fin_assist_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_fin_assist_meta_plot_object.RDS"  
independent_living_program_fin_assist_meta_plot_object <- readRDS(independent_living_program_fin_assist_meta_plot_object_location)

independent_living_program_net_worth_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_net_worth_meta_plot_object.RDS" 
independent_living_program_net_worth_meta_plot_object <- readRDS(independent_living_program_net_worth_meta_plot_object_location)

independent_living_program_delinquent_behaviours_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_delinquent_behaviours_meta_plot_object.RDS" 
independent_living_program_delinquent_behaviours_meta_plot_object <- readRDS(independent_living_program_delinquent_behaviours_meta_plot_object_location)

independent_living_program_pregnant_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_pregnant_meta_plot_object.RDS"  
independent_living_program_pregnant_meta_plot_object <- readRDS(independent_living_program_pregnant_meta_plot_object_location) 

independent_living_program_prep_overall_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_prep_overall_meta_plot_object.RDS"  
independent_living_program_prep_overall_meta_plot_object <- readRDS(independent_living_program_prep_overall_meta_plot_object_location) 

independent_living_program_prep_job_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_prep_job_meta_plot_object.RDS"  
independent_living_program_prep_job_meta_plot_object <- readRDS(independent_living_program_prep_job_meta_plot_object_location) 

independent_living_program_fin_account_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_fin_account_meta_plot_object.RDS" 
independent_living_program_fin_account_meta_plot_object <- readRDS(independent_living_program_fin_account_meta_plot_object_location)

independent_living_program_social_sec_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_social_sec_meta_plot_object.RDS"       
independent_living_program_social_sec_meta_plot_object <- readRDS(independent_living_program_social_sec_meta_plot_object_location) 

independent_living_program_birth_cert_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_birth_cert_meta_plot_object.RDS"      
independent_living_program_birth_cert_meta_plot_object <- readRDS(independent_living_program_birth_cert_meta_plot_object_location)

independent_living_program_drivers_lic_meta_plot_object_location <- "./review/figures/plot_data/independent_living_program_drivers_lic_meta_plot_object.RDS"  
independent_living_program_drivers_lic_meta_plot_object <- readRDS(independent_living_program_drivers_lic_meta_plot_object_location)

## MERGE ILP PLOTS

ilp_plots <- plot_grid(
  independent_living_program_homelessness_meta_plot_object, 
  independent_living_program_residential_moves_meta_plot_object,
  independent_living_program_hs_ged_meta_plot_object,
  independent_living_program_attended_university_meta_plot_object,
  independent_living_program_earnings_meta_plot_object,
  independent_living_program_employment_meta_plot_object,
  independent_living_program_fin_assist_meta_plot_object, 
  independent_living_program_net_worth_meta_plot_object,
  independent_living_program_delinquent_behaviours_meta_plot_object,
  independent_living_program_pregnant_meta_plot_object,
  independent_living_program_prep_overall_meta_plot_object, 
  independent_living_program_prep_job_meta_plot_object, 
  independent_living_program_fin_account_meta_plot_object, 
  independent_living_program_social_sec_meta_plot_object, 
  independent_living_program_birth_cert_meta_plot_object, 
  independent_living_program_drivers_lic_meta_plot_object,  
  align = "h",
  ncol = 2,
  labels = "AUTO",
  label_size = 18,
  vjust = 1
  ) +
  theme(
    plot.background = element_rect(
      fill = "#FFFFFF", 
      color = NA),
    plot.margin = margin(
      t = 0.2, 
      r = 0.1, 
      b = 0.1, 
      l = 0.1,
      unit = "cm") 
  )

## MERGE MY LIFE PLOTS

coach_peer_support_plots <- plot_grid(
  coach_peer_support_quality_of_life_meta_plot_object,
  coach_peer_support_hs_ged_meta_plot_object, 
  coach_peer_support_post_secondary_ed_meta_plot_object,
  coach_peer_support_employment_meta_plot_object,  
  align = "h",
  ncol = 2,
  labels = "AUTO",
  label_size = 18,
  vjust = 1
  ) +
  theme(
    plot.background = element_rect(
      fill = "#FFFFFF", 
      color = NA),
    plot.margin = margin(
      t = 0.1, 
      r = 0.1, 
      b = 0.1, 
      l = 0.1,
      unit = "cm") 
  )

## EXPORT PLOTS

ggsave(
  filename = "./review/figures/output/transitions_review_ilp_meta_analysis_summary.png",
  plot = ilp_plots,
  height = 14,
  width = 20
  )

ggsave(
  filename = "./review/figures/output/transitions_review_coach_peer_support_meta_analysis_summary.png",
  plot = coach_peer_support_plots,
  height = 3.5,
  width = 20
  )

