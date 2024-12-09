#### Run from source #####
pacman::p_load(tidyverse, here, beepr)

here::i_am("scripts/02_run_from_source.R")

#build data objects
{
  source(here("scripts/mrp_scripts/02_mrp_clean_with_edu_build_direct_estimates_build_poststrat.R"))
}

beep(2)

#run analyses
{
  #run models
  source(here("scripts/mrp_scripts/03_run_models.R"))
  #build
  }

beep(2)

#build plots and tables
source(here("scripts/mrp_scripts/999a_establish_baseline_proportions_cadoh_data.R"))
source(here("scripts/mrp_scripts/999b_compare_models_including_bym2.R"))

beep(8)

