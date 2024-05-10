#### Run from source #####
pacman::p_load(tidyverse, here, beepr)

here::i_am("scripts/02_run_from_source_no_data_building.R")

#build data objects
{
  #not sure I need the top one?
  #source(here("Aja/COVID_Behaviors/code/dissertation_code/mrp/01_mrp_clean_build_direct_estimates_build_poststrat.R"))
  source(here("Aja/COVID_Behaviors/code/dissertation_code/mrp/01_mrp_clean_with_edu_build_direct_estimates_build_poststrat.R"))
}

beep(2)

#run analyses
{
  #run models
  source(here("scripts/03_run_models.R"))
  #build
  }

beep(2)

#build plots and tables
source(here("Aja/COVID_Behaviors/code/dissertation_code/mrp/999_establish_baseline_proportions_cadoh_data.R"))
source(here("Aja/COVID_Behaviors/code/dissertation_code/mrp/999b_compare_models_including_bym2.R"))

beep(8)

