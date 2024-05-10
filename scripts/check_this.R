
here::i_am("Aja/COVID_Behaviors/code/dissertation_code/mrp/999c_extract_arrange_model_coefficients_DIC.R")

#3.1.2. binomial iid county rw1 age
m2a = read.csv(file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/model_results_tables/model_results_parametersrw1_age_m_iidcounty.csv"))
m2b = read.csv(file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/model_results_tables/model_results_parametersrw1_age_f_iidcounty.csv"))

#3.1.3. betabinomial bym2 county rw1 age
m3a = read.csv(file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/model_results_tables/model_results_parametersrw1_age_m_bym2_county.csv"))
m3b = read.csv(file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/model_results_tables/model_results_parametersrw1_age_f_bym2_county.csv"))


#3.1.4. betabinomial bym2 county grouped: iid age
m4a = read.csv(file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/model_results_tables/model_results_parametersm_bym2county_by_iidage.csv"))
m4b = read.csv(file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/model_results_tables/model_results_parametersf_bym2county_by_iidage.csv"))

#betabinomial bym2county group: iid age, plus rw2 education
m5a = read.csv(file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/model_results_tables/model_results_parametersm_bym2county_by_iidage_rw2edu.csv"))
m5b = read.csv(file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/model_results_tables/model_results_parametersf_bym2county_by_iidage_rw2edu.csv"))


res = rbind(m2a, m2b, m3a, m3b, m4a, m4b, m5a, m5b)
glimpse(res)

