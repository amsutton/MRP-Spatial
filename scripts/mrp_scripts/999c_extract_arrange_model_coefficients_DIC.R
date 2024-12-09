### MRP with Spatial Priors using CTIS Data ####
###  extract coefficients and DIC from models ####
pacman::p_load(tidyverse, here, janitor)

here::i_am("scripts/mrp_scripts/999c_extract_arrange_model_coefficients_DIC.R")

#3.1.1. state level binomial rw1 by age
m1a = read.csv(file = here("data/mrp/ch2_diss_tables/model_results_tables/model_results_parametersrw1_age_m.csv"))
m1b = read.csv(file = here("data/mrp/ch2_diss_tables/model_results_tables/model_results_parametersrw1_age_f.csv"))

m1a$sex = "male"
m1b$sex = "female"
m1 = rbind(m1a, m1b)
m1$model = "state-level\nRW(1) by age"
rm(m1a,m1b)

#3.1.2. binomial iid county rw1 age
m2a = read.csv(file = here("data/mrp/ch2_diss_tables/model_results_tables/model_results_parametersrw1_age_m_iidcounty.csv"))
m2b = read.csv(file = here("data/mrp/ch2_diss_tables/model_results_tables/model_results_parametersrw1_age_f_iidcounty.csv"))

m2a$sex = "male"
m2b$sex = "female"
m2 = rbind(m2a, m2b)
m2$model = "county-level (IID)\nRW(1) by age"
rm(m2a,m2b)

#3.1.3. betabinomial bym2 county rw1 age
m3a = read.csv(file = here("mrp/ch2_diss_tables/model_results_tables/model_results_parametersrw1_age_m_bym2_county.csv"))
m3b = read.csv(file = here("mrp/ch2_diss_tables/model_results_tables/model_results_parametersrw1_age_f_bym2_county.csv"))

m3a$sex = "male"
m3b$sex = "female"
m3 = rbind(m3a, m3b)
m3$model = "BYM2 by county, RW(1) by age"
rm(m3a,m3b)

#3.1.4. betabinomial bym2 county grouped: iid age
m4a = read.csv(file = here("mrp/ch2_diss_tables/model_results_tables/model_results_parametersm_bym2county_by_iidage.csv"))
m4b = read.csv(file = here("mrp/ch2_diss_tables/model_results_tables/model_results_parametersf_bym2county_by_iidage.csv"))

m4a$sex = "male"
m4b$sex = "female"
m4 = rbind(m4a, m4b)
m4$model = "BYM2 by age"
rm(m4a,m4b)

#3.1.5. betabinomial bym2county group: iid age, plus rw2 education
m5a = read.csv(file = here("mrp/ch2_diss_tables/model_results_tables/model_results_parametersm_bym2county_by_iidage_rw2edu.csv"))
m5b = read.csv(file = here("mrp/ch2_diss_tables/model_results_tables/model_results_parametersf_bym2county_by_iidage_rw2edu.csv"))

m5a$sex = "male"
m5b$sex = "female"
m5 = rbind(m5a, m5b)
m5$model = "BYM2 by age, RW(2) by education"
rm(m5a,m5b)

m = rbind(m1,m2,m3,m4,m5)

m = m %>%
  select(model, sex, dic, variable, id, mean, sd, X0.025quant, X0.5quant, X0.975quant)

age2xfips = as.character(c(1:58))
age3xfips = as.character(c(59:116))

m = m %>%
  mutate(new_id = case_when(id == "intercept" ~ id,
                              model == "RW(1) by age" & id != "intercept" ~ paste("age",id, sep= " "),
                              model == "RW(1) by age, IID by county" ~ id,
                              model == "BYM2 by age" & id %in% age2xfips ~ paste("age 2 xfips",id, sep= " "),
                              model == "BYM2 by age" & id %in% age3xfips ~ paste("age 3 xfips",id, sep= " "),
                              model == "RW(1) by age, BYM2 by county" & id != "intercept" &  id %in% age2xfips ~ paste("age 2",id, sep= " "),
                              model == "RW(1) by age, BYM2 by county" & id %in% age3xfips ~ paste("age 3",id, sep= " "),
                              model == "BYM2 by age, RW(2) by education" &  id %in% age2xfips ~ paste("age 2",id, sep= " "),
                              model == "BYM2 by age, RW(2) by education" & id %in% age3xfips ~ paste("age 3",id, sep= " ")))

m[,6:10] = round(m[,6:10], digits = 3) 

m$id = m$new_id
m = m %>% select(-new_id, model, sex, variable, id, X0.025quant, X0.5quant, X0.975quant)
write.csv(m, file = here("mrp/ch2_diss_tables/model_results_tables/model_results_coefficients.csv"))


### extract DIC ####
dic = m %>%
  select(model, sex, dic) %>%
  distinct() %>%
  group_by(sex) %>%
  arrange(desc(dic))
dic

write.csv(dic, file = here("mrp/ch2_diss_tables/model_results_tables/model_results_dic_comparison.csv"))

          