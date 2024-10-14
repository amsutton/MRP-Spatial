#### MRP with Spatial Priors using CTIS Data ####

#Script AREA MODELS: establish baseline(s): 
# compare CDC estimate to age, sex, area models

#Build a series of models representing vaccination probability (min. 1 dose)
#For June 30, 2021 using the COVID-19 Trends and Impacts Survey (CTIS)

#Post-stratification of age/sex/education at county level
#potential secondary post-stratification using urban/rural classification later

pacman::p_load(tidyverse, ggplot2, here, tidycensus, tigris, janitor, viridis,data.table)

here::i_am("scripts/999b_compare_models_including_bym2.R")

options(tigris_use_cache = TRUE)
options(scipen=999)
tidycensus::census_api_key(Sys.getenv("CENSUS_API_KEY"), overwrite = FALSE, install = FALSE)

# #whole time period for Ipsos wave 3 direct estimates (not bootstrapped) or bounded by specific dates?
# whole_time_period = TRUE

#### baseline for comparison: CA pop by sex and county (aged over 18 years) ####
#### source: ACS (used to produce poststratification table; built in script 02)
# load(file= here("data/cleaned_input_data/clean_acs_california_population_estimates.rda"))

capop <- get_acs(geography = "county",
                 state = "CA",
                 variables = c(
                   "18_plus_f" ="B05003_019",
                   "18_plus_m" = "B05003_008"
                 ),
                 survey = "acs5",
                 geometry = FALSE,
                 year = 2020)


#poststratification data for SAE estimates
load(file = here("data/cleaned_input_data/clean_postrat_age_sex_county_with_edu.rda"))
post = post %>%
  mutate(xfips = as.integer(fips)) %>%
  select(-fips)

postpoptotal = post %>% 
  group_by(xfips) %>%
  summarize(estimate = sum(estimate))
glimpse(capop)
capop = capop %>%
  clean_names %>%
  mutate(geoid = as.integer(geoid)) %>%
  group_by(geoid) %>%
  reframe(estimate = sum(estimate, na.rm=TRUE),
          moe = )

postpoptotal = left_join()

#should be zero:
#sum(capop$estimate) - sum(postpoptotal$estimate)

#### CDC STATE LEVEL estimates for CA by sex and age (3 categories match with CTIS data) ####
cdc = fread(here("data/cdc_data/COVID-19_Vaccination_Age_and_Sex_Trends_in_the_United_States__National_and_Jurisdictional.csv"))

cdc = cdc %>% 
  clean_names() %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  filter(location == "CA",
         date == "2021-06-30" | date == "2021-06-12",
         str_detect(demographic_category, "ale")) %>%
  mutate(sex = ifelse(str_detect(demographic_category, "Male"), 1, 0),
         age = case_when(str_detect(demographic_category, "18") ~ 1, #18-24 yrs
                         str_detect(demographic_category, "25-4") ~ 2, #25-49
                         str_detect(demographic_category, "50") ~ 2, #50-64, aggregate with above to make 25-64 to match ctis data
                         # str_detect(demographic_category, "65-74") ~ 3,
                         str_detect(demographic_category, "65+") ~ 3)) %>%
  select(date, sex, age, administered_dose1) %>%
  group_by(date, sex, age) %>%
  reframe(administered_dose1 = sum(administered_dose1)) %>%
  distinct() %>%
  na.omit() 

glimpse(cdc)
sum(cdc$administered_dose1)

#this is produced by rolling up the county-level pop estimates in this 
#case because of the bym2 on county
cdc = cdc %>%
  group_by(date,sex,age) %>%
  mutate(total_pop = sum(capop$estimate),
         estimate = administered_dose1/total_pop,
         model = "CDC official count") %>%
  ungroup () %>%
  mutate(popest = sum(administered_dose1))

cdc = write.csv(cdc, file = "Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/table4_cdc_baseline_data.csv")

#### load state of CA county vaccination data ####
c = read.csv(here("data/cdc_data/COVID-19_Vaccinations_in_the_United_States_County.csv"))
glimpse(c)
c = c %>%
  clean_names() %>%
  filter(recip_state == "CA",
         date == "06/30/2021" | date == "06/12/2021") %>%
  select(date, recip_county, fips, administered_dose1_recip_18plus,
         administered_dose1_recip_18plus_pop_pct, census2019_18plus_pop)

c_count = c %>%
  mutate(county = str_remove(recip_county," County")) %>%
  group_by(date, county) %>%
  summarize(county,
            total = sum(administered_dose1_recip_18plus))

county.index = c %>% 
  filter(date == "06/30/2021") %>% 
  select(fips,recip_county, census2019_18plus_pop) %>% 
  rename(county = recip_county)

county.index = county.index %>% filter(fips != "UNK")

write.csv(c_count, file = here("data/results/tables/table5_county_counts_cdc.csv"))

####model results: 3.1.2. binomial iid county rw1 age ####
iid.county.f = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_rw1_age_f_iidcounty.csv"))
iid.county.f = iid.county.f %>% 
  mutate(sex = 0,
         estimate = median_ps_prob,
         lw = lw_ps_prob,
         up = up_ps_prob) %>%
  select(xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)

iid.county.m = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_rw1_age_m_iidcounty.csv"))
iid.county.m = iid.county.m %>% 
  mutate(sex = 1,
         estimate = median_ps_prob,
         lw = lw_ps_prob,
         up = up_ps_prob) %>%
  select(xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)

iid.county = rbind(iid.county.f, iid.county.m)

iid.county = left_join(iid.county, post, by = c("xfips","sex","age"))

iid.county = iid.county %>%
  mutate(method = "county-level (IID)\nRW(1) by age",
         # sae_estimate = estimate.x*estimate.y, 
         # sae_lw.ci = lw*estimate.y, 
         # sae_up.ci = up*estimate.y,
         estimate = estimate.x
         ) %>%
  ungroup() %>%
  select(method, xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci) %>% 
  mutate(sae_estimate = ifelse(is.na(sae_estimate), 0, sae_estimate),
         sae_lw.ci = ifelse(is.na(sae_lw.ci), 0, sae_lw.ci),
         sae_up.ci = ifelse(is.na(sae_up.ci), 0, sae_up.ci))

####model results: fixed effect age, by sex, iid on county ####
fixedage.iid.county.f = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_rw1_fixedage_f_iidcounty.csv"))
fixedage.iid.county.f = fixedage.iid.county.f %>% 
  mutate(sex = 0,
         estimate = median_ps_prob,
         lw = lw_ps_prob,
         up = up_ps_prob) %>%
  select(xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)

fixedage.iid.county.m = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_rw1_fixedage_m_iidcounty.csv"))
fixedage.iid.county.m = fixedage.iid.county.m %>% 
  mutate(sex = 1,
         estimate = median_ps_prob,
         lw = lw_ps_prob,
         up = up_ps_prob) %>%
  select(xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)

fixedage.iid.county = rbind(fixedage.iid.county.f, fixedage.iid.county.m)

fixedage.iid.county = left_join(fixedage.iid.county, post, by = c("xfips","sex","age"))

fixedage.iid.county = fixedage.iid.county %>%
  mutate(method = "fixed effect age,\nIID on county",
         # sae_estimate = estimate.x*estimate.y, 
         # sae_lw.ci = lw*estimate.y, 
         # sae_up.ci = up*estimate.y,
         estimate = estimate.x) %>%
  ungroup() %>%
  select(method, xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci) %>% 
  mutate(sae_estimate = ifelse(is.na(sae_estimate), 0, sae_estimate),
         sae_lw.ci = ifelse(is.na(sae_lw.ci), 0, sae_lw.ci),
         sae_up.ci = ifelse(is.na(sae_up.ci), 0, sae_up.ci))

# ####model results: bym2 age, by sex, fixed effect education ####
# fixededu.bym2age.f = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_m_bym2age_fixededu.csv"))
# fixededu.bym2age.f = fixededu.bym2age.f %>% 
#   mutate(sex = 0,
#          estimate = median_ps_prob,
#          lw = lw_ps_prob,
#          up = up_ps_prob) %>%
#   select(xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)
# 
# fixededu.bym2age.m = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_m_bym2age_fixededu.csv"))
# fixededu.bym2age.m = fixededu.bym2age.m %>% 
#   mutate(sex = 1,
#          estimate = median_ps_prob,
#          lw = lw_ps_prob,
#          up = up_ps_prob) %>%
#   select(xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)
# 
# fixededu.bym2age = rbind(fixededu.bym2age.f, fixededu.bym2age.m)
# 
# fixededu.bym2age = fixededu.bym2age %>%
#   mutate(method = "fixed effect education,\nBYM2 by age") %>%
#   ungroup() %>%
#   select(method, xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)
# 

# #join the last three together
# 
# iid.county = rbind(iid.county, rw1.county, fixedage.iid.county,fixededu.bym2age)

####model results: 3.1.3. betabinomial bym2 county rw1 age ####
bym2.county.f = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_rw1_age_f_bym2_county.csv"))
bym2.county.f = bym2.county.f %>% 
  mutate(sex = 0,
         estimate = median_ps_prob,
         lw = lw_ps_prob,
         up = up_ps_prob) %>%
  select(xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)

bym2.county.m = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_rw1_age_m_bym2_county.csv"))
bym2.county.m = bym2.county.m %>% 
  mutate(sex = 1,
         estimate = median_ps_prob,
         lw = lw_ps_prob,
         up = up_ps_prob) %>%
  select(xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)

bym2.county = rbind(bym2.county.f, bym2.county.m)

bym2.county = left_join(bym2.county, post, by = c("xfips","sex","age"))

bym2.county = bym2.county %>%
  mutate(method = "BYM2 by county,\nRW(1) by age",
         # sae_estimate = estimate.x*estimate.y, 
         # sae_lw.ci = lw*estimate.y, 
         # sae_up.ci = up*estimate.y,
         estimate = estimate.x) %>%
  ungroup() %>%
  select(method, xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci) %>% 
  mutate(sae_estimate = ifelse(is.na(sae_estimate), 0, sae_estimate),
         sae_lw.ci = ifelse(is.na(sae_lw.ci), 0, sae_lw.ci),
         sae_up.ci = ifelse(is.na(sae_up.ci), 0, sae_up.ci))


####model results: 3.1.4. betabinomial bym2 county grouped: iid age####
bym2.age.f = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_f_bym2county_by_iidage.csv"))
bym2.age.f  = bym2.age.f  %>% 
  mutate(sex = 0,
         estimate = median_ps_prob,
         lw = lw_ps_prob,
         up = up_ps_prob) %>%
  select(xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)

bym2.age.m = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_m_bym2county_by_iidage.csv"))
bym2.age.m = bym2.age.m %>% 
  mutate(sex = 1,
         estimate = median_ps_prob,
         lw = lw_ps_prob,
         up = up_ps_prob) %>%
  select(xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)

bym2.age = rbind(bym2.age.f, bym2.age.m)

bym2.age = left_join(bym2.age, post, by = c("xfips","sex","age"))

bym2.age = bym2.age %>%
  mutate(method = "BYM2 by age",
         # sae_estimate = estimate.x*estimate.y, 
         # sae_lw.ci = lw*estimate.y, 
         # sae_up.ci = up*estimate.y,
         estimate = estimate.x) %>%
  ungroup() %>%
  select(method, xfips, sex, age, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci) %>% 
  mutate(sae_estimate = ifelse(is.na(sae_estimate), 0, sae_estimate),
         sae_lw.ci = ifelse(is.na(sae_lw.ci), 0, sae_lw.ci),
         sae_up.ci = ifelse(is.na(sae_up.ci), 0, sae_up.ci))


####model results: 3.1.5. betabinomial bym2county group: iid age, plus rw2 education ####
bym2age.rw2edu.f = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_f_bym2county_by_iidage_rw2edu.csv"))
bym2age.rw2edu.f  = bym2age.rw2edu.f  %>% 
  mutate(sex = 0,
         estimate = median_ps_prob,
         lw = lw_ps_prob,
         up = up_ps_prob) %>%
  select(xfips, sex, age, edu, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)

bym2age.rw2edu.m = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_m_bym2county_by_iidage_rw2edu.csv"))
bym2age.rw2edu.m = bym2age.rw2edu.m %>% 
  mutate(sex = 1,
         estimate = median_ps_prob,
         lw = lw_ps_prob,
         up = up_ps_prob) %>%
  select(xfips, sex, age, edu, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)

bym2age.rw2edu = rbind(bym2age.rw2edu.f, bym2age.rw2edu.m)

bym2age.rw2edu = left_join(bym2age.rw2edu, post, by = c("xfips","sex","age", "edu"))

bym2age.rw2edu = bym2age.rw2edu %>%
  mutate(method = "BYM2 by age,\nRW(2) by education",
         # sae_estimate = estimate.x*estimate.y, 
         # sae_lw.ci = lw*estimate.y, 
         # sae_up.ci = up*estimate.y,
         estimate = estimate.x) %>%
  ungroup() %>%
  select(method, xfips, sex, age, edu, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci) %>% 
  mutate(sae_estimate = ifelse(is.na(sae_estimate), 0, sae_estimate),
         sae_lw.ci = ifelse(is.na(sae_lw.ci), 0, sae_lw.ci),
         sae_up.ci = ifelse(is.na(sae_up.ci), 0, sae_up.ci))

# ####model results: rw1 on education, bym2 age ####
# bym2age.rw1edu.f = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_f_bym2age_rw1edu.csv"))
# bym2age.rw1edu.f  = bym2age.rw1edu.f  %>% 
#   mutate(sex = 0,
#          estimate = median_ps_prob,
#          lw = lw_ps_prob,
#          up = up_ps_prob) %>%
#   select(xfips, sex, age, edu, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)
# 
# bym2age.rw1edu.m = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_m_bym2age_rw1edu.csv"))
# bym2age.rw1edu.m = bym2age.rw1edu.m %>% 
#   mutate(sex = 1,
#          estimate = median_ps_prob,
#          lw = lw_ps_prob,
#          up = up_ps_prob) %>%
#   select(xfips, sex, age, edu, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci)
# 
# bym2age.rw1edu = rbind(bym2age.rw1edu.f, bym2age.rw1edu.m)
# 
# bym2age.rw1edu = bym2age.rw1edu %>%
#   mutate(method = "BYM2 by age,\nRW(1) on education") %>%
#   ungroup() %>%
#   select(method, xfips, sex, age, edu, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci) 
# 
# bym2age.rw2edu = rbind(bym2age.rw2edu,bym2age.rw1edu)

bym2age.rw2edu.noeduvar = bym2age.rw2edu %>% select(-edu)

iid.county = rbind(iid.county,fixedage.iid.county,bym2age.rw2edu.noeduvar,bym2.age,bym2.county)
iid.county = distinct(iid.county)
# iid.county = rbind(iid.county, bym2age.rw2edu.noeduvar)
# NO LONGER DOING FIXED EFFECTS/MIXED EFFECTS MODELS, BUT KEEPING IN CASE:
# ####model results: fixed effects age and edu, bym2 on county ####
# bym2.fixedeffectsageedu.f = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_f_bym2_fixedeffects_age_edu.csv"))
# bym2.fixedeffectsageedu.f  = bym2.fixedeffectsageedu.f  %>% 
#   mutate(sex = 0,
#          estimate = median_ps_prob,
#          lw = lw_ps_prob,
#          up = up_ps_prob) %>%
#   select(xfips, sex, age, edu, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci) %>%
#   distinct()
# 
# bym2.fixedeffectsageedu.m = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_m_bym2_fixedeffects_age_edu.csv"))
# bym2.fixedeffectsageedu.m = bym2.fixedeffectsageedu.m %>% 
#   mutate(sex = 1,
#          estimate = median_ps_prob,
#          lw = lw_ps_prob,
#          up = up_ps_prob) %>%
#   select(xfips, sex, age, edu, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci) %>%
#   distinct()
# head(bym2.fixedeffectsageedu.m)
# 
# bym2.fixedeffectsageedu = rbind(bym2.fixedeffectsageedu.f, bym2.fixedeffectsageedu.m)
# 
# bym2.fixedeffectsageedu = bym2.fixedeffectsageedu %>%
#   mutate(method = "Model 5:\nBYM2 on county,\nfixed effects:\nage and education") %>%
#   ungroup() %>%
#   select(method, xfips, sex, age, edu, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci) 
# 
# ####model results: fixed effects age and edu, bym2 on county ####
# iid.fixedeffectsageedu.f = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_f_iid_fixedeffects_age_edu.csv"))
# iid.fixedeffectsageedu.f  = iid.fixedeffectsageedu.f  %>% 
#   mutate(sex = 0,
#          estimate = median_ps_prob,
#          lw = lw_ps_prob,
#          up = up_ps_prob) %>%
#   select(xfips, sex, age, edu, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci) %>%
#   distinct()
# 
# iid.fixedeffectsageedu.m = read.csv(here("Aja/COVID_Behaviors/data/smrp/indirect_estimates/mrp_estimates_vax_m_iid_fixedeffects_age_edu.csv"))
# iid.fixedeffectsageedu.m = iid.fixedeffectsageedu.m %>% 
#   mutate(sex = 1,
#          estimate = median_ps_prob,
#          lw = lw_ps_prob,
#          up = up_ps_prob) %>%
#   select(xfips, sex, age, edu, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci) %>%
#   distinct()
# head(iid.fixedeffectsageedu.m)
# 
# iid.fixedeffectsageedu = rbind(iid.fixedeffectsageedu.f, iid.fixedeffectsageedu.m)
# 
# iid.fixedeffectsageedu = iid.fixedeffectsageedu %>%
#   mutate(method = "Model 6:\nIID on county,\nfixed effects:\nage and education") %>%
#   ungroup() %>%
#   select(method, xfips, sex, age, edu, estimate, lw, up, sae_estimate, sae_lw.ci, sae_up.ci) 
# 

#baseline estimates from cdc for viz comparison
add_lines = cdc %>% 
  dplyr::select(date, sex, age, estimate) %>% 
  mutate(age = factor(case_when(age == 1 ~ "18-24",
                                  age == 2 ~ "25-64",
                                  age == 3 ~ "65+")),
        sex= factor(ifelse(sex == 0, "female", "male"))) %>%
  as.data.frame()


#visualize results
bym2.county = distinct(bym2.county)
bym2.age = distinct(bym2.age)
temp1 = rbind(bym2.county, bym2.age)

temp1 = temp1 %>% mutate(xfips = as.character(paste0("0",xfips)))


simple.bym2.popests = temp1 %>% 
  group_by(method,sex,age) %>%
  summarize(method, sex, age,
            popest = sum(sae_estimate),
            popest.lw = sum(sae_lw.ci),
            popest.up = sum(sae_up.ci)) %>%
  distinct()

# plot=temp1 %>%
#   mutate(lw = ifelse(is.na(lw), estimate, lw),
#          up = ifelse(is.na(up), estimate, up),
#          age = factor(case_when(age == 1 ~ "18-24",
#                                 age == 2 ~ "25-64",
#                                 age == 3 ~ "65+")),
#          sex= factor(ifelse(sex == 0, "female", "male")),
#          xfips = factor(xfips)) %>%
#   ggplot(aes(x=xfips, y=estimate, color=age)) +
#   # geom_ribbon(aes(ymin = lw, ymax = up, group=interaction(method,sex,xfips)), color="grey", alpha = 0.2) +
#   # geom_line(aes(linetype=xfips)) +
#   facet_wrap(~method+sex, ncol=2) +
#   geom_point() +
#   theme_minimal() + 
#   geom_hline(data = add_lines %>% filter(date == "2021-06-30"),
#             aes(yintercept = estimate, color = age), 
#             linewidth = 1) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   labs(title = "Estimated proportion of the California population with one COVID-19 vaccination dose",
#        subtitle = "Comparison of modeled estimates as of June 30, 2021 by county, sex and age",
#        caption="The solid lines represent the CDC state estimate for each sex/age stratum",
#        y = "estimated proportion",
#        x = "county (FIPS code)") +
#   guides(color = element_text("age (years)"))
# plot
# ggsave(plot, file = here("Aja/COVID_Behaviors/plots/diss_chapter_mrp_plots/ca_county_comparison_bym2.tiff"),dpi = 320, bg = "white", width = 13, height = 5, unit = "in")


#visualize results

temp2 = bym2age.rw2edu %>% mutate(xfips = as.character(paste0("0",xfips)))

ageedu.bym2.popests = temp2 %>%
  group_by(method,sex,age) %>%
  summarize(method,sex, age,
            popest = sum(sae_estimate),
            popest.lw = sum(sae_lw.ci),
            popest.up = sum(sae_up.ci)) %>%
  distinct()

# plot = temp2 %>%
#   mutate(lw = ifelse(is.na(lw), estimate, lw),
#          up = ifelse(is.na(up), estimate, up),
#          age = factor(case_when(age == 1 ~ "18-24",
#                                 age == 2 ~ "25-64",
#                                 age == 3 ~ "65+")),
#          sex= factor(ifelse(sex == 0, "female", "male")),
#          xfips = factor(xfips),
#          edu = factor(case_when(edu == 1 ~ "less than high school",
#                                 edu == 2 ~ "high school or equivalent",
#                                 edu == 3 ~ "some college",
#                                 edu == 4 ~ "associate's degree",
#                                 edu == 5 ~ "bachelor's degree",
#                                 edu == 6 ~ "professional or graduate degree"),
#                       levels = c("less than high school",
#                                   "high school or equivalent",
#                                   "some college",
#                                   "associate's degree",
#                                   "bachelor's degree",
#                                   "professional or graduate degree"))) %>%
#   ggplot(aes(x=xfips, y=estimate, color=age,shape =sex)) +
#   # geom_ribbon(aes(ymin = lw, ymax = up, group=interaction(method,sex,xfips)), color="grey", alpha = 0.2) +
#   # geom_line(aes(linetype=xfips)) +
#   facet_wrap(~sex+method+edu, nrow=2) +
#   geom_point() +
#   theme_minimal() + 
#   geom_hline(data = add_lines %>% filter(date == "2021-06-30"),
#              aes(yintercept = estimate, color = age,linetype = factor(sex)), 
#              linewidth = 0.5) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   labs(caption="The solid lines represent the CDC state estimate for each sex/age category, irrespective of educational attainment")
# plot
# ggsave(plot, file = here("Aja/COVID_Behaviors/plots/diss_chapter_mrp_plots/ca_county_comparison_bym2_age_edu.tiff"),dpi = 320, bg = "white", width = 20, height = 8, unit = "in")


####state roll up####
# 
# popests = rbind(simple.bym2.popests, ageedu.bym2.popests)
# 
# popests = popests %>% ungroup()

iid.county.popests = iid.county %>%
  group_by(method, sex, age) %>%
  summarize(method,
            sex,
            age,
            popest = sum(sae_estimate),
            popest.lw = sum(sae_lw.ci),
            popest.up = sum(sae_up.ci)) %>% 
  distinct()

popests = iid.county.popests

add_lines2 = cdc %>% 
  group_by(date, sex, age) %>%
  summarize(date,sex,age,estimate = sum(administered_dose1)) %>%
  as.data.frame()

cdc2 = cdc %>% 
  group_by(sex, age) %>%
  filter(date == "2021-06-30") %>%
  summarize(sex,age,
            popest = sum(administered_dose1),
            popest.lw = popest,
            popest.up = popest,
            method = "CDC estimate") %>%
  as.data.frame()

# 
#   ggplot(aes(x=factor(model), y=popest, color=factor(age))) +
#   geom_pointrange(aes(ymin = lw, ymax = up)) +
#   facet_grid(~sex) +
#   geom_point() +
#     geom_hline(data = add_lines2,
#                aes(yintercept = estimate, color = factor(age)), 
#                linewidth = 0.5) +
#   theme_minimal()

add_lines2 = add_lines2 %>%
  group_by(date) %>%
  summarize(estimate=sum(estimate),
            prop_est = estimate/sum(capop$estimate),
            model="CDC estimate") %>%
  ungroup()

popests = rbind(popests, cdc2)
popests
popests = popests %>%
 group_by(method) %>%
  mutate(model = method) %>% 
  summarize(model, 
            popest = sum(popest),
            lw = sum(popest.lw),
            up = sum(popest.up)) %>%
  filter(model !="CDC estimate")

popests = distinct(popests)
table(popests$method)

glimpse(popests)

plot = popests %>%
  ggplot(aes(x=factor(model), y=popest, color=factor(model))) +
  geom_pointrange(aes(ymin = lw, ymax = up)) +
  geom_hline(data = add_lines2 %>% filter(date == "2021-06-30"),
             aes(yintercept = estimate, linetype = factor(date)),
             color = "blue", 
             linewidth = 0.5,) +
  # scale_linetype_manual(name ="",values = 'solid') +
  theme_minimal() +
  labs(x = "estimation method",
       y="population estimate (count)",
       # title = "The number of people in California who have received at least one\nCOVID-19 vaccination",
       # subtitle = "Comparison of modeled county-level estimates summed to the state level as of June 30, 2021",
       linetype = "CDC estimate") +
  guides(color = "none") + 
  scale_color_viridis(discrete = TRUE) 
plot
ggsave(plot, file = here("Aja/COVID_Behaviors/plots/diss_chapter_mrp_plots/ca_state_level_roll_up_all_models_counts.tiff"),dpi = 320, bg = "white", width = 7.5, height = 3, unit = "in")


plot = popests %>%
  ggplot(aes(x=factor(model), y=popest, color=factor(model))) +
  geom_pointrange(aes(ymin = lw, ymax = up)) +
  geom_hline(data = add_lines2,
             aes(yintercept = estimate, linetype = factor(date)),
             color = "blue", 
             linewidth = 0.5,) +
  # scale_linetype_manual(name ="",values = 'solid') +
  theme_minimal() +
  labs(x = "estimation method",
       y="population estimate (count)",
       # title = "The number of people in California who have received at least one\nCOVID-19 vaccination",
       # subtitle = "Comparison of modeled county-level estimates summed to the state level as of June 30, 2021",
       linetype = "CDC estimate") +
  guides(color = "none") + 
  scale_color_viridis(discrete = TRUE)
plot
ggsave(plot, file = here("Aja/COVID_Behaviors/plots/diss_chapter_mrp_plots/ca_state_level_roll_up_all_models_counts_june30vs12.tiff"),dpi = 320, bg = "white", width = 7.5, height = 3, unit = "in")


#proportion of popests as above

popests = popests %>%
    mutate(prop_vaccinated = popest/sum(capop$estimate),
           prop_vaccinated_lw = lw/sum(capop$estimate),
           prop_vaccinated_up = up/sum(capop$estimate))
  

#table for popests
write.csv(popests, file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/table7_compare_area_models_with_CDC_estimate.csv"))

plot = popests %>%
  ggplot(aes(x=factor(model), y=prop_vaccinated, color=factor(model))) +
  geom_pointrange(aes(ymin = prop_vaccinated_lw, ymax = prop_vaccinated_up)) +
  geom_hline(data = add_lines2 %>% filter(date == "2021-06-30"),
             aes(yintercept = prop_est, linetype = factor(model)),
             color = "blue", 
             linewidth = 0.5,) +
  scale_linetype_manual(name ="",values = 'solid') +
  theme_minimal() +
  labs(x = "estimation method",
       y="population estimate (proportion)",
       # title = "The proportion of people in California who have received at least one\nCOVID-19 vaccination",
       # subtitle = "Comparison of modeled county-level estimates summed to the state level as of June 30, 2021"
       ) +
  guides(color = "none") + 
  scale_color_viridis(discrete = TRUE)
plot
ggsave(plot, file = here("Aja/COVID_Behaviors/plots/diss_chapter_mrp_plots/ca_state_level_roll_up_all_models_proportions.tiff"),dpi = 320, bg = "white", width = 7.5, height = 3, unit = "in")


#### plot: compare county totals with CA CDC county counts ####
# glimpse(temp1)
# test = temp1 %>%
#   mutate(xfips = as.integer(xfips)) %>%
#   filter(xfips == 6005)
# glimpse(post)
# testpost = post %>% 
#   ungroup() %>%
#   filter(xfips == 6005) %>% select(age) %>% unlist() %>% table(., useNA = "ifany")
#   group_by(xfips, sex, age) %>% 
#   summarize(popest = sum(estimate)) %>% 
#   ungroup()
# 
# test = left_join(test, testpost, by = c("xfips","age","sex"))
# 
# test = test %>%
#   mutate(saetest = estimate*popest,
#          proptest = sae_estimate/popest)
# sum(test$saetest)
# glimpse(temp1)
# glimpse(bym2.sexage.plotdat)

iid.county2 = iid.county %>%
  group_by(method,xfips, sex, age) %>%
  summarize(method,
            xfips,
            sex,
            age,
            popest = sum(sae_estimate),
            popest.lw = sum(sae_lw.ci),
            popest.up = sum(sae_up.ci)) %>% 
  distinct()

# bym2.sexage.plotdat <- aggregate(cbind(sae_estimate,
#                                            sae_lw.ci,
#                                            sae_up.ci) ~ method + xfips,
#                                      data = temp1,
#                                      FUN = sum)
# 
# bym2.sexage.plotdat = temp1 %>%
#   mutate(xfips = as.factor(xfips),
#          method = as.factor(method)) %>%
#   group_by(xfips, method) %>%
#   dplyr::summarise(method,
#             estimate = sum(sae_estimate),
#             lw = sum(sae_lw.ci),
#             up = sum(sae_up.ci))
# 
# bym2.sexageedu.plotdat = temp2 %>%
#   mutate(xfips = as.factor(xfips),
#          method = as.factor(method)) %>%
#   group_by(xfips, method) %>%
#   dplyr::summarise(method,
#                    estimate = sum(sae_estimate),
#             lw = sum(sae_lw.ci),
#             up = sum(sae_up.ci))
# 
# iid.county2 = rbind(iid.county,fixedage.iid.county,bym2age.rw2edu.noeduvar,bym2.age,bym2.county)
# table(iid.county2$method)
# view(iid.county2)
# iid.county2  = iid.county2 %>% 
#   group_by(method,xfips) %>%
#   summarize(xfips,
#             method,
#             estimate = sum(sae_estimate),
#             lw = sum(sae_lw.ci),
#             up = sum(sae_up.ci))
# iid.county2 = distinct(iid.county2)

# rollup.plotdat = rbind(bym2.sexage.plotdat, bym2.sexageedu.plotdat)
rollup.plotdat = iid.county2 %>% 
  dplyr::ungroup() %>%
  rename(estimate = popest, 
         lw = popest.lw,
         up = popest.up) %>%
  select(method, xfips, estimate, lw, up) 

rollup.plotdat$xfips = as.integer(rollup.plotdat$xfips)

ctemp = c %>% 
    filter(date == "06/30/2021") %>%
    mutate(method = "CDC county estimate",
           xfips = fips,
           estimate = administered_dose1_recip_18plus,
           lw = estimate,
           up = estimate) 

ctemp = ctemp %>% filter(xfips != "UNK")

ctemp$xfips = as.integer(ctemp$xfips)
ctemp = ctemp %>% select(colnames(rollup.plotdat))

rollup.plotdat = rbind(rollup.plotdat,ctemp)
rollup.plotdat = na.omit(rollup.plotdat)

rollup.plotdat = rollup.plotdat %>% rename(model = method)

county.index = county.index %>% 
  select(fips,county) %>% 
  mutate(xfips = as.integer(fips))


rollup.plotdat = left_join(rollup.plotdat, county.index, by = "xfips")

rollup.plotdat$county = str_remove(rollup.plotdat$county, " County")
rollup.plotdat = rollup.plotdat %>% filter(xfips != "UNK")

rollup.plotdat2 = rollup.plotdat %>%
  group_by(xfips, model) %>%
  summarize(county,
            model,
            estimate = sum(estimate),
            lw = sum(lw),
            up = sum(up))

rollup.plotdat2 = rollup.plotdat2 %>% 
  ungroup() %>%
  mutate(model = str_replace(model, "\n", " ")) %>%
  select(-xfips)
rollup.plotdat2 = distinct(rollup.plotdat2)

#county level counts: sae estimates
write.csv(rollup.plotdat2, file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/rollup_state_level_compare_area_models_with_CDC_estimate_counts.csv"))

plot = rollup.plotdat %>%
  ggplot(aes(x=factor(county), y=estimate, color=factor(model))) +
  geom_pointrange(aes(ymin = lw, ymax = up),position="jitter") +
  # geom_hline(data = add_lines2,
  #            aes(yintercept = estimate, color = factor(model)), 
  #            linewidth = 0.5) +
  theme_minimal() +
  labs(x = "county",
       y="population estimate (count)",
       # title = "The number of people in California who have received at least one COVID-19 vaccination",
       color = "estimation method") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.spacing = unit(1, 'cm')) + 
  scale_color_viridis(discrete = TRUE)
plot
ggsave(plot, file = here("Aja/COVID_Behaviors/plots/diss_chapter_mrp_plots/ca_county_level_roll_up_all_models_counts.tiff"),dpi = 320, bg = "white", width =12, height = 4, unit = "in")

#save table
capop$xfips = as.character(capop$GEOID)
capop = capop %>%
  group_by(xfips) %>%
  summarize(xfips,
            popest = sum(estimate)) %>%
  ungroup()

capop = distinct(capop)

rollup.plotdat = left_join(rollup.plotdat, capop, by = c("fips"="xfips"))

rollup.plotdat = rollup.plotdat %>%
  group_by(model, xfips) %>%
  mutate(model,
         xfips,
         estimate = sum(estimate),
         prop.estimate = estimate/popest,
         prop.lw = sum(lw)/popest,
         prop.up = sum(up)/popest) %>%
  ungroup()

# rollup.plotdat = rollup.plotdat %>%
#   group_by(model, xfips) %>%
#   summarize(model,
#             xfips,
#             estimate = sum(estimate))
rollup.plotdat = rollup.plotdat %>%
  select(model, xfips, county, prop.estimate, prop.lw, prop.up) %>%
  distinct()


write.csv(rollup.plotdat, file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/rollup_state_level_compare_area_models_with_CDC_estimate_proportions.csv"))
glimpse(rollup.plotdat)
###proportions rather than counts: compare county totals with CA state issued counts ####

plot = rollup.plotdat %>%
  ggplot(aes(x=factor(county), y=prop.estimate, color=factor(model))) +
  geom_pointrange(aes(ymin = prop.lw, ymax = prop.up), position = "jitter") +
  # geom_hline(data = add_lines2,
  #            aes(yintercept = estimate, color = factor(model)), 
  #            linewidth = 0.5) +
  theme_minimal() +
  labs(x = "county",
       y="population estimate (proportion)",
       # title = "The number of people in California who have received at least one COVID-19 vaccination",
       color = "estimation method") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_color_viridis(discrete = TRUE)
plot
ggsave(plot, file = here("Aja/COVID_Behaviors/plots/diss_chapter_mrp_plots/ca_county_level_roll_up_all_models_proportions.tiff"),dpi = 320, bg = "white", width = 13, height =6.5, unit = "in")

#build Table 9
rollup.plotdat
dat = read.csv(file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/rollup_state_level_compare_area_models_with_CDC_estimate_counts.csv"))
dat2 = read.csv(file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/rollup_state_level_compare_area_models_with_CDC_estimate_proportions.csv"))

#clean them up
dat = dat %>%
  select(county, model, estimate, lw, up) %>%
  arrange(county, model) %>%
  mutate(model = str_replace(model, "/n"," "))
glimpse(dat2)
dat2 = dat2 %>%
  select(county, model, prop.estimate, prop.lw, prop.up) %>%
  mutate(model = str_replace(model, "\n"," ")) %>%
  arrange(county, model) 
glimpse(dat)
glimpse(dat2)

test = full_join(dat2, dat, by = c("county", "model"))

test = test %>%
  mutate(prop.estimate = round(prop.estimate, digits =3),
         prop.lw = round(prop.lw, digits =3),
         prop.up = round(prop.up, digits =3))

write.csv(test, file = here("Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/table9_compare_area_models_proportions.csv") )

#save table
#difference in the proportion of the entire population between cdc estimate and sae 
# commented out because wasn't necessary for dissertation; next plot might be helpful
# for journal article, though 
# 
# temp = rollup.plotdat
# temp = left_join(temp, county.index, by = c("xfips"="xfips"))
# temp
# temp = temp %>%
#   select(model, county.x, estimate,census2019_18plus_pop) %>%
#   rename(county = county.x) %>%
#   mutate(prop = estimate/census2019_18plus_pop)
# 
# temp2 = temp %>% filter(str_detect(model, "CDC"))
# temp2 = temp2 %>% select(county, prop) %>% rename(cdc.estimate.prop = prop)
# 
# temp = left_join(temp, temp2, by = "county")
# temp = temp %>% mutate(diff_prop =  cdc.estimate.prop - prop)
# temp
# med = temp %>% group_by(model) %>% mutate(median=median(diff_prop),
#                                    mean = mean(diff_prop)) %>% 
#   pivot_longer(cols=c("mean","median"), names_to = "est", values_to = "value")
# 
# med = med %>% select(model, est, value) %>% distinct
# 
# temp = temp %>%
#   group_by(model) %>%
#   mutate(median_diff = median(diff_prop),
#          mean_diff = mean(diff_prop)) %>%
#   select(model, mean_diff, median_diff,diff_prop)
# 
# temp = temp %>% filter(model != "CDC county estimate")
# temp
# 
# plot = temp %>%  
# ggplot(aes(x = model,y=diff_prop,color=factor(model))) +
#   geom_point(position="jitter") +
#   geom_hline(data = med,
#             aes(yintercept =value, color = factor(model),linetype = factor(est)), 
#              linewidth = 0.5)  +
#   theme_minimal() +
#   labs(x = "model",
#        y="difference in proportion\nfrom CDC baseline proportion",
#        linetype = "central measure") +
#   guides(color="none")  + 
#   scale_color_viridis(discrete = TRUE)
# plot
# ggsave(plot, file = here("Aja/COVID_Behaviors/plots/diss_chapter_mrp_plots/diff_proportion_from_baseline_models.jpeg"),dpi = 320, bg = "white", width = 8, height = 3, unit = "in")
# 
# 
# #### compare preferred methods: iid vs bym2 on area ####
# 
# plot = rollup.plotdat %>%
#   distinct() %>%
#   # filter(method == "bym2:county,\nfixed effects:\nage + education" |
#   #        method == "iid:county,\nfixed effects:\nage + education" |
#   #          method == "CDC county estimate") %>%
#   group_by(county) %>%
#   ggplot(aes(x=county, y=estimate, color=model)) +
#   geom_pointrange(aes(ymin = lw, ymax = up), position="jitter") +
#   # geom_hline(data = add_lines2,
#   #            aes(yintercept = estimate, color = factor(model)), 
#   #            linewidth = 0.5) +
#   theme_minimal() +
#   labs(x = "county",
#        y="population estimate (proportion)",
#        title = "The number of people in California who have received at least one COVID-19 vaccination",
#        subtitle = "Comparison of modeled county estimates as of June 30, 2021",
#        color = "estimation method") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         legend.spacing = unit(1, 'cm'),
#         legend.text = element_text(margin = margin(t = 10)))+ 
#   scale_color_viridis(discrete = TRUE)
# 
# ggsave(plot, file = here("Aja/COVID_Behaviors/plots/diss_chapter_mrp_plots/ca_county_level_roll_up_all_models_proportions.tiff"),dpi = 320, bg = "white", width = 10, height = 5, unit = "in")
# 



