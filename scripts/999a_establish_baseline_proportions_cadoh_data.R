#### MRP with Spatial Priors using CTIS Data ####

#Script 999: establish baseline(s): 
#1.by sex and age for vaccination (at least one dose) in the entire state of California
#2. TBD

#Build a series of models representing vaccination probability (min. 1 dose)
#For June 30, 2021 using the COVID-19 Trends and Impacts Survey (CTIS)

#Post-stratification of age/sex/education at ZCTA level
#potential secondary post-stratification using urban/rural classification later

pacman::p_load(tidyverse, ggplot2, here, tidycensus, tigris, janitor, viridis, data.table)

here::i_am("scripts/999a_establish_baseline_proportions_cadoh_data.R")

options(tigris_use_cache = TRUE)
tidycensus::census_api_key(Sys.getenv("CENSUS_API_KEY"), overwrite = FALSE, install = FALSE)

#whole time period for Ipsos wave 3 direct estimates (not bootstrapped) or bounded by specific dates?
whole_time_period = TRUE

#### baseline for comparison: CA pop by sex ####
capop <- get_acs(geography = "state",
                 state = "CA",
                 variables = c(
                   "18_plus_f" ="B05003_019",
                   "18_plus_m" = "B05003_008"
                 ),
                 survey = "acs5",
                 geometry = FALSE,
                 year = 2020)
# sum(capop$estimate)
# moe_sum(moe = capop$moe, estimate = capop$estimate)


#### CDC estimates for CA by sex and age (3 categories match with CTIS data) ####
cdc = fread("data/raw_poststrat_data/COVID-19_Vaccination_Age_and_Sex_Trends_in_the_United_States__National_and_Jurisdictional.csv")

cdc = cdc %>% 
      clean_names() %>%
      mutate(date = as.Date(date, "%m/%d/%Y")) %>%
      filter(location == "CA",
             date == "2021-06-30",
             str_detect(demographic_category, "ale")) %>%
      mutate(sex = ifelse(str_detect(demographic_category, "Male"), 1, 0),
             age = case_when(str_detect(demographic_category, "18") ~ 1, #18-24 yrs
                             str_detect(demographic_category, "25-4") ~ 2, #25-49
                             str_detect(demographic_category, "50") ~ 2, #50-64, aggregate with above to make 25-64 to match ctis data
                             # str_detect(demographic_category, "65-74") ~ 3,
                             str_detect(demographic_category, "65+") ~ 3)) %>%
      select(sex, age, administered_dose1) %>%
      group_by(sex, age) %>%
      reframe(sex=sex,
                age=age,
                administered_dose1 = sum(administered_dose1)) %>%
      distinct() %>%
      na.omit() %>%
      mutate(total_pop = sum(capop$estimate), #sum of male and female population counts at the state level
             estimate = administered_dose1/total_pop) %>%
      mutate(model = "CDC official count") %>%
      ungroup()


#44597 - 1.96 * sqrt(44597/cdc$ncounties[1])


#set these to be meaningless right now to match up later
# cdc$up = (cdc$estimate + (1.96*(sqrt(cdc$estimate * (1 - cdc$estimate)/1))))
# cdc$lw = (cdc$estimate - (1.96*(sqrt(cdc$estimate * (1 - cdc$estimate)/1))))
cdc$up = NA
cdc$lw = NA



##### build unweighted survey proportions used in analysis ####
#sex-specific vax ~ rw1 age comparison estimates

#load(file = here("data/cleaned_input_data/clean_data.rda"))

load(here("data/cleaned_input_data/clean_data_county_with_edu.rda"))

datm = 
  dat %>%
  filter(sex == 1) %>%
  select(vax, age, edu) %>%
  mutate(age = ifelse(age == 4, 3, age)) %>%
  group_by(age,edu) %>%
  reframe(sex = 1,
          estimate = sum(vax)/nrow(dat)) %>%
  distinct() %>%
  mutate(lw = NA,
         up = NA) %>%
  ungroup()

datf = 
  dat %>%
  filter(sex == 0) %>%
  select(vax, age, edu) %>%
  mutate(age = ifelse(age == 4, 3, age)) %>%
  group_by(age,edu) %>%
  reframe(sex = 0,
          estimate = sum(vax)/nrow(dat)) %>%
  distinct() %>%
  mutate(lw = NA,
         up = NA) %>%
  ungroup()

dat.by.sex = rbind(datm, datf)
 
rm(datm, datf) 

dat.by.sex = dat.by.sex %>%
  mutate(model = "unweighted survey\nproportions") %>%
  ungroup()

# ####model results: rw1 on age, data separated by sex####
# rw1.age.f = read.csv(file = here(paste0("data/mrp/indirect_estimates/mrp_estimates_vax_rw1_age_f.csv")))
# 
# rw1.age.m = read.csv(file = here(paste0("data/mrp/indirect_estimates/mrp_estimates_vax_rw1_age_m.csv")))
# 
# rw1.age.m = rw1.age.m %>% 
#             mutate(sex = 1,
#                    estimate = median_ps_prob,
#                    lw = lw_ps_prob,
#                    up = up_ps_prob) %>%
#             select(colnames(dat.by.sex))
# 
# rw1.age.f = rw1.age.f %>% 
#             mutate(sex = 0,
#                    estimate = median_ps_prob,
#                    lw = lw_ps_prob,
#                    up = up_ps_prob) %>%
#             select(colnames(dat.by.sex))
# 
# 
# rw1.age.by.sex = rbind(rw1.age.f, rw1.age.m)
# 
# rw1.age.by.sex = rw1.age.by.sex %>%
#                 mutate(model = "RW(1) on age") %>%
#                 ungroup()
# 
# rm(rw1.age.f, rw1.age.m)


#compare the probabilities in CTIS estimates to this

#clean up cdc to match the other objects for binding

cdc = cdc %>% 
      select(colnames(rw1.age.by.sex)) %>% 
      ungroup()


temp = rbind(cdc, rw1.age.by.sex,dat.by.sex) #

#direct estimates from CTIS of age and sex ####

direct = read.csv(here("data/mrp/direct_estimates/ctis_vax_direct_estimates_not_boostrapped.csv"))

direct = direct %>%
  mutate(vax = ifelse(vax == "No", 0, 1),
         age = case_when(str_detect(age, "18")~1,
                         str_detect(age, "25")~2,
                         str_detect(age, "65")~3),
         sex = ifelse(sex == "Female",0,1))

direct = direct %>% filter(vax == 1)

direct$model = "direct estimate\n(survey mean)"
direct$lw = direct$mean_low
direct$up = direct$mean_upp
direct$estimate = direct$mean

direct = direct %>% select(colnames(temp))

temp = rbind(temp,direct)
plot = temp %>%
  mutate(lw = ifelse(is.na(lw), estimate, lw),
         up = ifelse(is.na(up), estimate, up),
         method = model,
         age = factor(case_when(age == 1 ~ "18-24\nyears",
                         age == 2 ~ "25-64\nyears",
                         age == 3 ~ "65 years\nand older")),
         sex= factor(ifelse(sex == 1, "male", "female"))) %>%
    ggplot(aes(x=age, y=estimate, color=model)) +
  geom_pointrange(aes(ymin = lw, ymax = up), position = "jitter") + #, position = "jitter"
  facet_wrap(~sex) +
  theme_minimal() +
  labs(
       # title="Estimated proportion of the California population with one COVID-19 vaccination dose",
       # subtitle = "Comparison of modeled state-level estimates as of June 30, 2021, by sex and age",
       y = "estimated proportion",
       x = "age (category)") +
  guides(fill = "none") + 
  scale_color_viridis(discrete = TRUE)

ggsave(plot, file =here("results/plots/diss_chapter_mrp_plots/ca_state_comparison_no_bym2.tiff"), width = 6.5, height= 3, unit="in",dpi = 320, bg = "white")

write.csv(temp, file = here("data/mrp/indirect_estimates/table6_dissertationchapter3.csv"))





