#### MRP with Spatial Priors using CTIS Data ####


#Build a series of models representing vaccination probability (min. 1 dose)
#For June 30, 2021 using the COVID-19 Trends and Impacts Survey (CTIS)

pacman::p_load(tidyverse,here,fs,tidycensus,tigris,janitor,sf, viridis,data.table)

here::i_am("scripts/04_visualize_results.R")

options(scipen=999)
options(tigris_use_cache = TRUE)
tidycensus::census_api_key(Sys.getenv("CENSUS_API_KEY"), overwrite = FALSE, install = FALSE)

#Only have to run the one time (joins all the modeled results together) 
filelist <- list.files(
  path = here("data/indirect_estimates/complete_estimates_both_sexes/"),
  full.names = TRUE) # include the directory in the result)


dat = fs::dir_ls(here("data/indirect_estimates/complete_estimates_both_sexes/"), regexp = "\\.csv$")
dat = dat %>%
  map_dfr(read_csv)

save(dat, file = here("data/cleaned_input_data/clean_complete_model_indirect_estimates.rda"))
rm(filelist)
#load("data/cleaned_input_data/clean_complete_model_indirect_estimates.rda")


#load model information for intuitive labeling in figures

load(file = here("data/cleaned_input_data/model_descriptions.RData"))


#clean up
rm(model_formulae) #don't need it
models$poststrat_vars = 
  models$poststrat_vars %>% 
  gsub("\"\"", "", ., fixed=TRUE)

models = models %>%
  select(model_name, specs)

#model_name is now there for labels!
dat = left_join(dat, models, by = c("model" = "specs"),relationship = "many-to-many")

#add geographic data for mapping
geo = counties(state = "CA", cb = FALSE, year = 2020)
geo = geo %>% clean_names() %>% select(geoid,name) %>% st_drop_geometry

dat = left_join(dat, geo, by = c("xfips" = "geoid"),relationship = "many-to-many")

#### CDC STATE LEVEL estimates for CA by sex and age (3 categories match with CTIS data) ####
cdc = fread(here("data/cdc_data/COVID-19_Vaccination_Age_and_Sex_Trends_in_the_United_States__National_and_Jurisdictional.csv"))

#both dates included because I noticed that June 12 looks a 
#lot like some of the earlier estimates
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

pop = dat %>% filter(is.na(xfips)) %>% select(N) %>% distinct %>% reframe(pop=sum(N))

cdc = cdc %>%
  group_by(date,sex,age) %>%
  mutate(total_pop = pop$pop,
         estimate = administered_dose1/total_pop,
         model = "CDC official count") %>%
  group_by(date) %>%
  mutate(popest = sum(administered_dose1)) %>%
  ungroup

rm(pop)
fwrite(cdc, file = "results/tables/table4_cdc_baseline_data.csv")



#### load state of CA county vaccination data from the CDC ####
countyvax = fread(here("data/cdc_data/COVID-19_Vaccinations_in_the_United_States_County.csv"))

countyvax = countyvax %>%
  clean_names() %>%
  filter(recip_state == "CA",
         date == "06/30/2021" | date == "06/12/2021") %>%
  select(date, recip_county, fips, 
         administered_dose1_recip_18plus,
         administered_dose1_recip_18plus_pop_pct)

#get acs counts of total population by county in CA
acs = get_acs(geography = "county",
              state = "CA",
              table = "B15001",
              survey = "acs5",
              geometry = FALSE,
              year = 2020)

#total population
acs = acs %>% clean_names %>% filter(variable == "B15001_001") %>% select(geoid,estimate) 

countyvax = left_join(countyvax,acs, by = c("fips"="geoid"))
countyvax = countyvax %>% rename(countypop = estimate)

#prop in original file is lower than CDC estimates 
#because the denominator is different (2021 5-year ACS rather 
#than what they seem to have used: the 2019!)
countyvax = countyvax %>%
  mutate(county = str_remove(recip_county," County")) %>% #make it match dat naming
  group_by(date, county) %>%
  mutate(prop = administered_dose1_recip_18plus/countypop) %>%
  ungroup %>%
  na.omit #CDC has a weird phantom unkown county with no data

countyvax = countyvax %>%
  select(date,county,fips,administered_dose1_recip_18plus,countypop,prop)

countyvax %>% 
  group_by(date) %>%
  reframe(statedoses_fromcounty = sum(administered_dose1_recip_18plus),
          stateprop = statedoses_fromcounty/sum(acs$estimate))


write.csv(countyvax, file = here("results/tables/table5_county_counts_cdc.csv"))

#for benchmark lines in plots at the state level
cdc_state = cdc %>%
  group_by(date, sex, age) %>%
  mutate(estimate = sum(administered_dose1)) %>%
  group_by(date) %>%
  reframe(prop_est = sum(estimate)/total_pop,
            model="CDC estimate") %>%
  ungroup() %>% 
  distinct

#### County Estimates ####
# dat.county = dat %>%
#   filter(!str_detect(model_name, "county (BYM2 random effect)"), #nonsensical models
#          !is.na(xfips)
#          ) %>%
#   select(xfips,name,model_name,strata_pop,strata_pop_est,
#          strata_pop_lw.ci,strata_pop_up.ci,
#          sex,age,edu) %>% 
#   group_by(name,model_name,xfips,sex) %>%
#   mutate(N_county_sex = sum(strata_pop),
#          sae_county_sex = sum(strata_pop_est)) %>% #total number in each sex in the county
#   group_by(name,model_name,xfips) %>%
#   reframe(N_county = sum(strata_pop), #total in each county (both sexes)
#          sae_county = sum(strata_pop_est), #median sae estimate in the county
#          prop_county = sae_county/N_county,
#          prop_countylw = sum(strata_pop_lw.ci)/N_county,
#          prop_countyup = sum(strata_pop_up.ci)/N_county) %>% 
#  # select(name,model_name,xfips,sex,state_prop,state_proplw,state_propup) %>% #ungroup %>% distinct %>% view
#   ungroup() %>%
#   distinct %>% 
#   mutate(type = case_when(str_detect(model_name, "fixed") ~ "Mixed Effects",
#                           str_detect(model_name, "BYM2") ~ "Random Effects: BYM2",
#                           str_detect(model_name, "IID") ~ "Random Effects: IID"))
# 
# types = c("Non-Spatial Mixed Effects", "Spatial Random Effects", "Non-Spatial Random Effects")
# 
# for (i in types) {
#   
#   lab = str_to_lower(i) # Converts the entire string to lower case
#   lab = str_replace(lab,' ', '_') 
#   lab = str_replace(lab,': ', '_') 
#   lab = str_replace(lab, " ","")
#   lab = str_replace(lab, "-","")
#   
#   
#   dat.state %>%
#     filter(type == paste(i)) %>% 
#     ggplot(aes(x=factor(model_name), y=prop,color=factor(model_name))) +
#     geom_pointrange(aes(ymin = proplw, ymax = propup),position="jitter") +
#     # geom_hline(data =  cdc %>% filter(date == "2021-06-30"),
#     #             aes(yintercept = popest/total_pop, color = "darkorange"),
#     #             linewidth = 1) +
#     theme_minimal() +
#     guides(color="none") +
#     labs(x = "model",
#          y="proportion",
#          title = paste(i,": State-Level Estimates")) +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#           legend.spacing = unit(1, 'cm')) + 
#     scale_color_viridis(discrete = TRUE) 
#   
#   ggsave(file = here(paste0("plots/county_level/results_all_models_proportion_statelevel_",lab,".tiff")),dpi = 320, bg = "white", width =30, height =7, unit = "in")
# }
# 
test = read.csv(here("/Users/Aja/Documents/R Directories/NSF_RAPID_COVID19_Survey/Aja/COVID_Behaviors/data/smrp/ch2_diss_tables/rollup_state_level_compare_area_models_with_CDC_estimate_counts.csv"))
#something is rotten here.
#view(dat %>% filter(name == "Alameda",str_detect(model_name,"BYM2")) %>% group_by(xfips,model))

temp = dat %>% 
  select(xfips,sex,N) %>% 
  distinct %>%
  select(-sex) %>%
  group_by(xfips) %>%
  reframe(N_totalcounty = sum(N)) 


#### State Estimates ####

dat = left_join(dat,temp, by = 'xfips')

dat.state = dat %>%
    select(xfips,model_name,sex,age,edu,strata_pop,strata_pop_est,
           strata_pop_lw.ci,strata_pop_up.ci,N_totalcounty) %>% 
    group_by(model_name) %>%
    mutate(N = sum(strata_pop),
           sae_state = sum(strata_pop_est),
          sae_statelw = sum(strata_pop_lw.ci),
          sae_stateup = sum(strata_pop_up.ci)) %>% #total number in each sex in the county
    select(model_name,sae_state,
           sae_statelw,sae_stateup,N) %>% 
    distinct %>%
    ungroup %>%
    mutate(prop = sae_state/N,
            proplw = sae_statelw/N,
            propup = sae_stateup/N) %>% 
    # select(name,model_name,xfips,sex,state_prop,state_proplw,state_propup) %>% #ungroup %>% distinct %>% view
    ungroup() %>%
    distinct %>% 
    mutate(type = case_when(str_detect(model_name, "fixed") ~ "Non-Spatial Mixed Effects",
                            str_detect(model_name, "BYM2") ~ "Spatial Random Effects",
                            str_detect(model_name, "IID|AR|RW") ~ "Non-Spatial Random Effects"))
  

# Build Plots 
types = c("Non-Spatial Mixed Effects", "Spatial Random Effects", "Non-Spatial Random Effects")

for (i in types) {
  
  lab = str_to_lower(i) # Converts the entire string to lower case
  lab = str_replace(lab,' ', '_') 
  lab = str_replace(lab,': ', '_') 
  lab = str_replace(lab, " ","")
  lab = str_replace(lab, "-","")
  
  dat.state %>%
    filter(type == paste(i)) %>% 
    group_by(model_name) %>%
    ggplot(aes(x=factor(model_name), y=prop,color=factor(model_name))) +
      geom_pointrange(aes(ymin = proplw, ymax = propup)) + #,position="jitter"
      geom_hline(data =  cdc %>% filter(date == "2021-06-30"),
                  aes(yintercept = popest/total_pop),
                 color = "darkorange",
                  linewidth = 1) +
      # geom_hline(data =  cdc %>% filter(date == "2021-06-12"),
      #          aes(yintercept = popest/total_pop),
      #          color = "blue",
      #          linewidth = 1) +
      theme_minimal() +
      guides(color="none") +
      labs(x = "model",
           y="proportion",
           title = paste0(i,": State-Level Estimates")) +
      scale_x_discrete(labels = ~ str_wrap(.x, 10)) +
      theme(axis.text.x = element_text(vjust = 0.5, hjust = 1),
            legend.spacing = unit(1, 'cm')) + 
      scale_color_viridis(discrete = TRUE) +
      facet_wrap(~type, nrow = 3)

  ggsave(file = here(paste0("plots/state_level/results_proportion_statelevel_",lab,".tiff")),dpi = 320, bg = "white", width =14, height =4, unit = "in")
}

