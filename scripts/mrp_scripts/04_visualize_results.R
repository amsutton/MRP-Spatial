#### MRP with Spatial Priors using CTIS Data ####


#Build a series of models representing vaccination probability (min. 1 dose)
#For June 30, 2021 using the COVID-19 Trends and Impacts Survey (CTIS)

pacman::p_load(tidyverse,here,fs,tidycensus,tigris,janitor,sf, viridis,data.table)

here::i_am("scripts/04_visualize_results.R")

options(scipen=999)
options(tigris_use_cache = TRUE)
tidycensus::census_api_key(Sys.getenv("CENSUS_API_KEY"), overwrite = FALSE, install = FALSE)

#Only have to run the one time (joins all the modeled results together) 
# filelist <- list.files(
#   path = here("data/indirect_estimates/complete_estimates_both_sexes/"),
#   full.names = TRUE) # include the directory in the result)
# 
# 
# dat = fs::dir_ls(here("data/indirect_estimates/complete_estimates_both_sexes/"), regexp = "\\.csv$")
# dat = dat %>%
#   map_dfr(read_csv)
# save(dat, file = here("data/cleaned_input_data/clean_complete_model_indirect_estimates.rda"))
# rm(filelist)

load("data/cleaned_input_data/clean_complete_model_indirect_estimates.rda")
#load model information for intuitive labeling in figures

load(file = here("data/cleaned_input_data/model_descriptions.RData"))
models = models %>% filter(!str_detect(specs,"fixed"))



#clean up
rm(model_formulae) #don't need it
models$poststrat_vars = 
  models$poststrat_vars %>% 
  gsub("\"\"", "", ., fixed=TRUE)

models = models %>%
  select(model_name, specs) %>%
  filter(str_detect(specs,"xfips|bym2"),
         !str_detect(specs,"bym2_xfips"))
write.csv(models,here("results/tables/Table2_model_list.csv"))
glimpse(models)

#model_name is now there for labels!
dat = right_join(dat,models, by = c("model"="specs"),relationship = "many-to-many")
glimpse(dat)
#add geographic data for mapping
geo = counties(state = "CA", cb = FALSE, year = 2020)
geo = geo %>% clean_names() %>% select(geoid,name) %>% st_drop_geometry

dat = left_join(dat, geo, by = c("xfips" = "geoid"),relationship = "many-to-many")

#### CDC STATE LEVEL estimates for CA by sex and age (3 categories match with CTIS data) ####
cdc = fread(here("data/cdc_data/COVID-19_Vaccination_Age_and_Sex_Trends_in_the_United_States__National_and_Jurisdictional.csv"))

#clean version for table in paper
cdc_bysexage_table = cdc %>% 
  clean_names() %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  filter(location == "CA",
         date == "2021-06-30" | date == "2021-06-12",
         str_detect(demographic_category, "ale")) %>%
  mutate(sex = ifelse(str_detect(demographic_category, "Male"), "male", "female"),
         age = case_when(str_detect(demographic_category, "18") ~ "18-24 years", #18-24 yrs
                         str_detect(demographic_category, "25-4") ~ "25-49 years", #25-49
                         str_detect(demographic_category, "50") ~ "25-49 years", #50-64, aggregate with above to make 25-64 to match ctis data
                         # str_detect(demographic_category, "65-74") ~ 3,
                         str_detect(demographic_category, "65+") ~ "65+ years")) %>%
  select(date, sex, age, administered_dose1) %>%
  group_by(date, sex, age) %>%
  reframe(administered_dose1 = sum(administered_dose1)) %>%
  distinct() %>%
  na.omit() 
glimpse(dat)
pop = dat %>% select(xfips,N) %>% distinct %>% reframe(pop=sum(N))

cdc_bysexage_table = cdc_bysexage_table %>%
  group_by(date,sex,age) %>%
  mutate(total_pop = pop$pop,
         estimate = administered_dose1/total_pop,
         model = "CDC official count") %>%
  group_by(date) %>%
  mutate(popest = sum(administered_dose1),
         estimate = round(estimate,3)) %>%
  ungroup %>%
  select(date:administered_dose1,estimate,popest)

fwrite(cdc_bysexage_table, file = "results/tables/table4_cdc_baseline_data_by_sexage.csv")


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

cdc = cdc %>%
  group_by(date,sex,age) %>%
  mutate(total_pop = pop$pop,
         estimate = administered_dose1/total_pop,
         model = "CDC official count") %>%
  group_by(date) %>%
  mutate(popest = sum(administered_dose1)) %>%
  ungroup

rm(pop)

#### Read in direct estimates ####

dir = fread(here("data/direct_estimates/ctis_vax_direct_estimates_not_bootstrapped_with_edu_simplevax.csv"))

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
  mutate(statedoses_fromcounty = sum(administered_dose1_recip_18plus),
          stateprop = statedoses_fromcounty/sum(acs$estimate)) %>%
  select(date,county,fips,prop,stateprop)


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

temp = dat %>% 
  select(xfips,sex,N) %>% 
  distinct %>%
  select(-sex) %>%
  group_by(xfips) %>%
  reframe(N_totalcounty = sum(N)) 


#### State Aggregated Estimates ####

dat = left_join(dat,temp, by = 'xfips')
dat = left_join(dat,models, by ="model_name")
dat = dat %>% mutate(specs = str_replace_all(specs,"xfips","county"))
glimpse(dat)
table(dat$specs)
#dat = dat %>% filter(!str_detect(model,"bym2_xfips"))

dat.state = dat %>%
    select(xfips,model_name,specs,sex,age,edu,strata_pop,strata_pop_est,
           strata_pop_lw.ci,strata_pop_up.ci,N_totalcounty) %>% 
    group_by(model_name) %>%
    mutate(N = sum(strata_pop),
           sae_state = sum(strata_pop_est),
          sae_statelw = sum(strata_pop_lw.ci),
          sae_stateup = sum(strata_pop_up.ci)) %>% #total number in each sex in the county
    select(model_name,sae_state,specs,
           sae_statelw,sae_stateup,N) %>% 
    distinct %>%
    ungroup %>%
    mutate(prop = sae_state/N,
            proplw = sae_statelw/N,
            propup = sae_stateup/N) %>% 
    # select(name,model_name,xfips,sex,state_prop,state_proplw,state_propup) %>% #ungroup %>% distinct %>% view
    ungroup() %>%
    distinct %>% 
    mutate(type = case_when(str_detect(specs, "bym2") ~ "Spatial",
                            str_detect(specs, "iid") ~ "Non-Spatial"),
           specs = str_replace_all(specs,"_"," ")) %>%
  select(type,model_name,prop,specs,proplw,propup)


dat.state %>%
    filter(type == "Spatial",
           !str_detect(specs, "county")) %>% 
    group_by(model_name) %>%
    ggplot(aes(x=factor(specs), y=prop)) +
      geom_pointrange(aes(ymin = proplw, ymax = propup)) + #,position="jitter"
      geom_hline(data =  cdc %>% filter(date == "2021-06-30"),
                  aes(yintercept = popest/total_pop,
                      linetype = "CDC estimate: June 30, 2021"),
                 color = "darkorange",
                  linewidth = 1) +
      geom_hline(data =  dir %>% filter(vax == "Yes"),
               aes(yintercept = mean,
                   linetype = "Direct Survey Estimate"),
               color = "darkorange",
                  # linetype = "CTIS State-level Estimate"),
               #color = "blue",
               linewidth = 1) +
    # geom_hline(data =  dir %>% filter(vax == "Yes"),
    #            aes(yintercept = mean_low),
    #            color = "blue",
    #            linewidth = 0.5,
    #            linetype = "dashed",
    #            alpha = 0.5) +
    # geom_hline(data =  dir %>% filter(vax == "Yes"),
    #            aes(yintercept = mean_upp),
    #            color = "blue",
    #            linewidth = 0.5,
    #            linetype = "dashed",
    #            alpha = 0.5) +
      theme_minimal() +
      guides(color="none") +
      labs(x = "Model Parameters",
           y="Proportion",
           #title = paste0(i,": State-Level Estimates"),
           linetype = "Comparative Estimate") +
      scale_x_discrete(labels = ~ str_wrap(.x, 10)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
      scale_linetype_discrete(name = "") +
      theme(
        #axis.text.x = element_text(vjust = 0.5, hjust = 1,angle = 10),
            legend.spacing = unit(1, 'cm'),
            legend.position="bottom") 
  # + 
  #     scale_color_manual(values = c("#7ad151",
  #                                 "#22a884",
  #                                 "#2a788e",
  #                                 "#414487","#440154")) 
      #facet_wrap(~type, nrow = 3)

ggsave(file = here(paste0("plots/state_level/results_proportion_statelevel_spatial.pdf")),dpi = 320, bg = "white", width =12, height =4, unit = "in")

#check performance by age, sex, education
dat.state.demog = dat %>%
  select(xfips,model_name,specs,sex,age,edu,strata_pop,strata_pop_est,
         strata_pop_lw.ci,strata_pop_up.ci,N_totalcounty) %>%
  group_by(model_name,sex,age,edu) %>%
  mutate(N = sum(strata_pop),
         sae_state = sum(strata_pop_est),
         sae_statelw = sum(strata_pop_lw.ci),
         sae_stateup = sum(strata_pop_up.ci)) %>% #total number in each sex in the county
  select(model_name,sex,age,edu,sae_state,specs,
         sae_statelw,sae_stateup,N) %>%
  distinct %>%
  group_by(sex,age,edu) %>%
  mutate(prop = sae_state/N,
         proplw = sae_statelw/N,
         propup = sae_stateup/N) %>%
  # select(name,model_name,xfips,sex,state_prop,state_proplw,state_propup) %>% #ungroup %>% distinct %>% view
  ungroup() %>%
  distinct %>%
  mutate(type = case_when(str_detect(specs, "bym2") ~ "Spatial",
                          !str_detect(specs, "bym2") ~ "Non-Spatial"),
         specs = str_replace_all(specs,"_"," ")) %>%
  select(type,model_name,sex,age,edu,prop,specs,proplw,propup) %>%
  mutate(sex = ifelse(sex == 0,"female","male"),
         age = case_when(age==1 ~ "18-24 years",
                         age==2 ~ "25-64 years",
                         age==3 ~ "65 years and over"),
         edu = case_when(edu==1 ~ "less than high school",
                         edu==2 ~ "high school or equivalent",
                         edu==3 ~ "some college",
                         edu==4 ~ "associate’s degree",
                         edu==5 ~ "bachelor's degree",
                         edu==6 ~ "professional or graduate degree"),
         demog = paste(sex,age,sep=", "),
         demog = str_remove_all(demog, " NA,| NA"))

nonspatial_statelevel_order = c("rw1 age iid county",
                                  "rw1 edu iid county",
                                  "rw2 edu iid county",
                                  "rw1 age rw1 edu iid county",
                                  "rw1 age rw2 edu iid county")

  dat.state.demog %>% 
    filter(demog != "female,",
           demog !=  "male,",
           type == "Spatial") %>%
    group_by(model_name) %>%
    ggplot(aes(x=factor(specs), y=prop)) + #
    geom_pointrange(aes(ymin = proplw, ymax = propup),position="jitter") + #
    geom_hline(data =  cdc %>% filter(date == "2021-06-30"),
               aes(yintercept = popest/total_pop,
                   linetype = "CDC estimate: June 30, 2021"),
               color = "darkorange",
               linewidth = 1) +
    geom_hline(data =  dir %>% filter(vax == "Yes"),
               aes(yintercept = mean,
                   linetype = "Direct Survey Estimate"),
               color = "darkorange",
               # linetype = "CTIS State-level Estimate"),
               #color = "blue",
               linewidth = 1) +
    # geom_hline(data =  dir %>% filter(vax == "Yes"),
    #            aes(yintercept = mean_low),
    #            color = "blue",
    #            linewidth = 0.5,
    #            linetype = "dashed",
    #            alpha = 0.5) +
    # geom_hline(data =  dir %>% filter(vax == "Yes"),
    #            aes(yintercept = mean_upp),
    #            color = "blue",
    #            linewidth = 0.5,
    #            linetype = "dashed",
    #            alpha = 0.5) +
    theme_minimal() +
    #guides(color="none") +
    labs(x = "Model Parameters",
         y="Proportion",
         #title = paste0(i,": State-Level Estimates"),
         linetype = "Comparative Estimate") +
    scale_x_discrete(labels = ~ str_wrap(.x, 10)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_linetype_discrete(name = "") +
    theme(
      #axis.text.x = element_text(vjust = 0.5, hjust = 1),
          legend.spacing = unit(1, 'cm'),
          legend.position="bottom") + #+
  #   scale_color_manual(values = c("#7ad151",
  #                                 "#22a884",
  #                                 "#2a788e",
  #                                 "#414487","#440154"))
  facet_grid(type+demog~edu)

ggsave(file = here(paste0("plots/state_level/results_proportion_statelevel_bydemographics_spatial.pdf")),dpi = 320, bg = "white", width =35, height =15, unit = "in")
  
dat.state.demog %>% 
  filter(demog != "female,",
         demog !=  "male,",
         type == "Non-Spatial") %>%
  na.omit() %>%
  group_by(model_name) %>%
  ggplot(aes(x=factor(specs), y=prop)) + #
  geom_pointrange(aes(ymin = proplw, ymax = propup),position="jitter") + #
  geom_hline(data =  cdc %>% filter(date == "2021-06-30"),
             aes(yintercept = popest/total_pop,
                 linetype = "CDC estimate: June 30, 2021"),
             color = "darkorange",
             linewidth = 1) +
  geom_hline(data =  dir %>% filter(vax == "Yes"),
             aes(yintercept = mean,
                 linetype = "Direct Survey Estimate"),
             color = "darkorange",
             # linetype = "CTIS State-level Estimate"),
             #color = "blue",
             linewidth = 1) +
  # geom_hline(data =  dir %>% filter(vax == "Yes"),
  #            aes(yintercept = mean_low),
  #            color = "blue",
  #            linewidth = 0.5,
  #            linetype = "dashed",
  #            alpha = 0.5) +
  # geom_hline(data =  dir %>% filter(vax == "Yes"),
  #            aes(yintercept = mean_upp),
  #            color = "blue",
  #            linewidth = 0.5,
  #            linetype = "dashed",
  #            alpha = 0.5) +
  theme_minimal() +
  #guides(color="none") +
  labs(x = "Model Parameters",
       y="Proportion",
       #title = paste0(i,": State-Level Estimates"),
       linetype = "Comparative Estimate") +
  scale_x_discrete(labels = ~ str_wrap(.x, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_linetype_discrete(name = "") +
  theme(
    #axis.text.x = element_text(vjust = 0.5, hjust = 1),
        legend.spacing = unit(1, 'cm'),
        legend.position="bottom") + #+
  #   scale_color_manual(values = c("#7ad151",
  #                                 "#22a884",
  #                                 "#2a788e",
  #                                 "#414487","#440154"))
  facet_grid(type+demog~edu)

ggsave(file = here(paste0("plots/state_level/results_proportion_statelevel_bydemographics_nonspatial.pdf")),dpi = 320, bg = "white", width =30, height =25, unit = "in")

#direct estimate for plots
dirmean = dir %>% filter(vax == "Yes") %>% select(mean)
cdcest = cdc %>% filter(date == "2021-06-30") %>% reframe(est = sum(estimate))
cdcest

dat.state %>%
  filter(type == "Non-Spatial") %>%
  #!str_detect(specs, "xfips")) %>% 
  group_by(model_name) %>%
  ggplot(aes(x=factor(specs), y=prop)) +
  geom_pointrange(aes(ymin = proplw, ymax = propup)) + #,position="jitter"
  geom_hline(data =  cdcest,
             aes(yintercept = est,
                 linetype = "CDC estimate: June 30, 2021"),
             color = "darkorange",
             linewidth = 1) +
  geom_hline(data = dirmean,
             aes(yintercept = mean,
                 linetype = "Direct Survey Estimate"),
             color = "darkorange",
             linewidth = 1) +
  theme_minimal() +
  guides(color="none") +
  labs(x = "Model Parameters",
       y="Proportion",
       linetype = "Comparative Estimate") +
  scale_x_discrete(labels = ~ str_wrap(.x, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_linetype_discrete(name = "") +
  theme(
    #axis.text.x = element_text(vjust = 0.5, hjust = 1, angle = 10),
        legend.spacing = unit(1, 'cm'),
        legend.position="bottom") 

ggsave(file = here(paste0("plots/state_level/results_proportion_statelevel_nonspatial.pdf")),dpi = 320, bg = "white", width =12, height =4, unit = "in")

# No longer comparing state-level models in this paper  
  # 
  # nogeog_statelevel_order = c("iid age", "iid edu", 
  #                             "ar1 age", "ar1 edu", 
  #                             "rw1 age", "rw1 edu", 
  #                             "rw2 edu", "iid age iid edu",
  #                             "ar1 age ar1 edu",
  #                             "rw1 age rw1 edu",
  #                             "rw1 age rw2 edu")
  # dat.state %>%
  #   filter(type == "Non-Spatial",
  #          !str_detect(specs, "xfips")) %>%
  #   group_by(model_name) %>%
  #   ggplot(aes(x=factor(specs,levels = nogeog_statelevel_order), y=prop)) +
  #   geom_hline(data =  cdcest,
  #              aes(yintercept = est,
  #                  linetype = "CDC estimate: June 30, 2021"),
  #              color = "darkorange",
  #              linewidth = 1) +
  #   geom_hline(data =  dirmean,
  #              aes(yintercept = mean,
  #                  linetype = "Direct Survey Estimate"),
  #              color = "darkorange",
  #              # linetype = "CTIS State-level Estimate"),
  #              #color = "blue",
  #              linewidth = 1) +
  #   geom_pointrange(aes(ymin = proplw, ymax = propup)) + #,position="jitter"
  #   # geom_hline(data =  dir %>% filter(vax == "Yes"),
  #   #            aes(yintercept = mean_low),
  #   #            color = "blue",
  #   #            linewidth = 0.5,
  #   #            linetype = "dashed",
  #   #            alpha = 0.5) +
  #   # geom_hline(data =  dir %>% filter(vax == "Yes"),
  #   #            aes(yintercept = mean_upp),
  #   #            color = "blue",
  #   #            linewidth = 0.5,
  #   #            linetype = "dashed",
  #   #            alpha = 0.5) +
  #   theme_minimal() +
  #   guides(color="none") +
  #   labs(x = "Model Parameters",
  #        y="Proportion",
  #        #title = paste0(i,": State-Level Estimates"),
  #        linetype = "Comparative Estimate") +
  #   scale_x_discrete(labels = ~ str_wrap(.x, 10)) +
  #   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  #   scale_linetype_discrete(name = "") +
  #   theme(axis.text.x = element_text(vjust = 0.5, hjust = 1),
  #         legend.spacing = unit(1, 'cm'),
  #         legend.position="bottom") 
  # #facet_wrap(~type, nrow = 3)
  # 
  # ggsave(file = here(paste0("plots/state_level/results_proportion_statelevel_nosubgeography.pdf")),dpi = 320, bg = "white", width =10, height =4, unit = "in")
  # 
  

#### County Estimates ####
ctdir = fread(here("data/direct_estimates/ctis_vax_direct_estimates_not_bootstrapped_with_edu_simplevax_bycounty.csv"))
ctdir = ctdir %>% filter(vax=="Yes")

dat.county = dat %>%
  filter(!is.na(xfips)) %>%
  select(xfips,model_name,specs,sex,age,edu,strata_pop,strata_pop_est,
         strata_pop_lw.ci,strata_pop_up.ci,N_totalcounty) %>% 
  group_by(xfips, model_name) %>%
  mutate(N = sum(strata_pop),
         sae_state = sum(strata_pop_est),
         sae_statelw = sum(strata_pop_lw.ci),
         sae_stateup = sum(strata_pop_up.ci)) %>% #total number in each sex in the county
  select(xfips,model_name,sae_state,specs,
         sae_statelw,sae_stateup,N) %>% 
  distinct %>%
  ungroup %>%
  mutate(prop = sae_state/N,
         proplw = sae_statelw/N,
         propup = sae_stateup/N) %>% 
  # select(name,model_name,xfips,sex,state_prop,state_proplw,state_propup) %>% #ungroup %>% distinct %>% view
  ungroup() %>%
  distinct %>% 
  mutate(type = case_when(str_detect(specs, "bym2") ~ "Spatial",
                          !str_detect(specs, "bym2") ~ "Non-Spatial"),
         specs = str_replace_all(specs,"_"," ")) %>%
  select(xfips,type,model_name,prop,specs,proplw,propup)


dat.county$xfips = as.integer(dat.county$xfips)

ctdir = ctdir %>%
    rename(prop = mean,
           proplw = mean_low,
           propup = mean_upp) %>%
    select(-V1,-ci) %>%
    mutate(model_name = "direct survey estimate",
           specs = "direct survey estimate") %>%
    select(xfips,model_name,prop,specs,proplw, propup)

ctdir1 = ctdir %>%
  mutate(type = "Spatial")
ctdir2 = ctdir %>%
  mutate(type = "Non-Spatial")

ctdir = rbind(ctdir1,ctdir2)
rm(ctdir1,ctdir2)

ctdir = ctdir %>% select(colnames(dat.county))

dat.county = rbind(dat.county,ctdir)
table(dat.county$model_name)
# glimpse(dat.county)
# glimpse(ctdir)
# # 
# # temp = dat %>% select(model_name,xfips) %>% na.omit %>% distinct %>% mutate(xfips = as.integer(xfips))
# # glimpse(temp)
# #dat.county=left_join(dat.county, temp, by =c("xfips","model_name"),relationship = "many-to-many")
# 
# ctdir=left_join(ctdir, temp, by ="xfips",relationship = "many-to-many")
# glimpse(ctdir)
# dat.county = rbind(dat.county,ctdir)
# glimpse(dat.county)
# 
# spatial_colors = c("#7ad151","darkorange",
#                    "#22a884",
#                    "#2a788e",
#                    "#414487","#440154")
# 
# nonspatial_colors = c("darkorange","#7ad151",
#                       "#22a884",
#                       "#2a788e",
#                       "#414487","#440154")

#scales::show_col(viridis(5))

# for (i in types) {
# {
#   
#   lab = str_to_lower(types) # Converts the entire string to lower case
#   lab = str_replace(lab,' ', '_') 
#   lab = str_replace(lab,': ', '_') 
#   lab = str_replace(lab, " ","")
#   lab = str_replace(lab, "-","")
# # ?relevel
#  t2 =  dat.county %>%
#     mutate(specs = factor(specs),
#            specs = ifelse(type == "Spatial",
#                      forcats::fct_relevel(specs,"direct survey estimate",
#                                             "bym2 age",
#                                             "bym2 edu",
#                                             "rw1 age bym2 edu",
#                                             "rw1 edu bym2 age",
#                                             "rw2 edu bym2 age"),
#                      forcats::fct_relevel(specs,"direct survey estimate",
#                                                    "rw1 age iid xfips",
#                                                    "rw1 edu iid xfips",
#                                                    "rw2 edu iid xfips",
#                                                    "rw1 age rw1 edu iid xfips",
#                                                    "rw1 age rw2 edu iid xfips")))
#   glimpse(t2)


cdccounty = countyvax %>%
  filter(date == "06/30/2021") %>%
  rename(xfips = fips,
         name = county) %>%
  mutate(xfips = as.integer(xfips),
         model_name = "CDC reported count",
         proplw = prop, #for viz
         propup = prop,
         specs = "cdc reported count")

cdccounty2 = cdccounty
cdccounty$type = "Spatial"
cdccounty2$type = "Non-Spatial"
cdccounty = rbind(cdccounty,cdccounty2)
rm(cdccounty2)
cdccounty = cdccounty %>% select(name,colnames(dat.county))

temp = countyvax %>% select(county,fips) %>% rename(name = county) %>% mutate(fips = as.integer(fips))
dat.county = left_join(dat.county,temp, by=c("xfips"="fips"),relationship="many-to-many")
dat.county = dat.county %>% select(colnames(cdccounty))

dat.county =rbind(dat.county,cdccounty)
rm(temp)
gc()

dat.county = dat.county %>%
  mutate(specs = str_replace_all(specs,"xfips","county"))

nonspatial_countylevel_order = c("cdc reported count",
                                "direct survey estimate",
                                "rw1 age iid county",
                                "rw1 edu iid county",
                                "rw2 edu iid county",
                                "rw1 age rw1 edu iid county",
                                "rw1 age rw2 edu iid county")

dat.county = dat.county %>% distinct()

glimpse(dat.county)

dat.county %>%
    filter(
      type == "Non-Spatial") %>% 
  distinct() %>%
    ggplot(aes(x=name, y=prop, color=factor(specs, levels = nonspatial_countylevel_order),group=type)) +
    geom_pointrange(aes(ymin = proplw, ymax = propup),position = position_jitter(width = 0.3, height = NULL, seed = 2)) +
    theme_minimal() +
    labs(x = "County",
         y="Proportion",
         color = "Model") +
    scale_color_manual(values = c("red",
                                  "darkorange",
                                  "#C7E020FF",
                                  "#7ad151",
                                  "#22a884",
                                  "#2a788e",
                                  "#414487",
                                  "#440154")) +
   #scale_y_continuous(limits = c(0.45, 1), breaks = seq(0, 1, by = 0.1)) +
    #scale_linetype_discrete(name = "") +
    theme(axis.text.x = element_text(vjust = 0.5, hjust = 1, angle = 45),
          legend.spacing = unit(1, 'cm'),
          legend.position="bottom") #+
    #scale_color_viridis(discrete = TRUE) +
  #facet_wrap(~type, nrow = 2)
  
  ggsave(file = here::here(paste0("plots/county_level/results_proportion_countylevel_nonspatial.pdf")),dpi = 320, bg = "white", width =20, height =10, unit = "in")

spatial_countylevel_order = c("cdc reported count",
                              "direct survey estimate",
                              #"bym2 county",
                              "bym2 age",
                              "bym2 edu",
                              "rw1 age bym2 edu",
                              "rw1 edu bym2 age",
                              "rw2 edu bym2 age")

#dat.county %>% filter(specs %nin% spatial_countylevel_order) %>% distinct(specs)
dat.county %>%
    filter(
      type == "Spatial"
    ) %>%
    na.omit() %>%
    distinct() %>%
    ggplot(aes(x=name, y=prop, 
               color=factor(specs,levels=spatial_countylevel_order),
               group=type)) +
    geom_pointrange(aes(ymin = proplw, ymax = propup),
                    position = position_jitter(width = 0.3, height = NULL, seed = 2)) +
    theme_minimal() +
    labs(x = "County",
         y="Proportion",
         color = "Model") +
    scale_color_manual(values = c("red",
                                  "darkorange",
                                  "#C7E020FF",
                                  "#7ad151",
                                  "#22a884",
                                  "#2a788e",
                                  "#414487",
                                  "#440154")) +
    #scale_y_continuous(limits = c(0.45, 1), breaks = seq(0, 1, by = 0.1)) +
    #scale_linetype_discrete(name = "") +
    theme(axis.text.x = element_text(vjust = 0.5, hjust = 1, angle = 45),
          legend.spacing = unit(1, 'cm'),
          legend.position="bottom") #+
  #scale_color_viridis(discrete = TRUE) +
  #facet_wrap(~type, nrow = 2)
  
  ggsave(file = here::here(paste0("plots/county_level/results_proportion_countylevel_spatial.pdf")),dpi = 320, bg = "white", width =20, height =10, unit = "in")

joined_order = c(spatial_countylevel_order,nonspatial_countylevel_order)
joined_order = unique(joined_order)

dat.county %>%
    filter(
      !str_detect(specs,"bym2 county")
    )%>%
  na.omit() %>%
    ggplot(aes(x=name, y=prop, 
               color=factor(specs,levels=joined_order),
               group=type)) +
    geom_pointrange(aes(ymin = proplw, ymax = propup),
                    position = position_jitter(width = 0.3, height = NULL, seed = 2)) +
    theme_minimal() +
    labs(x = "County",
         y="Proportion",
         color = "Model") +
    scale_color_manual(values = c("red",
                                  "darkorange",
                                  "#fde725",
                                  "#b5de2b",
                                  "#6ece58",
                                  "#35b779",
                                  "#1f9e89",
                                  "#26828e",
                                  "#31688e",
                                  "#3e4989",
                                  "#482878",
                                  "#440154")) +
    #scale_y_continuous(limits = c(0.45, 1), breaks = seq(0, 1, by = 0.1)) +
    #scale_linetype_discrete(name = "") +
    theme(axis.text.x = element_text(vjust = 0.5, hjust = 1, angle = 45),
          legend.spacing = unit(1, 'cm'),
          legend.position="bottom") + #+
  #scale_color_viridis(discrete = TRUE) +
  facet_wrap(~type, nrow = 2)
  
    
ggsave(file = here(paste0("plots/county_level/results_proportion_countylevel.pdf")),dpi = 320, bg = "white", width =20, height =14, unit = "in")

#save tables so that they can go into the paper

#tidy them up a little
dat.county = dat.county %>%
  mutate(prop = round(prop,3),
         proplw = round(proplw,3),
         propup = round(propup,3)) %>%
  arrange(type,name,model_name) %>%
  select(type,name,model_name,prop,proplw,propup)

dat.state = dat.state %>%
  mutate(prop = round(prop,3),
         proplw = round(proplw,3),
         propup = round(propup,3)) %>%
  arrange(type,model_name)  %>%
  select(type,model_name,prop,proplw,propup)

write.csv(dat.county,here("results/tables/county_regression_results.csv"))
write.csv(dat.state,here("results/tables/state_regression_results.csv"))


