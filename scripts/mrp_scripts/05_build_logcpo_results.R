#### Produce log-cpo summary ####

pacman::p_load(tidyverse,fs,here,janitor,qdapRegex,viridis,data.table,stringr)

options(scipen=999)
here::i_am("scripts/mrp_scripts/05_build_logcpo_results.R")

#load list of models in final analysis
load(file = here("data/cleaned_input_data/model_descriptions.RData"))
models =
  models %>% 
  select(model_name,specs)
rm(model_formulae) #don't need it

# load("data/cleaned_input_data/clean_complete_model_indirect_estimates.rda")
# glimpse(dat)

#get DIC from all models
#Only have to run the one time (joins all the modeled results together) 
filelist <- list.files(
  path = here("data/raw_model_output/model_results/"),
  full.names = TRUE) # include the directory in the result)
filelist
dat = fs::dir_ls(here("/Users/Aja/Documents/R Directories/MRP_Spatial/data/raw_model_output/model_results"), regexp = "\\.csv$")

dat = map_dfr(dat,fread, .id = 'id')

dat = dat %>%
  mutate(sex = as.character(ex_between(id,"sex",".csv")),
         id =  as.character(ex_between(id,"model_results/","sex")))

#save a legible version of the regression results
dat = dat %>% 
  select(id,sex,dic,lcpo,mean:kld) %>% 
  mutate(sex = as.double(sex)) %>%
  distinct()

dat = left_join(dat,models, by = c("id"="specs"))

dat = 
  dat %>% 
  mutate(lcpo = round(lcpo, 2),
        dic = round(dic, 2),
        sex = ifelse(sex == 0, "female","male"),
        mean = round(mean,3),
        sd = round(sd,3),
        `0.025quant` = round(`0.025quant`,3),
        `0.5quant` = round(`0.5quant`,3),
        `0.975quant` = round(`0.975quant`,3),
        mode = round(mode,3))

dat = 
  dat %>%
  mutate(type = case_when(str_detect(id, "bym2") ~ "Spatial",
                          str_detect(id, "iid_xfips|fixed_xfips") ~ "Non-Spatial")) %>%
  select(type,everything(),-id) 

#save regression results for paper before filtering to log-CPO table details
write.csv(dat,here("results/tables/inla_regression_results.csv"))

dat1 = 
  dat %>% 
  select(model_name, sex, lcpo) %>% 
  group_by(sex) %>% 
  distinct() %>% 
  arrange(lcpo)
write.csv(dat1, file = here("results/tables/table3_model_logcpo_results.csv"))

#DIC results 
dat = 
  dat %>% 
  select(model_name, sex, dic) %>% 
  group_by(sex) %>% 
  distinct() %>% 
  arrange(dic)
write.csv(dat, file = here("results/tables/table3b_model_dic_results.csv"))
dat1
