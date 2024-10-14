#### Produce DIC summary ####

pacman::p_load(tidyverse,fs,here,janitor,qdapRegex,viridis,data.table,stringr)

options(scipen=999)
here::i_am("scripts/05_build_dic_results.R")

#load list of models in final analysis
load(file = here("data/cleaned_input_data/model_descriptions.RData"))
models = models %>% filter(!str_detect(specs,"fixed")) %>% select(model_name,specs)
rm(model_formulae) #don't need it

# load("data/cleaned_input_data/clean_complete_model_indirect_estimates.rda")
# glimpse(dat)

#get DIC from all models
#Only have to run the one time (joins all the modeled results together) 
filelist <- list.files(
  path = here("data/raw_model_output/model_results/"),
  full.names = TRUE) # include the directory in the result)

dat = fs::dir_ls(here("/Users/Aja/Documents/R Directories/MRP_Spatial/data/raw_model_output/model_results"), regexp = "\\.csv$")

dat = map_dfr(dat,fread, .id = 'id')

dat = dat %>%
  mutate(sex = as.character(ex_between(id,"sex",".csv")),
          id =  as.character(ex_between(id,"model_results/","sex"))) %>%
filter(!str_detect(variable,"fixed"))

dat = dat %>% 
  select(id,sex,dic) %>% 
  mutate(sex = as.double(sex)) %>%
  filter(!str_detect(id,"fixed")) %>%
  distinct()

dat = left_join(dat,models, by = c("id"="specs"))

dat = dat %>% mutate(dic = round(dic, 2),
                     sex = ifelse(sex == 0, "female","male"))

dat = dat %>% select(model_name, sex, dic, id) %>% arrange(sex,dic)
dat %>% filter(!str_detect(id,"bym2_xfips")) %>% view

write.csv(dat, file = here("data/Table_model_dic_results.csv"))
