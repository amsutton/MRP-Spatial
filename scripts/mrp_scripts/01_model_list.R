#### Principled Approach to Model Building and Selection ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here)

here::i_am("scripts/mrp_scripts/01_model_list.R")

#this is just a quick, convenient way to think and relabel them
simple_model_list = list(
  
  #### Fixed Effect Model: Sanity-Check Baseline ####
  vax ~ county + age + edu,
  
  #### IID models ####
  vax ~ 1 + age + edu + f(id, iid),
  vax ~ 1 + age*edu + f(id, iid),
  vax ~ 1 + age + f(edu, rw1) + f(id, iid),
  vax ~ 1 + age + f(edu, rw2) + f(id, iid),
  
  
  #### BYM2 Models ####
  vax ~ 1 + age + f(edu, bym2),
  vax ~ 1 + edu + f(age, bym2),
  vax ~ 1 + f(edu, rw1) + f(age, bym2),
  vax ~ 1 + f(edu, rw2) + f(age, bym2)
)

model_names = list()
for (i in 1:length(simple_model_list)){
  model_names[i] = format(simple_model_list[[i]])
}

#view(t(t(model_names)))

#### Intuitively build INLA model data to match formulae ####
model_formulae = list(
  #### Fixed Effect Model: Sanity-Check Baseline ####
  vax ~ id + age + edu,
  
  #### IID models ####
  vax ~ 1 + age + edu + f(id, model="iid", constr = TRUE),
  vax ~ 1 + age*edu + f(id, model="iid", constr = TRUE),
  vax ~ 1 + age + f(edu, model="iid", constr = TRUE) + f(id, model="iid", constr = TRUE),
  vax ~ 1 + age + f(edu, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1))), scale.model=TRUE, constr = TRUE) + f(id, model="iid", constr = TRUE),
  vax ~ 1 + age + f(edu, model = "rw2", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1))), scale.model=TRUE, constr = TRUE) + f(id, model="iid", constr = TRUE),

  #### BYM2 Models ####
  vax ~ 1 + age + f(edu,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + f(edu, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1))), scale.model=TRUE, constr = TRUE) + f(age,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + f(edu, model = "rw2", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1))), scale.model=TRUE, constr = TRUE) + f(age,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01))))

 )


models = as.data.frame(unlist(model_names))
#there are a maximum of 4 covariates, including intercept 1.
models = models %>%
  rename(model = `unlist(model_names)`)

models = models %>%
  separate_wider_delim(model, 
                       delim="+",
                       names = c("c1","c2","c3","c4"),
                       too_few = "align_start") %>% 
  mutate_if(is.character, str_trim)

#possible options:
#f(age,iid),f(edu, iid),f(id,iid),f(edu, ar1),f(edu, rw1),f(edu, rw2),
#f(id, bym2),f(age, bym2),f(edu, bym2), age*edu interaction

models =
  models %>%
  mutate(c2name = case_when(c2 == "id" ~ 'fixed effect on county',
                            c2 == "age" ~ 'fixed effect on age',
                            c2 == "age * edu" ~ 'age-education interaction',
                            c2 == "edu" ~ 'fixed effect on education',
                            c2 == "f(age, iid)" ~ 'IID by age',
                            c2 == "f(edu, iid)" ~ 'IID by education',
                            c2 == "f(id, iid)" ~ 'IID by county',
                            c2 == "f(age, rw1)" ~ 'RW(1) by age',
                            c2 == "f(edu, rw1)" ~ 'RW(1) by education',
                            c2 == "f(edu, rw2)" ~ 'RW(2) by education',
                            c2 == "f(id, bym2)" ~ 'BYM2 by county',
                            c2 == "f(age, bym2)" ~ 'BYM2 by age',
                            c2 == "f(edu, bym2)" ~ 'BYM2 by education',
                            TRUE ~ c2),
         c3name = case_when(c3 == "id" ~ 'fixed effect on county',
                            c3 == "age" ~ 'fixed effect on age',
                            c3 == "edu" ~ 'fixed effect on education',
                            c3 == "f(age, iid)" ~ 'IID by age',
                            c3 == "f(edu, iid)" ~ 'IID by education',
                            c3 == "f(id, iid)" ~ 'IID by county',
                            c3 == "f(age, rw1)" ~ 'RW(1) by age',
                            c3 == "f(edu, rw1)" ~ 'RW(1) by education',
                            c3 == "f(edu, rw2)" ~ 'RW(2) by education',
                            c3 == "f(id, bym2)" ~ 'BYM2 by county',
                            c3 == "f(age, bym2)" ~ 'BYM2 by age',
                            c3 == "f(edu, bym2)" ~ 'BYM2 by education',
                            TRUE ~ c3),
         c4name = case_when(c4 == "id" ~ 'fixed effect on county',
                            c4 == "edu" ~ 'fixed effect on education',
                            c4 == "f(age, iid)" ~ 'IID by age',
                            c4 == "f(edu, iid)" ~ 'IID by education',
                            c4 == "f(id, iid)" ~ 'IID by county',
                            c4 == "f(age, rw1)" ~ 'RW(1) by age',
                            c4 == "f(edu, rw1)" ~ 'RW(1) by education',
                            c4 == "f(edu, rw2)" ~ 'RW(2) by education',
                            c4 == "f(id, bym2)" ~ 'BYM2 by county',
                            c4 == "f(age, bym2)" ~ 'BYM2 by age',
                            c4 == "f(edu, bym2)" ~ 'BYM2 by education',
                            TRUE ~ c4),
         c2specs = case_when(c2 == "id" ~ 'fixed_xfips',
                             c2 == "age" ~ 'fixed_age',
                             c2 == "edu" ~ 'fixed_edu',
                             c2 == "f(age, iid)" ~ 'iid_age',
                             c2 == "f(edu, iid)" ~ 'iid_edu',
                             c2 == "f(id, iid)" ~ 'iid_xfips',
                             c2 == "f(age, rw1)" ~ 'rw1_age',
                             c2 == "f(age, rw1)" ~ 'rw1_age',
                             c2 == "f(edu, rw1)" ~ 'rw1_edu',
                             c2 == "f(edu, rw2)" ~ 'rw2_edu',
                             c2 == "f(id, bym2)" ~ 'bym2_xfips',
                             c2 == "f(age, bym2)" ~ 'bym2_age',
                             c2 == "f(edu, bym2)" ~ 'bym2_edu',
                             TRUE ~ c2),
         c3specs = case_when(c3 == "id" ~ 'fixed_xfips',
                             c3 == "edu" ~ 'fixed_edu',
                             c3 == "f(age, iid)" ~ 'iid_age',
                             c3 == "f(edu, iid)" ~ 'iid_edu',
                             c3 == "f(id, iid)" ~ 'iid_xfips',
                             c3 == "f(age, rw1)" ~ 'rw1_age',
                             c3 == "f(edu, rw1)" ~ 'rw1_edu',
                             c3 == "f(edu, rw2)" ~ 'rw2_edu',
                             c3 == "f(id, bym2)" ~ 'bym2_xfips',
                             c3 == "f(age, bym2)" ~ 'bym2_age',
                             c3 == "f(edu, bym2)" ~ 'bym2_edu',
                             TRUE ~ c3),
         c4specs = case_when(c4 == "id" ~ 'fixed_xfips',
                             c4 == "f(age, iid)" ~ 'iid_age',
                             c4 == "f(edu, iid)" ~ 'iid_edu',
                             c4 == "f(id, iid)" ~ 'iid_xfips',
                             c4 == "f(age, rw1)" ~ 'rw1_age',
                             c4 == "f(edu, rw1)" ~ 'rw1_edu',
                             c4 == "f(edu, rw2)" ~ 'rw2_edu',
                             c4 == "f(id, bym2)" ~ 'bym2_xfips',
                             c4 == "f(age, bym2)" ~ 'bym2_age',
                             c4 == "f(edu, bym2)" ~ 'bym2_edu',
                             TRUE ~ c4),
         model_name = case_when(
           !is.na(c2) & !is.na(c3) & !is.na(c4) ~ paste(c2name,c3name,c4name,sep= ", "),
           !is.na(c2) & !is.na(c3) & is.na(c4) ~ paste(c2name,c3name,sep= ", "),
           !is.na(c2) & is.na(c3) & is.na(c4) ~ paste(c2name,sep= ", ")
         ),
         specs = case_when(
           !is.na(c2) & !is.na(c3) & !is.na(c4) ~ paste(c2specs,c3specs,c4specs,sep= "_"),
           !is.na(c2) & !is.na(c3) & is.na(c4) ~ paste(c2specs,c3specs,sep= "_"),
           !is.na(c2) & is.na(c3) & is.na(c4) ~ paste(c2specs,sep= "_")
         )
  )

#create some quick verification columns to help with poststratification data filtering
models = models %>%
  mutate(age_check = ifelse(str_detect(model_name, "age"), TRUE, FALSE),
         edu_check = ifelse(str_detect(model_name, "education"),TRUE,FALSE),
         id_check = case_when(str_detect(model_name, "county") ~ TRUE,
                              str_detect(model_name, "BYM2") ~ TRUE,
                              TRUE ~ FALSE)
  )

#add poststrat_vars column for ease
models = models %>%
  mutate(poststrat_vars = case_when(age_check == TRUE & edu_check == TRUE & id_check == TRUE ~  '"xfips","age","edu"',
                                    age_check == TRUE & edu_check == TRUE & id_check == TRUE ~ '"xfips","age","edu"',
                                    age_check == TRUE & edu_check == FALSE & id_check == TRUE ~ '"xfips","age"',
                                    age_check == FALSE & edu_check == TRUE & id_check == TRUE ~ '"xfips","edu"',
                                    age_check == TRUE & edu_check == TRUE & id_check == FALSE ~ '"age","edu"',
                                    age_check == TRUE & edu_check == FALSE & id_check == FALSE ~ '"age"',
                                    age_check == FALSE & edu_check == TRUE & id_check == FALSE ~ '"edu"'))


models = models %>%
  select(model_name,specs,age_check,edu_check,id_check,poststrat_vars)
models[1,1] = "fixed effect on county, fixed effect on age, fixed effect on education"
models[1,2] = "fixed_xfips_fixed_age_fixed_edu"
models$poststrat_vars[1] = "\"xfips\",\"age\",\"edu\""

save(models,model_formulae, file = here("data/cleaned_input_data/model_descriptions.RData"))
#load(file = here("data/cleaned_input_data/model_descriptions.RData"))
