#### Principled Approach to Model Building and Selection ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here)

here::i_am("scripts/01_model_list.R")

simple_model_list = list(
  #### IID models ####
  #this is just a quick, convenient way to think and relabel them
  #random effects
  vax ~ 1 + f(age, iid),
  vax ~ 1 + f(edu, iid),
  vax ~ 1 + f(age, iid) + f(edu, iid),
  #mixed effects
  vax ~ 1 + age + f(id,iid),
  vax ~ 1 + edu + f(id,iid),
  vax ~ 1 + age + edu + f(id, iid),
  
  #### AR models ####
  #random effects
  vax ~ 1 + f(age, ar1),
  vax ~ 1 + f(edu, ar1),
  vax ~ 1 + f(age, ar1) + f(edu, ar1),
  
  #mixed effects
  vax ~ 1 + edu + f(age, ar1),
  vax ~ 1 + age + f(edu, ar1),
  
  
  #### RW models ####
  #random effects
  vax ~ 1 + f(age, rw1),
  vax ~ 1 + f(edu, rw1),
  vax ~ 1 + f(age, rw1) + f(edu, rw1),
  vax ~ 1 + f(edu, rw2),
  vax ~ 1 + f(age, rw1) + f(edu, rw2),
  
  #mixed effects
  vax ~ 1 + edu + f(age, rw1),
  vax ~ 1 + age + f(edu, rw1),
  vax ~ 1 + age + f(edu, rw2),
  
  #### RW with IID on id Models ####
  #random effects
  vax ~ 1 + f(age, rw1) + f(id, iid),
  vax ~ 1 + f(edu, rw1) + f(id, iid),
  vax ~ 1 + f(age, rw1) + f(edu, rw1) + f(id, iid),
  vax ~ 1 + f(edu, rw2) + f(id, iid),
  vax ~ 1 + f(age, rw1) + f(edu, rw2) + f(id, iid),
  
  #mixed effects
  vax ~ 1 + edu + f(age, rw1) + f(id, iid),
  vax ~ 1 + age + f(edu, rw1) + f(id, iid),
  vax ~ 1 + age + f(edu, rw2) + f(id, iid),
  
  
  #### RW with BYM2 on id Models ####
  #random effects
  vax ~ 1 + f(age, rw1) + f(id, bym2),
  vax ~ 1 + f(edu, rw1) + f(id, bym2),
  vax ~ 1 + f(age, rw1) + f(edu, rw1) + f(id, bym2),
  vax ~ 1 + f(edu, rw2) + f(id, bym2),
  vax ~ 1 + f(age, rw1) + f(edu, rw2) + f(id, bym2),
  
  #mixed effects
  vax ~ 1 + edu + f(age, rw1) + f(id, bym2),
  vax ~ 1 + age + f(edu, rw1) + f(id, bym2),
  vax ~ 1 + age + f(edu, rw2) + f(id, bym2),
  
  
  #### RW with BYM2 on age or edu Models ####
  #random effects
  vax ~ 1 + f(age, bym2),
  vax ~ 1 + f(edu, bym2),
  vax ~ 1 + f(age, rw1) + f(edu, bym2),
  vax ~ 1 + f(edu, rw1) + f(age, bym2),
  vax ~ 1 + f(edu, rw2) + f(age, bym2),
  
  #fixed effects
  vax ~ 1 + edu + f(age, bym2),
  vax ~ 1 + age + f(edu, bym2)
)

model_names = list()
for (i in 1:length(simple_model_list)){
  model_names[i] = format(simple_model_list[[i]])
}



#### Intuitively build INLA model data to match formulae ####
model_formulae = list(
  #### IID models ####
  #random effects
  vax ~ 1 + f(age, model="iid", constr = TRUE),
  vax ~ 1 + f(edu, model="iid", constr = TRUE),
  vax ~ 1 + f(age, model="iid", constr = TRUE) + f(edu, model="iid", constr = TRUE),
  #mixed effects
  vax ~ 1 + age + f(id, model="iid", constr = TRUE),
  vax ~ 1 + edu + f(id, model="iid", constr = TRUE),
  vax ~ 1 + age + edu + f(id, model="iid", constr = TRUE),
  
  #### AR models ####
  #random effects
  vax ~ 1 + f(age, model = "ar1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  vax ~ 1 + f(edu, model = "ar1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  vax ~ 1 + f(age, model = "ar1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(edu, model = "ar1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  
  #mixed effects
  vax ~ 1 + edu + f(age, model = "ar1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  vax ~ 1 + age + f(edu, model = "ar1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  
  
  #### RW models ####
  #random effects
  vax ~ 1 + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  vax ~ 1 + f(edu, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  vax ~ 1 + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(edu, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  vax ~ 1 + f(edu, model = "rw2", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  vax ~ 1 + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(edu, model = "rw2", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  
  #mixed effects
  vax ~ 1 + edu + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  vax ~ 1 + age + f(edu, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  vax ~ 1 + age + f(edu, model = "rw2", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))),
  
  #### RW with IID on id Models ####
  #random effects
  vax ~ 1 + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id, model="iid", constr = TRUE),
  vax ~ 1 + f(edu, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id, model="iid", constr = TRUE),
  vax ~ 1 + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(edu, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id, model="iid", constr = TRUE),
  vax ~ 1 + f(edu, model = "rw2", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id, model="iid", constr = TRUE),
  vax ~ 1 + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(edu, model = "rw2", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id, model="iid", constr = TRUE),
  
  #mixed effects
  vax ~ 1 + edu + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id, model="iid", constr = TRUE),
  vax ~ 1 + age + f(edu, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id, model="iid", constr = TRUE),
  vax ~ 1 + age + f(edu, model = "rw2", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id, model="iid", constr = TRUE),
  
  
  #### RW with BYM2 on id Models ####
  #random effects
  vax ~ 1 + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + f(edu, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(edu, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + f(edu, model = "rw2", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(edu, model = "rw2", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  
  #mixed effects
  vax ~ 1 + edu + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + age + f(edu, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + age + f(edu, model = "rw2", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(id,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  
  
  #### RW with BYM2 on age or edu Models ####
  #random effects
  vax ~ 1 + f(age,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + f(edu,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + f(age, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(edu,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + f(edu, model = "rw1", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(age,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + f(edu, model = "rw2", hyper = list(prec = list(prior="pc.prec", param=c(1, 0.1)))) + f(age,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  
  #fixed effects
  vax ~ 1 + edu + f(age,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))),
  vax ~ 1 + age + f(edu,model="bym2", graph= g.graph, scale.model=TRUE, constr = TRUE, hyper = list(phi = list(prior = "pc", param = c(0.5, 2/3)), prec = list(prior = "pc.prec", param = c(1, 0.01))))
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
#f(age,iid),f(edu, iid),f(id,iid),f(age, ar1),f(edu, ar1),f(age, rw1),
#f(edu, rw1),f(edu, rw2),f(id, bym2),f(age, bym2),f(edu, bym2)
#
models =
  models %>%
  mutate(c2name = case_when(c2 == "age" ~ 'age (fixed effect)',
                            c2 == "edu" ~ 'education (fixed effect)',
                            c2 == "f(age, iid)" ~ 'age (IID random effect)',
                            c2 == "f(edu, iid)" ~ 'education (IID random effect)',
                            c2 == "f(id, iid)" ~ 'county (IID random effect)',
                            c2 == "f(age, ar1)" ~ 'age (AR(1) random effect)',
                            c2 == "f(edu, ar1)" ~ 'education (AR(1) random effect)',
                            c2 == "f(age, rw1)" ~ 'age (RW(1) random effect)',
                            c2 == "f(edu, rw1)" ~ 'education (RW(1) random effect)',
                            c2 == "f(edu, rw2)" ~ 'education (RW(2) random effect)',
                            c2 == "f(id, bym2)" ~ 'county ((BYM2) random effect)',
                            c2 == "f(age, bym2)" ~ 'age ((BYM2) random effect)',
                            c2 == "f(edu, bym2)" ~ 'education ((BYM2) random effect)',
                            TRUE ~ c2),
         c3name = case_when(c3 == "age" ~ 'age (fixed effect)',
                            c3 == "edu" ~ 'education (fixed effect)',
                            c3 == "f(age, iid)" ~ 'age (IID random effect)',
                            c3 == "f(edu, iid)" ~ 'education (IID random effect)',
                            c3 == "f(id, iid)" ~ 'county (IID random effect)',
                            c3 == "f(age, ar1)" ~ 'age (AR(1) random effect)',
                            c3 == "f(edu, ar1)" ~ 'education (AR(1) random effect)',
                            c3 == "f(age, rw1)" ~ 'age (RW(1) random effect)',
                            c3 == "f(edu, rw1)" ~ 'education (RW(1) random effect)',
                            c3 == "f(edu, rw2)" ~ 'education (RW(2) random effect)',
                            c3 == "f(id, bym2)" ~ 'county ((BYM2) random effect)',
                            c3 == "f(age, bym2)" ~ 'age ((BYM2) random effect)',
                            c3 == "f(edu, bym2)" ~ 'education ((BYM2) random effect)',
                            TRUE ~ c3),
         c4name = case_when(c4 == "f(age, iid)" ~ 'age (IID random effect)',
                            c4 == "f(edu, iid)" ~ 'education (IID random effect)',
                            c4 == "f(id, iid)" ~ 'county (IID random effect)',
                            c4 == "f(age, ar1)" ~ 'age (AR(1) random effect)',
                            c4 == "f(edu, ar1)" ~ 'education (AR(1) random effect)',
                            c4 == "f(age, rw1)" ~ 'age (RW(1) random effect)',
                            c4 == "f(edu, rw1)" ~ 'education (RW(1) random effect)',
                            c4 == "f(edu, rw2)" ~ 'education (RW(2) random effect)',
                            c4 == "f(id, bym2)" ~ 'county ((BYM2) random effect)',
                            c4 == "f(age, bym2)" ~ 'age ((BYM2) random effect)',
                            c4 == "f(edu, bym2)" ~ 'education ((BYM2) random effect)',
                            c4 == "edu" ~ "education (fixed effect)",
                            TRUE ~ c4),
         c2specs = case_when(c2 == "age" ~ 'fixed_age',
                            c2 == "edu" ~ 'fixed_edu',
                            c2 == "f(age, iid)" ~ 'iid_age',
                            c2 == "f(edu, iid)" ~ 'iid_edu',
                            c2 == "f(id, iid)" ~ 'iid_xfips',
                            c2 == "f(age, ar1)" ~ 'ar1_age',
                            c2 == "f(edu, ar1)" ~ 'ar1_edu',
                            c2 == "f(age, rw1)" ~ 'rw1_age',
                            c2 == "f(age, rw1)" ~ 'rw1_age',
                            c2 == "f(edu, rw1)" ~ 'rw1_edu',
                            c2 == "f(edu, rw2)" ~ 'rw2_edu',
                            c2 == "f(id, bym2)" ~ 'bym2_xfips',
                            c2 == "f(age, bym2)" ~ 'bym2_age',
                            c2 == "f(edu, bym2)" ~ 'bym2_edu',
                            TRUE ~ c2),
         c3specs = case_when(c3 == "edu" ~ 'fixed_edu',
                             c3 == "f(age, iid)" ~ 'iid_age',
                             c3 == "f(edu, iid)" ~ 'iid_edu',
                             c3 == "f(id, iid)" ~ 'iid_xfips',
                             c3 == "f(age, ar1)" ~ 'ar1_age',
                             c3 == "f(edu, ar1)" ~ 'ar1_edu',
                             c3 == "f(age, rw1)" ~ 'rw1_age',
                             c3 == "f(edu, rw1)" ~ 'rw1_edu',
                             c3 == "f(edu, rw2)" ~ 'rw2_edu',
                             c3 == "f(id, bym2)" ~ 'bym2_xfips',
                             c3 == "f(age, bym2)" ~ 'bym2_age',
                             c3 == "f(edu, bym2)" ~ 'bym2_edu',
                            TRUE ~ c3),
         c4specs = case_when(c4 == "f(age, iid)" ~ 'iid_age',
                             c4 == "f(edu, iid)" ~ 'iid_edu',
                             c4 == "f(id, iid)" ~ 'iid_xfips',
                             c4 == "f(age, ar1)" ~ 'ar1_age',
                             c4 == "f(edu, ar1)" ~ 'ar1_edu',
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


save(models,model_formulae, file = here("data/cleaned_input_data/model_descriptions.RData"))

