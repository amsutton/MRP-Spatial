#### MRP + Spatial Priors using CTIS Data ####

#Build a series of models representing vaccination probability (min. 1 dose)
#For June 12, 2021 using the COVID-19 Trends and Impacts Survey (CTIS)

#Post-stratification of age/sex/education at county level, according to what is in the model

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, INLA, SUMMER,data.table,beepr)

here::i_am("scripts/03_run_models.R")


load_data = function(poststrat_vars){

  #load geo, dat, spatial graph, and post for analysis
  load(file = here("data/cleaned_input_data/clean_data_county_with_edu.rda"))
  load(file = here("data/cleaned_input_data/clean_postrat_age_sex_county_with_edu.rda"))

  dat <<- dat
  
  if (str_detect(poststrat_vars,"xfips")) {
    load(file=here("data/cleaned_input_data/ca_geo_sf_county.rda"))
    load(file = here("data/cleaned_input_data/spatialgraph.rda"))
    geo <<- geo
    g.graph <<- g.graph
    
  }

  post <<- post %>%
    rename(xfips = fips)
  
  state_choice <<- "CA" #for pulling data from the US Census API with tidycensus

}

prepare_data = function(dat,post,poststrat_vars){
  
  #if there are poststratification categories missing, it means that
  #there isn't data for these in the Census (it's a zero/censored category)
  
  require(tidyverse,sf)
  
  if (poststrat_vars == "xfips,age,edu") {
  
    print("poststrat_vars == xfips,age,edu")
    dat = dat %>% 
      #filter(sex == models$sex) %>%
      select(xfips,vax,sex,age,edu) 
    
    post = post %>% 
      #filter(sex == models$sex) %>% 
      group_by(xfips,sex,age,edu) %>%
      reframe(
        vax = NA,
        estimate = sum(estimate)) %>%
      distinct() %>%
      ungroup() %>%
      select(vax,everything())
  
    pred_dat = post %>% select(colnames(dat))
    #add the empty prediction categories to the bottom of the real data
    dat = rbind(dat, pred_dat)
    
    index = geo %>% select(xfips, id) %>% sf::st_drop_geometry()
    
    dat = left_join(dat, index, by =c("xfips"))
    
    dat <<- dat
    post <<- post
    pred_dat <<- pred_dat
    
  }
  
  if (poststrat_vars == "xfips,age") {
    
    print("poststrat_vars == xfips,age")
    
    dat = dat %>% 
      #filter(sex == models$sex) %>%
      select(xfips,vax,sex,age) %>%
      distinct()
  
    post = post %>% 
      #filter(sex == models$sex) %>% 
      select(-edu) %>%
      group_by(xfips,sex,age) %>%
      reframe(
        vax = NA,
        estimate = sum(estimate)) %>%
      distinct() %>%
      ungroup() %>%
      select(xfips,sex,vax,age,estimate)
    
    pred_dat = post %>% select(colnames(dat))
    
    #add the empty prediction categories to the bottom of the real data
    dat = rbind(dat, pred_dat)
    
    index = geo %>% select(xfips, id) %>% sf::st_drop_geometry()
    
    dat = left_join(dat, index, by =c("xfips"))
    
    dat <<- dat
    post <<- post
    pred_dat <<- pred_dat
  }
  
  if (poststrat_vars == "xfips,edu") {
    
    print("poststrat_vars == xfips,edu")
   
    dat = dat %>% 
      #filter(sex == models$sex) %>%
      select(xfips,vax,sex,edu) %>%
      distinct()
    
    post = post %>% 
      #filter(sex == models$sex) %>% 
      select(-age) %>%
      group_by(xfips,sex,edu) %>%
      reframe(
        vax = NA,
        estimate = sum(estimate)) %>%
      distinct() %>%
      ungroup() %>%
      select(xfips,sex,vax,edu,estimate)
    
    pred_dat = post %>% select(colnames(dat))
    
    #add the empty prediction categories to the bottom of the real data
    dat = rbind(dat, pred_dat)
    
    index = geo %>% select(xfips, id) %>% sf::st_drop_geometry()
    
    dat = left_join(dat, index, by =c("xfips"))
    
    dat <<- dat
    post <<- post
    pred_dat <<- pred_dat
  }
  
  if (poststrat_vars == "age,edu") {
    
    print("poststrat_vars == age,edu")
    
    dat = dat %>% 
      #filter(sex == models$sex) %>%
      select(vax,sex,age,edu) 
        
    post = post %>% 
      #filter(sex == models$sex) %>% 
      select(-xfips) %>%
      group_by(sex,age,edu) %>%
      reframe(
        vax = NA,
        estimate = sum(estimate)) %>%
      distinct() %>%
      ungroup() %>%
      select(vax,everything())
   
    pred_dat = post %>% select(colnames(dat))
   
    #add the empty prediction categories to the bottom of the real data
    dat = rbind(dat, pred_dat)

    dat <<- dat
    post <<- post
    pred_dat <<- pred_dat
    
  }
  
  if (poststrat_vars == "edu") {
    
    print("poststrat_vars == edu")
    
    dat = dat %>% 
      #filter(sex == models$sex) %>%
      select(vax,sex,edu) %>%
      distinct()
    
    post = post %>% 
      #filter(sex == models$sex) %>% 
      select(-xfips, -age) %>%
      group_by(sex,edu) %>%
      reframe(
        vax = NA,
        estimate = sum(estimate)) %>%
      distinct() %>%
      ungroup() %>%
      select(vax,everything())
    
    pred_dat = post %>% select(colnames(dat))
    
    #add the empty prediction categories to the bottom of the real data
    dat = rbind(dat, pred_dat)
    
    dat <<- dat
    post <<- post
    pred_dat <<- pred_dat
  }
  
  if (poststrat_vars == "age") {
    
    print("poststrat_vars == age")
    
    dat = dat %>% 
      #filter(sex == models$sex) %>%
      select(vax,sex,age) %>%
      distinct()
    
    post = post %>% 
      #filter(sex == models$sex) %>% 
      select(-xfips, -edu) %>%
      group_by(sex,age) %>%
      reframe(
        vax = NA,
        estimate = sum(estimate)) %>%
      distinct() %>%
      ungroup() %>%
      select(vax,everything())
    
    pred_dat = post %>% select(colnames(dat))
    
    #add the empty prediction categories to the bottom of the real data
    dat = rbind(dat, pred_dat)

    dat <<- dat
    post <<- post
    pred_dat <<- pred_dat
  }

}

run_save_model = function(mf,dat,sex_spec) {

  require(tidyverse,INLA,data.table)
  
  sim_start = length(mf)+2 #number of covariates + number of outcome variables (1) + 1
  
  #NB: For a spatial model to work, the formula MUST refer to the "id"
  #built into the spatial graph, NOT a code like xfips. Later, we can join on
  #xfips, but this will not work without referring to the graph id!
  model <<- inla(mf, 
               data = dat, 
               family="betabinomial", 
               Ntrials = 1,
               control.family=list(link='logit'),
               control.compute = list(config=TRUE,
                                      return.marginals.predictor=TRUE,
                                      dic = TRUE),
               control.predictor = list(compute=TRUE), #would need expit it
               verbose = FALSE)
  
  
  ###Not sure about this code chunk
  #if (str_detect(as.character(mf), "age +")) {
    model.fixed = as.data.frame(model$summary.fixed)
    model.fixed = model.fixed %>%
      mutate(variable = "fixed",
             id = "intercept") %>%
      select(variable, id, everything())
  #}
  
  #extract random effects
  if (any(str_detect(as.character(mf), "age, model"))) {
  model.random.age = model$summary.random$age
  model.random.age = model.random.age %>%
    mutate(variable = "random",
           ID = as.character(ID),
           id = paste(poststrat_vars[1], ID, sep = " ")) %>%
    select(-ID)
  } else {
    model.random.age = data.frame(variable = c(),
                                  ID = c(),
                                  id = c())
  }
    
  
  if (any(str_detect(as.character(mf), "edu, model"))) {
    model.random.edu = model$summary.random$edu
    model.random.edu = model.random.edu %>%
      mutate(variable = "random",
             ID = as.character(ID),
             id = paste(poststrat_vars[1], ID, sep = " ")) %>%
      select(-ID)
  } else {
    model.random.edu = data.frame(variable = c(),
                                  ID = c(),
                                  id = c())
  }
    
  
  if (any(str_detect(as.character(mf), "xfips, model"))) {
  model.random.xfips = model$summary.random$id
  model.random.xfips = model.random.xfips %>%
    mutate(variable = "random",
           ID = as.character(ID),
           id = paste(poststrat_vars[2],ID, sep = " ")) %>%
    select(-ID)
  } else {
    model.random.xfips = data.frame(variable = c(),
                                  ID = c(),
                                  id = c())
  }
    
  
  model.results = rbind(model.fixed, model.random.age,model.random.edu,model.random.xfips)
  model.results$dic = model$dic$dic
  
  rownames(model.results) = NULL
  
  fwrite(model.results, file = here(paste0("data/raw_model_output/model_results/",specs,"sex",sex_spec,".csv")))
  
  }

posterior_draw = function(model,post,poststrat_vars,pred_dat,dat,sex_spec) {

  #take a thousand draws of the posterior of the INLA model,
  #extract the joint posterior distribution draws for each
  #stratum in the poststratification matrix given as an row-bound
  #object affixed to the survey data in the model (used to predict Y
  #for all strata in all areas), determine the posterior median probabilities,
  #and credibility intervals, and calculate the small area estimates for each
  #stratum in each area based on these summary estimates of the 
  #posterior probabilities.
  
  #model == fitted INLA model
  #sim_start == the number of covariates + 1 (assures the draws only pertain
  #to the simulated strata, and skips the predictor variables' draws)
  #dat == the data given to the model as built in
  #2023-02_ctis_mrp_vaccination_with_sex_specific_models.R
  #post == the poststratification df as built in 
  #2023-02_ctis_mrp_vaccination_with_sex_specific_models.R 
  
  nsim <- 1000
  sim <- inla.posterior.sample(n=nsim, result=model)
  
  icar_index <- grep("Predictor", rownames(sim[[1]]$latent))
  
  #start with an empty list; 
  #make a list of lists of each #latent result, 
  #and expit() it (b/c model is on logit scale)
  sims <- list()
  for (i in 1:nsim){
    sims[[i]] <- as.vector(expit(sim[[i]]$latent))
  }
  sims <- t(do.call(rbind, sims))
  sims <- as.data.frame(sims)
  
  #filter to Predictors, which again are the observations 
  #(survey data + PS variables to use for prediction of outcome)
  sims <- sims[1:max(icar_index),]
  colnames(sims) <- NULL
  colnames(sims)[1:ncol(sims)] <- paste("sim_",1:nsim,sep="")
  sims <- cbind(dat, sims)
  
  #leave sims for predictions from post-strat data only -- 
  #get rid of sims for real survey data -- i.e, observations where vax is NA
  sims = sims %>% filter(is.na(vax))

  # Now poststratify
  
  #sex is an implicit poststratification variable, so we add it here for ease:
  poststrat_vars = paste0("sex,",poststrat_vars)
  post <- post %>% select(-vax)
  sims <- left_join(sims, post, by = c(strsplit(poststrat_vars,split = ",")[[1]]))

  if("xfips" %in% colnames(sims)){
  sims = sims %>% 
    group_by(xfips) %>%
    distinct() %>%
    mutate(N = sum(estimate, na.rm = TRUE),
           N= replace_na(N,0)) %>%
    ungroup() %>%  #ditto -- can't have zeroes for mrp; handles NAs introduced by estimate/zcta_pop; in this case, NA == zero for pop counts
    select(colnames(dat), estimate, everything())
  }

  `%nin%` = Negate(`%in%`)
  if("xfips" %nin% colnames(sims)){
    sims = sims %>% 
      distinct() %>%
      mutate(N = sum(estimate, na.rm = TRUE),
             N= replace_na(N,0)) %>%
      ungroup() %>%  #ditto -- can't have zeroes for mrp; handles NAs introduced by estimate/zcta_pop; in this case, NA == zero for pop counts
      select(colnames(dat), estimate, everything())
  }
 
  save(sims, file = here::here("data","raw_model_output", "sims", paste0(state_choice,"_vaccination_",specs,"sex",sex_spec,"_sims.rda")))

  sims <<- sims 

}

poststratify = function(sims,pred_dat,sex_spec){
  #begin post-stratification: multiply the xfips_pop by the sims across each row 
  #(remember: each row is an observation in the PS table)
  options(scipen=999)

  temp = pred_dat %>% select(-vax) 
  colnames(temp)
  #mean sim estimation * estimate in stratum /estimate in zcta
  rowmedians <- sims %>%
    group_by(across(colnames(temp))) %>%
    pivot_longer(cols = sim_1:sim_1000, 
                 names_to = "sim", 
                 values_to = "sim_prob")  %>%
    ungroup()

  rowmedians <- rowmedians %>%
    mutate(weight = estimate/N,
           ps_prob = sim_prob * weight) %>% #poststrat prob = posterior prob * proportion of pop in strata
    group_by(across(colnames(temp))) %>% 
    mutate(median_ps_prob = median(ps_prob),
           lw_ps_prob = quantile(ps_prob, probs = c(0.025)),
           up_ps_prob = quantile(ps_prob, probs = c(0.975))) %>% 
    dplyr::select(colnames(temp), median_ps_prob, estimate, 
                  N, lw_ps_prob, up_ps_prob) %>%
    distinct() %>%
    ungroup() %>%
    rename(strata_pop = estimate)
  
  rowmedians <- rowmedians %>%
    mutate(strata_pop_est = N * median_ps_prob,
           strata_pop_lw.ci = N * lw_ps_prob,
           strata_pop_up.ci = N * up_ps_prob) %>% 
    group_by(across(colnames(temp))) %>%
    mutate(sae_estimate = as.integer(sum(strata_pop_est)),
           sae_lw.ci = as.integer(sum(strata_pop_lw.ci)),
           sae_up.ci = as.integer(sum(strata_pop_up.ci))) %>%
    ungroup() %>%
    distinct() %>%
    mutate(model = specs)
  
  
  fwrite(rowmedians, file = here(paste0("data/indirect_estimates/sex_specific_estimates/",state_choice,"_mrp_indirect_estimates_vax_",specs,"_sex",sex_spec,".csv")))
  
  rowmedians <<- rowmedians
  
}

run_sex_specific_steps = function(dat,post,mf,poststrat_vars,pred_dat,sex_spec,x) {

  dat = dat %>%
    filter(sex == sex_spec)
  
  post = post %>%
    filter(sex == sex_spec)
  
  pred_dat = pred_dat %>%
    filter(sex == sex_spec)
  
  #warning about "identity link will be used to compute the fitted values 
  #for NA data" is fine. We leave warnings verbose to catch other issues.
  run_save_model(mf,dat,sex_spec)
  print(paste0("model ",x,": done running and saving"))
  posterior_draw(model,post,poststrat_vars,pred_dat,dat,sex_spec)
  print(paste0("model ",x,": done posterior draw"))
  poststratify(sims,pred_dat,sex_spec)
  print(paste0("model ",x,": done poststratifying"))
  
}

run_analysis = function(model_formulae,mod,specs,state_choice,poststrat_vars) {

    prepare_data(dat,post,poststrat_vars)
    print(paste0("model ",x,": done preparing data"))
    #build and run sex-specific models, run poststrat, and save the output
    sex_list = list(1,0)
    
      for (i in 1:length(sex_list)){
        sex_spec = as.integer(sex_list[i])
        run_sex_specific_steps(dat,post,mf,poststrat_vars,pred_dat,sex_spec,x) 
        if (sex_spec == 1){
          rowmedians1 <- rowmedians
          }
      }
    
    estimates = rbind(rowmedians1,rowmedians)
    
    fwrite(estimates,file=here(paste0("data/indirect_estimates/complete_estimates_both_sexes/",state_choice,"_mrp_indirect_estimates_vax_",specs,"_bothsexes.csv")))

  }


#### Run Full Analysis ####
#there are 45 uniquely described models, not considering running it for 
#both sexes (90 models to run total)
#
#because of the list of formulae and the df of model data, 
#it's simplest to for-loop


#contains two data objects that are ordered meaningfully to represent 
#the same model by list/row index (i.e., model_formulae[[1]] = models[1,]):
#1. a list of formulae (`model_formulae`), 
#and
#2. a dataframe of model information (`models`) 
load(file = here("data/cleaned_input_data/model_descriptions.RData"))

#clean up
models$poststrat_vars = 
  models$poststrat_vars %>% 
  gsub("\"\"", "", ., fixed=TRUE)


#Warning: this is a bit memory intensive as written.
#NB: each model runs twice because we run each model for each sex independently.
for (x in 1:nrow(models)) {

  print(paste("***","model",x,"***",sep=" "))
  print(Sys.time())
  start.time <- Sys.time()
  
  mod = models[,]
  mf = as.formula(model_formulae[[x]])
  specs = as.character(models$specs[x])
  poststrat_vars =
    models$poststrat_vars[x] %>%
    gsub('\"',"",.)

  #reload the clean data (omit geographic data if not a spatial model).
  #takes maybe half an hour to run depending on your computer.
  load_data(poststrat_vars)
  
  run_analysis(mf,mod,specs,state_choice,poststrat_vars)
  
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time,2)
  print(paste0("execution time:",time.taken))
  rm(list=c())
  gc()
}

#done running when you hear:
beepr::beep(3)





