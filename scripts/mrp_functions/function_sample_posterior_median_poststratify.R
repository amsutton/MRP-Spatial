#### Function: Sample from the joint posterior, determine median, poststratify ####


sample_posterior_median_poststratify <- function(model,sim_start, dat, post, state_choice, specs) {
    
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
  
    require(tidyverse)
    require(INLA)
  model <- f.rw1.age

    nsim <- 1000
    sim <- inla.posterior.sample(n=nsim, result=model)
    
    icar_index <- grep("Predictor", rownames(sim[[1]]$latent))
    
    icar_intercept_samples_f <- function(x) { #x = samples[[i]]
      return(x$latent[icar_index])
    }
    
    
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
    
    sims <- cbind(dat, sims)
    
    colnames(sims)[1:10] #sims start at row 7 of this df; "1"..."2"... etc sim_start set at top of script.
    colnames(sims)[sim_start:ncol(sims)] <- paste("sim_",1:nsim,sep="")
    
    #leave sims for predictions from post-strat data only -- 
    #get rid of sims for real survey data
    
    np <- nrow(sims) - nrow(pred_dat)
    
    sims <- sims[(np+1):nrow(sims),]
    
    #### Poststratification Step ####
    
    sims <- left_join(post, sims, by = c("id", "sex", "age", "edu")) 
    
    sims <- sims %>% 
      dplyr::relocate(estimate, .after = edu) %>% #tidy
      # select(-vax) %>% #tidy
      group_by(id) %>%
      mutate(N = sum(estimate)) %>%
      ungroup() %>%
      na.replace(0) #ditto -- can't have zeroes for mrp; handles NAs introduced by estimate/zcta_pop; in this case, NA == zero for pop counts
    
    
    save(sims, file = here::here("Aja", "COVID_Behaviors", "data","smrp", "sims", "sims_spatial", paste0(state_choice,"_sims_spatial_vaccination",specs,".RData")))
    
    #begin post-stratification: multiply the zcta_pop by the sims across each row 
    #(remember: each row is an observation in the PS table)
    
    #mean sim estimation * estimate in stratum /estimate in zcta
    rowmedians <- sims %>%
      group_by(id, sex, age, edu) %>%
      pivot_longer(cols = sim_1:sim_1000, names_to = "sim", values_to = "sim_prob")  %>%
      ungroup()
    
    rowmedians <- rowmedians %>%
      mutate(weight = estimate/N,
             ps_prob = sim_prob * weight) %>% #poststrat prob = posterior prob * proportion of pop in strata
      group_by(id,sex,age,edu) %>%
      mutate(median_ps_prob = median(ps_prob),
             lw_ps_prob = quantile(ps_prob, probs = c(0.05),
                                   up_ps_prob = quantile(ps_prob, probs = c(0.975)))) %>%
               dplyr::select(id, sex, age, edu, mean_ps_prob, estimate, 
                             N, lw_ps_prob, up_ps_prob) %>%
               distinct() %>%
               ungroup() %>%
               rename(zcta_strata_pop = estimate)
             
             rowmedians <- rowmedians %>%
               mutate(strata_pop_est = N * median_ps_prob,
                      strata_pop_lw.ci = strata_pop_est * lw_ps_prob,
                      strata_pop_up.ci = strata_pop_est * up_ps_prob) %>% 
               group_by(id) %>%
               mutate(estimate = as.integer(sum(strata_pop_est))) %>% #actual SAE estimate
               mutate(lw.ci = as.integer(sum(strata_pop_lw.ci))) %>%
               mutate(up.ci = as.integer(sum(strata_pop_up.ci))) %>%
               ungroup() %>%
               distinct()         
             
             smrp_summary <- rowmedians %>%
               select(id, estimate, lw.ci, up.ci, N,
                      median_ps_prob, lw_ps_prob, up_ps_prob) %>%
               group_by(id) %>%
               distinct() %>%
               mutate(model = model_specs) %>%
               ungroup()  
    
  }

         