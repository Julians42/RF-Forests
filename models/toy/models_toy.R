# Models for toy simulation study
# Julian Schmitt - Senior Thesis '23



ps_estimator <- function(sim_index, pop_dat) {
  # load sim data
  #sim_dat <- read.csv(paste("data/sim_samples/samples_", sim_size, "n.csv", sep = ""))
  sim_data = sim_samples[[sim_index]]
  # first get proportion tnt for population data 
  pop_n_sec <- pop_dat %>% group_by(domain, real_x) %>% summarize(n = n()) %>% ungroup()
  pop_n <- pop_dat %>% group_by(domain) %>% 
    summarize(n = n()) %>% ungroup() 
  pop_weights <- pop_n_sec %>% left_join(pop_n, by = c("domain" = "domain")) %>%
    mutate(frac = n.x/n.y, n.x = NULL, n.y = NULL)

  # # get averages across samples, sections, and stratifier (tnt)
  sim_avg <- sim_data %>% group_by(domain, sample, real_x) %>%
    summarize(y = mean(y)) %>% ungroup()

  # # compute estimate 
  BA_est <-sim_avg %>% left_join(pop_weights, by = c("domain" = "domain", "real_x" = "real_x")) %>% 
    mutate(mult = y * frac) %>% 
    group_by(domain, sample) %>%
    summarize(y_hat = sum(mult)) %>% ungroup()
  
  # # compute "truth"
  true_ba <- pop_dat %>% group_by(domain) %>% 
    summarize(y = mean(y))
  
  # # merge to our standard format to compare 
  part_df <- BA_est %>% left_join(true_ba, by = c("domain" = "domain")) %>% 
    mutate(sample_size = sim_index, model = "ps", domain = domain, sample = NULL)
  
  return(part_df)
}

# random forest
rf_model <- function(samp_dat, pop_dat, formula, domain_level = "SUBSECTION",
                    ntree = 300, mtry = max(floor(ncol(samp_dat)/3), 1), maxnodes = NULL) {
    # ensure erroneous errors don't crash simulation
    result <- tryCatch(
        {
            # convert formula string to type formula if needed
            if (!rlang::is_formula(formula)) {
                formula <- as.formula(formula)
                #message("model formula was converted to class 'formula'")
            }
            # fit random forest model
            model <- randomForest(formula, 
                                    data = samp_dat, 
                                    ntree = ntree,
                                    mtry = mtry,
                                    maxnodes = maxnodes)

            # predict on population data
            model_preds <- data.frame(BA_est = predict(model, pop_dat, type = "response")) %>%
                            mutate(domain = pop_dat$domain, BA = pop_dat$y) %>%
                            group_by(domain) %>%
                            summarize(BA_hat = mean(BA_est), BA = mean(BA))
            
            # add df params and return 
            model_preds <- model_preds %>% mutate(model = "rf", sample_size = dim(samp_dat)[1]/8)

            model_preds
        },
        # on error return empty dataframe
        error = function(cond) {
            data.frame()
        }
    )
    return(result)
    #return(model)
}

# rf_single = rf_model(samp, full_samp, "y ~ noise1 + noise2 + real_x", domain_level = "domain")
# importance(rf_single)

# wrapper for vanilla RF model
rf_wrapper <- function(formula, sample_size, xpop = pixel_data, reps = 2000, domain_level = "SUBSECTION") {
    
    # select list of dataframes with sample conditions
    sample_list = sim_samples[[sample_size]]

    # parallelize over samples
    sim_runs <- foreach(i = 1:reps) %dopar% {
        return(rf_model(samp_dat = sample_list %>% filter(sample == i), pop_dat = xpop, formula = formula, domain_level))
        gc()
    }
    # bind to full dataframe
    full_res = bind_rows(sim_runs)
    
    return(full_res)

}


############################## SMERF ##########################

merf_wrapper <- function(formula, sample_size = "100", reps = 2000, pop_dat = pixel_data,
                        domain_level = "SUBSECTION", initial_random_effects = 0, max_iter = 100,
                        loglik_error_tol = 0.001, ntree = 500, mtry = 1, 
                        nodesize = 5, maxnodes = NULL, importance = FALSE) {
    
    # select list of dataframes with sample conditions
    sample_list = sim_samples[[sample_size]]

    # parallelize over samples
    sim_runs <- foreach(i = 1:reps) %dopar% {
        print(str_interp("Running Simulation ${i}...", 
                                    list(i=i)))
        #print(sample_list %>% filter(sample ==i))
        return(SMERF(samp_dat = (sample_list %>% filter(sample == i)), pop_dat = pop_dat, 
        formula = as.formula(formula), domain_level = domain_level, initial_random_effects = initial_random_effects,
        max_iter = max_iter, loglik_error_tol = loglik_error_tol, ntree = ntree, mtry = mtry,
        nodesize = nodesize, maxnodes = maxnodes, importance = importance))
        gc()
    }
    results = bind_rows(sim_runs)
    results$sample_size = sample_size
    return(results)
}


SMERF <- function(samp_dat, pop_dat, formula, domain_level = "SUBSECTION", 
                initial_random_effects = 0, max_iter = 100, loglik_error_tol = 0.001,
                ntree = 500, mtry = 1, nodesize = 5, maxnodes = NULL,
                importance = FALSE) {
        
        if (!rlang::is_formula(formula)) {
                formula <- as.formula(formula)
                #message("model formula was converted to class 'formula'")
        }
        samp_dat <- as.data.frame(samp_dat)

        # extract data from formula
        y_name <- deparse(formula[[2]])
        X_name <- stringr::str_extract_all(deparse(formula[[3]]), "\\w+")[[1]]
        y <- samp_dat %>% dplyr::select(all_of(y_name))
        X_samp <- samp_dat %>% dplyr::select(all_of(X_name))

        # Adjust y based on initial random effects
        adjusted_target = (y - initial_random_effects)[,1]

        # stop if max iter or error tolerance reached
        b = 0; mu = NULL; oldloglik = Inf
        continue_condition = TRUE
        fixed_effects_list <- data.frame(domain = NULL, fixeff = NULL, iter = NULL)

        while(continue_condition){
                b <- b + 1 # track iterations

                # b: fit random forest 
                rf <- randomForest(x = X_samp, y = adjusted_target,
                                importance = importance, ntree = ntree, mtry = mtry, nodesize = nodesize, maxnodes = maxnodes)

                # c: get OOB predictions
                domain = pull(samp_dat %>% select(domain_level))
                #preds = data.frame(rf_OOB = rf$predicted) %>% left_join(fixed_effects, by = c("SUBSECTION" = 'domain'))
                predictions <- data.frame(OOB = rf$predicted, y = pull(y), domain = domain)

                # d: fit linear mixed model without intercept (-1 term), fixed effects across domain `(1|domain)`,
                # and restricted regression coefficient 1 (done with `offset()`) for the OOB predictions
                l = lmer("y ~ -1 + offset(OOB) + (1|domain)", data = predictions)

                # e: extract the variance components - we don't actually need these because lmer does the calculation for us
                mu_var_hat <- data.frame(summary(l)$varcor)[1, ]$vcov
                eps_var_hat <- data.frame(summary(l)$varcor)[2, ]$vcov

                # estimate random effects - relying on krennmair 2022, we note that this is exactly the random effects returned by lmer
                fixed_effects <- ranef(l)$domain %>% rename(fixeff = "(Intercept)")
                fixed_effects$domain = rownames(fixed_effects)
                fixed_effects$iter = b

                fixed_effects_list <- rbind(fixed_effects_list, fixed_effects)

                # 2: assess convergence by the change in log likelihood between updates. 
                newloglik = logLik(l)

                continue_condition <- (abs(newloglik - oldloglik) > loglik_error_tol & b < max_iter)

                # a: adjust to y* by removing the fixed effects - nothing to be done on first step so we update at end of each iteration
                adjusted_df <- samp_dat %>% left_join(fixed_effects, by = c("domain" = 'domain')) %>%
                                mutate(adjusted = y - fixeff)
                adjusted_target <- adjusted_df$adjusted
        }
        #return(fixed_effects_list)

        # compute section estimates on auxiliary data using MERF model by predicting on sample per K&S2022
        mu_merf <- data.table(rf_pred = predict(rf, pop_dat), 
                                BA = pull(pop_dat %>% 
                                          select(all_of(y_name))), 
                                domain = pull(pop_dat %>% 
                                          select(all_of(domain_level)))) %>%
                    group_by(domain) %>%
                    summarize(rf_pred = mean(rf_pred), BA = mean(BA)) %>%
                    left_join(fixed_effects, by = "domain") %>%
                    mutate(BA_hat = rf_pred+fixeff, model = "SMERF") %>%
                    dplyr::select(BA_hat, BA, model, domain)

        return(mu_merf)
        #return(list(forest = rf, mixed_model = l, iterations = b, mu_merf = mu_merf))
}
# stest <- SMERF(samp, full_samp, "y ~ noise1+noise2+real_x", domain_level = "domain")



############################## area EBLUP ##########################
area_eblup <- function(samp_dat, pop_dat, formula, domain_level = SUBSECTION, mse_est = F) {
  
  # just in case
  formula <- as.formula(formula)
  
  Y <- formula[[2]]
  X <- str_extract_all(deparse(formula[[3]]), "\\w+")[[1]]
  
  rep_vardir <- samp_dat %>%
    group_by({{ domain_level }}) %>%
    summarise(vardir = var({{ Y }})/n()) %>%
    as.data.frame()
  
  # need these at the section level
  pop_dat_x <- pop_dat %>%
    select({{ domain_level}}, all_of(X)) %>%
    as.data.frame()
  
  rep_means <- samp_dat %>%
    select({{ Y }}, {{ domain_level }}) %>%
    group_by({{ domain_level }}) %>%
    summarise(
      across(.cols = c({{ Y }}), .fns = mean)
    ) %>%
    ungroup() %>%
    left_join(rep_vardir) %>%
    left_join(pop_dat_x) %>%
    as.data.frame()
  
  area_EBLUP <- mseFH(formula, vardir = vardir,
                      data = rep_means, method = "REML")
  
  if (mse_est == T) {
    res <- data.frame(
      section = rep_means[ ,deparse(substitute(domain_level))],
      mse = area_EBLUP$mse,
      est = area_EBLUP$est$eblup[ ,1]
    )
  } else {
    res <- data.frame(
      section = rep_means[ ,deparse(substitute(domain_level))],
      est = area_EBLUP$est$eblup[ ,1]
    )
  }
  
  return(list(res))
  
}
area_eblup_sim <- function(formula, sim_size, xpop_mean, reps, domain_level = "domain") {
  
  # extract simulations corresponding to zero inflation and sim size
  samplelist <- sim_samples[[sim_size]]
  
  results <- foreach(i = 1:reps, .combine = 'rbind') %dopar% {
    #model_attempt(i, xpop_mean)
    area_eblup(
          samp_dat = samplelist %>% filter(sample == i),
          pop_dat = xpop_mean,
          as.formula(formula),
          domain_level = domain,
          mse_est = F
        )[[1]]
  }
  results <- as.data.frame(results) %>% rename(domain = section, BA_est = est)
  results$model = "area_eblup"
  results$sample_size = sim_size
  results <- results %>% left_join(pixel_means %>% select(domain, y) %>% rename(BA = y), by = "domain")
  return(results)
}

  model_attempt <- function(index, xpop_mean) {
    res <- tryCatch(
      {
        area_eblup(
          samp_dat = samplelist %>% filter(sample == index),
          pop_dat = xpop_mean,
          as.formula(formula),
          domain_level = domain,
          mse_est = F
        )[[1]]
      },
      error = function(cond) {
        data.frame()
      }
    )
    return(res)
  }


############################## unit EBLUP ##########################
unit_eblup_sim <- function(formula, sim_size, xpop_mean, reps, domain_level = SUBSECTION) {
  
  samplelist <- sim_samples[[sim_size]]
  
  # compute unit eblup estimator for each sample
  results <- foreach(i = 1:reps, .combine = 'rbind') %dopar% {
    unit_eblup(
              samp_dat = samplelist %>% filter(sample == i),
              pop_dat = xpop_mean,
              formula = as.formula(formula),
              domain_level = domain
            )
  }

  # process results into standard format
  results <- as.data.frame(results) %>% rename(domain = section, BA_est = est)
  results$model = "unit_eblup"
  results$sample_size = sim_size
  results <- results %>% left_join(pixel_means %>% select(domain, y) %>% rename(BA = y), by = "domain")
  return(results)
}

unit_eblup <- function(samp_dat, pop_dat_means, formula, domain_level = SUBSECTION) {
  
  formula <- as.formula(formula)

  # extract X matrix and ys from sample
  response <- formula[[2]]
  vars <- str_extract_all(deparse(formula[[3]]), "\\w+")[[1]]
  Y <- samp_dat[ , deparse(substitute(response))]
  X <- model.matrix(
    as.formula(paste0("~ ", deparse(formula[[3]]))), data = samp_dat
  )
  area <- samp_dat[ ,deparse(substitute(domain_level))]
  #area <- samp_dat[ ,deparse(substitute(domain_level))]


  Narea <- pop_dat_means$npixels
  names(Narea) <- pop_dat_means[ ,deparse(substitute(domain_level))]
  xpop_mat <- model.matrix(
    as.formula(paste0("~ ", deparse(formula[[3]]))), data = pop_dat_means
  )
  rownames(xpop_mat) <- pop_dat_means[ ,deparse(substitute(domain_level))]
  
  unit_EBLUP <- fSAE.Unit(y = Y,
                          X = X,
                          area = area,
                          Narea = Narea,
                          Xpop = xpop_mat,
                          # arguments for wrapper function
                          fpc = FALSE,
                          method = "REML",
                          silent = T)
  
  
  res <- as.data.frame(unit_EBLUP$est) %>% 
    rownames_to_column("section") %>% 
    rename(est = `unit_EBLUP$est`)

  
  return(res)  
}

#unit_eblup(samp, pixel_means, "y ~ noise1 + noise2 + real_x", domain_level = domain)








#################### SCRATCH ##########################



# old (works but not for reps)

# post_strat <- function(samp_dat, pop_dat, strata, response, domain_level = SUBSECTION) {
  
#   # extract all unique domain level names
#   domain_ids <- unique(samp_dat[ , deparse(substitute(domain_level)), drop = T])
#   ps_estimates <- data.frame()
  
#   # ps is a direct estimator so need to run model once for each domain
#   for (i in domain_ids) {
    
#     samp_dat_filtered <- samp_dat %>%
#       dplyr::filter({{ domain_level }} == i)
    
#     pop_dat_filtered <- pop_dat %>% 
#       dplyr::filter({{ domain_level }} == i)
    
#     # computing ps estimator
#     ps <- mase::postStrat(
#       y = samp_dat_filtered[ , deparse(substitute(response))],
#       xsample = samp_dat_filtered[ , deparse(substitute(strata))],
#       xpop = pop_dat_filtered[ , deparse(substitute(strata))],
#       N = nrow(pop_dat_filtered),
#       var_est = T,
#       var_method = "SRSunconditional",
#       datatype = "raw"
#     )
    
#     results_df <- data.frame(section = i, est = ps$pop_mean)
#     ps_estimates <- rbind(ps_estimates, results_df)
    
#   }
  
#   return(ps_estimates)
  
  
# }

#post_strat(samp, full_samp, real_x, y, domain_level = domain)