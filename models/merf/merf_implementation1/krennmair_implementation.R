# wrapper function for SMERF
# wrapper for vanilla RF model
merf_wrapper <- function(formula, sample_size = "100", reps = 2000, pop_dat = pixel_data,
                        domain_level = "SUBSECTION", initial_random_effects = 0, max_iter = 100,
                        loglik_error_tol = 0.001, ntree = 500, mtry = 1, 
                        nodesize = 5, maxnodes = NULL, importance = FALSE) {
    
    # select list of dataframes with sample conditions
    sample_list = all_sim_samples[[sample_size]]

    # parallelize over samples
    sim_runs <- foreach(i = 1:reps) %dopar% {
        #.GlobalEnv$
        return(SMERF(samp_dat = sample_list[[i]], pop_dat = pop_dat, 
        formula = as.formula(formula), domain_level = domain_level, initial_random_effects = initial_random_effects,
        max_iter = max_iter, loglik_error_tol = loglik_error_tol, ntree = ntree, mtry = mtry,
        nodesize = nodesize, maxnodes = maxnodes, importance = importance))
        gc()
    }
    return(sim_runs)
#     # bind to full dataframe
#     full_res = bind_rows(sim_runs)
    
#     return(full_res)

}


SMERF <- function(samp_dat, pop_dat, formula, domain_level = "SUBSECTION", 
                initial_random_effects = 0, max_iter = 100, loglik_error_tol = 0.001,
                ntree = 500, mtry = 1, nodesize = 5, maxnodes = NULL,
                importance = FALSE) {
        
        if (!rlang::is_formula(formula)) {
                formula <- as.formula(formula)
                message("model formula was converted to class 'formula'")
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

                # a: adjust to y* - nothing to be done on first step so we update at end of each iteration
                adjusted_df <- samp_dat %>% left_join(fixed_effects, by = c("SUBSECTION" = 'domain')) %>%
                                mutate(adjusted = BA - fixeff)
                adjusted_target <- adjusted_df$adjusted
        }
        return(fixed_effects_list)

        # compute section estimates on auxiliary data using MERF model by predicting on sample per K&S2022
        mu_merf <- data.table(rf_pred = predict(rf, pop_dat), 
                                BA = pull(pop_dat %>% dplyr::select(all_of(y_name))), 
                                domain = pull(pop_dat[, domain_level])) %>%
                    group_by(domain) %>%
                    summarize(rf_pred = mean(rf_pred), BA = mean(BA)) %>%
                    left_join(fixed_effects, by = "domain") %>%
                    mutate(BA_hat = rf_pred+fixeff, model = "MERF") %>%
                    dplyr::select(BA_hat, BA, model)

        return(list(forest = rf, mixed_model = l, iterations = b, mu_merf = mu_merf))
}

# # example function call
t_data <- SMERF(all_sim_samples[["100"]][[1]], pixel_data, "BA ~ tcc16+evi", max_iter = 100)
t_data




tdata2 <- t_data
# library(data.table)
# pixel_data %>% select("BA")
# samp = all_sim_samples[["100"]][[1]]
# samp %>% select("BA")
