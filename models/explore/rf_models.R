# models

# vanilla random forest model
rf_model <- function(samp_dat, pop_dat, formula, domain_level = "SUBSECTION",
                    ntree = 300, mtry = max(floor(ncol(samp_dat)/3), 1), maxnodes = NULL) {
    # ensure erroneous errors don't crash simulation
    result <- tryCatch(
        {
            # convert formula string to type formula if needed
            if (!rlang::is_formula(formula)) {
                formula <- as.formula(formula)
                message("model formula was converted to class 'formula'")
            }
            # fit random forest model
            model <- randomForest(formula, 
                                    data = samp_dat, 
                                    ntree = ntree,
                                    mtry = mtry,
                                    maxnodes = maxnodes)

            # predict on population data
            model_preds <- data.frame(BA_est = predict(model, pop_dat, type = "response")) %>%
                            mutate(domain = pop_dat$SUBSECTION, BA = pop_dat$BA) %>%
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
}

# wrapper for vanilla RF model
rf_wrapper <- function(formula, sample_size = 100, xpop = pixel_data, reps = 2000) {
    
    # select list of dataframes with sample conditions
    sample_list = all_sim_samples[[sample_size]]

    # parallelize over samples
    sim_runs <- foreach(i = 1:reps) %dopar% {
        return(rf_model(samp_dat = sample_list[[i]], pop_dat = xpop, formula = formula))
        gc()
    }
    # bind to full dataframe
    full_res = bind_rows(sim_runs)
    
    return(full_res)

}