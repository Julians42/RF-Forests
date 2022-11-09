# MERF Models
library(here)
source(here("models/explore/load_data.R"))
library(tidyverse)
library(MixRF)
library(data.table)

# one iteration of krennmair

# get sample
samp = all_sim_samples[["30"]][[1]]

# a: adjust to y* - nothing to be done on first step
adjusted_target <- samp$BA - 0

# b: fit random forest 
m <- randomForest(y = adjusted_target, x = samp %>% select(tcc16, evi))

# c: get OOB predictions
df <- data.frame(OOB = m$predicted, y = samp$BA, domain = samp$SUBSECTION)

# d: fit linear mixed model without intercept (-1 term), fixed effects across domain `(1|domain)`,
# and restricted regression coefficient 1 (done with `offset()`) for the OOB predictions
l = lmer(y ~ -1 + offset(OOB) + (1|domain), data = df)

# e: extract the variance components 
mu_var_hat <- data.frame(summary(l)$varcor)[1, ]$vcov
eps_var_hat <- data.frame(summary(l)$varcor)[2, ]$vcov

# estimate random effects - relying on krennmair 2022, we note that this is exactly the random effects returned by lmer
fixed_effects <- ranef(l)$domain %>% rename(fixeff = "(Intercept)")
fixed_effects$SUBSECTION = rownames(fixed_effects)

adjusted_df <- samp %>% left_join(fixed_effects, by = "SUBSECTION") %>%
    mutate(adjusted = BA - fixeff)
adjusted_target <- adjusted_df$adjusted

# 2: assess convergence by the change in log likelihood between updates. 
newloglik = logLik(l)

# terminate if iterations reached or log-likelihood changes are small enough
hist(samp$BA)
hist(samp$BA -predict(l))




library(ggplot2)
library(stringr)
#ggplot(data = tmixrf$FixedEffectsList, aes(x = iter, y = fixeff, color = domain))+geom_point()+geom_line()
ggplot(data = t_data, aes(x = iter, y = fixeff, color = domain))+geom_point()+geom_line()


tmixrf$FixedEffectsList[,1]





# for (i in 1:length(merf_results)) {
#     print(merf_results[[1]]$iterations)
# }


# SMERF(all_sim_samples[["100"]][[1]], pop_dat, "BA~evi+tcc16")

# rf_results <- rf_wrapper("BA~evi+tcc16", 
#                         sample_size = s,
#                         xpop = pixel_data, 
#                         reps = reps)


# note than raneff and the following summarize return the same values!!!
ranef(l)
df %>% 
    group_by(domain) %>%
    summarize(avg_dom = mean(y-OOB), num_obs = n(), weight = mu_var_hat/(mu_var_hat+eps_var_hat/num_obs), raneff = avg_dom*weight)
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
        continue_condition = TRUE; predictions = NULL
        fixed_effects_list <- data.frame(domain = NULL, fixeff = NULL, iter = NULL)

        while(continue_condition){
                b <- b + 1 # track iterations

                # b: fit random forest 
                rf <- randomForest(x = X_samp, y = adjusted_target,
                                importance = importance, ntree = ntree, mtry = mtry, nodesize = nodesize, maxnodes = maxnodes)

                # c: get OOB predictions
                domain = pull(samp_dat %>% select(domain_level))
                preds = data.frame(rf_OOB = rf$predicted) %>% left_join(fixed_effects, by = c("SUBSECTION" = 'domain'))
                # if (b == 1) {
                #         predictions <- data.frame(OOB = rf$predicted, y = pull(y), domain = domain)
                # } else {
                #         predictions <- data.frame(OOB = rf$predicted, y = pull(y), domain = domain) %>% 
                #                                 left_join(fixed_effects, on = c("SUBSECTION" = 'domain')) %>%
                #                                 mutate(OOB = OOB + fixeff)
                # }


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