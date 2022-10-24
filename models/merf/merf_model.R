# MERF Models
library(here)
source(here("models/explore/load_data.R"))
library(tidyverse)
library(MixRF)

MixRF()

# get sample
samp = all_sim_samples[["30"]][[1]]

# fit to MixRF
merf1 <- MixRF(Y = samp$BA, X = samp %>% select(evi, tcc16), 
        random = "(1 | SUBSECTION)", data = samp)

merf1$MixedModel


merf1$RandomEffects$SUBSECTION %>% summarize(m = mean("(Intercept)"))
mean(data.frame(merf1$RandomEffects$SUBSECTION)$X.Intercept.)
merf1$MixedModel
fpreds <- predict(merf1$forest, samp)

sub <- data.frame(fpreds = fpreds, SUBSECTION = samp$SUBSECTION)

fixed_eff <- data.frame(SUBSECTION = rownames(merf1$RandomEffects$SUBSECTION),
                        FE = merf1$RandomEffects$SUBSECTION[[1]])

sub <- sub %>% left_join(fixed_eff, by = "SUBSECTION") 
sub%>%mutate(MERF_preds = fpreds + FE)
f = as.formula("BA~tcc16+evi")
t  = randomForest(f, data = samp)
resid = samp$BA - t$predicted
f = as.formula(paste0('resid ~ -1+ (1|', "SUBSECTION", ")"))
lm <- lmer(f, data = samp)
mean(predict(lm))


MERF <- function(samp_dat, pop_dat = NULL, formula, domain_level = "SUBSECTION", 
                initial_random_effects = 0, max_iter = 100, loglik_error_tol = 0.001,
                ntree = 500, mtry = max(floor(ncol(X)/3), 1), nodesize = 5, maxnodes = NULL,
                importance = FALSE) {
        
        if (!rlang::is_formula(formula)) {
                formula <- as.formula(formula)
                message("model formula was converted to class 'formula'")
        }

        # extract data from formula
        y_name <- deparse(formula[[2]])
        X_name <- stringr::str_extract_all(deparse(formula[[3]]), "\\w+")[[1]]
        y <- samp_dat %>% select(all_of(y_name))
        X <- samp_dat %>% select(all_of(X_name))

        # Adjust y based on initial random effects
        adjusted_y = y - initial_random_effects

        # stop if max iter or error tolerance reached
        iter = 0; mu = NULL; loglik = Inf
        continue_condition = TRUE

        while(continue_condition){
                iter <- iter + 1
                # fit RF model

                rf <- randomForest(x = X, y = adjusted_y[,1],
                                importance = importance, ntree = ntree, mtry = mtry, nodesize = nodesize, maxnodes = maxnodes)

                # get residuals 
                residuals <- unlist(y - rf$predicted)

                # estimate new random effects using lmer 
                f_rand_effects <- as.formula(paste0('residuals ~ -1 +(1|', domain_level, ")"))
                lme_fit <- lmer(f_rand_effects, data = samp_dat)

                newloglik <- as.numeric(logLik(lme_fit))

                # assess stopping criteria
                continue_condition <- (abs(newloglik - loglik) > loglik_error_tol & iter < max_iter)
                loglik <- newloglik

                # get domain fixed effects 
                fixed_effects <- predict(lme_fit)

                # update target y so we're not using rf to predict fixed effects
                adjusted_y <- y - fixed_effects 
        }

        # estimators from lme4 model - assume that pulling these from the final residual fit is ok? 
        # K&S2022 says they are found "implicitly by taking the expectation of ML estimators given the data"
        mu_var_hat <- data.frame(summary(lme_fit)$varcor)[1,]$vcov
        eps_var_hat <- data.frame(summary(lme_fit)$varcor)[2, ]$vcov

        # compute section estimates using MERF model by predicting on sample per K&S2022
        mu_merf <- data.frame(predict(rf), as.vector(y), as.vector(samp_dat[, domain_level])) %>%
                setNames(c("y_pred", "y_true", "domain")) %>%
                mutate(resids = y_true - y_pred) %>%
                group_by(domain) %>%
                summarize(n_domain = n(), pred_avg = mean(y_pred, na.rm = TRUE), resid_avg = mean(resids, na.rm = TRUE)) %>%
                mutate(mu_merf = pred_avg + mu_var_hat/(mu_var_hat+eps_var_hat/n_domain)*resid_avg)

        # if(typeof(pop_dat) != "NULL"){
        #         pred_dat <- data.frame(y = y, preds = predict(rf, pop_dat), 
        #                         domain = pop_dat %>% select(all_of(domain_level)))
                
        #         # follow krennmair and Schmid for 
        # }
        
        return(list(forest = rf, mixed_model = lme_fit, iterations = iter, mu_merf = mu_merf))
}


m <- MERF(samp, pixel_data, "BA ~ tcc16+evi", max_iter = 10)
m
