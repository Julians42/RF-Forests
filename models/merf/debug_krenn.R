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
fixed_effects <- ranef(l)$domain



# 2: assess convergence by the change in log likelihood between updates. 
newloglik = logLik(l)

# terminate if iterations reached or log-likelihood changes are small enough
hist(samp$BA)
hist(samp$BA -predict(l))

















# note than raneff and the following summarize return the same values!!!
ranef(l)
df %>% 
    group_by(domain) %>%
    summarize(avg_dom = mean(y-OOB), num_obs = n(), weight = mu_var_hat/(mu_var_hat+eps_var_hat/num_obs), raneff = avg_dom*weight)
