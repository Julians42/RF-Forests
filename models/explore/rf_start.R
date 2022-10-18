# build new unit zero-inflation model using random forest for predicting zeros

library(here)
library(tidyverse)
library(LongituRF)
library(randomForest)
source(here("models", "explore", "summer_modeling_script.R"))
set.seed(53)

# 

samp <- all_sim_samples[[1]][[1]] 

# MERF algorithm 
library(LongituRF)
dlg <- DataLongGenerator()

oldpar <- par()
oldopt <- options()
data <- DataLongGenerator(n=17, p=6,G=6) # Generate the data
# Let's see the output :
w <- which(data$id==1)
plot(data$time[w],data$Y[w],type="l",ylim=c(min(data$Y),max(data$Y)), col="grey")
for (i in unique(data$id)){
      w <- which(data$id==i)
      lines(data$time[w],data$Y[w], col='grey')
}
# Let's see the fixed effects predictors:
par(mfrow=c(2,3), mar=c(2,3,3,2))
for (i in 1:ncol(data$X)){
  w <- which(data$id==1)
  plot(data$time[w],data$X[w,i], col="grey",ylim=c(min(data$X[,i]),
  max(data$X[,i])),xlim=c(1,max(data$time)),main=latex2exp::TeX(paste0("$X^{(",i,")}$")))
  for (k in unique(data$id)){
    w <- which(data$id==k)
    lines(data$time[w],data$X[w,i], col="grey")
} }
par(oldpar)
options(oldopt) 

# random forest modelrf_
m_rf <- randomForest(BA ~ evi+tcc16, 
                    data = samp, 
                    ntree = 300, 
                    maxnodes = 10)
rf_preds <- predict(m_rf, pixel_data, 
                                   type = "response")
rf_preds <- data.frame(BA_est = rf_preds)
rf_preds <- rf_preds %>% mutate(domain = pixel_data$SUBSECTION, 
                    BA = pixel_data$BA)

rf_preds %>% group_by(domain) %>%
    summarize(BA_hat = mean(BA_est), BA = mean(BA))

rf_model <- function(samp_dat, pop_dat, formula, domain_level = "SUBSECTION",
                    ntree = 300, mtry = max(floor(ncol(samp_dat)/3), 1), maxnodes = NULL) {

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

  return(model_preds)
}
res <- rf_model(samp, pixel_data, "BA ~ evi+tcc16+tmean+tnt")
res


unit_zi_rf <- function(samp_dat, pop_dat, formula, domain_level = "SUBSECTION",
                      B = 100, mse_est = F){
  
  if (!rlang::is_formula(formula)) {
    formula <- as.formula(formula)
    message("model formula was converted to class 'formula'")
  }
  
  # creating strings of original X, Y names
  Y <- deparse(formula[[2]])
  X <- stringr::str_extract_all(deparse(formula[[3]]), "\\w+")[[1]]
  
  # function to fit a zero-inflation model 
  # this function pulls in domain_level from it's parent function input
  fit_zi <- function(samp_dat, pop_dat, formula) {
    
    Y <- deparse(formula[[2]])
    X <- stringr::str_extract_all(deparse(formula[[3]]), "\\w+")[[1]]
    
    # function will always treat domain_level as the random intercept 
    rand_intercept <- paste0("( 1 | ", domain_level, " )")
    
    # of form y ~ x_1 + ... + x_n + (1 | domain_level)
    lin_reg_formula <- as.formula(
      paste0(deparse(formula[[2]]), " ~ ",
             deparse(formula[[3]]), " + ",
             rand_intercept)
    )
    
    # of form y != 0 ~ x_1 + ... + x_n + (1 | domain_level)
    log_reg_formula <- as.formula(
      paste(deparse(f[[2]]), "!= 0 ~", 
            deparse(f[[3]]), sep = "")
      )

    
    # creating nonzero version of our sample data set 
    nz <- samp_dat[samp_dat[ , Y] > 0, ]
    
    # fit linear mixed model on nonzero data
    lmer_nz <- suppressMessages(lme4::lmer(lin_reg_formula, data = nz))
    
    # Fit logistic mixed effects on ALL data
    glmer_z <- suppressMessages(
      lme4::glmer(log_reg_formula, data = samp_dat, family = "binomial")
    )
    
    lin_pred <- predict(lmer_nz, pop_dat)
    log_pred <- predict(glmer_z, pop_dat, type = "response")
    
    unit_level_preds <- lin_pred*log_pred
    
    # d x 2 dataframe
    # where d = # of domains
    zi_domain_preds <- data.frame(
      domain = pop_dat[ , domain_level, drop = T],
      unit_level_preds = unit_level_preds) %>%
      dplyr::group_by(domain) %>%
      dplyr::summarise(Y_hat_j = mean(unit_level_preds)) %>%
      ungroup()
    
    
    return(list(lmer = lmer_nz, glmer = glmer_z, res = zi_domain_preds))
    
  }
  
  # get model estimates for user supplied data
  original_res <- fit_zi(samp_dat, pop_dat, formula)
  
  R2 <- r.squaredGLMM(original_res$lmer)[1]
  

}


f <- as.formula(BA ~ evi+tcc16)
deparse(paste(f[[2]], "!= 0" ))
