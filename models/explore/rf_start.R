# build new unit zero-inflation model using random forest for predicting zeros

library(here)
source(here("models", "final_sim_files", "updated_modeling_script.R"))
set.seed(53)

library(randomForest)
samp <- all_sim_samples[[1]][[1]]

# logistic model
m_log <- glmer(BA !=0 ~ evi+tcc16 + (1 | SUBSECTION), data = samp, family = 'binomial')
preds <- as.numeric(predict(m_log, pixel_data, type = "response") >0.5)
table(preds, pixel_data$BA !=0)

mean(preds == (pixel_data$BA !=0))


# random forest model
m_rf <- randomForest(factor(BA !=0) ~ evi+tcc16, data = samp, ntree = 300, maxnodes = 10)
rf_preds <- predict(m_rf, pixel_data %>% mutate(BA = as.factor(BA !=0)), 
                                   type = "response")
table(rf_preds, (pixel_data$BA !=0))



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
