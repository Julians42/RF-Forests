library(tidyverse)
library(mase)
library(sae)
library(hbsae)
library(here)
library(foreach)
library(broom)
library(doParallel)
library(MuMIn)
registerDoParallel(cores = 6)
select <- dplyr::select


# loading in pixel level population data
pixel_data <- read_csv(here("data", "pixel_population_level", "pixel_m333_ba.csv"))

pixel_data <- pixel_data %>% 
  rename(BA = BA_new)


pixel_means <- pixel_data %>% 
  group_by(SUBSECTION) %>% 
  summarise(
    across(.cols = evi:BA, .fns = mean)
  ) %>% 
  ungroup() %>% 
  as.data.frame()

npixel_add <- function(pop, means){
  pixels <- pop %>% 
    count(SUBSECTION) %>% 
    rename(npixels = n) %>% 
    as.data.frame()
  
  new_means <- means %>% 
    left_join(pixels) %>% 
    as.data.frame()
  
  return(new_means)
}


# final pixel_means
pixel_means <- npixel_add(pixel_data, pixel_means)

# function to generate samples under our unique sampling technique
sample_generation <- function(data, k) {
  
  unique_hex <- unique(pixel_data$FIAHEX_ID)
  selected_hexes <- sample(unique_hex, size = k)
  
  sample_data <- data %>% 
    filter(FIAHEX_ID %in% selected_hexes) %>% 
    group_by(FIAHEX_ID) %>% 
    slice_sample(n = 1) %>% 
    ungroup()
  
  sample_data
  
}


# generating simulation samples
set.seed(130)

full_samples_gen <- function(k, data) {
  samples_df <- data.frame()
  
  for (i in 1:2000) {
    samp <- data %>% 
      sample_generation(k) %>% 
      mutate(sample = i)
    samples_df <- rbind(samples_df, samp)
  }
  
  samples_df
}

# only run once
# samples_30n <- full_samples_gen(k = 240, pixel_data)
# samples_50n <- full_samples_gen(k = 400, pixel_data)
# samples_100n <- full_samples_gen(k = 800, pixel_data)

# sample data
samples_30n <- read_csv(here("data", "sim_samples", "samples_30n.csv")) %>% 
  mutate(s_size = 30) %>% 
  rename(BA = BA_new)
samples_50n <- read_csv(here("data", "sim_samples", "samples_50n.csv")) %>% 
  mutate(s_size = 50) %>% 
  rename(BA = BA_new)
samples_100n <- read_csv(here("data", "sim_samples", "samples_100n.csv")) %>% 
  mutate(s_size = 100) %>% 
  rename(BA = BA_new)

full_sim_data <- rbind(samples_30n, samples_50n, samples_100n)

labels <- c(30, 50, 100)

all_sim_samples <- list()

for (i in labels) {
  conds <- full_sim_data %>%
    filter(s_size == i) %>%
    group_by(sample) %>%
    group_split()

  all_sim_samples[[as.character(i)]] <- conds
}


# example data grab

# sample number one from the sim samples with sample sizes around 30
# all_sim_samples$`30`[[1]]


# models

# ------------------------------------------------------------------------------
# area level EBLUP

## function code
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

# ------------------------------------------------------------------------------
# unit level EBLUP

## function code
unit_eblup <- function(samp_dat, pop_dat_means, formula, domain_level = SUBSECTION,
                       var_method = "fSAE", mse_est = F) {
  
  response <- formula[[2]]
  vars <- str_extract_all(deparse(formula[[3]]), "\\w+")[[1]]
  Y <- samp_dat[ , deparse(substitute(response))][[1]]
  X <- model.matrix(
    as.formula(paste0("~ ", deparse(formula[[3]]))), data = samp_dat
  )
  area <- samp_dat[ ,deparse(substitute(domain_level))][[1]]
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
  
  R2 <- unit_EBLUP$R2
  
  if (mse_est == T) {
    
    if (var_method == "fSAE") {
      
      mse <- as.data.frame(unit_EBLUP$mse) %>% 
        rownames_to_column("SECTION") %>%
        rename(mse = `unit_EBLUP$mse`)
      
    } else if (var_method == "Bootstrap") {
      
      # helper function to turn domain levels into integer codes
      domain_to_code <- function(data) {
        
        unique_domains <- unique(data[["SECTION"]])
        codes <- seq(1, length(unique_domains), by = 1)
        
        domain_ids <- data.frame(
          unique_domains = unique_domains,
          codes = as.integer(codes)
        )
        
        data %>% 
          left_join(domain_ids, by = c("SECTION" = "unique_domains")) %>% 
          select(-SECTION) %>% 
          relocate(codes)
        
      }
      
      codes_pop_means <- domain_to_code(pop_dat_means) %>% 
        select(codes, tcc16)
      
      codes_pop_n <- domain_to_code(pop_dat_means) %>% 
        select(codes, npixels)
      
      codes_sample <- domain_to_code(samp_dat)
      
      mse <- pbmseBHF(formula = codes_sample$BA ~ codes_sample$tcc16,
                      dom = codes_sample$codes,
                      meanxpop = codes_pop_means,
                      popnsize = codes_pop_n,
                      B = 1000,
                      method = "REML")$mse
      
    }
    
    estimates <- as.data.frame(unit_EBLUP$est) %>%
      rownames_to_column("section") %>%
      rename(est = `unit_EBLUP$est`)
    
    
    res <- suppressMessages(cbind(mse, estimates)) %>% 
      select(-domain) %>% 
      relocate(section)
    
  } else {
    
    res <- as.data.frame(unit_EBLUP$est) %>% 
      rownames_to_column("section") %>% 
      rename(est = `unit_EBLUP$est`)
    
  }
  
  return(list(res, R2))
  
}


# unit level zero-inflation ----------------------------------------------------
# function code

# dependencies:
#     - dplyr, lme4, boot, doParallel, foreach (both optional but very helpful)

# samp_dat, and pop_dat MUST be data.frame's not tibbles

unit_zi <- function(samp_dat, pop_dat, formula, domain_level = "SUBSECTION",
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
      paste0(deparse(formula[[2]]), " != 0 ~ ",
             deparse(formula[[3]]), " + ",
             rand_intercept)
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
  
  if (mse_est == T) {
    
    # MSE estimation -------------------------------------------------------------
    
    # helper function use to extract model coefs for bootstrap data generation
    mse_coefs <- function(lmer_model, glmer_model) {
      
      # from lmer model
      beta_hat <- lmer_model@beta # linear model coefficients
      model_summary_df <- data.frame(summary(lmer_model)$varcor)
      
      sig2_mu_hat <- model_summary_df[1, ]$vcov 
      sig2_eps_hat <- dplyr::filter(model_summary_df, grp == "Residual")$vcov
      
      # from glmer model
      alpha_1 <- glmer_model@beta
      b_i <- glmer_model@u
      
      domain_levels <- levels(factor(glmer_model@flist[[1]]))
      
      return(list(
        beta_hat = beta_hat, sig2_mu_hat = sig2_mu_hat,
        sig2_eps_hat = sig2_eps_hat, alpha_1 = alpha_1,
        b_i = b_i, domain_levels = domain_levels))
      
    }
    
    # extract coefs from models fit to user supplied data
    zi_model_coefs <- mse_coefs(original_res$lmer, original_res$glmer)
    
    params_and_domain <- data.frame(dom = zi_model_coefs$domain_levels,
                                    b_i = zi_model_coefs$b_i)
    
    joined_pop_bi <- dplyr::left_join(
      data.frame(dom = pop_dat[ , domain_level, drop = T]),
      params_and_domain,
      by = "dom"
    )
    
    x_matrix <- model.matrix(
      as.formula(paste0(" ~ ", deparse(formula[[3]]))),
      data = pop_dat[ , X, drop = F]
    )
    
    # generating random errors
    individual_random_errors <- data.frame(
      dom = pop_dat[ , domain_level, drop = T],
      individual_random_errors = rnorm(
        n = length(pop_dat[ , domain_level, drop = T]),
        mean = 0,
        sd = sqrt(zi_model_coefs$sig2_eps_hat)
      )
    )
    
    area_random_errors <- data.frame(
      dom = zi_model_coefs$domain_levels,
      area_random_errors = rnorm(
        length(zi_model_coefs$domain_levels),
        mean = 0,
        sd = sqrt(zi_model_coefs$sig2_mu_hat)
      )
    )
    
    random_effects <- individual_random_errors %>%
      dplyr::left_join(area_random_errors, by = "dom")
    
    # predict probability of non-zeros
    p_hat_i <- exp(x_matrix %*% zi_model_coefs$alpha_1 + joined_pop_bi$b_i)/
      (1 + exp(x_matrix %*% zi_model_coefs$alpha_1 + joined_pop_bi$b_i))
    
    # Generate corresponding deltas
    delta_i_star <- rbinom(length(p_hat_i), 1, p_hat_i)
    
    boot_data_generation_params <- list(random_effects = random_effects,
                                        p_hat_i = p_hat_i,
                                        delta_i_star = delta_i_star)
    
    linear_preds <- x_matrix %*% zi_model_coefs$beta_hat + 
      boot_data_generation_params$random_effects$area_random_errors +
      boot_data_generation_params$random_effects$individual_random_errors
    
    boot_pop_response <- as.vector(
      linear_preds * boot_data_generation_params$delta_i_star
    )
    
    # bootstrap population data
    boot_pop_data <- data.frame(
      domain = pop_dat[ , domain_level, drop = T],
      response = boot_pop_response
    ) 
    
    # domain level estimates for bootstrap population data
    boot_pop_param <- boot_pop_data %>%
      dplyr::group_by(domain) %>%
      dplyr::summarize(domain_est = mean(response))
    
    
    ## bootstrapping -------------------------------------------------------------
    
    boot_pop_data <- cbind(pop_dat, boot_pop_data)
    
    # creating bootstrap formula to be used to fit zi-model to bootstrap samples
    boot_formula <- as.formula(paste0("response ~ ", deparse(formula[[3]])))
    
    
    boot_rep <- function(pop_boot) {
      # maybe sample by domain to be safer (look at lit)
      # i.e group_by(domain_level) and ungroup() after
      
      
      boot_samp <- pop_boot %>%
        dplyr::slice_sample(n = nrow(samp_dat), replace = TRUE) %>%
        as.data.frame()
      
      boot_samp_fit  <- tryCatch(
        {
          fit_zi(boot_samp, pop_boot, boot_formula)
        },
        error = function(cond) {
          boot_samp <- pop_boot %>%
            dplyr::slice_sample(n = nrow(samp_dat), replace = TRUE) %>%
            as.data.frame()
          
          fit_zi(boot_samp, pop_boot, boot_formula)
        }
      )
      
      
      squared_error <- suppressMessages(
        dplyr::left_join(boot_samp_fit$res, boot_pop_param)
      ) %>%
        dplyr::mutate(sq_error = (Y_hat_j - domain_est)^2) %>%
        dplyr::select(domain, sq_error)
      
      
      return(squared_error)
    }
    
    
    mse_df <- foreach(i = 1:B, .combine = 'rbind') %dopar% {
      boot_rep(boot_pop_data)
    }
    
    final_df <- mse_df %>%
      dplyr::group_by(domain) %>%
      dplyr::summarise(mse = sum(sq_error)/B) %>%
      dplyr::left_join(original_res$res) %>%
      dplyr::select(domain, mse, est = Y_hat_j ) %>%
      dplyr::ungroup()
    
  } else {
    final_df <- original_res$res
  }
  
  return(list(final_df, R2))
  
}



 




