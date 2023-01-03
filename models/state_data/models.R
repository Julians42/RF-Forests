# Julian Schmitt Senior Thesis
# Full Models

ps_estimator <- function(sim_index, pop_dat, 
                        domain = "SECTION", strat = "tnt", 
                        y = "CARBON_AG_TPA_live_ADJ", sample = "sample") {

  sim_dat <- sim_samples[[sim_index]]
  # map sim data names
  sim_dat <- sim_dat %>% rename(domain = all_of(domain), strat = all_of(strat), y = all_of(y), sample = all_of(sample)) %>%
                         select(domain, strat, y, sample) %>% 
                         mutate(strat = strat - 1)

  pop_dat <- pop_dat %>% rename(domain = all_of(domain), strat = all_of(strat), y = all_of(y)) %>%
                         select(domain, strat, y) %>% 
                         mutate(strat = strat - 1)

  # get stratified proportion for population 
  pop_n_sec <- pop_dat %>% group_by(domain, strat) %>% 
                           summarize(n = n()) %>%
                           ungroup()
  pop_n <- pop_dat %>% group_by(domain) %>% 
                       summarize(n = n()) %>% 
                       ungroup() 

  pop_weights <- pop_n_sec %>% left_join(pop_n, by = "domain") %>%
                               mutate(frac = n.x/n.y, n.x = NULL, n.y = NULL)

  # get averages across samples, sections, and stratifier (tnt)
  sim_avg <- sim_dat %>% group_by(domain, sample, strat) %>%
                          summarize(y = mean(y)) %>% 
                          ungroup()

  # compute estimate 
  BA_est <- sim_avg %>% left_join(pop_weights, by = c("domain" = "domain", "strat" = "strat")) %>% 
    mutate(mult = y * frac) %>% 
    group_by(domain, sample) %>%
    summarize(y_hat = sum(mult)) %>% ungroup()
  
  # # compute "truth"
  true_ba <- pop_dat %>% group_by(domain) %>% 
    summarize(y = mean(y))
  
  # # merge to our standard format to compare 
  part_df <- BA_est %>% left_join(true_ba, by = c("domain" = "domain")) %>% 
    mutate(sample_size = sim_index, model = "ps", domain = domain, sample = NULL)
  
  part_df <- part_df %>% rename(BA_hat = y_hat, BA = y)
  return(part_df)
}

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
          domain_level = SECTION,
          mse_est = F
        )[[1]]
  }
  results <- as.data.frame(results) %>% rename(domain = section, BA_est = est)
  results$model = "area_eblup"
  results$sample_size = sim_size
  #return(results)
  results <- results %>% left_join(pixel_means %>% 
                        select(SECTION, CARBON_AG_TPA_live_ADJ) %>% 
                        rename(BA = CARBON_AG_TPA_live_ADJ), by = ("domain" = "SECTION"))
  return(results)
}