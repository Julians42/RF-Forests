# JS Thesis
# loads 4 state sim results

library(tidyverse)
library(data.table)

results.tcc <- read.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/tcc_2000_w4.csv")
results.ntcc <- read.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/ntcc_2000_w4.csv")


state.df <- unique(results.ntcc %>% select(domain))
state.df$state <- c("Iowa", "Iowa", "Georgia", "Georgia", "Georgia", "Georgia",
                    "Georgia", "Oregon", "Iowa", "Iowa", "Idaho", "Oregon",
                    "OR/ID", "Idaho", "Oregon", "Oregon", "Idaho",
                    "Georgia",
                    "Oregon", "Oregon", "Oregon", "Oregon", "Oregon", "Idaho",
                    "Idaho", "Idaho", "Idaho", "Oregon", "Idaho", "Idaho")


get.bias <- function(df) {
  res <- df %>% 
    group_by(domain, model, sample_size) %>% 
    summarise(sim_est = mean(BA_hat, na.rm = T), true_resp = mean(BA, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(e_bias = sim_est - true_resp,
           perc_rel_e_bias = (e_bias/true_resp)*100,
           sample_size = as.factor(sample_size)) 
  res <- res %>% mutate(section = domain) %>% 
    left_join(state.df, by = "domain")
  res
}

get.emse <- function(df, bias.res) {
  res <- df %>% 
    mutate(sample_size = as.factor(sample_size)) %>% 
    left_join(bias.res, by = c("domain", "model", "sample_size")) %>% 
    group_by(domain, model, sample_size) %>% 
    summarise(e_var = (2000/1999)*mean((BA_hat - sim_est)^2)) %>% 
    ungroup() 
  res <- res %>% mutate(section = domain) %>% 
    left_join(state.df, by = "domain")
  res
}

# get bias and emse results
tcc.bias <- get.bias(results.tcc)
ntcc.bias <- get.bias(results.ntcc)
tcc.emse <- get.emse(results.tcc, tcc.bias)
ntcc.emse <- get.emse(results.ntcc, ntcc.bias)
