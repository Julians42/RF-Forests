library(tidyverse)
library(dplyr)
library(here)
library(doParallel)
library(sae)
library(lme4)
library(data.table)
library(hbsae)
select <- dplyr::select
registerDoParallel(cores = 7)

setwd("~/Documents/Research/Thesis/RF-Forests/models/toy/")

source("models_toy.R")

sim_reps = 500 # up to 500
n_small_areas = 8

set.seed(101)
# generate subsections and means
subsec = c("A", "B", "C", "D", "E", "F", "G", "H")
means = rbeta(n_small_areas, 3, 3)*100
mdf = data.frame(domain = subsec, means = means)

# generate ys, a binary predictor, and random noise
real_x = sample(c(0,1), 2000, replace =TRUE)
noise0 = rnorm(2000, 0, sd = 5) +5*real_x# y variable that depends on real_x, shifted one deviation apart
noise1 = rnorm(2000, 100, sd = 10) # precip
noise2 = rnorm(2000, 50, sd = 20) # temp
samples = data.frame(y = noise0, noise1 = noise1, noise2 = noise2, real_x = real_x, domain = sample(subsec, 2000, replace = TRUE))

# join samples with subsections 
full_samp <- samples %>% left_join(mdf, by = "domain")
full_samp <- full_samp %>% mutate(y = y+means) 
full_samp %>% head()


# generate simulation samples for the toy data to match real - lets only do 500 samples for now
sample_generation <- function(data, k, domain = "domain") {
  # Generates samples of size k by

  sample_data <- data %>% 
    group_by_at(domain) %>% 
    slice_sample(n = k) %>% 
    ungroup()
  
  sample_data
  
}

full_gen <- function(data, k, reps=2000) {
  samples_df <- data.frame()
  
  for (i in 1:reps) {
    samp <- data %>% 
      sample_generation(k) %>% 
      mutate(sample = i)
    samples_df <- rbind(samples_df, samp)
  }
  
  samples_df
}

# only need to run once 
# full_gen(full_samp, 8, reps = 500) %>%
#     write.csv("~/Documents/Research/Thesis/RF-Forests/models/toy/sim_samp_8n.csv")

# full_gen(full_samp, 16, reps = 500)  %>%
#     write.csv("~/Documents/Research/Thesis/RF-Forests/models/toy/sim_samp_16n.csv")
  
# full_gen(full_samp, 32, reps = 500) %>%
#     write.csv("~/Documents/Research/Thesis/RF-Forests/models/toy/sim_samp_32n.csv")



################ run the models on the toy dataset ####################

# load data
sim_samps_8  = read.csv("~/Documents/Research/Thesis/RF-Forests/models/toy/sim_samp_8n.csv")
sim_samps_16 = read.csv("~/Documents/Research/Thesis/RF-Forests/models/toy/sim_samp_16n.csv")
sim_samps_32 = read.csv("~/Documents/Research/Thesis/RF-Forests/models/toy/sim_samp_32n.csv")

sim_samples = list(sim_samps_8, sim_samps_16, sim_samps_32)
names(sim_samples)  = c("8", "16", "32")

# load models
source("models_toy.R")

# example sample for debugging
samp = sim_samples[["8"]] %>% filter(sample == 1)


########################### PS ################################
res8 = ps_estimator("8", full_samp) %>% rename(BA_hat = y_hat, BA = y) %>% head(n_small_areas*sim_reps)
res16 = ps_estimator("16", full_samp) %>% rename(BA_hat = y_hat, BA = y)%>% head(n_small_areas*sim_reps)
res32 = ps_estimator("32", full_samp) %>% rename(BA_hat = y_hat, BA = y)%>% head(n_small_areas*sim_reps)

# concatenate to full results df
ps_results <- list(res8, res16, res32) %>% reduce(full_join)

print(ps_results)
########################### RF ################################
rf_res8 = rf_wrapper("y ~ noise1+noise2+real_x", "8", xpop = full_samp, reps=sim_reps, domain_level = "domain")
rf_res16 = rf_wrapper("y ~ noise1+noise2+real_x", "8", xpop = full_samp, reps=sim_reps, domain_level = "domain")
rf_res32 = rf_wrapper("y ~ noise1+noise2+real_x", "8", xpop = full_samp, reps=sim_reps, domain_level = "domain")

rf_results <- list(rf_res8, rf_res16, rf_res32) %>% reduce(full_join) %>% 
        mutate(sample_size = as.character(sample_size))
print(rf_results)

########################### SMERF ################################
smerf8 <- merf_wrapper("y ~ noise1+noise2+real_x", sample_size = "8", 
                reps = sim_reps, pop_dat = full_samp, max_iter = 50, 
                loglik_error_tol = 0.5, domain_level = "domain")
smerf16 <- merf_wrapper("y ~ noise1+noise2+real_x", sample_size = "16", 
                reps = sim_reps, pop_dat = full_samp, max_iter = 50, 
                loglik_error_tol = 0.5, domain_level = "domain")
smerf32 <- merf_wrapper("y ~ noise1+noise2+real_x", sample_size = "32", 
                reps = sim_reps, pop_dat = full_samp, max_iter = 50, 
                loglik_error_tol = 0.5, domain_level = "domain")

smerf_results <- list(smerf8, smerf16, smerf32) %>% reduce(full_join)


########################### Area-EBLUP ################################
# area and unit needs mean values by domain
pixel_means <- full_samp %>% 
  group_by(domain) %>% 
  summarise(
    across(.cols = y:real_x, .fns = mean), npixels = n()
  ) %>% 
  ungroup() %>% 
  as.data.frame()

# run results
a_res8 = area_eblup_sim("y ~ noise1 + noise2 + real_x", "8", pixel_means, sim_reps)
a_res16 = area_eblup_sim("y ~ noise1 + noise2 + real_x", "16", pixel_means, sim_reps)
a_res32 = area_eblup_sim("y ~ noise1 + noise2 + real_x", "32", pixel_means, sim_reps)

aeblup_results <- list(a_res8, a_res16, a_res32) %>% reduce(full_join) %>% rename(BA_hat = BA_est)

#area_eblup(samp, pixel_means, "y ~ noise1 + noise2 + real_x", domain)


########################### Unit-EBLUP ################################

ueblupt8  <- unit_eblup_sim("y ~ noise1 + noise2 + real_x",  "8", pixel_means, sim_reps, domain_level = domain)
ueblupt16 <- unit_eblup_sim("y ~ noise1 + noise2 + real_x", "16", pixel_means, sim_reps, domain_level = domain)
ueblupt32 <- unit_eblup_sim("y ~ noise1 + noise2 + real_x", "32", pixel_means, sim_reps, domain_level = domain)

ueblupt_res <- list(ueblupt8, ueblupt16, ueblupt32) %>% reduce(full_join) %>% rename(BA_hat = BA_est)


######################## Combine and SAVE #############################


all_results = list(ps_results, rf_results, smerf_results, aeblup_results, ueblupt_res) %>% reduce(full_join)



all_results %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/toy/results500.csv")



######################## Evaluate and Plot ##############################
all_results <- read.csv("~/Documents/Research/Thesis/RF-Forests/models/toy/results500.csv")

all_results <- all_results %>% filter(model != "rf")

# get bias results
e_bias_results <- all_results %>% 
  group_by(domain, model, sample_size) %>% 
  summarise(sim_est = mean(BA_hat, na.rm = T), true_resp = mean(BA, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(e_bias = sim_est - true_resp,
         perc_rel_e_bias = (e_bias/true_resp)*100,
         sample_size = as.factor(sample_size)) 

# get empirical variance estimates
e_var_results <- all_results %>% 
  mutate(sample_size = as.factor(sample_size)) %>% 
  left_join(e_bias_results, by = c("domain", "model", "sample_size")) %>% 
  group_by(domain, model, sample_size) %>% 
  summarise(e_var = (2000/1999)*mean((BA_hat - sim_est)^2)) %>% 
  ungroup() 


# plot bias
unique(e_bias_results$model)

e_bias_results %>% 
  mutate(subsection = str_sub(domain, -2L, -1L)) %>% 
  mutate(subsection_label = paste0("Subsection ", subsection)) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("8", "16", "32"))) %>% 
  ggplot(aes(x = sample_size, y = abs(perc_rel_e_bias)/100, fill = model)) +
  geom_col(position = "dodge", alpha = 0.9, color = "grey50") +
  facet_wrap(~subsection_label, ncol = 4) +
  scale_fill_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d"),
    labels = c("Area EBLUP", "Post-Stratified",  "SMERF", "Unit EBLUP")
  ) +
  labs(x = "Sample Size", y = "Absolute Percent Relative Bias", fill = "Model") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(
    text = element_text(size = 20)
  ) 




e_var_results %>%
  left_join(e_bias_results, by = c("domain", "model", "sample_size")) %>% 
  mutate(e_mse = e_var + e_bias^2) %>% 
  mutate(subsection = str_sub(domain, -2L, -1L)) %>% 
  mutate(subsection_label = paste0("Subsection ", subsection)) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("30", "50", "100"))) %>% 
  ggplot(aes(x = sample_size, y = e_mse, fill = model)) +
  geom_col(position = "dodge", alpha = 0.9, color = "grey50") +
  facet_wrap(~subsection_label, ncol = 4) +
  scale_fill_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d"),
    labels = c("Area EBLUP", "Post-Stratified",  "SMERF", "Unit EBLUP")
  ) +
  labs(x = "Sample Size", y = "Empirical MSE", fill = "Model") +
  theme_bw()  +
  theme(
    text = element_text(size = 20)
  )











# e_bias_results %>% 
#   mutate(subsection = str_sub(domain, -2L, -1L)) %>% 
#   mutate(subsection_label = paste0("Subsection ", subsection)) %>% 
#   mutate(sample_size = fct_relevel(sample_size, c("8", "16", "32"))) %>% 
#   ggplot(aes(x = sample_size, y = abs(perc_rel_e_bias)/100, fill = model)) +
#   geom_col(position = "dodge", alpha = 0.9, color = "grey50") +
#   facet_wrap(~subsection_label, ncol = 4) +
#   scale_fill_manual(
#     values = c("#92abd6", "#3a32d1", "#d94c4c", "#bf60b7", "#96b88d"),
#     labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP")
#   ) +
#   labs(x = "Sample Size", y = "Absolute Percent Relative Bias", fill = "Model") +
#   scale_y_continuous(labels = scales::percent) +
#   theme_bw() +
#   theme(
#     text = element_text(size = 20)
#   ) 
# 
# 
# 
# 
# e_var_results %>%
#   left_join(e_bias_results, by = c("domain", "model", "sample_size")) %>% 
#   mutate(e_mse = e_var + e_bias^2) %>% 
#   mutate(subsection = str_sub(domain, -2L, -1L)) %>% 
#   mutate(subsection_label = paste0("Subsection ", subsection)) %>% 
#   mutate(sample_size = fct_relevel(sample_size, c("30", "50", "100"))) %>% 
#   ggplot(aes(x = sample_size, y = e_mse, fill = model)) +
#   geom_col(position = "dodge", alpha = 0.9, color = "grey50") +
#   facet_wrap(~subsection_label, ncol = 4) +
#   scale_fill_manual(
#     values = c("#92abd6", "#3a32d1", "#d94c4c", "#bf60b7", "#96b88d", "#e3a88a"),
#     labels = c("Area EBLUP","MERF","Post-Stratified", "Random Forest", "Unit EBLUP", "Unit ZI")
#   ) +
#   labs(x = "Sample Size", y = "Empirical MSE", fill = "Model") +
#   theme_bw()  +
#   theme(
#     text = element_text(size = 20)
#   )




all_results %>% group_by(model) %>%
  summarize(cnt = n())
bias_results <- all_results %>% group_by(model, sample_size, domain) %>%
  summarize(bias = abs(mean(BA_hat) - mean(BA))) %>%
  group_by(model) %>%
  summarize(mean_bias = mean(bias))
bias_results
