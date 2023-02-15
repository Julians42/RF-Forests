library(tidyverse)
library(dplyr)
library(here)
library(doParallel)
library(sae)
library(lme4)
library(data.table)
library(hbsae)
library(randomForest)
select <- dplyr::select
registerDoParallel(cores = 7) # register parallel backend - leave one core for other processes

# equation with tcc16
tree_eq = as.formula("CARBON_AG_TPA_live_ADJ ~ tcc16 + elev + ppt + tmean + tmin01 + tri + def")

# rep parameter
sim_reps = 2000 # up to 2000

n_small_areas = 30

# set directory and load models
setwd("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/")
source("models/state_data/models.R")

# population data
pop_dat <- read.csv("/Users/julianschmitt/Documents/Research/Thesis/states/allv_carbon_dat_section_filtered.csv")

# sim data
sim8 <- read.csv(here("data/state4_sim8_2000.csv"))
sim16 <- read.csv(here("data/state4_sim16_2000.csv"))
sim32 <- read.csv(here("data/state4_sim32_2000.csv"))

sim_samples <- list(sim8, sim16, sim32)
names(sim_samples) = c("8", "16", "32")

# sample for debugging 
samp = sim_samples[["8"]] %>% filter(sample == 1)



########################### PS ################################
print("Running PS...")
ps8 <- ps_estimator("8", pop_dat) %>% head(n_small_areas*sim_reps) # ps_estimator runs all models on call 
ps16 <- ps_estimator("16", pop_dat) %>% head(n_small_areas*sim_reps)
ps32 <- ps_estimator("32", pop_dat) %>% head(n_small_areas*sim_reps)

ps_results <- list(ps8, ps16, ps32) %>% reduce(full_join)
ps_results %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/ps_res.csv")
######################## get section means ##########################
varnames <- sim_samples[["8"]] %>% colnames()# %>% as.list()
varnames <- varnames[varnames != "sample"]
pixel_means <- pop_dat %>% select(varnames, -SUBSECTION, -X) %>% 
  drop_na() %>%
  group_by(SECTION) %>% 
  summarise(
    across(.cols = tcc16:CARBON_AG_TPA_live_ADJ, .fns = mean), npixels = n()
  ) %>% 
  ungroup() %>% 
  as.data.frame()
pixel_means

############################## area EBLUP ##########################
print("Running Area EBLUP...")
aeblup8 <- area_eblup_sim(tree_eq, "8", pixel_means, sim_reps, domain_level = "SECTION")
aeblup16 <- area_eblup_sim(tree_eq, "16", pixel_means, sim_reps, domain_level = "SECTION")
aeblup32 <- area_eblup_sim(tree_eq, "32", pixel_means, sim_reps, domain_level = "SECTION")

aeblup_results <- list(aeblup8, aeblup16, aeblup32) %>% reduce(full_join) %>% rename(BA_hat = BA_est)
aeblup_results %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/aeblup_results.csv")
########################### Unit-EBLUP ################################
print("Running Unit EBLUP...")
ueblup8  <- unit_eblup_sim(tree_eq,  "8", pixel_means, sim_reps, domain_level = domain)
ueblup16 <- unit_eblup_sim(tree_eq, "16", pixel_means, sim_reps, domain_level = domain)
ueblup32 <- unit_eblup_sim(tree_eq, "32", pixel_means, sim_reps, domain_level = domain)

ueblup_res <- list(ueblup8, ueblup16, ueblup32) %>% reduce(full_join) %>% rename(BA_hat = BA_est)
ueblup_res %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/ueblup_res.csv")


########################### Mixed ZI ################################
print("Running Zero-Inflated Model...")
uzi8 <- unit_zi_sim(tree_eq, pop_dat, "8", sim_reps, pixel_means)
uzi16 <- unit_zi_sim(tree_eq, pop_dat, "16", sim_reps, pixel_means)
uzi32 <- unit_zi_sim(tree_eq, pop_dat, "32", sim_reps, pixel_means)

uzi_res <- list(uzi8, uzi16, uzi32) %>% reduce(full_join) %>% rename(BA_hat = BA_est)
uzi_res %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/uzi_res.csv")


########################### RF ################################
print("Running Random Forest...")
rf8 <- rf_wrapper(tree_eq, "8", xpop = pop_dat, reps = sim_reps, domain_level = "SECTION")
rf16 <- rf_wrapper(tree_eq, "16", xpop = pop_dat, reps = sim_reps, domain_level = "SECTION")
rf32 <- rf_wrapper(tree_eq, "32", xpop = pop_dat, reps = sim_reps, domain_level = "SECTION")

rf_res <- list(rf8, rf16, rf32) %>% reduce(full_join) %>% 
        mutate(sample_size = as.character(sample_size))
rf_res %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/rf_res.csv")


########################### SMERF ################################
print("Running SMERF...")
smerf8 <- smerf_wrapper(tree_eq, sample_size = "8", 
            reps = sim_reps, pop_dat = pop_dat, 
            domain_level = "SECTION", max_iter = 10)
smerf16 <- smerf_wrapper(tree_eq, sample_size = "16", 
            reps = sim_reps, pop_dat = pop_dat, 
            domain_level = "SECTION", max_iter = 10)
smerf32 <- smerf_wrapper(tree_eq, sample_size = "32", 
            reps = sim_reps, pop_dat = pop_dat, 
            domain_level = "SECTION", max_iter = 10)


smerf_res <- list(smerf8, smerf16, smerf32) %>% reduce(full_join)
smerf_res %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/smerf_res.csv")


all_results = list(ps_results, rf_res, smerf_res, aeblup_results, ueblup_res, uzi_res) %>% reduce(full_join)

all_results %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/results2000.csv")