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
sim4 <- read.csv(here("data/state4_sim4_2000.csv"))


sim_samples <- list(sim4)
names(sim_samples) = c("4")

# sample for debugging 
samp = sim_samples[["4"]] %>% filter(sample == 1)



########################### PS ################################
print("Running PS...")
ps8 <- ps_estimator("4", pop_dat) %>% head(n_small_areas*sim_reps) # ps_estimator runs all models on call 

ps_results <- ps8
ps_results %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/ps_res4.csv")
######################## get section means ##########################
varnames <- sim_samples[["4"]] %>% colnames()# %>% as.list()
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
aeblup8 <- area_eblup_sim(tree_eq, "4", pixel_means, sim_reps, domain_level = "SECTION")

aeblup_results <- aeblup8
aeblup_results %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/aeblup_results4.csv")
########################### Unit-EBLUP ################################
print("Running Unit EBLUP...")
ueblup8  <- unit_eblup_sim(tree_eq, "4", pixel_means, sim_reps, domain_level = domain)

ueblup_res <- ueblup8
ueblup_res %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/ueblup_res4.csv")


########################### Mixed ZI ################################
print("Running Zero-Inflated Model...")
uzi4 <- unit_zi_sim(tree_eq, pop_dat, "4", sim_reps, pixel_means)

uzi_res <- uzi4
uzi_res %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/uzi_res4.csv")


########################### RF ################################
print("Running Random Forest...")
rf4 <- rf_wrapper(tree_eq, "4", xpop = pop_dat, reps = sim_reps, domain_level = "SECTION")

rf_res <- rf4
rf_res %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/rf_res4.csv")


########################### SMERF ################################
print("Running SMERF...")
smerf4 <- smerf_wrapper(tree_eq, sample_size = "4", 
            reps = sim_reps, pop_dat = pop_dat, 
            domain_level = "SECTION", max_iter = 10)



smerf_res <- smerf4
smerf_res %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/smerf_res4.csv")


all_results = list(ps_results, rf_res, smerf_res, aeblup_results, ueblup_res, uzi_res) %>% reduce(full_join)

all_results %>% write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/results2000_4.csv")




smerf_res4 <- read.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/smerf_res4.csv")
rf_res4 <- read.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/rf_res4.csv")
uzi_res4 <- read.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/uzi_res4.csv")
ueblup_res4 <- read.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/ueblup_res4.csv")
aeblup_res4 <- read.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/aeblup_results4.csv")
ps_res4 <- read.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/ps_res4.csv")

res4_tcc <- list(ps_res4, rf_res4, smerf_res4, aeblup_res4, ueblup_res4, uzi_res4) %>% reduce(full_join) %>%
  write.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/results2000_4.csv")

