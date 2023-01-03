library(tidyverse)
library(dplyr)
library(here)
library(doParallel)
library(sae)
library(lme4)
library(data.table)
library(hbsae)
select <- dplyr::select


# set directory and load data
setwd("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/")

# population data
pop_dat <- read.csv("/Users/julianschmitt/Documents/Research/Thesis/states/allv_carbon_dat_section.csv")

# sim data
sim8 <- read.csv(here("data/state4_sim8_2000.csv"))
sim16 <- read.csv(here("data/state4_sim16_2000.csv"))
sim32 <- read.csv(here("data/state4_sim32_2000.csv"))

sim_samples <- list(sim8, sim16, sim32)
names(sim_samples) = c("8", "16", "32")



########################### PS ################################

ps8 <- ps_estimator("8", pop_dat)
ps16 <- ps_estimator("16", pop_dat)
ps32 <- ps_estimator("32", pop_dat)

ps_results <- list(ps8, ps16, ps32) %>% reduce(full_join)

############################## area EBLUP ##########################
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



samp = sim_samples[["8"]] %>% filter(sample == 1)
tree_eq = as.formula("CARBON_AG_TPA_live_ADJ ~ tcc16 + elev + ppt + tmean + tmin01 + tri + def")


test <- area_eblup_sim(tree_eq, "8", pixel_means, 2000, domain_level = "SECTION")

