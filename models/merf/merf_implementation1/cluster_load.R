# load packages and data on cluster

library(dplyr)
library(readr)
library(data.table)
library(mase)
library(sae)
library(hbsae)
library(here)
library(foreach)
library(broom)
library(doParallel)
library(stringr)
library(LongituRF)
library(randomForest)
registerDoParallel(cores = detectCores())
select <- dplyr::select


# loading in pixel level population data
pixel_data <- read_csv("/n/holylabs/LABS/kmcconville_lab/Everyone/UFDS22/data/pixel_m333_ba.csv")


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

samples_30n <- read_csv("/n/holylabs/LABS/kmcconville_lab/Everyone/UFDS22/data/knn_imputed_sim_samples/samples_30n.csv") %>% 
  mutate(s_size = 30) %>% 
  rename(BA = BA_new)
samples_50n <- read_csv("/n/holylabs/LABS/kmcconville_lab/Everyone/UFDS22/data/knn_imputed_sim_samples/samples_50n.csv") %>% 
  mutate(s_size = 50) %>% 
  rename(BA = BA_new)
samples_100n <- read_csv("/n/holylabs/LABS/kmcconville_lab/Everyone/UFDS22/data/knn_imputed_sim_samples/samples_100n.csv") %>% 
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