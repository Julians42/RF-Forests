# Creates Simulation Samples for 4 State dataset
# Julian Schmitt - Senior Thesis '23

# packages
library(tidyverse)
library(dplyr)

# 4 state dataframe
df <- read.csv("/Users/julianschmitt/Documents/Research/Thesis/states/carbon_dat_section.csv") %>% 
  filter(SECTION != "Water")

# Sample by section 
df
dim(df)
31023/39
counts <- df %>% group_by(SECTION) %>% 
  summarize(cnt = n()) %>% 
  filter(cnt > 200) %>% 
  dplyr::select(SECTION)
  #write.csv("~/Downloads/counts.csv")
df_bs <- df %>% filter(SECTION %in% counts$SECTION)
counts


sample_generation <- function(data, k) {
  
  unique_sections <- unique(data$SECTION)

  sample_data <- data %>% 
    group_by(SECTION) %>% 
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

sim_samps_8 = full_gen(df_bs, 8, reps=2000)
sim_samps_25 = full_gen(df_bs, 25, reps=2000)
sim_samps_50 = full_gen(df_bs, 50, reps=2000)

sim_samps_8 %>% write.csv("~/Documents/Research/Thesis/states/sim_samp_8n.csv")
sim_samps_25 %>% write.csv("~/Documents/Research/Thesis/states/sim_samp_25n.csv")
sim_samps_50 %>% write.csv("~/Documents/Research/Thesis/states/sim_samp_50n.csv")

# sim_samps_10 %>% filter(sample %in% c(1,2)) %>% 
#   filter(SECTION=="222L") %>% dplyr::select(tcc16, SECTION, sample)
