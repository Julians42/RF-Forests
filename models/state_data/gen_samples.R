library(tidyverse)
library(dplyr)
library(here)
library(data.table)

setwd("~/Documents/Research/Thesis/RF-Forests/models/state_data/")

vars_sub = c("SECTION", "SUBSECTION", "tcc16","elev","ppt","tmean", "tmin01", "tri", "tnt", "def", "CARBON_AG_TPA_live_ADJ")

df <- read.csv("/Users/julianschmitt/Documents/Research/Thesis/states/allv_carbon_dat_section.csv")

subdf <- df %>% select(vars_sub)

bigs <- subdf %>% group_by(SECTION) %>%
    summarize(cnt = n()) %>%
    filter(cnt > 200) %>%
    select(SECTION)


subdf_big <- subdf %>% filter(SECTION %in% bigs$SECTION)



# generate simulation samples for the toy data to match real - lets only do 500 samples for now
sample_generation <- function(data, k, domain = "domain") {
  # Generates samples of size k by

  sample_data <- data %>% 
    group_by_at(domain) %>% 
    slice_sample(n = k) %>% 
    ungroup()
  
  sample_data
  
}

full_gen <- function(data, k, reps=2000, domain = "domain") {
  samples_df <- data.frame()
  
  for (i in 1:reps) {
    samp <- data %>% 
      sample_generation(k, domain) %>% 
      mutate(sample = i)
    samples_df <- rbind(samples_df, samp)
  }
  
  samples_df
}

sim8 <- full_gen(subdf_big, 8,reps = 2000, "SECTION")
sim8 %>% write.csv("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/data/state4_sim8_2000.csv")
sim16 <- full_gen(subdf_big, 16,reps = 2000, "SECTION")
sim16 %>% write.csv("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/data/state4_sim16_2000.csv")
sim32 <- full_gen(subdf_big, 32,reps = 2000, "SECTION")
sim32 %>% write.csv("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/data/state4_sim32_2000.csv")


subdf_big %>% head()
df %>% colnames()
