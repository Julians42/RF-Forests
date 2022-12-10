library(tidyverse)
library(dplyr)

set.seed(101)
# generate subsections and means
subsec = c("A", "B", "C", "D", "E", "F", "G", "H")
means = rbeta(8, 3, 3)*100
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
sim_samps_8 = full_gen(full_samp, 8, reps = 500) %>%
    write.csv("~/Documents/Research/Thesis/RF-Forests/models/toy/sim_samp_8n.csv")

sim_samps_16 = full_gen(full_samp, 16, reps = 500) %>%
    write.csv("~/Documents/Research/Thesis/RF-Forests/models/toy/sim_samp_16n.csv")
  
sim_samps_32 = full_gen(full_samp, 32, reps = 500) %>%
    write.csv("~/Documents/Research/Thesis/RF-Forests/models/toy/sim_samp_32n.csv")


# run the models on the toy dataset. 
