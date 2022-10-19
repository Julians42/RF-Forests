# compute results and plots

library(tidyverse)
library(here)

other <- read_csv(here("models/explore/results/full_results_w_ps.csv"))
rf30 <- read_csv(here("models/explore/results/rf_sim_results_30_2000.csv"))
rf50 <- read_csv(here("models/explore/results/rf_sim_results_50_2000.csv"))
rf100 <- read_csv(here("models/explore/results/rf_sim_results_100_2000.csv"))
rf30$...1 <- NULL
rf50$...1 <- NULL
rf100$...1 <- NULL
# merge into one results file
full_res <- full_join(other, rf30)
full_res <- full_join(full_res, rf50)
full_res <- full_join(full_res, rf100)

full_res %>% write.csv(here("data/all_sim_results_with_rf.csv"))


# load data
full_res <- read_csv(here("~/Documents/Research/Thesis/RF-Forests/data/all_sim_results_with_rf.csv"))

# empirical bias df
e_bias_results <- full_res %>% 
  group_by(domain, model, sample_size) %>% 
  summarise(sim_est = mean(BA_hat, na.rm = T), true_resp = mean(BA, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(e_bias = sim_est - true_resp,
         perc_rel_e_bias = (e_bias/true_resp)*100) 


e_var_results <- full_res %>% 
  left_join(e_bias_results, by = c("domain", "model", "sample_size")) %>% 
  group_by(domain, model, sample_size) %>% 
  summarise(e_var = (2000/1999)*mean((BA_hat - sim_est)^2)) %>% 
  ungroup()


e_bias_results$sample_size <- as.factor(e_bias_results$sample_size)
e_bias_results %>% 
  mutate(subsection = str_sub(domain, -2L, -1L)) %>% 
  mutate(subsection_label = paste0("Subsection ", subsection)) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("30", "50", "100"))) %>% 
  ggplot(aes(x = sample_size, y = abs(perc_rel_e_bias)/100, fill = model)) +
  geom_col(position = "dodge", alpha = 0.9, color = "grey50") +
  facet_wrap(~subsection_label, ncol = 4) +
  scale_fill_manual(
    values = c("#92abd6", "#d94c4c", "#bf60b7", "#96b88d", "#e3a88a"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "Unit EBLUP", "Unit ZI")
  ) +
  labs(x = "Sample Size", y = "Absolute Percent Relative Bias", fill = "Model") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(
    text = element_text(size = 20)
  ) 



e_bias_results %>% ggplot(aes(x = sample_size, y = abs(perc_rel_e_bias)/100, fill = model)) +
  geom_col(position = "dodge", alpha = 0.9, color = "grey50") +
  facet_wrap(~domain, ncol = 4)



e_var_results$sample_size <- as.factor(e_var_results$sample_size)
e_var_results %>% 
  left_join(e_bias_results, by = c("domain", "model", "sample_size")) %>% 
  mutate(e_mse = e_var + e_bias^2) %>% 
  ggplot(aes(x = sample_size, y = e_mse, fill = model)) +
  geom_col(position = "dodge", alpha = 0.9, color = "grey50") +
  facet_wrap(~domain, ncol = 4)


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
    values = c("#92abd6", "#d94c4c", "#bf60b7", "#96b88d", "#e3a88a"),
    labels = c("Area EBLUP","Post-Stratified", "Random Forest", "Unit EBLUP", "Unit ZI")
  ) +
  labs(x = "Sample Size", y = "Empirical MSE", fill = "Model") +
  theme_bw()  +
  theme(
    text = element_text(size = 20)
  )

# generating bias summary table results
eb <- e_bias_results

eb$COMP <- abs(eb$perc_rel_e_bias)

eb_wide <- eb %>% select(domain, sample_size, COMP, model) %>% 
  pivot_wider(names_from = model, values_from = COMP)
  
eb_wide$best_model <- names(eb_wide)[apply(eb_wide, 1, which.min)]
table(eb_wide$best_model)


eb %>% select(model, COMP) %>% group_by(model) %>% 
  summarise(s = mean(COMP))


# generate MSE summary table results
e_var_results$sample_size <- as.factor(e_var_results$sample_size)
ev_wide <- e_var_results %>% 
  pivot_wider(names_from = model, values_from = e_var)
ev_wide$sample_size <- NULL
ev_wide$best_model <- names(ev_wide)[apply(ev_wide, 1, which.min)]
table(ev_wide$best_model)
e_var_results %>% select(model, e_var) %>% group_by(model) %>% 
  summarize(avg_mse = mean(e_var))


#sim_30 %>% group_by(sample) %>% summarize(n = n())


# average samples in each sample, section and tnt
sim_30_avg <- sim_30 %>% group_by(SUBSECTION, sample, tnt) %>%
  summarize(BA = mean(BA_new)) %>% ungroup()

# compute weights from the population
pop_n_sec <- pop_dat %>% group_by(SUBSECTION, tnt) %>% summarize(n = n()) %>% ungroup()
pop_n <- pop_dat %>% group_by(SUBSECTION) %>% 
  summarize(n = n()) %>% ungroup() #%>% right_join(pop_n, by = c(SUBSECTION, tnt))
pop_weights <- pop_n_sec %>% left_join(pop_n, by = c("SUBSECTION" = "SUBSECTION")) %>%
  mutate(frac = n.x/n.y, n.x = NULL, n.y = NULL)

# compute estimate 
BA_est <-sim_30_avg %>% left_join(pop_weights, by = c("SUBSECTION" = "SUBSECTION", "tnt" = "tnt")) %>% 
  mutate(mult = BA * frac) %>% 
  group_by(SUBSECTION, sample) %>%
  summarize(BA_hat = sum(mult)) %>% ungroup()


# compute truth
true_ba <- read.csv("data/pixel_population_level/pixel_m333_ba.csv") %>% group_by(SUBSECTION) %>% 
  summarize(BA = mean(BA_new))
true_ba

part_df <- BA_est %>% left_join(true_ba, by = c("SUBSECTION" = "SUBSECTION")) %>% 
  mutate(sample_size = 30, model = "ps", domain = SUBSECTION, SUBSECTION = NULL, sample = NULL)
part_df

rbind(part_df, full_results_df)


# get percent bias by subsection
part_df %>% group_by(domain) %>% 
  summarize(BA_hat_avg = mean(BA_hat), BA = mean(BA), var =var(BA_hat)) %>% 
  mutate(bias = (BA_hat_avg - BA)/BA)



# compute average zero inflation to use for correlation analysis
samp_size = 100
df <- read.csv(paste("data/sim_samples/samples_", samp_size, "n.csv", sep = ""))
bias_comp <- df %>% group_by(SUBSECTION) %>% count(Bool = BA_new ==0) %>% 
  pivot_wider(names_from = Bool, values_from = n) %>% 
  mutate(perc_zero = `TRUE`/(`FALSE`+`TRUE`), `TRUE`=NULL, `FALSE`=NULL) %>% 
  full_join(e_bias_results %>% filter(sample_size ==samp_size), by = c("SUBSECTION" = "domain")) %>% 
  mutate(BIAS = abs(perc_rel_e_bias), sample_size = NULL, sim_est = NULL, true_resp = NULL, e_bias = NULL)  
  
bias_comp %>% ggplot(aes(x = perc_zero, y = BIAS, color = model))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) + # simple linear fits
  scale_color_manual(
    values = c("#92abd6", "#BD9CBE", "#96b88d", "#e3a88a", "#bf60b7"),
    labels = c("Area EBLUP","Post-Stratified", "Unit EBLUP", "Unit ZI", "RF")
  ) +
  labs(x = "Average Percent Zeros by Subsection", y = "Average Absolute Bias (sample size = 30)", 
       color = "Model") +
  theme_bw()  +
  theme(
    text = element_text(size = 20)
  )
