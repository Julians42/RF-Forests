library(tidyverse)


source("models/state_data/state4_get_results.R")
# all_results <- read.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/results2000.csv")
# all_results <- all_results %>% drop_na()

# # get bias results
# e_bias_results <- all_results %>% 
#   group_by(domain, model, sample_size) %>% 
#   summarise(sim_est = mean(BA_hat, na.rm = T), true_resp = mean(BA, na.rm = T)) %>% 
#   ungroup() %>% 
#   mutate(e_bias = sim_est - true_resp,
#          perc_rel_e_bias = (e_bias/true_resp)*100,
#          sample_size = as.factor(sample_size)) 
# 
# # get empirical variance estimates
# e_var_results <- all_results %>% 
#   mutate(sample_size = as.factor(sample_size)) %>% 
#   left_join(e_bias_results, by = c("domain", "model", "sample_size")) %>% 
#   group_by(domain, model, sample_size) %>% 
#   summarise(e_var = (2000/1999)*mean((BA_hat - sim_est)^2)) %>% 
#   ungroup() 
# 
# dim(all_results %>% drop_na())
# 
# all_results %>% filter(model == "area_eblup") %>% head()
# e_bias_results %>% group_by(model) %>% summarize(avg_bias = mean(abs(e_bias)))
# e_var_results %>% group_by(model) %>% summarize(avg_emse = mean(abs(e_var)))
# 
# #anti_join(all_results, all_results %>% drop_na())
# 
# add_states <- unique(all_results %>% select(domain))
# add_states$state <- c("IA", "IA", "GA", "GA", "GA", "GA",
#                       "GA", "OR", "IA", "IA", "ID", "OR/ID",
#                       "OR/ID", "OR/ID", "OR/ID", "OR/ID", "OR/ID",
                      "")
# plot bias
#unique(e_bias_results$model)

e_bias_results %>% 
  mutate(subsection = domain) %>% 
  mutate(subsection_label = paste0("Section ", subsection)) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("8", "16", "32"))) %>% 
  ggplot(aes(x = sample_size,  y = abs(e_bias), fill = model)) +
  geom_col(position = "dodge", alpha = 0.9, color = "grey50") +
  facet_wrap(~subsection_label,scales = "free_y", ncol = 6) +
  scale_fill_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "Sample Size", y = "Absolute Bias", fill = "Model") +
  scale_y_continuous() +
  theme_bw() +
  theme(
    text = element_text(size = 20)
  ) 

sum(is.na(e_var_results %>% filter(model == "area_eblup") %>% select(e_var)))


e_var_results %>%
  left_join(e_bias_results, by = c("domain", "model", "sample_size")) %>% 
  mutate(e_mse = e_var + e_bias^2) %>% 
  mutate(subsection = domain) %>% 
  mutate(subsection_label = paste0("Section ", subsection)) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("8", "16", "32"))) %>% 
  ggplot(aes(x = sample_size, y = e_mse, fill = model)) +
  geom_col(position = "dodge", alpha = 0.9, color = "grey50") +
  facet_wrap(~subsection_label, scales = "free_y", ncol = 6) +
  scale_fill_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "Sample Size", y = "Empirical MSE", fill = "Model") +
  theme_bw()  +
  theme(
    text = element_text(size = 20)
  )






e_var_results %>%
  left_join(e_bias_results, by = c("domain", "model", "sample_size")) %>% 
  mutate(e_mse = e_var + e_bias^2) %>% 
  mutate(subsection = domain) %>% 
  mutate(subsection_label = paste0("Section ", subsection)) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("8", "16", "32"))) %>% 
  ungroup() %>%
  ggplot(aes(x = sample_size, y = e_mse, color = model, group = model)) +
  geom_line() +
  facet_wrap(~subsection_label, scales = "free_y", ncol = 6) +
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "Sample Size", y = "Empirical MSE", fill = "Model") +
  theme_bw()  +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  )

head(e_bias_results)

e_bias_results %>% 
  mutate(subsection = domain) %>% 
  mutate(subsection_label = paste0("Section ", subsection)) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("8", "16", "32"))) %>% 
  ggplot(aes(x = sample_size, y = abs(perc_rel_e_bias), color = model, group = model)) +
  geom_line() +
  facet_wrap(~subsection_label,scales = "free_y", ncol = 6) +
  scale_fill_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "Sample Size", y = "Absolute Bias", fill = "Model") +
  scale_y_continuous() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 20)
  ) 

