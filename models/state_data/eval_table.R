# Julian Schmitt Senior Thesis
# Evaluate Models
library(tidyverse)
library(gt)

results.ntcc <- read.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/results2000_ntcc.csv") %>%
                        drop_na()
results.tcc <- read.csv("~/Documents/Research/Thesis/RF-Forests/models/state_data/results2000.csv") %>%
                        drop_na()

state.df <- unique(results.ntcc %>% select(domain))
state.df$state <- c("Iowa", "Iowa", "Georgia", "Georgia", "Georgia", "Georgia",
                      "Georgia", "Oregon", "Iowa", "Iowa", "Idaho", "Oregon",
                      "OR/ID", "Idaho", "Oregon", "Oregon", "Idaho",
                      "Georgia",
                      "Oregon", "Oregon", "Oregon", "Oregon", "Oregon", "Idaho",
                      "Idaho", "Idaho", "Idaho", "Oregon", "Idaho", "Idaho")


get.bias <- function(df) {
  res <- df %>% 
    group_by(domain, model, sample_size) %>% 
    summarise(sim_est = mean(BA_hat, na.rm = T), true_resp = mean(BA, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(e_bias = sim_est - true_resp,
           perc_rel_e_bias = (e_bias/true_resp)*100,
           sample_size = as.factor(sample_size)) 
  res <- res %>% mutate(section = domain) %>% 
              left_join(state.df, by = "domain")
  res
}

get.emse <- function(df, bias.res) {
  res <- df %>% 
    mutate(sample_size = as.factor(sample_size)) %>% 
    left_join(bias.res, by = c("domain", "model", "sample_size")) %>% 
    group_by(domain, model, sample_size) %>% 
    summarise(e_var = (2000/1999)*mean((BA_hat - sim_est)^2)) %>% 
    ungroup() 
  res <- res %>% mutate(section = domain) %>% 
    left_join(state.df, by = "domain")
  res
}

# get bias and emse results
tcc.bias <- get.bias(results.tcc)
ntcc.bias <- get.bias(results.ntcc)
tcc.emse <- get.emse(results.tcc, tcc.bias)
ntcc.emse <- get.emse(results.ntcc, ntcc.bias)

tcc.emse
################### comparison plots #####################
comparison.tcc <- tcc.bias %>% select(section, model, sample_size, e_bias) %>%
  group_by(section, model, sample_size) %>% 
  pivot_wider(names_from = model, values_from = e_bias) %>% 
  mutate_all(abs) %>% 
  get.df()



get.df <- function(dat.comp) {
  models <- c("area_eblup", "ps", "rf", "SMERF", "unit_eblup", "unit_zi")
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  
  for (model in models) {
    for (model2 in models) {
      num <- sum(dat.comp[, model] < dat.comp[, model2])
      df <- rbind(df, (c(model, model2, num)))
      
    }
  }
  colnames(df) <- c("model1", "model2", "wins")
  df2 <-df %>% pivot_wider(names_from = model2, values_from = wins)
  df2$model1 <- c("Area-EBLUP", "Post-Stratified", "Random-Forest", 
                  "SMERF", "Unit-EBLUP", "Unit-ZI")
  colnames(df2) <- c("Model", "Area-EBLUP", "Post-Stratified", "Random-Forest",
                     "SMERF", "Unit-EBLUP", "Unit-ZI")
  # add total
  df2 <- df2 %>% 
    mutate(Total = rowSums(df2 %>% 
                             select(-Model) %>% 
                             mutate_all(as.numeric)))
}

comparison.tcc.emse <- tcc.emse %>% select(domain, model, sample_size, e_var) %>%
  left_join(tcc.bias, by = c("domain", "model", "sample_size")) %>% 
  mutate(e_mse = e_var + e_bias^2) %>% 
  select(section, model, sample_size, e_mse) %>% 
  group_by(section, model, sample_size) %>% 
  pivot_wider(names_from = model, values_from = e_mse) %>% 
  get.df() #%>% 
comparison.tcc.emse
# comparison.tcc.emse <- comparison.tcc.emse %>% 
#   mutate(Total = rowSums(comparison.tcc.emse %>% 
#                            select(-Model) %>% 
#                            mutate_all(as.numeric)))


# 
# df2 <- df2 %>% 
#   mutate(Total = rowSums(df2 %>% 
#                            select(-Model) %>% 
#                            mutate_all(as.numeric)))


gt(comparison.tcc)%>% tab_header(
    title= md("**Pairwise Bias Comparison**"),
    subtitle = md("*Number of wins of row model over column model (with tcc16)*")
  ) %>%   
  opt_row_striping() %>%
  cols_align(align = "center") #%>%
  gtsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/pairwise_bias_tcc.png")

gt(comparison.tcc.emse) %>% tab_header(
  title= md("**Pairwise Empirical MSE Comparison**"),
  subtitle = md("*Number of wins of row model over column model (with tcc16)*")
) %>%   
  opt_row_striping() %>%
  cols_align(align = "center") %>% 
  gtsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/pairwise_emse_tcc.png")


# without tcc
comparison.ntcc.emse <- ntcc.emse %>% select(domain, model, sample_size, e_var) %>%
  left_join(ntcc.bias, by = c("domain", "model", "sample_size")) %>% 
  mutate(e_mse = e_var + e_bias^2) %>% 
  select(section, model, sample_size, e_mse) %>% 
  group_by(section, model, sample_size) %>% 
  pivot_wider(names_from = model, values_from = e_mse) %>% 
  get.df() 
comparison.ntcc.emse


gt(comparison.ntcc.emse) %>% tab_header(
  title= md("**Pairwise Empirical MSE Comparison**"),
  subtitle = md("*Number of wins of row model over column model (without tcc16)*")
) %>%   
  opt_row_striping() %>%
  cols_align(align = "center") %>% 
  gtsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/pairwise_emse_ntcc.png")




comparison.ntcc.bias <- ntcc.bias %>% select(section, model, sample_size, e_bias) %>%
  mutate(e_bias = abs(e_bias)) %>%
  group_by(section, model, sample_size) %>% 
  pivot_wider(names_from = model, values_from = e_bias) %>% 
  get.df() 

gt(comparison.ntcc.bias) %>% tab_header(
  title= md("**Pairwise Bias Comparison**"),
  subtitle = md("*Number of wins of row model over column model (without tcc16)*")
) %>%   
  opt_row_striping() %>%
  cols_align(align = "center") %>% 
  gtsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/pairwise_bias_ntcc.png")

################## Get reordering for carbon ############
pop.summ <- pop.dat %>% group_by(SECTION) %>% 
  summarize(car_mean = mean(CARBON_AG_TPA_live_ADJ)) %>%
  left_join(state.df %>% rename(SECTION = domain), by = "SECTION") %>% 
  mutate(sec.st.label = paste0(state, ": ", SECTION)) %>% 
  arrange(., car_mean)

pop.summ$SECTION
################### Facet Plots ##################
ptest <- ntcc.bias %>% 
  mutate(section = domain) %>% 
  mutate(section_label = paste0(state, ": ", section)) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("4", "8", "16", "32")),
         section_label = fct_relevel(section_label, pop.summ$sec.st.label)) %>% 
  ggplot(aes(x = sample_size, y = perc_rel_e_bias, color = model, group = model)) +
  geom_line() +
  facet_wrap(~section_label, scale = "free_y",ncol = 6) +
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "Sample Size", y = "Percent Relative Empirical Bias", 
       group = "Model", title = "Percent Relative Empirical Bias",
       subtitle = "Model run without tcc16 as predictor, facets in ascending order by Carbon") +
  scale_y_continuous() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  )
ptest
ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/facet/ntcc_rel_bias.png",
      device = "png",
      width = 9,
      height = 7,
      dpi = 200)


p.tcc.bias <- tcc.bias %>% 
  mutate(section = domain) %>% 
  mutate(section_label = paste0(state, ": ", section)) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("4", "8", "16", "32")),
         section_label = fct_relevel(section_label, pop.summ$sec.st.label)) %>% 
  ggplot(aes(x = sample_size, y = perc_rel_e_bias, color = model, group = model)) +
  geom_line() +
  facet_wrap(~section_label, scale = "free_y",ncol = 6) +
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", 
               "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "Sample Size", y = "Percent Relative Empirical Bias", group = "Model", 
       title = "Percent Relative Empirical Bias",
       subtitle = "Model run with tcc16 as predictor, facets in ascending order by Carbon") +
  scale_y_continuous() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  )
p.tcc.bias
ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/facet/tcc_rel_bias.png",
       device = "png",
       width = 9,
       height = 7,
       dpi = 200)


p.ntcc.emse <- ntcc.emse %>% select(-state, -section) %>% 
  left_join(ntcc.bias, by = c("domain", "model", "sample_size")) %>% 
  mutate(e_mse = e_var + e_bias^2) %>% 
  mutate(section = domain) %>% 
  mutate(section_label = paste0(state, ": ", section)) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("4", "8", "16", "32")),
         section_label = fct_relevel(section_label, pop.summ$sec.st.label)) %>% 
  ggplot(aes(x = sample_size, y = sqrt(e_mse), color = model, group = model)) +
  geom_line() +
  facet_wrap(~section_label, scales = "free_y", ncol = 6) +
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "Sample Size", y = "Root Empirical MSE", color = "Model", 
       title = "Model Root EMSE",
       subtitle = "Model run without tcc16 as predictor, facets in ascending order by Carbon") +
  theme_bw()  +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  )
p.ntcc.emse
ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/facet/ntcc.rt.emse.png",
       device = "png",
       width = 9,
       height = 7,
       dpi = 200)


tcc.emse
p.tcc.emse <- tcc.emse %>% select(-state, -section) %>%
  left_join(tcc.bias, by = c("domain", "model", "sample_size")) %>% 
  mutate(e_mse = e_var + e_bias^2) %>% 
  mutate(section = domain) %>% 
  mutate(section_label = paste0(state, ": ", section)) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("4", "8", "16", "32")),
         section_label = fct_relevel(section_label, pop.summ$sec.st.label)) %>% 
  ggplot(aes(x = sample_size, y = sqrt(e_mse), color = model, group = model)) +
  geom_line() +
  facet_wrap(~section_label, scales = "free_y", ncol = 6) +
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "Sample Size", y = "Root Empirical MSE", color = "Model", 
       title = "Model Root EMSE",
       subtitle = "Model Run with tcc16 as predictor, facets in ascending order by Carbon") +
  theme_bw()  +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  )
p.tcc.emse
ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/facet/tcc.rt.emse.png",
       device = "png",
       width = 9,
       height = 7,
       dpi = 200)







#%>%
# data_color(
#   columns = c(area_eblup, ps, rf, SMERF, unit_eblup, unit_zi),
#   # columns = c("Area-EBLUP", "Post-Stratified", "Random-Forest",
#   #             "SMERF", "Unit-EBLUP", "Unit-ZI"),
#   colors = scales::col_numeric(
#     palette = c("#1C5C9F","white", "#A51429"),
#     domain = c(0, 90)
#   ),
#   alpha = 1
# )

#v <- c(area_eblup, ps, rf, SMERF, unit_eblup, unit_zi)
