# Julian Schmitt Senior Thesis
# Evaluate Models
library(tidyverse)
library(gt)
library(patchwork)

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

################## Get reordering for carbon ############
pop.summ <- pop.dat %>% group_by(SECTION) %>% 
  summarize(car_mean = mean(CARBON_AG_TPA_live_ADJ)) %>%
  left_join(state.df %>% rename(SECTION = domain), by = "SECTION") %>% 
  mutate(sec.st.label = paste0(state, ": ", SECTION, ", ", round(car_mean*2.2417,1), " tonnes/ha")) %>% 
  arrange(., car_mean)

pop.summ$SECTION

pop.summ

# add facet labels, convert evar to rmse
m.ntcc.bias <- ntcc.bias %>% select(-state) %>% mutate(SECTION = domain) %>% 
  left_join(pop.summ, by = "SECTION") %>% 
  mutate(section = domain, section_label = sec.st.label) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("4", "8", "16", "32")),
         section_label = fct_relevel(section_label, pop.summ$sec.st.label))


m.tcc.bias <- tcc.bias %>% select(-state) %>% mutate(SECTION = domain) %>% 
  left_join(pop.summ, by = "SECTION") %>% 
  mutate(section = domain, section_label = sec.st.label) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("4", "8", "16", "32")),
         section_label = fct_relevel(section_label, pop.summ$sec.st.label))
m.tcc.bias

m.ntcc.emse <- ntcc.emse %>% select(-state, -section) %>% 
  left_join(ntcc.bias, by = c("domain", "model", "sample_size")) %>% 
  mutate(e_mse = e_var + e_bias^2) %>% 
  select(-state) %>% 
  mutate(SECTION = domain) %>% 
  left_join(pop.summ, by = "SECTION") %>% 
  mutate(section = domain, section_label = sec.st.label) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("4", "8", "16", "32")),
         section_label = fct_relevel(section_label, pop.summ$sec.st.label))


m.tcc.emse <- tcc.emse %>% select(-state, -section) %>%
  left_join(tcc.bias, by = c("domain", "model", "sample_size")) %>% 
  mutate(e_mse = e_var + e_bias^2) %>% 
  select(-state) %>% 
  mutate(SECTION = domain) %>% 
  left_join(pop.summ, by = "SECTION") %>% 
  mutate(section = domain, section_label = sec.st.label) %>% 
  mutate(sample_size = fct_relevel(sample_size, c("4", "8", "16", "32")),
         section_label = fct_relevel(section_label, pop.summ$sec.st.label))

m.ntcc.emse


################### Facet Plots ##################

p.ntcc.bias <- m.ntcc.bias %>% 
  filter(section %in% c("342I", "331A", "232B", "M242A")) %>% 
  ggplot(aes(x = sample_size, 
            y = perc_rel_e_bias, 
            color = model, 
            group = model)) +
  geom_line() +
  facet_wrap(~section_label, scale = "free_y",ncol = 2) +
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = NULL, y = NULL, 
       color = "Model", title = "Percent Relative Empirical Bias, without tcc16")+
  scale_y_continuous() +
  theme_bw() +
  theme(
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    text = element_text(size = 12)
  )



p.tcc.bias <- m.tcc.bias %>% 
  filter(section %in% c("342I", "331A", "232B", "M242A")) %>% 
  ggplot(aes(x = sample_size, y = perc_rel_e_bias, color = model, group = model)) +
  geom_line() +
  facet_wrap(~section_label, scale = "free_y",ncol = 2) +
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", 
               "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = NULL, y = "Percent Relative Empirical Bias", color = "Model", 
       title = "Percent Relative Empirical Bias, with tcc16")+
  scale_y_continuous() +
  theme_bw() +
  theme(
    #axis.text.x=element_blank(),
    text = element_text(size = 12)
  )

#  axis.text.x=element_blank()
p.ntcc.emse <- m.ntcc.emse %>% 
  filter(section %in% c("342I", "331A", "232B", "M242A")) %>% 
  ggplot(aes(x = sample_size, y = sqrt(e_mse), color = model, group = model)) +
  geom_line() +
  facet_wrap(~section_label, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "Sample Size", y = NULL, color = "Model", 
       title = "Model Root EMSE, without tcc16")+
  theme_bw()  +
  theme(
    text = element_text(size = 12)
  )


p.tcc.emse <- m.tcc.emse %>%   
  filter(section %in% c("342I", "331A", "232B", "M242A")) %>% 
  ggplot(aes(x = sample_size, y = sqrt(e_mse), color = model, group = model)) +
  geom_line() +
  facet_wrap(~section_label, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "Sample Size", y = "Root Empirical MSE", color = "Model", 
       title = "Model Root EMSE, with tcc16")+
  theme_bw()  +
  theme(
    text = element_text(size = 12)
  )

p.tcc.bias + p.ntcc.bias + p.tcc.emse+p.ntcc.emse+ 
  plot_layout(ncol = 2, guides = "collect")&
  theme_gray()

ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/facet/sample4.png",
       device = "png",
       width = 13,
       height = 10,
       dpi = 200)
