# Julian Schmitt Senior Thesis 
# Correlation of Model Performance with Proportion Zero Inflated

library(tidyverse)
library(here)
source("models/state_data/state4_get_results.R")

# load population data 
pop.dat <- read.csv("/Users/julianschmitt/Documents/Research/Thesis/states/allv_carbon_dat_section_filtered.csv")

# get proportion zero obs by section
zi.vs.bias <- pop.dat %>% group_by(SECTION) %>%
                  count(Bool = CARBON_AG_TPA_live_ADJ == 0) %>%
                  pivot_wider(names_from = Bool, values_from = n) %>% 
                  mutate(perc_zero = `TRUE`/(`FALSE`+`TRUE`), `TRUE`=NULL, `FALSE`=NULL) %>% 
                  left_join(pop.dat %>% select(SECTION, CARBON_AG_TPA_live_ADJ) %>% 
                              group_by(SECTION) %>% 
                              summarize_all(mean), by = "SECTION")

pop.dat %>% group_by(SECTION) %>% 
  summarize(mean_CAR = mean(CARBON_AG_TPA_live_ADJ), 
            Bool = CARBON_AG_TPA_live_ADJ ==0,
            n = n()) %>%
  pivot_wider(names_from = Bool, values_from = n) #%>%
  #mutate(perc_zero = `TRUE`/(`FALSE`+`TRUE`), `TRUE`=NULL, `FALSE`=NULL)

# compare with bias for tcc and ntcc
mult <- 10
p.zi.vs.bias <- zi.vs.bias %>% right_join(tcc.bias, by = c("SECTION" = "domain")) %>% 
  mutate(e_bias = e_bias, perc_e_bias = perc_rel_e_bias) %>% 
  #filter(sample_size == "8") %>%
  ggplot(aes(x = perc_zero*100, y = perc_e_bias, color = model))+
  geom_point(aes(shape = factor(sample_size)))+
  geom_smooth(method = "lm", se = FALSE) + 
  geom_line(aes(x = perc_zero*100, y = CARBON_AG_TPA_live_ADJ*2.2417*mult), linetype = "dashed")+
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ . * 1/mult, 
      name = "Carbon (metric tonnes per hectare)"#,
      #breaks = seq(70, 165, 5)
    )
  )+
  labs(x = "Section Percent Zero", 
       y = "Percent Relative Bias", 
       color = "Model",
       title = "Relative Percent Bias by Percent Zero",
       subtitle = "Models include tcc16 as predictor",
       shape = "Sample Size") +
  theme_bw()  +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  )
p.zi.vs.bias
ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/r2/zi.vs.bias.w.carbon.png",
       device = "png",
       width = 9,
       height = 7,
       dpi = 200)
p.zi.vs.bias

################### EMSE by Percent Zero ##################
colnames(tcc.bias)
mult2 <- .1
test <- zi.vs.bias %>% right_join(tcc.emse, 
                                  by = c("SECTION" = "domain")) %>% 
  left_join(tcc.bias %>% select(domain, e_bias, model, sample_size) %>% 
              rename("SECTION" = "domain"), 
            by = c("SECTION", "model", "sample_size")) %>% 
  mutate(e_mse = e_var + e_bias^2) %>% 
  ungroup() %>% 
  filter(e_mse < 200) 
test
test %>% ggplot(aes(x = perc_zero*100, y = sqrt(e_mse), color = model))+
  geom_point(aes(shape = factor(sample_size)))+
  geom_smooth(method = "lm", se = FALSE) + 
  geom_line(aes(x = perc_zero*100, y = CARBON_AG_TPA_live_ADJ*mult2*2.2417), linetype = "dashed")+
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", 
               "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", 
               "Random Forest", "SMERF", "Unit EBLUP", 
               "Unit Zero-Inflated")
  ) +
  #ylim(-10, 250)+
  labs(x = "Section Percent Zero", 
       y = "Root Mean Squared Error", 
       color = "Model",
       title = "RMSE by Percent Zero",
       subtitle = "Models include tcc16 as predictor",
       shape = "Sample Size") +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ . * 1/mult2, 
      name = "Carbon (metric tonnes per hectare)",
      breaks = seq(0, 200, 50)
    )
  )+
  theme_bw()  +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  )
test

ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/r2/zi.vs.emse.w.carbon.png",
       device = "png",
       width = 9,
       height = 7,
       dpi = 200)



















# zi.tcc.bias.df <- zi.vs.bias %>% right_join(tcc.bias, by = c("SECTION" = "domain")) %>% 
#   mutate(e_bias = abs(e_bias), perc_e_bias = abs(perc_rel_e_bias)) %>% 
#   filter(sample_size == "8")
# 
# zi.vs.bias %>% right_join(tcc.bias, by = c("SECTION" = "domain")) %>% 
#   mutate(e_bias = abs(e_bias), perc_e_bias = abs(perc_rel_e_bias)) %>% 
#   filter(sample_size == "32") %>%
#   ggplot(aes(x = perc_zero, y = e_bias, color = model))+
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE) + 
#   scale_color_manual(
#     values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
#     labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
#   ) +
#   labs(x = "Section Percent Zero", 
#        y = "Absolute Bias", 
#        color = "Model",
#        title = "Absolute Bias by Percent Zero",
#        subtitle = "Model with tcc, sample size 32") +
#   theme_bw()  +
#   theme(
#     legend.position = "bottom",
#     text = element_text(size = 12)
#   )

