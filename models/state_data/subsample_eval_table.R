# Comparison table subset by carbon

# get carbon levels
pop.summ2 <- pop.dat %>% group_by(SECTION) %>% 
  summarize(car_mean = mean(CARBON_AG_TPA_live_ADJ)*2.2417, 
            perc_zero = sum(CARBON_AG_TPA_live_ADJ == 0)/n()) %>%
  left_join(state.df %>% rename(SECTION = domain), by = "SECTION") %>% 
  mutate(sec.st.label = paste0(state, ": ", SECTION, ", ", round(car_mean,1), " tonnes/ha")) %>% 
  arrange(., car_mean)

ggplot(pop.summ2, aes(x = car_mean, y = perc_zero))+geom_point()+
  labs(title = "Percent Zero Observations by Average Section Carbon",
       x = "Average Section Carbon (Metric tonnes /acre)",
       y = "Section Percent Zero Observations")+
  theme_gray()

ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/perc_zero_by_carbon.png",
       device = "png",
       width=5,
       height = 4,
       dpi = 200)

pop.summ3 <- pop.summ2 %>% select(SECTION, car_mean, perc_zero) %>% rename(section = SECTION)


# REDO EVAR to RMSE geez big error 
comparison.tcc.emse <- tcc.emse %>% select(section, model, sample_size, e_var) %>%
  left_join(pop.summ3, by = "section") %>% 
  filter(perc_zero > .75) %>% 
  group_by(section, model, sample_size) %>% 
  pivot_wider(names_from = model, values_from = e_var) %>% 
  get.df() %>% 
  gt() %>% tab_header(
    title= md("**Pairwise Empirical MSE Comparison**"),
    subtitle = md("*Number of wins of row model over column model (with tcc16)*")
  ) %>%   
  opt_row_striping() %>%
  cols_align(align = "center")
comparison.tcc.emse

tcc.emse



