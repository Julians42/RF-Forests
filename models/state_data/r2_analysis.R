# Julian Schmitt
# R2 analysis by section

library(tidyverse)
library(lme4)

pop.dat <- read.csv("/Users/julianschmitt/Documents/Research/Thesis/states/allv_carbon_dat_section_filtered.csv")


sec.names <- unique(pop.dat$SECTION)
pop.samp %>% filter(SECTION == sec.names[1]) %>% head()

pop.dat %>% group_by(SECTION) %>%
  summarise(cnt = n())

r2ed.df <- data.frame(matrix(ncol = 2))
colnames(r2ed.df) <- c("SECTION", "R2")

for (i in 1:30) {
  
  sec <- sec.names[i]
  dat <- pop.dat %>% filter(SECTION ==sec) %>% drop_na()
  if (dim(dat)[1] <5) {
    next
  }
  # run regression 
  r2.model <- lm(CARBON_AG_TPA_live_ADJ ~ tcc16, dat)
  
  # extract r2
  r2.val <- summary(r2.model)$adj.r.squared
  # add to dataframe
  r2ed.df <- rbind(r2ed.df, c(sec, r2.val))
}
r2ed.df <- r2ed.df %>% drop_na()

########################## Bias by section R2 value #######################
p.r2ed.df <- r2ed.df %>% full_join(tcc.bias, by = c("SECTION" = "domain")) %>% 
  mutate(R2 = as.numeric(R2)) %>%
  ggplot(aes(x = R2, y = abs(e_bias), color = model)) + #, shape = factor(sample_size)
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) + 
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "R2", 
       y = "Absolute Bias", 
       color = "Model",
       title = "Bias by Section R2 Value",
       shape = "Sample Size",
       subtitle = "Model with tcc, all sample sizes, R2 calculated from section regressions") +
  theme_bw()  +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 60, hjust=1)
  )
p.r2ed.df
ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/r2/r2.tcc.bias_lr.png",
       device = "png",
       width = 9,
       height = 7,
       dpi = 200)

########################## EMSE by section R2 value #######################
tcc.emse2 <- tcc.emse %>%
  left_join(tcc.bias, by = c("domain", "model", "sample_size", "section", "state")) %>% 
  mutate(e_mse = e_var + e_bias^2)
tcc.emse2

r2.df.emse <- r2ed.df %>% full_join(tcc.emse2, by = c("SECTION" = "domain")) %>% 
  mutate(R2 = as.numeric(R2)) %>%
  ggplot(aes(x = R2, y = abs(e_mse), color = model)) + #, shape = factor(sample_size)
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) + 
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "R2", 
       y = "Empirical MSE", 
       color = "Model",
       title = "EMSE by Section R2 Value",
       shape = "Sample Size",
       subtitle = "Model with tcc, all sample sizes, R2 calculated from section regressions") +
  theme_bw()  +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 60, hjust=1)
  )
r2.df.emse
ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/r2/r2.tcc.emse_lr.png",
       device = "png",
       width = 9,
       height = 7,
       dpi = 200)

head(r2.df.emse)
colnames(tcc.bias)

r2.model <- lm(e_bias ~ R2, r2.df %>% mutate(e_bias = abs(e_bias)) %>% 
                 select(R2, e_bias) %>% mutate_all(as.numeric))
r2.model


# regress average tcc vs carbon, find residuals for each section

# first aggregate dataframe to section means
pop.means <- pop.dat %>% group_by(SECTION) %>% 
  summarize_all(mean)

means.lm1 <- lm(CARBON_AG_TPA_live_ADJ ~ tcc16, data = pop.means)


resid(means.lm1)

predict(means.lm1)


means.lm6 <- lm(CARBON_AG_TPA_live_ADJ ~ tcc16 + elev + tmean + tmin01 + tri + def, 
                data = pop.means)

summary(means.lm6)$Residuals

pop.resid <- data.frame(SECTION = pop.means$SECTION, 
                        resid.tcc = resid(means.lm1), 
                        resid.all = resid(means.lm6))
pop.resid <- pop.resid %>% left_join(tcc.bias %>% filter(sample_size == "8") %>% 
  mutate(e_bias = abs(e_bias), perc_rel_e_bias = abs(perc_rel_e_bias), domain = NULL) %>%
  rename(SECTION = section), by = "SECTION")

pop.resid %>% ggplot(aes(x = abs(resid.tcc), y = e_bias, color = model)) +
  geom_line() +
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
    labs(x = "Area Means Regression Residual", 
         y = "Absolute Bias", 
         color = "Model",
         title = "Influence of area-mean regression residual on simulation bias",
         subtitle = "Model with tcc alone, 8 sample size") +
    theme_bw()  +
    theme(
      legend.position = "bottom",
      text = element_text(size = 12)
    )


# Same for EMSE
pixel_means <- pop.dat %>% select(-SUBSECTION, -X) %>% 
  drop_na() %>%
  group_by(SECTION) %>% 
  summarise(
    across(.cols = tcc16:CARBON_AG_TPA_live_ADJ, .fns = mean), npixels = n()
  ) %>% 
  ungroup() %>% 
  as.data.frame()
pixel_means




pop.resid <- data.frame(SECTION = pixel_means$SECTION, 
                        resid.tcc = resid(means.lm1), 
                        resid.all = resid(means.lm6))
pop.resid <- pop.resid %>% left_join(tcc.emse %>% filter(sample_size == "32") %>% 
                                       mutate(domain = NULL) %>%
                                       rename(SECTION = section), by = "SECTION") %>% 
  left_join(tcc.bias %>% rename("SECTION" = "domain"), 
            by = c("SECTION", "model", "sample_size")) %>% 
    mutate(e_mse = e_var + e_bias^2)

pop.resid %>% ggplot(aes(x = resid.all, y = e_mse, color = model)) +
  geom_line() +
  scale_color_manual(
    values = c("#92abd6", "#3a32d1", "#d94c4c", "#96b88d", "#fcba03", "#1e6e36"),
    labels = c("Area EBLUP", "Post-Stratified", "Random Forest", "SMERF", "Unit EBLUP", "Unit Zero-Inflated")
  ) +
  labs(x = "Area Means Regression Residual", 
       y = "Empirical MSE", 
       color = "Model",
       title = "Influence of area-mean regression residual on simulation EMSE",
       subtitle = "Model with all predictors, 8 sample size") +
  theme_bw()  +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12)
  )

tcc.emse
