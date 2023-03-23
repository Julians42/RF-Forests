# JS Senior Thesis
# Explore outliers

library(tidyverse)
library(latex2exp)

pop.mean.dat <- pop.dat %>% group_by(SECTION) %>% 
  summarize_all(mean) %>% 
  left_join(pop.dat %>% group_by(SECTION) %>% 
  summarize(num_obs = n()))


ggplot(data = pop.mean.dat,
       aes(x = SECTION, y = CARBON_AG_TPA_live_ADJ))+
  geom_bar(stat="identity", color = "green", fill = "forest green")+
  labs(
    title = "Average Above-Ground Carbon by Section",
    y = "Carbon",
    x = "Section"
  )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 20), 
        legend.position = "none",
        plot.title = element_text(size = 30),
        axis.title = element_text(size = 25),
        axis.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 15))


ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/summary/carbon_bar.png",
       device = "png",
       width = 12,
       height = 8,
       dpi = 300)

vars = c(TeX(r'(\textbf{tcc16})'), "elev", "ppt", "tmean", "tmin01", "tri", "tnt", "def", "num_obs")
pop.mean.dat.long <- gather(pop.mean.dat, key="measure", value="value", all_of(vars))
pop.mean.dat.long
ggplot(pop.mean.dat.long, aes(x=SECTION, y=value))+
  geom_bar(stat='identity', fill="forest green")+
  facet_wrap(~measure, scales= "free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9.5), 
           plot.title = element_text(size = 20),
           axis.title = element_text(size = 15),
           axis.text.y = element_text(size = 15),
          strip.text.x = element_text(size = 15))+
  labs(
    title= "Population Section Means",
    x = "Section", 
    y = "Value"
  )

ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/summary/facet_var_bar.png",
       device = "png",
       width = 6.5*1.6,
       height = 6.5,
       dpi = 250)
