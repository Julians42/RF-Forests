# JS Senior Thesis
# Explore outliers

library(tidyverse)
pop.mean.dat <- pop.dat %>% group_by(SECTION) %>% 
  summarize_all(mean) %>% 
  left_join(pop.dat %>% group_by(SECTION) %>% 
  summarize(num_obs = n()))


ggplot(data = pop.mean.dat,
       aes(x = SECTION, y = CARBON_AG_TPA_live_ADJ))+
  geom_bar(stat="identity", color = "green", fill = "forest green")+
  labs(
    title = "Average Live Carbon by Section",
    y = "Carbon",
    x = "Section"
  )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position = "none")

ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/summary/carbon_bar.png",
       device = "png",
       width = 10,
       height = 7,
       dpi = 200)

vars = c("tcc16", "elev", "ppt", "tmean", "tmin01", "tri", "tnt", "def", "num_obs")
pop.mean.dat.long <- gather(pop.mean.dat, key="measure", value="value", all_of(vars))
pop.mean.dat.long
ggplot(pop.mean.dat.long, aes(x=SECTION, y=value))+
  geom_bar(stat='identity', fill="forest green")+
  facet_wrap(~measure, scales= "free_y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(
    title= "Population Section Means",
    x = "Section", 
    y = "Value"
  )
ggsave("/Users/julianschmitt/Documents/Research/Thesis/RF-Forests/visualization/summary/facet_var_bar.png",
       device = "png",
       width = 10,
       height = 7,
       dpi = 200)
