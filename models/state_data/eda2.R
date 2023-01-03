library(tidyverse)
library(ggplot2)
library(here)
library(randomForest)
library(tidycensus)
library(sf)
library(leaflet)
library(raster)
library(viridis)
library(dplyr)
library(tidyr)
library(gt)


df <- read.csv("/Users/julianschmitt/Documents/Research/Thesis/states/carbon_dat_section.csv")
df$COUNTYFIPS <- as.character(df$COUNTYFIPS)
df$STATECD <- as.character(df$STATECD)

ggplot(df)+geom_histogram(aes(BA_TPA_live_ADJ))

census_api_key("0839b9dfec5190b45a8435fa4d7597b59a4ffbdc")
idaho_census <- get_decennial(state = "ID",
                       geography = "county", 
                       variables = "P013001", 
                       year = 2000,
                       geometry = T)
idaho_geo <- idaho_census %>% 
  dplyr::select(GEOID, geometry)




idaho = df %>% filter(STATECD == "16") %>% group_by(COUNTYFIPS) %>% 
    summarise(across(everything(), list(mean)))


idaho_forest_geo <- idaho_geo %>% 
  right_join(idaho, by = c("GEOID" = "COUNTYFIPS"))

p1 <- ggplot(data = idaho_forest_geo, mapping = aes(geometry = geometry, fill = CARBON_AG_TPA_live_ADJ_1)) + 
    geom_sf() +
    coord_sf() +
    scale_fill_viridis_c(limits = c(0, 50)) + 
    theme_void() 
p1

mean(idaho_forest_geo$CARBON_AG_TPA_live_ADJ)


##### Lets do it by histograms instead

df
p1 <- ggplot(df, aes(x = CARBON_AG_TPA_live_ADJ, fill = STATECD))+
    #facet_wrap(~STATECD)+
    geom_histogram(binwidth = 10, alpha = 0.5)  + 
    scale_fill_manual(
      labels = c("Georgia", "Idaho", "Iowa", "Oregon"),
      values = c("Yellow", "Green", "Red", "Blue"))+
  labs(y = "Count", fill = "State")
  

show(p1)
ggsave("~/Downloads/hist.png")

unique(df$STATECD)



#### Multiple State Plot
idaho_census <- get_decennial(state = c("ID", "IA", "OR", "GA"),
                              geography = "county", 
                              variables = "P013001", 
                              year = 2000,
                              geometry = T)

idaho_geo <- idaho_census %>% 
  dplyr::select(GEOID, geometry)

all_summary = df %>% group_by(STATECD, COUNTYFIPS) %>% 
  summarise(across(everything(), list(mean))) %>% ungroup()

summary_geo <- idaho_geo %>% 
  right_join(all_summary, by = c("GEOID" = "COUNTYFIPS"))

p1 <- ggplot(data = summary_geo %>% filter(STATECD==16), mapping = aes(geometry = geometry, fill = CARBON_AG_TPA_live_ADJ_1)) + 
  geom_sf() +
  coord_sf() +
  scale_fill_viridis_c(limits = c(0, 50)) + 
  theme_void() #+
  #facet_wrap(~STATECD)

p1

state_names <- c(
  `13` = "Georgia",
  `16` = "Idaho",
  `19` = "Iowa",
  `41` = "Oregon"
)

df %>% dplyr::select(STATECD, COUNTYFIPS, CARBON_AG_TPA_live_ADJ) %>% 
  group_by(STATECD, COUNTYFIPS) %>% 
  summarize(cnt = sum(CARBON_AG_TPA_live_ADJ==0)/n()) %>% 
  ggplot()+geom_histogram(aes(cnt, fill=STATECD, color=STATECD), alpha=0.5, bins=10) +
  #facet_wrap(~STATECD) + 
  scale_fill_manual(
    labels = c("Georgia", "Idaho", "Iowa", "Oregon"),
    values = c("Yellow", "Green", "Red", "Blue"))+
  scale_color_manual(
    labels = c("Georgia", "Idaho", "Iowa", "Oregon"),
    values = c("Yellow", "Green", "Red", "Blue"))+
  labs(x="Proportion Zero Observations", 
       y = "Counties (Count)", 
       fill = "State", 
       title="Distribution of Zero Observations by County")+
  facet_grid(STATECD ~ ., labeller = as_labeller(state_names))+
  guides(color="none")

ggsave("~/Downloads/dist_zero_obs_state.png",device=png, dpi=175)

library(ggplot2)
library(GGally)
df %>% head()
vars_sub = c("tcc16","elev","ppt","tmean", "tmin01", "tri", "tnt", "def", "CARBON_AG_TPA_live_ADJ")
pp <-ggpairs(df[vars_sub], lower=list(continuous="density"))#+labs(title="Pair Plots for Selected Variables")

pp+labs(title="Pair Plots and Correlation for Study Variables")


# Look at all variables

all <- read.csv("/Users/julianschmitt/Documents/Research/Thesis/states/allv_carbon_dat_section.csv")

ggplot(data = all, aes(x=MEASYEAR))+geom_histogram() +facet_wrap(~STATECD)

