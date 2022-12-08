library(rpart)
library(rpart.plot)
library(ggpubr)
source("models/explore/load_data.R")

# https://blog.exploratory.io/visualizing-a-decision-tree-using-r-packages-in-explortory-b26d4cb5e71f

samp = all_sim_samples[["30"]][[1]]

# plotting random forest trees and assessing scaling potential
rf <- rpart(BA ~evi+tcc16, data = samp)
rpart.plot(rf, box.palette = "RdBu", shadow.col = "gray", nn=TRUE)
samp_scale <- samp %>% mutate_at(c("evi", "tcc16", "BA"), ~(scale(.) %>% as.vector))


rf_scale <- rpart(BA ~evi+tcc16, data = samp_scale)
rpart.plot(rf_scale, box.palette = "RdBu", shadow.col = "gray", nn=TRUE)


# assess correlation by subsection 
pixel_samp = pixel_data %>% group_by(SUBSECTION) %>% slice_sample(n=500)
library(ggplot2)
ggplot(data = pixel_samp, aes(evi, BA)) +
    geom_point(alpha = 0.2, color = "#134d13") +
    facet_wrap(~SUBSECTION) + 
    labs(x = "EVI", y = "Basal Area (BA)", title = "EVI and BA Distribution")+
    stat_cor(method = 'pearson') #%>%
    ggsave(filename = "visualization/plots/evi_ba_dist.png", dpi = 175)

ggplot(data = pixel_samp, aes(tcc16, BA)) +
    geom_point(alpha = 0.2, color = "#134d13") +
    facet_wrap(~SUBSECTION) + 
    labs(x = "Tree Canopy Cover (tcc16)", y = "Basal Area (BA)", 
            title = "TCC16 and BA Distribution") +
    stat_cor(method = 'pearson')
    ggsave(filename = "visualization/plots/tcc16_ba_dist.png", dpi = 175)

# predictors
ggplot(data = pixel_samp, aes(tcc16, evi)) +
    geom_point(alpha = 0.2, color = "#134d13") +
    facet_wrap(~SUBSECTION) + 
    labs(x = "Tree Canopy Cover (tcc16)", y = "Enhanced Vegetation Index (EVI)", 
            title = "TCC16 and EVI Distribution") +
    stat_cor(method = 'pearson')
    ggsave(filename = "visualization/plots/tcc16_evi_dist.png", dpi = 175)


library(gt)
library(scales)
# calculate correlation coefficients on entire pixel_data set
pixel_data %>% group_by(SUBSECTION) %>%
    summarize(EVI_BA_cor = cor(evi, BA), 
              TCC_BA_cor = cor(tcc16, BA), 
              EVI_TCC_cor = cor(evi,tcc16)) %>%
    gt() %>% 
    tab_header(title = "Assessment of Study Variable Correlation by Subsection") %>%
    data_color(columns = c("EVI_BA_cor", "TCC_BA_cor", "EVI_TCC_cor"),
                colors = scales::col_numeric(
                    palette = c("red","blue"),
                    domain = c(.45,.83)
                )) %>%
    cols_label(
        EVI_BA_cor = "EVI and BA",
        TCC_BA_cor = "TCC and BA",
        EVI_TCC_cor = "EVI and TCC"
    ) %>%
    gtsave(filename = "visualization/plots/study_var_corr.png")



# boxplot of BA by tnt split
pixel_samp %>% mutate(tnt = as.factor(tnt)) %>% ggplot() +
    geom_boxplot(aes(group = tnt, BA, fill = tnt)) +
    facet_wrap(~SUBSECTION)+
    labs(fill = "Tree No Tree", title = "Distribution of BA by tnt strata", x = "Tree no Tree Variable (tnt)", y = "BA")
    ggsave(filename = "visualization/plots/tnt_split_BA_facet.png")
