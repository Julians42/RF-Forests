library(lme4)
library(randomForest)
library(tidyverse)

data = readRDS("debugdata.RDS")


# datasets (different sizes) - 2 of 6k sim samples (the larger samples tend not to work)
sample = data$works 
sample = data$doesnt

# fit random forest model
m <- randomForest(y = sample$BA, x = sample %>% select(tcc16, evi), ntree = 500, nodesize = 1)

# put residuals in dataframe
resid <- data.frame(residuals = m$predicted - sample$BA, SUBSECTION = sample$SUBSECTION)
resid_avg = resid %>% group_by(SUBSECTION) %>%
        summarise(resid_mean = mean(residuals))

# fit lmer model
f = as.formula(paste0('residuals ~ 1+ (1|', "SUBSECTION", ")"))
lm <- lmer(f, data = resid)

# coefficients
ranef(lm)

# lets do this randomly - doesn't bug 
set.seed(185)
# generate random centers for noise by subsection
ran_centers <- data.frame(SUBSECTION = unique(samp$SUBSECTION), centers = rnorm(8, mean=0, sd=10))

# generate 800 noisy observations and center by subsection
ran_data <- data.frame(SUBSECTION= samp$SUBSECTION, resids = rnorm(length(samp$SUBSECTION), mean = 0, sd = 50)) %>%
                left_join(ran_centers, by = "SUBSECTION") %>%
                mutate(resid_sum = resids + centers)

# sample mean and true center comparison
ran_data %>% group_by(SUBSECTION) %>%
        summarize(ran_mean = mean(resid_sum), true_centers = mean(centers))

# model and fit
# 1 and nothing both give intercept, -1 removes intercept
lme_fit <- lmer(as.formula(paste0('resids ~ (1|', "SUBSECTION", ")")), data = ran_data)

# get random coefficients
# why don't these closely match the random sample means??
ranef(lme_fit)$SUBSECTION

# two approaches - fit with linear model or calculate empirically
# mixRF way - shrink intercepts significantly. 
# krennmair way - they calculate the sigma's empirically. 
