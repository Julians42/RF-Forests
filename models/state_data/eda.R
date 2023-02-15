library(tidyverse)
library(here)

setwd("~/Documents/Research/Thesis/RF-Forests/")

georgia <- readRDS("../states/Georgia/pltassgn.rds")
idaho <- readRDS("../states/Idaho/pltassgn.rds")
iowa <- readRDS("../states/Iowa/pltassgn.rds")
oregon <- readRDS("../states/Oregon/pltassgn.rds")

pltassgn <- do.call("rbind", list(georgia, idaho, iowa, oregon))
pltassgn %>% select(tcc16) %>% summarize(cnt = sum(tcc16!=0))
dim(unique(pltassgn %>% select(COUNTYFIPS)))
dim(unique(pltassgn %>% select(CN)))
pltassgn %>% head()

georgia <- readRDS("../states/Georgia/tsumdatp.rds")
idaho <- readRDS("../states/Idaho/tsumdatp.rds")
iowa <- readRDS("../states/Iowa/tsumdatp.rds")
oregon <- readRDS("../states/Oregon/tsumdatp.rds")

datp <- do.call("rbind", list(georgia, idaho, iowa, oregon))
datp %>% drop_na(CCLIVEPLT)%>% select(CCLIVEPLT) %>% summarize(cnt = sum(CCLIVEPLT ==0))

sum(datp$BA_TPA_live_ADJ == 0)/length(datp$CARBON_AG_TPA_live_ADJ)

datp %>% select(BA_TPA_live_ADJ, CARBON_AG_TPA_live_ADJ) %>% head(15)

georgia <- readRDS("../states/Georgia/tsumdatc.rds")
idaho <- readRDS("../states/Idaho/tsumdatc.rds")
iowa <- readRDS("../states/Iowa/tsumdatc.rds")
oregon <- readRDS("../states/Oregon/tsumdatc.rds")

unitarea <- do.call("rbind", list(georgia, idaho, iowa, oregon))
unitarea%>% select(BALIVE) %>% summarize(cnt = sum(BALIVE != 0))
unitarea %>% drop_na(BALIVE)%>% select(BALIVE) %>% summarize(cnt = sum(BALIVE == 0))
dim(unitarea %>% drop_na(BALIVE) %>% select(COUNTYCD) %>% summarize(cnt = unique(COUNTYCD)))
hist(unitarea$MEASMON)


plt_dat <- right_join(pltassgn, datp, by = "CN")
plt_dat %>% head()
colnames(plt_dat)
dim(plt_dat)

plt_dat %>%write.csv("/Users/julianschmitt/Documents/Research/Thesis/states/allv_carbon_dat_section.csv")


sub_dat <- plt_dat %>% select(CN, PROVINCE, SECTION, SUBSECTION, tcc16, elev, ppt, tmean, tmin01, tri, tnt, def, CARBON_AG_TPA_live_ADJ, BA_TPA_live_ADJ, STATECD, COUNTYFIPS)
dim(unique(sub_dat %>% select(SECTION)))
sub_dat %>% write.csv("/Users/julianschmitt/Documents/Research/Thesis/states/carbon_dat_section.csv")

sub_dat


# scratch 
Y <- deparse(tree_eq[[2]])
X <- stringr::str_extract_all(deparse(tree_eq[[3]]), "\\w+")[[1]]

rand_intercept <- paste0("( 1 | ", "SECTION", " )")

lin_reg_formula <- as.formula(
    paste0(deparse(tree_eq[[2]]), " ~ ",
            deparse(tree_eq[[3]]), " + ",
            rand_intercept)
)
lin_reg_formula

log_reg_formula <- as.formula(
    paste0(deparse(tree_eq[[2]]), " != 0 ~ ",
            deparse(tree_eq[[3]]), " + ",
            rand_intercept)
)

# creating nonzero version of our sample data set 
nz <- samp[samp[ , Y] > 0, ]

# fit linear mixed model on nonzero data
lmer_nz <- lme4::lmer(lin_reg_formula, data = nz %>% mutate(SECTION = as.factor(SECTION)))

# Fit logistic mixed effects on ALL data
glmer_z <- suppressMessages(
    lme4::glmer(log_reg_formula, data = samp, family = "binomial")
)

pop_dat_sel <- pop_dat %>% select(samp %>% select(-sample) %>% colnames()) %>%
                drop_na()
pop_dat_sel %>% head()

lin_pred <- predict(lmer_nz, pop_dat %>% mutate(SECTION = as.factor(SECTION)), allow.new.levels = TRUE)

setdiff(as.set(pop_dat_sel$SECTION), as.set(samp$SECTION))

unique(pop_dat$SECTION)
unique(samp$SECTION)
unique(samp$SECTION)[!(unique(samp$SECTION) %in% unique(pop_dat$SECTION))]


log_pred <- predict(glmer_z, pop_dat, type = "response")
unit_level_preds <- lin_pred*log_pred

# d x 2 dataframe
# where d = # of domains
zi_domain_preds <- data.frame(
    domain = pop_dat[ , domain_level, drop = T],
    unit_level_preds = unit_level_preds) %>%
    dplyr::group_by(domain) %>%
    dplyr::summarise(Y_hat_j = mean(unit_level_preds)) %>%
    ungroup()

dim(samp)
dim(samp %>% drop_na())
