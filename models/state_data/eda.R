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
