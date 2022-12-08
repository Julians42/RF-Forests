library(tidyverse)
library(here)

georgia <- readRDS("../states/Georgia/pltassgn.rds")
idaho <- readRDS("../states/Idaho/pltassgn.rds")
iowa <- readRDS("../states/Iowa/pltassgn.rds")
oregon <- readRDS("../states/Oregon/pltassgn.rds")

pltassgn <- do.call("rbind", list(georgia, idaho, iowa, oregon))
pltassgn %>% select(tcc16) %>% summarize(cnt = sum(tcc16!=0))
dim(unique(pltassgn %>% select(COUNTYFIPS)))


georgia <- readRDS("../states/Georgia/tsumdatp.rds")
idaho <- readRDS("../states/Idaho/tsumdatp.rds")
iowa <- readRDS("../states/Iowa/tsumdatp.rds")
oregon <- readRDS("../states/Oregon/tsumdatp.rds")

datp <- do.call("rbind", list(georgia, idaho, iowa, oregon))
datp %>% drop_na(CCLIVEPLT)%>% select(CCLIVEPLT) %>% summarize(cnt = sum(CCLIVEPLT ==0))


georgia <- readRDS("../states/Georgia/tsumdatc.rds")
idaho <- readRDS("../states/Idaho/tsumdatc.rds")
iowa <- readRDS("../states/Iowa/tsumdatc.rds")
oregon <- readRDS("../states/Oregon/tsumdatc.rds")

unitarea <- do.call("rbind", list(georgia, idaho, iowa, oregon))
unitarea%>% select(BALIVE) %>% summarize(cnt = sum(BALIVE != 0))
unitarea %>% drop_na(BALIVE)%>% select(BALIVE) %>% summarize(cnt = sum(BALIVE == 0))
dim(unitarea %>% drop_na(BALIVE) %>% select(COUNTYCD) %>% summarize(cnt = unique(COUNTYCD)))
hist(unitarea$MEASMON)
