# MERF Models


source("../explore/load_data.R")


MixRF()

# get sample
samp = all_sim_samples[["30"]][[1]]

# fit to MixRF
merf1 <- MixRF(Y = samp$BA, X = samp %>% select(evi, tcc16), 
        random = "(1 | SUBSECTION)", data = samp)
merf1$RandomEffects$SUBSECTION %>% summarize(m = mean("(Intercept)"))
mean(data.frame(merf1$RandomEffects$SUBSECTION)$X.Intercept.)
merf1$MixedModel
fpreds <- predict(merf1$forest, samp)

sub <- data.frame(fpreds = fpreds, SUBSECTION = samp$SUBSECTION)

fixed_eff <- data.frame(SUBSECTION = rownames(merf1$RandomEffects$SUBSECTION),
                        FE = merf1$RandomEffects$SUBSECTION[[1]])

sub <- sub %>% left_join(fixed_eff, by = "SUBSECTION") 
sub%>%mutate(MERF_preds = fpreds + FE)
