library(tidyverse)
library(MixRF)

ModMixRF = function(samp_dat, pop_dat, formula, random, initialRandomEffects = 0, ErrorTolerance = 0.001, MaxIterations = 1000, 
                 importance = FALSE, ntree = 500, mtry = max(floor(ncol(X)/3), 1), nodesize = 5, maxnodes = NULL) {

    if (!rlang::is_formula(formula)) {
            formula <- as.formula(formula)
            message("model formula was converted to class 'formula'")
    }
    print("pre data")
    samp_dat <- as.data.frame(samp_dat)
    # extract data from formula
    y_name <- deparse(formula[[2]])
    X_name <- stringr::str_extract_all(deparse(formula[[3]]), "\\w+")[[1]]
    Y <- samp_dat %>% dplyr::select(all_of(y_name))
    X <- samp_dat %>% dplyr::select(all_of(X_name))
  
    Target = Y
    print("data complete")
    # Condition that indicates the loop has not converged or run out of iterations
    ContinueCondition = TRUE

    iterations <- 0

    # Get initial values
    AdjustedTarget <- (Target - initialRandomEffects)
    oldLogLik <- -Inf
    FixedEffectsList <- data.frame(domain = NULL, fixeff = NULL, iter = NULL)
    print("preloop")
    while(ContinueCondition){

        iterations <- iterations+1
        # randomForest
        rf = randomForest(X, AdjustedTarget[, 1], 
                            importance = importance, ntree = ntree, mtry = mtry, nodesize = nodesize, maxnodes = maxnodes)

        # y - X*beta (out-of-bag prediction)
        resi = unlist(Target - rf$predicted)
        # samp_dat$resi = unlist(resi)

        ## Estimate New Random Effects and Errors using lmer
        f0 = as.formula(paste0('resi ~ -1 + ',random))
        lmefit <- lmer(f0, data=samp_dat)

        # check convergence
        newLogLik <- as.numeric(logLik(lmefit))

        print(abs(newLogLik-oldLogLik))

        ContinueCondition <- (abs(newLogLik-oldLogLik)>ErrorTolerance & iterations < MaxIterations)
        oldLogLik <- newLogLik

        # Extract random effects to make the new adjusted target
        AllEffects <- predict(lmefit)

        #return(ranef(lmefit))
        fixed_effects <- ranef(lmefit)$SUBSECTION %>% rename(fixeff = "(Intercept)")
        fixed_effects$domain = rownames(fixed_effects)
        fixed_effects$iter = iterations

        FixedEffectsList <- rbind(FixedEffectsList, fixed_effects)

        #  y-Zb
        AdjustedTarget <- Target - AllEffects
    }

    result <- list(forest=rf, MixedModel=lmefit, RandomEffects=ranef(lmefit),
                    IterationsUsed=iterations, FixedEffectsList=FixedEffectsList)

    return(result)
}
samp = as.data.frame(all_sim_samples[["100"]][[1]])
tmixrf <- ModMixRF(samp, pixel_data, formula = "BA ~ tcc16+evi", random = "(1|SUBSECTION)", MaxIterations = 100, ErrorTolerance = 0.01)
tmixrf


wrap.MixRF <- function(Y, X, data, pop_data, random = "(1 | SUBSECTION)" initialRandomEffects = 0, ErrorTolerance = 0.01, MaxIterations = 100, 
                 importance = FALSE, ntree = 500, mtry = max(floor(ncol(X)/3), 1), nodesize = 5, maxnodes = NULL) {
                        # fit MixRF model
                        mixmerf <- MixRF(Y = Y, X = X, random = random, data = data, initialRandomEffects = initialRandomEffects, 
                        ErrorTolerance = ErrorTolerance, MaxIterations = MaxIterations, importance = importance, ntree = ntree, 
                        mtry = mtry, nodesize = nodesize, maxnodes = maxnodes)

                        # predict and add residuals
                        preds <- predict(mixmerf$forest, pop_data)

                 }