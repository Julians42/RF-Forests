#!/usr/bin/env Rscript
args = commandArgs(TRUE)

# load datasets and packages | R Code
source("cluster_load.R")
source("rf_models.R")


# read in args 
if(length(args)==0){
    print("No arguments supplied.")
    ##supply default values
    s = "100"
    reps = 2000
}else{
    for(i in 1:length(args)){
      eval(parse(text=args[[i]]))
    }
}
s = toString(s)
print(s)
print(reps)

rf_results <- rf_wrapper("BA~evi+tcc16", 
                        sample_size = s,
                        xpop = pixel_data, 
                        reps = reps)


rf_results %>% write.csv(str_interp("results/rf_sim_results_${s}_${reps}.csv", 
                                    list(s = s, reps = reps)))
