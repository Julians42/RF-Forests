#!/usr/bin/env Rscript
args = commandArgs(TRUE)
# load datasets and packages | R Code
source("cluster_load.R")
source("krennmair_implementation.R")

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



merf_results <- merf_wrapper("BA~evi+tcc16",  s, reps, pixel_data)


merf_results %>% write.csv(str_interp("results/merf01_sim_results_${s}_${reps}.csv", 
                                    list(s = s, reps = reps)))