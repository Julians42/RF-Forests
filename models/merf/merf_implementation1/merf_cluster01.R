#!/usr/bin/env Rscript
args = commandArgs(TRUE)
# load datasets and packages | R Code
source("models/explore/load_data.R")
source("models/merf/merf_implementation1/krennmair_implementation.R")

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
# rf_results <- rf_wrapper("BA~evi+tcc16", 
#                         sample_size = s,
#                         xpop = pixel_data, 
#                         reps = reps)


merf_results <- merf_wrapper("BA~evi+tcc16",  "30", 10, pixel_data)

for (i in 1:length(merf_results)) {
    print(merf_results[[1]]$iterations)
}


SMERF(all_sim_samples[["100"]][[1]], pop_dat, "BA~evi+tcc16")
rf_results %>% write.csv(str_interp("results/merf01_sim_results_${s}_${reps}.csv", 
                                    list(s = s, reps = reps)))