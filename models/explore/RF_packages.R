install.packages(c("sae", "hbsae", "mase", "here", 
                    "foreach", "broom", "doParallel", "docstring",
                    "stringr", "LongituRF", "randomForest", "dplyr",
                    "readr", "data.table"), # packages we want
    repos="http://cran.r-project.org", # specify cran as package source
    lib = paste(Sys.getenv("HOME"), "/apps/R_4.1.0", sep = "")) # install here!


