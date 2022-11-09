# R on FASRC

## Starting
The [Harvard FASRC](https://docs.rc.fas.harvard.edu/kb/quickstart-guide/) has a really helpful walkthrough for how to get started with their computing resources. While the documentation is written for FASRC it is actually a pretty good resource for HPC in general so I recommend at least taking a look :) 



## Our configuration
The lab home directory is at `cd /n/holyscratch01/kmcconville_lab/` - I believe we have 50 TB of storage for free (or something of that order) as part of FASRC. I recommend read and write to here unless you're writing a lot of files that you don't want to delete later - in that case write to a temporary folder. 

 - Batch Script (ex. `job_rf.batch`): This script specifies the SBATCH job constraints and contains the `R` script we want to run.
    - R Package Script (ex. `install_packages.R`): script which can be run with `R CMD BATCH` to install 
    - Job script - can specify command line arguments with `"--args a=0 b=1"`
        - If you're script has a lot of custom functions it's a good idea to source them from different files. In our workflow, we use a file for our model functions and a separate file to load packages and datasets

#### This might look like:
```bash
#!/bin/bash
#SBATCH -c 5                # Number of cores
#SBATCH -t 0-00:15          # Runtime in D-HH:MM, minimum of 10 minutes
#SBATCH -p test           # Partition to submit to
#SBATCH --mem=10000          # Memory pool for all cores (see also --mem-per-cpu)
#SBATCH -o out/myRjob_%j.out    # File to which STDOUT will be written, %j inserts jobid
#SBATCH -e err/myRjob_%j.err     # File to which STDERR will be written, %j inserts jobid

module load R/4.1.0-fasrc01 #Load R module with common packages
mkdir -pv ~/apps/R_4.1.0 # make directory to store R packages
export R_LIBS_USER=$HOME/apps/R_4.1.0:$R_LIBS_USER # specify where to store packages
R CMD BATCH --quiet --no-restore --no-save RF_packages.R # install R packages if needed
R CMD BATCH --quiet --no-restore --no-save "--args s=30 reps=10" RF_CL_RUN02.R outputfile2 # run script
```


## Check Logs
 - Logs are the most helpful way to debug! In the above, REPL print output is saved to `outputfile2` while error messages are saved to `err/myRjob_%j.err`. Also note that errors from `RF_packages.R` are saved to `RF_packages.Rout` since we haven't specified an output file name. 

 ## Other Helpful Commands
   - `squeue` - shows all jobs, their job status (running, in queue, interrupted, etc), and JOBID
   - `scontrol show job <JOBID>` - shows complete information about job, including what node(s) it's running on
   - `ssh <NODENAME>` and `htop` can be used to monitor the job on the cluster 
   - `sacct` - tells you job status of all jobs that you've recently run. This is helpful if you get confused after running a lot of test jobs haha 


## Troubleshooting
Sometimes packages aren't available for certain modules. For example the `randomForest` package requires `R >= 4.1.0` and so the preferred build on the cluster of `R = 4.0.2` won't fly. The minimum required `R` version is listed in the package vignette. FASRC currently only has up to module `R = 4.1.0`.