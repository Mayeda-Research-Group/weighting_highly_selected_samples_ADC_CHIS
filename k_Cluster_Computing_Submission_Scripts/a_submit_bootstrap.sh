#!/bin/bash
#$ -cwd #uses current working directory
# error = Merged with joblog
#$ -o joblogs/joblog.$JOB_ID.$TASK_ID #creates a file called joblog.jobidnumber to write to. 
#$ -j y 
#$ -l h_rt=3:00:00,h_data=4G,arch=intel-E5-*|intel-gold-* #requests 3 hours, 4GB of data (per core), specific intel nodes (required for twang package)
#$ -pe shared 1 #requests 1 core
# Email address to notify
#$ -M $USER@mail #don't change this line, finds your email in the system 
# Notify when
#$ -m bea #sends you an email (b) when the job begins (e) when job ends (a) when job is aborted (error)
# submit array job:
# TEST RUN:
##$ -t 1-3:1
# FULL RUN:
#$ -t 1-2000:1
## 

# load the job environment:
. /u/local/Modules/default/init/modules.sh
module load R/4.0.2 #loads R/4.0.2 for use 
module load gcc/10.2.0 #loads gcc which is necessary for twang's xgboost dependency
export OMP_NUM_THREADS=1 #uses max 1 threads (needs to match -pe shared)
## 
# run R code

echo "======"
echo SGE_TASK_ID=$SGE_TASK_ID      
R CMD BATCH --no-save --no-restore "--args seed=$SGE_TASK_ID "  hoffman_Analysis_18May2022.R output/output.$JOB_ID.$SGE_TASK_ID
echo R CMD BATCH --no-save --no-restore  hoffman_Analysis_18May2022.R output/output.$JOB_ID.$SGE_TASK_ID


