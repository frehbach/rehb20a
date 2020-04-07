###
### Main file for running a single optimization with randomSearch
### The file takes a set of configuration parameters via the command line and applies them to alter 
### the optimization run
###

source("runBBOB.R")
source("readBBOB.R")
source("wrapExternalFunction.R")

args = commandArgs(trailingOnly=TRUE)

### RUN Parameters #########################################
#
# 1) Seed / Repeat
# 2) Total Budget
# 3) Function ID of BBOB set
# 4) Infill ID (BP or EI)
# 5) nDim - Amount of dimensions 

### Recieve Seed
### 
instance <- as.numeric(args[1])
seed <- 7
set.seed(seed)

### Recieve BUDGET
TOTAL_FUN_BUDGET = as.numeric(args[2])

### Recieve Function ID
### 
### 1-24 bbob functions
funID <- as.numeric(args[3])

### NDIM
### 
nDim <- as.numeric(args[5])

######################################################################################################
######################################################################################################
################# SPOT

solverRandom <- function(fun,lower,upper,solverParameterList){
  for(i in 1:TOTAL_FUN_BUDGET){
    fun(runif(length(lower), min = lower, max = upper))
  }
}

runCOCO(solverRandom,current_batch = 1,number_of_batches = 1,dimensions=nDim, instances = instance,
        functions = funID,solver_name = paste("RandomSearch",paste(args,collapse="_"),sep="_"))

