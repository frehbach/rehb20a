###
### Main file for running a single optimization with spot
### The file takes a set of configuration parameters via the command line and applies them to alter 
### the optimization run
###

library(SPOT)
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

model <- buildKriging
modellingFunction <- NULL

### Recieve Function ID
### 
### 1-24 bbob functions
funID <- as.numeric(args[3])

### Infill ID
### 
### 1 - Standard Best Predicted Point
### 2 - Expected Improvement - Via Original Kriging
infillID <- as.numeric(args[4])
uncertaintyEstimator <- NULL
if(infillID == 1){
  infillCriterion <- NULL
}else{
  infillCriterion <- infillExpectedImprovement
  uncertaintyEstimator <- "s"
}

### NDIM
### 
nDim <- as.numeric(args[5])

######################################################################################################
######################################################################################################
################# SPOT

solverSPOT <- function(fun,lower,upper,solverParameterList){
  ########target function wrapper for SPOT
  tfun <- function(x){
    apply(x,1,fun)
  }
  res <- spot(NULL, tfun, lower, upper, 
              control = list(optimizer = optimDE, 
                             optimizerControl = list(funEvals = length(lower) * 1000, 
                                                     populationSize = 30 * length(lower)), 
                             infillCriterion = infillCriterion,
                             model = model, 
                             seedSPOT = seed,
                             modelControl = list(target = c("y", "s"),
                                                 uncertaintyEstimator = uncertaintyEstimator,
                                                 useLambda = T, 
                                                 lambdaUpper = -4,
                                                 algTheta = optimDE,
                                                 budgetAlgTheta = 500,
                                                 optimizeP = T
                             ), 
                             funEvals = TOTAL_FUN_BUDGET,
                             logUncertainties = T))
  
  return(res)
}

runCOCO(solverSPOT,current_batch = 1,number_of_batches = 1,dimensions=nDim, instances = instance,
        functions = funID,solver_name = paste("SPOT",paste(args,collapse="_"),sep="_"))

