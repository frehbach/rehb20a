###
### Main file for running a single optimization with mlrmbo
### The file takes a set of configuration parameters via the command line and applies them to alter 
### the optimization run
###

library(mlrMBO)
source("runBBOB.R")
source("readBBOB.R")
source("wrapExternalFunction.R")

### RUN Parameters #########################################
#
# 1) Seed / Repeat
# 2) Total Budget
# 3) Function ID of Smoof Test functions (1-74)
# 4) Infill ID (BP or EI)
# 5) nDim - Amount of dimensions (if the problem is scalable)
args = commandArgs(trailingOnly=TRUE)

#args = c(1,50,3,2,10)

instance <- as.numeric(args[1])
seed <- 7
set.seed(seed)

### Recieve BUDGET
TOTAL_FUN_BUDGET = as.numeric(args[2])

### Recieve Function ID
### 
### 1-24 bbob functions
funID <- as.numeric(args[3])

### Infill ID
### 
### 1 - Standard Best Predicted Point
### 2 - Expected Improvement - Via Original Kriging
infillID <- as.numeric(args[4])

### NDIM
### 
nDim <- as.numeric(args[5])

solverMlrMbo <- function(fun,lower,upper,solverParameterList){
    configureMlr(show.learner.output = FALSE)
    
    obj.fun <- makeSingleObjectiveFunction(
        fn = fun,
        par.set = makeNumericParamSet(lower = lower, upper = upper, len = length(lower))
    )
    
    ctrl <- makeMBOControl(propose.points = 1)
    ctrl <- setMBOControlTermination(ctrl, iters = TOTAL_FUN_BUDGET-11)
    
    if(infillID == 1){
        ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritMeanResponse(), #makeMBOInfillCritEI(), 
                                    opt = "focussearch", opt.focussearch.points = 1000L)
    }else if(infillID == 2){
        ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(), 
                                    opt = "focussearch", opt.focussearch.points = 1000L)
    }
    
    lrn <- makeMBOLearner(ctrl, obj.fun)
    
    design <- generateDesign(11L, getParamSet(obj.fun), fun = lhs::maximinLHS)
    
    res <- mbo(obj.fun, design = design, learner = lrn,
               control = ctrl, show.info = TRUE)
    
    return(res)
}

runCOCO(solverMlrMbo,current_batch = 1,number_of_batches = 1,dimensions=nDim, instances = instance,
        functions = funID,solver_name = paste("MlrMbo",paste(args,collapse="_"),sep="_"))


