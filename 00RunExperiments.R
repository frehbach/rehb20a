###
### Main File for starting the Experiments.
### There is a small test set of experiments, with reduced budget, repeats, etc. 
### Use this test set (FULL_EXPERIMENTS <- F) to see that all the codes etc. are running correctl on your system. 
### Once that is working you can try to run the full experiment set (FULL_EXPERIMENTS <- T)
###

## Should the full experiments be run? 
## Otherwise a quick test set is run
FULL_EXPERIMENTS <- F

##
## Function for starting a specific set of experiments
runExperiments <- function(idList, startFile = "mainSPOT.R"){
    configs <- expand.grid(idList)
    
    print(paste("Starting Experiments on:", startFile))
    dir.create("slurmOut", showWarnings = FALSE)
    
    startSingle <- function(r){
        print(r)
        system(paste0("cp runSingleCore.slurm delete.slurm"))
        write(paste("/opt/software/R/R-current/bin/Rscript", startFile ,paste(r, collapse=" ")),file="delete.slurm",append=TRUE)
        system("sbatch delete.slurm")
        system("rm delete.slurm") 
    }
    
    apply(configs, 1, startSingle)
    print("done!")
}

if(FULL_EXPERIMENTS){
    seed <- 1:15 ## Seed / Repeat of the experiment 
    budget <- 300 ## Total Budget for the optimizer
    funs <- 1:24 ## Function ID of BBOB function (1-24)
    infillID <- 1:2 ## used infill criterion (if any) (1: BP, 2:EI)
    d <- c(2,3,5,10) ## Problem Dimensionality
    startParams <- list(seed,budget,funs,infillID,d)
    runExperiments(startParams,"mainSPOT.R")
    
    seed <- 1:15 ## Seed / Repeat of the experiment 
    budget <- 300 ## Total Budget for the optimizer
    funs <- 1:24 ## Function ID of BBOB function (1-24)
    infillID <- 1:2 ## used infill criterion (if any) (1: BP, 2:EI)
    d <- c(2,3,5,10) ## Problem Dimensionality
    startParams <- list(seed,budget,funs,infillID,d)
    runExperiments(startParams,"mainMlrMbo.R")
    
    seed <- 1:15 ## Seed / Repeat of the experiment 
    budget <- 300 ## Total Budget for the optimizer
    funs <- 1:24 ## Function ID of BBOB function (1-24)
    infillID <- 1 ## used infill criterion (if any) (1: BP, 2:EI)
    d <- c(2,3,5,10) ## Problem Dimensionality
    startParams <- list(seed,budget,funs,infillID,d)
    runExperiments(startParams,"mainRandomSearch.R")
}else{
    seed <- 1:3 ## Seed / Repeat of the experiment 
    budget <- 50 ## Total Budget for the optimizer
    funs <- c(3,13) ## Function ID of BBOB function (1-24)
    infillID <- 1:2 ## used infill criterion (if any) (1: BP, 2:EI)
    d <- c(2) ## Problem Dimensionality
    startParams <- list(seed,budget,funs,infillID,d)
    runExperiments(startParams,"mainSPOT.R")
    
    seed <- 1:3 ## Seed / Repeat of the experiment 
    budget <- 50 ## Total Budget for the optimizer
    funs <- c(3,13) ## Function ID of BBOB function (1-24)
    infillID <- 1:2 ## used infill criterion (if any) (1: BP, 2:EI)
    d <- c(2) ## Problem Dimensionality
    startParams <- list(seed,budget,funs,infillID,d)
    runExperiments(startParams,"mainMlrMbo.R")
    
    seed <- 1:3 ## Seed / Repeat of the experiment 
    budget <- 50 ## Total Budget for the optimizer
    funs <- c(3,13) ## Function ID of BBOB function (1-24)
    infillID <- 1 ## used infill criterion (if any) (1: BP, 2:EI)
    d <- c(2) ## Problem Dimensionality
    startParams <- list(seed,budget,funs,infillID,d)
    runExperiments(startParams,"mainRandomSearch.R")
}
