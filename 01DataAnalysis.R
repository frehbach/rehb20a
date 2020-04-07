##
## Main File for analysing the experiment data and creating plots.
## Running this file should produce .pdf Files with the plots similar to the papers supplementary material.
## Running this Code on the data generated with (FULL_EXPERIMENTS <- F) results in some warnings, these can
## be ignored.
##

library(ggplot2)
source("bbobResultAnalysis.R")

### RUN Parameters #########################################
# 1) Seed / Repeat
# 2) Total Budget
# 3) Function ID of Smoof Test functions (1-74)
# 4) Infill ID (BP or EI)
# 5) nDim - Amount of dimensions (if the problem is scalable)

############
############ SPOT BBOB Plots
############

## Load SPOT 
bDF <- readBBOB("exdata", filterString = "SPOT")
bDF <- applyFrNamingScheme(bDF)
bDF <- appendVariablesByAlgoName(bDF, c("seed","budget","functionID","infillID","nDim"))
bDF$infillID <- replaceConfigVector(bDF$infillID, 1:2, c("PV","EI"))
bDF$nDim <- as.numeric(as.character(bDF$nDim))
bDF <- cleanAlgoName(bDF)
bDF$algName <- "SBO"

## LOAD random Search
rDF <- readBBOB("exdata", filterString = "Random")
rDF <- applyFrNamingScheme(rDF)
rDF <- appendVariablesByAlgoName(rDF, c("seed","budget","functionID","infillID","nDim"))
rDF$nDim <- as.numeric(as.character(rDF$nDim))
rDF$infillID <- ""
rDF <- cleanAlgoName(rDF)

##### put together spot results + randomsearch
####
bDF$budget <- NULL
rDF$budget <- NULL
pDF <- rbind(bDF,rDF)

## Prepare for plotting:
pDF <- addVariablesToAlgoName(pDF, "infillID")

pdf("spotOnBBOB.pdf", width = 14, height = 4.5)
bbobTitles <- c("Sphere",
                "Ellipsoidal",
                "Rastrigin",
                "Büche-Rastrigin",
                "Linear Slope",
                "Attractive Sector",
                "Step Ellipsoidal",
                "Rosenbrock",
                "Rosenbrock, rotated",
                "Ellipsoidal",
                "Discus",
                "Bent Cigar",
                "Sharp Ridge",
                "Different Powers",
                "Rastrigin",
                "Weierstrass",
                "Schaffers F7",
                "Schaffers F7",
                "Griwank-Rosenbrock",
                "Schwefel",
                "Gallagher Gaussian 101-me Peaks",
                "Gallagher Gaussian 21-hi Peaks",
                "Katsuura",
                "Lunacek bi-Rastrigin")
for(i in 1:24){
  at <- filter(pDF,functionID == i)
  if(nrow(at) == 0){
    next
  }
  print(convergencePlotBBOB(at, colorOrder = c("SBO-PV"="red","RandomSearch"="darkgreen","SBO-EI"="blue"), title = bbobTitles[i]))
}
dev.off()

############
############ MLR BBOB Plots
############

## Load SPOT 
bDF <- readBBOB("exdata", filterString = "Mlr")
bDF <- applyFrNamingScheme(bDF)
bDF <- appendVariablesByAlgoName(bDF, c("seed","budget","functionID","infillID","nDim"))
bDF$infillID <- replaceConfigVector(bDF$infillID, 1:2, c("PV","EI"))
bDF$nDim <- as.numeric(as.character(bDF$nDim))
bDF <- cleanAlgoName(bDF)
bDF$algName <- "SBO"

##### put together spot results + randomsearch
####
bDF$budget <- NULL
pDF <- bDF

## Prepare for plotting:
pDF <- addVariablesToAlgoName(pDF, "infillID")

pdf("mlrOnBBOB.pdf", width = 14, height = 4.5)
plots <- list()
for(i in 1:24){
  at <- filter(pDF,functionID == i)
  if(nrow(at) == 0){
    next
  }
  print(convergencePlotBBOB(at, colorOrder = c("SBO-PV"="red","RandomSearch"="darkgreen","SBO-EI"="blue"), title = bbobTitles[i]))
}
dev.off()

## Load SPOT 
bDF <- readBBOB("exdata", filterString = "SPOT")
bDF <- applyFrNamingScheme(bDF)
bDF <- appendVariablesByAlgoName(bDF, c("seed","budget","functionID","infillID","nDim"))
bDF$infillID <- replaceConfigVector(bDF$infillID, 1:2, c("PV","EI"))
bDF$nDim <- as.numeric(as.character(bDF$nDim))
bDF <- cleanAlgoName(bDF)
bDF$algName <- "SBO"

pdf("dominationTilesSPOTWilcox.pdf", width = 6, height = 10)
tilePlotDominations(bDF, plotMode = "onlySign", removeLegend = F)
dev.off()


## Load MLR 
bDF <- readBBOB("exdata", filterString = "Mlr")
bDF <- applyFrNamingScheme(bDF)
bDF <- appendVariablesByAlgoName(bDF, c("seed","budget","functionID","infillID","nDim"))
bDF$infillID <- replaceConfigVector(bDF$infillID, 1:2, c("PV","EI"))
bDF$nDim <- as.numeric(as.character(bDF$nDim))
bDF <- cleanAlgoName(bDF)
bDF$algName <- "SBO"

pdf("dominationTilesMlrWilcox.pdf", width = 6, height = 10)
tilePlotDominations(bDF, plotMode = "onlySign", removeLegend = F)
dev.off()


source("pathAnalyzer.R")

#####
##### Neighbor Image for SPOT
#####

df <- readBBOB("exdata", filterString = "SPOT")
df$gEvaluations <- NULL
df$bestNoiseFreeFitness <- NULL
df$measuredFitness <- NULL
df$bestMeasuredFitness <- NULL

readData <- df #%>% filter(funID == 3)
pathDF <- buildDistanceIntoDataFrame(readData,useNeighbor = T)
nPathDf <- pathDF
nPathDf$iteration <- nPathDf$fEvaluations
possibleNames <- paste("x",1:10,sep="")
nPathDf <- nPathDf[,-1]
nPathDf <- nPathDf[,-which(colnames(nPathDf) %in% possibleNames)]
nPathDf <- appendVariablesByAlgoName(nPathDf, c("seed","budget","functionID","infillID","nDim"))
nPathDf$infillID <- replaceConfigVector(nPathDf$infillID, 1:2, c("PV","EI"))
nPathDf$nDim <- as.numeric(as.character(nPathDf$nDim))
nPathDf <- cleanAlgoName(nPathDf)
nPathDf$algName <- "SBO"
nPathDf <- addVariablesToAlgoName(nPathDf, "infillID")

pdf("pathDistanceSPOTonBBOB.pdf", width = 14, height = 5)
for(i in 1:24){
  at <- filter(nPathDf,functionID == i)
  if(nrow(at) == 0){
    next
  }
  at$y <- at$distance
  print(convergencePlotBBOB(at, y = "Euclidean Distance",
                            colorOrder = c("SBO-PV"="red","RandomSearch"="darkgreen","SBO-EI"="blue"), title = bbobTitles[i]))
}
dev.off()

#####
##### Neighbor Image for MLRMBO
#####

df <- readBBOB("exdata", filterString = "Mlr")
df$gEvaluations <- NULL
df$bestNoiseFreeFitness <- NULL
df$measuredFitness <- NULL
df$bestMeasuredFitness <- NULL

readData <- df #%>% filter(funID == 3)
pathDF <- buildDistanceIntoDataFrame(readData,useNeighbor = T)
nPathDf <- pathDF
nPathDf$iteration <- nPathDf$fEvaluations
possibleNames <- paste("x",1:10,sep="")
nPathDf <- nPathDf[,-1]
nPathDf <- nPathDf[,-which(colnames(nPathDf) %in% possibleNames)]
nPathDf <- appendVariablesByAlgoName(nPathDf, c("seed","budget","functionID","infillID","nDim"))
nPathDf$infillID <- replaceConfigVector(nPathDf$infillID, 1:2, c("PV","EI"))
nPathDf$nDim <- as.numeric(as.character(nPathDf$nDim))
nPathDf <- cleanAlgoName(nPathDf)
nPathDf$algName <- "SBO"
nPathDf <- addVariablesToAlgoName(nPathDf, "infillID")

pdf("pathDistanceMLRonBBOB.pdf", width = 14, height = 5)
for(i in 1:24){
  at <- filter(nPathDf,functionID == i)
  if(nrow(at) == 0){
    next
  }
  at$y <- at$distance
  print(convergencePlotBBOB(at, y = "Euclidean Distance",
                            colorOrder = c("SBO-PV"="red","RandomSearch"="darkgreen","SBO-EI"="blue"), title = bbobTitles[i]))
}
dev.off()