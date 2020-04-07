###
### This file collects functions that can be used to load and analyze the results of BBOB benchmark results
###

source("wrapExternalFunction.R")
source("readBBOB.R")
source("statisticalRanking.R")
library(gridExtra)

##
## In order to figure out the parameters with which an algorithm was run on bbob, the algorithm name is parsed.
## This assumes that the algorithm name was appended with all run parameters before the algorithm was run.
## Each parameter is seperated with a '_'. Keeping the order of the parameters is required!
cutParametersFromString <- function(strName, 
                                    paramNameList = c("seed","budget","functionID","infillID","nDim")){
  require(stringr)
  paramValues <- str_split(strName,"_")[[1]][-1]
  names(paramValues) <- paramNameList
  return(as.data.frame(t(paramValues), stringsAsFactors=F))
}

## 
## Append a data.frame with additional columns regarding the run parameters of an algorithm.
## The lines are filled based on the the parameters that can be parsed from the algorithm name via 'cutParametersFromString'
appendVariablesByAlgoName <- function(df, paramNameList = c("seed","budget","functionID","infillID","nDim")){
  require(dplyr)
  print("Starting to append variables")
  cutParams <- function(str){cutParametersFromString(str, paramNameList)}
  additionalVars <- bind_rows(pbapply::pblapply(df$algName,cutParams))
  additionalVars[,which(names(additionalVars) %in% names(df))] <- NULL
  return(cbind(df,additionalVars))
}

## 
## Exchange numeric parameters with character strings
## for example replace infillID=1 with infillID=BP
replaceConfigVector <- function(vec, numericLevels, strLevels){
  for(i in 1:length(numericLevels)){
    nLev <- numericLevels[i]
    strLev <- strLevels[i]
    if(length(which(vec == nLev)) > 0){
      vec[which(vec == nLev)] <- strLev
    }
  }
  return(vec)
}

cleanAlgoName <- function(at){
  splits <- stringr::str_split(at$algName,stringr::fixed("_"))
  splits <- sapply(splits,function(x) x[1])
  at$algName <- splits
  return(at)
}

addVariablesToAlgoName <- function(at, varNames){
  for(v in varNames){
    at$algName <- paste(at$algName,at[[v]],sep="-")
    at$algName[str_ends(at$algName,"-")] <- str_remove(at$algName[str_ends(at$algName,"-")], fixed("-"))
  }
  return(at)
}

## Create a convergence plot
convergencePlotBBOB <- function(at, title = "", x = "iteration", y = "Objective Function Value", colorOrder = NULL){
  require(dplyr)
  
  #at <- at %>%  mutate(isSPOT = str_starts(algName,"SPOT"))
  at <- at %>% group_by(iteration, nDim, algName) %>%  mutate(upper = summary(y)[[5]]) %>%
    mutate(lower = summary(y)[[2]]) %>% mutate(med = summary(y)[[3]]) %>% mutate(mmin = mean(y))
  
  if(!is.null(colorOrder)){
    #at$algName <- factor(as.character(at$algName), levels = colorOrder)
    ggplot(at, aes(x=iteration,y=med))  + 
      geom_line(aes(color = algName, linetype = algName)) + 
      scale_color_manual(values = colorOrder) + 
      geom_ribbon(aes(fill = algName, ymin=lower, ymax=upper), alpha=0.3) + 
      scale_fill_manual(values = colorOrder) + 
      scale_linetype_manual(values=c("solid", "dotted","dashed"))+
      #facet_grid(rows = vars(),cols = vars(nDim), scales = "free_y") + 
      facet_wrap(facets = vars(nDim), nrow = 1, scales = "free") + 
      theme(text=element_text(size=16)) + 
      labs(x=x, y = y) + scale_y_continuous(trans = 'log10') + 
      ggtitle(title)
  }else{
    ggplot(at, aes(x=iteration,y=med))  + 
      geom_line(aes(color = algName)) + 
      geom_ribbon(aes(fill = algName, ymin=lower, ymax=upper), alpha=0.3) + 
      #facet_grid(rows = vars(),cols = vars(nDim), scales = "free_y") + 
      facet_wrap(facets = vars(nDim), nrow = 1, scales = "free") + 
      theme(text=element_text(size=16)) + 
      labs(x=x, y = y) + scale_y_continuous(trans = 'log10') + 
      ggtitle(title)
  }
}

rankWilcox <- function(at,...){
  y1 <- filter(at, infillID == "PV")$y
  y2 <- filter(at, infillID == "EI")$y
  
  res <- suppressWarnings(wilcox.test(x = y1, y = y2, alternative = "two.sided", paired = F, conf.int = T))
  if(is.na(res$p.value)){
    return(data.frame("dominant" = NA, "p" = res$p.value, "leading" = "PV"))
  }
  if(res$p.value > 0.05){
    return(data.frame("dominant" = NA, "p" = res$p.value, "leading" = ifelse(res$estimate < 0,"PV","EI")))
  }
  if(res$estimate < 0){
    return(data.frame("dominant" = "PV", "p" = res$p.value, "leading" = "PV"))
  }
  return(data.frame("dominant" = "EI", "p" = res$p.value, "leading" = "EI"))
}

rankEIvsBP <- function(at, iter, dim, returnSum = T, useWilcox = T){
  ## Filter the data for the given iteration and dimension
  at <- filter(at, iteration == iter)
  at <- filter(at, nDim == dim)
  
  ## Group by functions
  at <- at %>% group_by(functionID)
  
  if(nrow(at) == 0){
    warning("Missing Data in Rank Test")
    return(NULL)
  }
  
  ## Calculate ranks (1-x) and subtract 1 to know amount of dominations instead of rank
rank <- group_modify(at, rankWilcox) 

  if(!returnSum){
    rank$iter <- iter
    rank$nDim <- dim
    return(rank)
  }
  
  ## Aggregate sum over all functions
  sumRank <- rank %>% group_by() %>% 
    mutate(sumEI = length(which(rank$dominant == "EI"))) %>% 
    mutate(sumBP = length(which(rank$dominant == "PV"))) 
  
  ## Remove unnecessary rows and columns from aggregated df
  sumRank <- sumRank %>% filter(functionID == min(as.numeric(as.character(sumRank$functionID))))
  sumRank <- sumRank[,c(3:4)]
  
  ## Add columns with run-information
  sumRank$iter <- iter
  sumRank$nDim <- dim
  sumRank
}

## Rank EI vs BP at a given iteration and dimension
## 
rankEIvsBPMultiComparison <- function(at, iter, dim){
  appRankData <- function(at,...){
    df <- suppressWarnings(rankDataByPostHocKruskalConover(
      at$y,as.factor(at$infillID)))
    df <- t(data.frame(df))
    return(as.data.frame(df))
  }
  
  ## Filter the data for the given iteration and dimension
  at <- filter(at, iteration == iter)
  at <- filter(at, nDim == dim)
  
  ## Group by functions
  at <- at %>% group_by(functionID)
  
  if(nrow(at) == 0){
    warning("Missing Data in Rank Test")
    return(NULL)
  }
  
  ## Calculate ranks (1-x) and subtract 1 to know amount of dominations instead of rank
  rank <- group_modify(at, appRankData) 
  rank[,2:3] <- rank[,2:3]-1
  
  ## Aggregate sum over all functions
  sumRank <- rank %>% group_by() %>% 
    mutate(sumEI = sum(EI)) %>% 
    mutate(sumBP = sum(PM)) 
  
  ## Remove unnecessary rows and columns from aggregated df
  sumRank <- sumRank %>% filter(functionID == min(as.numeric(as.character(sumRank$functionID))))
  sumRank <- sumRank[,c(4:5)]
  
  ## Add columns with run-information
  sumRank$iter <- iter
  sumRank$nDim <- dim
  sumRank
}

##
## Count amount of dominations and plot over given iterations and dimensions
rankDominatedAmountsEIvsBP <- function(at, iters = unique(at$iteration), nDims = unique(at$nDim), doPlot = T){
  ## Iterate over all dimensions and iters, apply ranking function and collect result in single data.frame
  df <- NULL
  for(d in nDims){
    getSingleDF <- function(i){
      df <- rankEIvsBP(at,i,d)
      df
    }
    df <- dplyr::bind_rows(df,lapply(iters,getSingleDF))
  }
  
  names(df) <- c("EI","PM","iteration","nDim")
  
  df <- df[,c(2,1,3,4)]
  
  if(doPlot){
    ## melt the data for plotting
    melted <- reshape2::melt(df, id.vars = c("iteration","nDim"))
    names(melted) <- c("iteration","nDim","InfillCriterion","CountDominated")

    ## generate plot
    p <- ggplot(melted, aes(x=iteration, y = CountDominated)) + geom_line(aes(color = InfillCriterion)) + 
      facet_grid(rows = vars(),cols = vars(nDim), scales = "free_y") + 
      theme(text=element_text(size=16)) + ylab("Amount of Dominations") + xlab("Iteration")
    return(p)
  }
  df
}


##
tilePlotDominations <- function(at, useWilcox = T, plotMode = "onlySign", removeLegend = F){
  iters <- unique(at$iteration)
  iters <- iters[iters >= 10]
  iters <- iters[!iters %in% c(20,30,40,60,150,200,250)]
  nDims <- unique(at$nDim)
  
  df <- NULL
  for(d in nDims){
    getSingleDF <- function(i){
      df <- rankEIvsBP(at,i,d,F, useWilcox)
      df
    }
    df <- dplyr::bind_rows(df,lapply(iters,getSingleDF))
  }
  
  df$functionID <- factor(df$functionID, levels = c(24:1))
  df$iter <- as.factor(df$iter)
  df$dominant <- factor(df$dominant, levels = c("PV","EI"))
  #df$groupID <- c(rep(1,5),rep(2,4),rep(3,5),rep(4,5),rep(5,5))
  #df$groupID <- paste("Group:", df$groupID)
  df$p <- 1 - df$p
  #df$nDim <- paste("nDim:", df$nDim)
  df$p[df$leading == "EI"] <- -df$p[df$leading == "EI"]

  df$p[abs(df$p) >= 0.95] <- 2*df$p[abs(df$p) >= 0.95]
  df$dominant <- as.character(df$dominant)
  df$dominant[is.na(df$dominant)] <- "None"
  df$dominant <- factor(df$dominant, levels = c("PV","EI","None"))
  p <- ggplot(df, aes(iter, functionID)) +
      geom_tile(aes(fill = df$dominant), colour = "grey10", width = 0.9, height = 0.9, size = 0.1) +
      facet_grid(rows = vars(nDim),cols = vars()) +
      scale_y_discrete(breaks=c(1,5,10,15,20,24)) + 
      scale_x_discrete(breaks=iters[seq(1, length(iters), 3)]) + 
      scale_fill_manual(values = c("red","blue","grey90")) + 
      theme(text=element_text(size=16)) + 
      ylab("BBOB Function ID") + xlab("Iteration") + labs("fill" = "Criterion")
  if(removeLegend){
    p <- p + theme(legend.position="none")
  }
  return(p)
}


