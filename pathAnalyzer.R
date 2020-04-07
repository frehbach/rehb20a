## 
## Codes for the "exploration estimation" based on distances between search points

## Calculate distances to the neirest neighbor of each point
getNextNeighborDistance <- function(df){
    possibleNames <- paste("x",1:10,sep="")
  X <- df[,which(colnames(df) %in% possibleNames)]
  X <- X[colSums(!is.na(X)) > 0]
  d <- as.matrix(dist(X))
  dists <- NA
  for(i in 2:nrow(d)){
    vars <- d[i,1:i]
    dists <- c(dists,min(vars[which(vars!=0)]))
  }
  return(dists)
}

## Collect calculated distances into a data.frame for plotting
buildDistanceIntoDataFrame <- function(df, useNeighbor = F){
  df$distance <- NA
  runs <- unique(df$algName)
  i <- 1
  for(r in runs){
      ## Some progress indicator
    if(i %% 10 == 0){
      print(i)
    }
    i <- i + 1
    ind <- which(df$algName == r)
    d <- getNextNeighborDistance(df[ind,])
    df[ind,]$distance <- d
  }
  return(df)
}



