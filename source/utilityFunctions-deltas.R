#/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

source("utilityFunctions.R")

createDeltaMD <- function(algName,useOrigDS=TRUE){
  file.to.write <-paste(c("D://csi//md-delta//replaceMissingVals//relativeImp//",algName,"_MD_DELTA.csv"),collapse="")
  #file.to.write <- file(description=paste(c(algName,"_MD_DELTA.csv"),collapse=""), "w")
  print(file.to.write)
  md.ds <- getDS(algName,FALSE)
  md.ds <- replaceMeaninglessValsWithNA(md.ds)
  md.delta <- data.frame()
  
  if(!useOrigDS) md.trans <- getTransformations(algName) #don't include the non-transformed datasets into the delta.md
  else md.trans <- readFile(algName,FALSE)
  
  for(i in 1:dim(md.ds)[1]){
    ds <- md.ds[i,]$Dataset
    print(ds)
    trans.per.ds <- md.trans[md.trans$Dataset %in% ds,]
    for(j in 1:dim(trans.per.ds)[1]){
      mf.delta <- createDeltaRow(trans.per.ds[j,],md.ds[i,],5,71)
      delta.row <- cbind(trans.per.ds[j,],mf.delta)
      md.delta <- rbind(md.delta,delta.row)
    }
  }
  write.table(md.delta, file = file.to.write,row.names=FALSE, na="",col.names=TRUE, sep=",")
  return(md.delta)
}

createDeltaMDscaled <- function(algName,useOrigDS=TRUE){
  file.to.write <-paste(c("..//md//relativeImp//relativeMetaFeatures//scaled_moreNA//",algName,"_MD_DELTA.csv"),collapse="")
  #file.to.write <- file(description=paste(c(algName,"_MD_DELTA.csv"),collapse=""), "w")
  print(file.to.write)
  md.ds <- getDS(algName,FALSE)
  md.ds <- replaceMeaninglessValsWithNA(md.ds)
  md.delta <- data.frame()
  
  if(!useOrigDS) md.trans <- getTransformations(algName) #don't include the non-transformed datasets into the delta.md
  else md.trans <- readFile(algName,FALSE)
  
  scaled.md.ds <- scale(md.ds[,c(5:66)])
  scaled.md.trans <- scale(md.trans[,c(5:66)], center = attributes(scaled.md.ds)$'scaled:center' , scale = attributes(scaled.md.ds)$'scaled:scale')
  md.ds <- cbind(md.ds[,c(1:4)],scaled.md.ds,md.ds[,c(67:71)])
  md.trans <- cbind(md.trans[,c(1:4)],scaled.md.trans,md.trans[,c(67:71)])
  print(dim(md.trans))
  #return(md.trans)
  for(i in 1:dim(md.ds)[1]){
    ds <- md.ds[i,]$Dataset
    print(ds)
    trans.per.ds <- md.trans[md.trans$Dataset %in% ds,]
    for(j in 1:dim(trans.per.ds)[1]){
      mf.delta <- createDeltaRowWithRelativeMetaFeatures(trans.per.ds[j,],md.ds[i,],5,71)
      delta.row <- cbind(trans.per.ds[j,],mf.delta)
      md.delta <- rbind(md.delta,delta.row)
    }
  }
  write.table(md.delta, file = file.to.write,row.names=FALSE, na="",col.names=TRUE, sep=",")
  return(md.delta)
}

createDeltaRowWithRelativePerformanceMeasures <- function(ds.transformed,ds,start,end){
  mf.to.select <- c(c(start:end)) #index of meta feature for which to calculate deltas
  mf.names <- names(ds.transformed)[mf.to.select]
  dsVals <- ds[,mf.to.select]
  
  mf.delta <- ds.transformed[,mf.to.select] - replace(dsVals,which(is.na(dsVals)),0) #replace NA with 0
  names(mf.delta) <- paste(mf.names,"_delta",sep="")
  #print(names(mf.delta))
  measures_delta <- paste(measures,"_delta",sep="")
  mf.delta[,measures_delta] <- mf.delta[,measures_delta]/ds[,measures]
  return(mf.delta)
}

createDeltaRowWithRelativeMetaFeatures <- function(ds.transformed,ds,start,end){
  mf.to.select <- c(c(start:end)) #index of meta feature for which to calculate deltas
  mf.names <- names(ds.transformed)[mf.to.select]
  dsVals <- ds[,mf.to.select]
  
  #mf.delta <- ds.transformed[,mf.to.select] - replace(dsVals,which(is.na(dsVals)),0) #replace NA with 0, to generate less_NA
  mf.delta <- ds.transformed[,mf.to.select] - dsVals # to generate more_NA
  mf.delta <- mf.delta/replace(dsVals,which(is.na(dsVals)),0) # calculate the relative features (all of them, including the performance measures)
  names(mf.delta) <- paste(mf.names,"_delta",sep="")
  #print(names(mf.delta))
  
  #measures_delta <- paste(measures,"_delta",sep="")
  #mf.delta[,measures_delta] <- mf.delta[,measures_delta]/ds[,measures] #calculate the relative improvement, of the performance measures!!!
  
  return(mf.delta)
}


algs <- c("weka.J48","weka.NaiveBayes","weka.JRip","weka.PART","weka.IBk","weka.Logistic")

alg <- args[1]
if(alg %in% algs){
  md.dl <- createDeltaMDscaled(alg)
}
