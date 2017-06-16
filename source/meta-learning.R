#/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

source("utilityFunctions.R") #additional functions 
source("feature-extraction.R") #functions for performing feauture extraction and selection : PCA + VARIMAX, PARTIAL CORRELATIONS
source("meta-learning-helper.R") #functions for performing meta-learning

algs <- c("weka.J48","weka.NaiveBayes","weka.JRip","weka.PART","weka.IBk","weka.Logistic")
#algs <- c("weka.J48","weka.PART","weka.IBk","weka.Logistic")

alg <- args[1]
trans <- c("All",transformations)

#if(alg == "weka.J48") {trans <- c("All",transformationsJ48)} else 
#if(alg == "weka.PART") {trans <- c("All",transformationsPART)} else 
#if(alg == "weka.IBk") {trans <- c("All",transformationsIBk)} else 
#{trans <- c("All",transformationsLog)}

if(alg %in% algs){
  message(alg)
  md.ds <- getDS(alg,readDelta=TRUE)
  md.trans <- getTransformations(alg,readDelta=TRUE,"")
  #md.trans <- md.trans[md.trans$Transformation %in% trans,] #remove the bad transformations

  #it used to be md.ds instead of md.trans
  pca <- performPCA(md.trans,variance=90,old=FALSE,deltas=FALSE)
  md.rot <- performVarimax(pca$md.pca,ncomp=pca$ncomp)
  
  md.latent <- performFeatureExtraction(md.rot,md.ds,md.trans,measure ="pa",sign=0.05,withWeights=FALSE,union=FALSE)
  new.md <-prepareMetaFeatures(md.latent$latent.ds,md.latent$latent.trans,md.ds,md.trans,"pa_delta")
  
  #for(i in 1:1){
  i=1    
  validation <- performValidation(new.md$md.ds,
                                    new.md$md.trans,
                                    neutralZone=seq(0,0.01,0.0001),
                                    folds="LOOV",
                                    transformation=trans[i],
                                    nrTrees = 100,
                                    algorithm = "randomForest")
    
    fileName <- paste(alg,trans[i],sep = "_")
    
    writeToFile(validation$transNeutralZonesResults,alg,fileName,"confMatrix")
    validation$predictions[,4] <- as.numeric(as.character(validation$predictions[,4]))
    writeToFile(validation$predictions,alg,paste(alg,trans[i],sep = "_"),"predictions")
  #}
  #}
  
} else {print("WRONG INPUT!")}

#source("plotResults.R")
#p <- plotCondMatrixAllNeutralZonesAllTransNew(validation$transNeutralZonesResults,neutralZones=seq(0,0.01,0.0001))

