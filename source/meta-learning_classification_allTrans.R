#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

source("utilityFunctions.R") #additional functions 
source("feature-extraction.R") #functions for performing feauture extraction and selection : PCA + VARIMAX, PARTIAL CORRELATIONS
#source("meta-learning-helper.R") #functions for performing meta-learning
source("meta-learning-helper_classification.R")

algs <- c("weka.J48","weka.NaiveBayes","weka.JRip","weka.PART","weka.IBk","weka.Logistic")
trans <- c("All",transformations)
alg <- args[1]
ml_alg <- "randomForest"
    
if(alg %in% algs){
  
  md.ds <- getDS(alg,readDelta=TRUE)
  md.trans <- getTransformations(alg,readDelta=TRUE,"")
  
  #it used to be md.ds instead of md.trans
  pca <- performPCA(md.trans,variance=90,old=FALSE,deltas=FALSE)
  md.rot <- performVarimax(pca$md.pca,ncomp=pca$ncomp)
  
  md.latent <- performFeatureExtraction(md.rot,md.ds,md.trans,measure ="pa",sign=0.05,withWeights=FALSE,union=FALSE)
  new.md <-prepareMetaFeatures(md.latent$latent.ds,md.latent$latent.trans,md.ds,md.trans,"pa_delta")
  
  md.ds <- new.md$md.ds
  if(alg =="weka.IBk") t<-0 else t <- 0.0001
  
  md.trans <- convertToClassification(new.md$md.trans,t) #if IBK it should be 0
  
  if(ml_alg =="randomForest") md.trans <- cbind(md.trans[,1:3],na.roughfix(md.trans[,4:dim(md.trans)[2]]))
  #for(i in 2:length(trans)){
  i= 1
 	validation <- performValidation_classification(md.ds,
                                                 md.trans,
                                                 neutralZone=0,
                                                 folds="LOOV",
                                                 transformation=trans[i],
                                                 nrTrees = 100,
                                                 algorithm = ml_alg)
  
 	writeToFile(validation$transNeutralZonesResults,alg,paste(alg,trans[i],sep = "_"),"confMatrix")
 	#validation$predictions[,4] <- as.numeric(as.character(validation$predictions[,4]))
	#validation$predictions[,5] <- as.numeric(as.character(validation$predictions[,5]))
	#validation$predictions[,6] <- as.numeric(as.character(validation$predictions[,6]))
	#validation$predictions[,7] <- as.numeric(as.character(validation$predictions[,7]))
  writeToFile(validation$predictions,alg,paste(alg,trans[i],sep = "_"),"predictions")
	#}
}
#source("plotResults.R")
#(test <- plotBarImpactPerTrans_RealvsPred_classification(algs[1],trans[1],trans[1],FALSE,nrTrees = "1Tree",customOrNot = ""))


