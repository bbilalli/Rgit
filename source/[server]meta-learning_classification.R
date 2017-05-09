#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = TRUE)

source("utilityFunctions.R") #additional functions 
source("feature-extraction.R") #functions for performing feauture extraction and selection : PCA + VARIMAX, PARTIAL CORRELATIONS
source("meta-learning-helper.R") #functions for performing meta-learning
source("meta-learning-helper_classification.R")

algs <- c("weka.J48","weka.NaiveBayes","weka.JRip","weka.PART","weka.IBk","weka.Logistic")
#algs <- c("weka.JRip","weka.PART","weka.IBk","weka.Logistic")
trans <- c("All",transformations)
alg <- algs[1]

if(alg == "weka.J48") {trans <- c("All", transformationsJ48)} else
if(alg == "weka.PART") {trans <- c("All", transformationsPART)} else
if(alg == "weka.IBk") {trans <- c("All", transformationsJ48)} else
trans <- c("All",transformationsLog)
    
if(alg %in% algs){
  
  md.ds <- getDS(alg,readDelta=TRUE)
  md.trans <- getTransformations(alg,readDelta=TRUE,"")
  
  #it used to be md.ds instead of md.trans
  pca <- performPCA(md.trans,variance=90,old=FALSE,deltas=FALSE)
  md.rot <- performVarimax(pca$md.pca,ncomp=pca$ncomp)
  
  md.latent <- performFeatureExtraction(md.rot,md.ds,md.trans,measure ="pa",sign=0.05,withWeights=FALSE,union=FALSE)
  new.md <-prepareMetaFeatures(md.latent$latent.ds,md.latent$latent.trans,md.ds,md.trans,"pa_delta")
  
  md.ds <- new.md$md.ds
  md.trans <- getTransformations(alg,TRUE,"classification")
  md.trans$response <- as.factor(md.trans$response)
  md.trans <- cbind(new.md$md.trans[,-ncol(new.md$md.trans)],"responseNumeric"=new.md$md.trans$response,"response"=md.trans$response)
  md.trans <- md.trans[md.trans$Transformation %in% trans,]
  i= 1
  validation <- performValidation_classification(md.ds,
                                                 md.trans,
                                                 neutralZone=0,
                                                 folds="LOOV",
                                                 transformation=trans[i],
                                                 nrTrees = 1)
  
  writeToFile(validation$transNeutralZonesResults,paste(alg,trans[i],sep = "_"),"confMatrix")
  validation$predictions[,4] <- as.numeric(as.character(validation$predictions[,4]))
  writeToFile(validation$predictions,paste(alg,trans[i],sep = "_"),"predictions")
}
#source("plotResults.R")
#(test <- plotBarImpactPerTrans_RealvsPred_classification(algs[1],trans[1],trans[1],FALSE,nrTrees = "1Tree",customOrNot = ""))


