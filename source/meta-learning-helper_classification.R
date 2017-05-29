library("rpart")
library("caret")
#library("randomForest")
library("party")
source("utilityFunctions.R")
#library("e1071")





#it needs to change for performing the validation on deltas!!!
performValidation_classification <-function(md.ds_allTrans,md.trans_allTrans,neutralZone=0,folds="10Fold",transformation=NULL,nrTrees){
set.seed(111)
  if(transformation != "All"){
    md.trans <- md.trans_allTrans[md.trans_allTrans$Transformation == transformation,]
    md.ds <- md.ds_allTrans[md.ds_allTrans$Dataset %in% md.trans$Dataset,]
    transAll <- FALSE
  } else {
    md.trans <- md.trans_allTrans
    md.ds <- md.ds_allTrans
    transAll <- TRUE
    folds = "10Fold"
  }
  print(dim(md.ds))
  print(dim(md.trans))
  folds <- getFolds(md.ds,folds) #gives back the row/rows of a fold
  #print(folds)
  folds.results <- list()
  formula <- getFormulaFromTrainingData(md.ds)
  for(i in 1:length(folds)){ #10){#
    print(i)
    validate_md <- md.ds[folds[[i]],] #the rows of the fold, actually the datasets before the transformations are applied
    train_md <- md.trans[-(md.trans$Dataset %in% validate_md$Dataset),]# when delta
    
    test_md <- md.trans[md.trans$Dataset %in% validate_md$Dataset,]
    results.per.fold <- getPredictionsConfMatrix(formula,train_md,validate_md,test_md,neutralZone,nrTrees,transAll)
    folds.results[[i]] <- results.per.fold
  }
  matrixTransNeutralZones <-0
  predictionResults <- data.frame(Dataset=character(),Transformation=character(),Attributes=character(),predicted=numeric())
  for(i in 1:length(folds.results)){
    matrixTransNeutralZones <- folds.results[[i]]$transNeutralZoneMatrix + matrixTransNeutralZones
    predictionResults <- rbind(predictionResults,folds.results[[i]]$predictedVals)
  }
  colnames(matrixTransNeutralZones)[2:ncol(matrixTransNeutralZones)] <- rep(neutralZone,1)
  
  folds.results$transNeutralZonesResults <- matrixTransNeutralZones
  #print(colnames(predictionResults))
  folds.results$predictions <- predictionResults
  return(folds.results)
}

getFolds <- function(datasets_md,folds="LOOV"){
  if(folds=="LOOV"){
    return(createFolds(datasets_md$Dataset,nrow(datasets_md)))
  } else if(folds =="10Fold"){
    return(createFolds(datasets_md$Dataset,10))
  }
}

getPredictionsConfMatrix<-function(formula,train_md,validate_md,test_md_orig,neutralZone,nrTrees,transAll){
  
  model <- cforest(formula,data=train_md,controls = cforest_control(ntree=nrTrees))
  #model <- svm(formula,data=train_md)
  
  matrices <- list()
  transNeutralZoneMatrix <- matrix(0,ncol=1,nrow=6) # the matrix which will contain the results for every transformation for all its neutral zones
  
  #test_md <- test_md_orig[test_md_orig$Transformation %in% transformations[i],]
  test_md <- test_md_orig
  confMatrix <- getConfusionMatrixNZnew(model,test_md,validate_md,neutralZone,transAll)
  transNeutralZoneMatrix <- cbind(transNeutralZoneMatrix,confMatrix$confMatrix)
  
  matrices$transNeutralZoneMatrix <- transNeutralZoneMatrix
  matrices$predictedVals <- confMatrix$predictedVals
  return(matrices)
}




getConfusionMatrixNZnew <- function(model,test_md,validate_md,neutralZone,transAll){
  results <- list()
  if(dim(test_md)[1]>0){
    #predicted <- predict(model,test_md,OOB=TRUE)[,1] # predictions
    predicted <- predict(model,test_md,OOB=TRUE)
	
	predictionsAsProb <- predict(model,test_md,type="prob",OOB=TRUE) #TO DO: check  the probabilities 
    negativeProb <- sapply(predictionsAsProb,function(x){x[,1]})
    poositiveProb <- sapply(predictionsAsProb,function(x){x[,2]})
    neutralProb <- sapply(predictionsAsProb,function(x){x[,3]})
    df.predictionsAsProb <- cbind(negativeProb,poositiveProb,neutralProb)
	
    #print(predicted)
    real <- test_md$response #real accuracies
    real.before <- validate_md$response #the real response before transformation was applied

    if(transAll)
      weight <- test_md$weightsAll
    else
      weight <- test_md$weights # I need the weight vector for all the testing datasets and 
    # I need to make sure that this vector will be properly multiplyid down there
    
    l <- length(neutralZone)
    TP <- numeric(l)
    TN <- numeric(l)
    FP <- numeric(l)
    FN <- numeric(l)
    TNeutrals <- numeric(l)
    FNeutrals <- numeric(l)
    
    for(i in 1:length(neutralZone)){
      nz <- neutralZone[i]
      positivePredictions <- real[predicted == "POSITIVE"] # the neutral zone both in the predicted and the real cases!
      negativePredictions <- real[predicted == "NEGATIVE"]
      #print(c(positivePredictions,negativePredictions))
      
      positiveWeights <- weight[predicted == "POSITIVE"]
      negativeWeights <- weight[predicted == "NEGATIVE"]
      
      TP[i] <- sum(positiveWeights[positivePredictions == "POSITIVE"]) #since the weight will be a vector in 10Fold I need to make sure that the proper weights are multiples
      FP[i] <- sum(positiveWeights[positivePredictions != "POSITIVE"])
      FN[i] <- sum(negativeWeights[negativePredictions != "NEGATIVE"])
      TN[i] <- sum(negativeWeights[negativePredictions == "NEGATIVE"])
      
      neutralPredictions <- real[predicted == "ZERO"]
      neutralWeights <- weight[predicted == "ZERO"]
      
      TNeutrals[i] <- sum(neutralWeights[neutralPredictions == "ZERO"])
      FNeutrals[i] <- sum(neutralWeights[neutralPredictions != "ZERO"])
    
    }
    confMatrix <- rbind(TP,FP,FN,TN,TNeutrals,FNeutrals)
  }
  else confMatrix <- rep(0,6)
  
  results$confMatrix <- confMatrix #new
  predictionResults <- as.data.frame(cbind(Dataset=test_md$Dataset,Transformation=test_md$Transformation,Attributes=test_md$Attributes,predicted,df.predictionsAsProb)) #rename also the columns
  results$predictedVals <- predictionResults #new
  return(results) #new
  #return(confMatrix)
}

