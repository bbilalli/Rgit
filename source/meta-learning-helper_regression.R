library("rpart")
library("caret")
library("randomForest")
library("party")
source("utilityFunctions.R")
#library("e1071")

set.seed(111)
#it needs to change for performing the validation on deltas!!!
performValidation <-function(md.ds_allTrans,md.trans_allTrans,neutralZone=0,folds="10Fold",transformation=NULL,nrTrees,algorithm){
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
  print(formula)
  formula <- getFormulaFromTrainingData(md.ds)
  for(i in 1:length(folds)){ #10){#
    print(i)
    validate_md <- md.ds[folds[[i]],] #the rows of the fold, actually the datasets before the transformations are applied
    train_md <- md.trans[-which(md.trans$Dataset %in% validate_md$Dataset),]# when delta
    
    test_md <- md.trans[md.trans$Dataset %in% validate_md$Dataset,]
    results.per.fold <- getPredictionsConfMatrix(formula,train_md,validate_md,test_md,neutralZone,nrTrees,transAll,algorithm)
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

getPredictionsConfMatrix<-function(formula,train_md,validate_md,test_md_orig,neutralZone,nrTrees,transAll,algorithm){

  if(algorithm == "randomForest_RandomFeatures") 
    model <- randomForest(formula,data=train_md,ntree=nrTrees)
  else if(algorithm =="randomForest_AllFeatures")
    model <- randomForest(formula,data=train_md,ntree=nrTrees, mtry = (length(formula)-1))
  else if(algorithm =="cforest_RandomFeatures")
    model <- cforest(formula,data=train_md,controls = cforest_control(ntree=nrTrees))
  else if(algorithm =="cforest_AllFeatures") 
    model <- cforest(formula,data=train_md,controls = cforest_control(ntree=nrTrees, mtry = (length(formula)-1)))
  
  matrices <- list()
  transNeutralZoneMatrix <- matrix(0,ncol=1,nrow=7) # the matrix which will contain the results for every transformation for all its neutral zones
  
  #test_md <- test_md_orig[test_md_orig$Transformation %in% transformations[i],]
  test_md <- test_md_orig
  confMatrix <- getConfusionMatrixNZnew(model,test_md,validate_md,neutralZone,transAll,algorithm)
  transNeutralZoneMatrix <- cbind(transNeutralZoneMatrix,confMatrix$confMatrix)
  
  matrices$transNeutralZoneMatrix <- transNeutralZoneMatrix
  matrices$predictedVals <- confMatrix$predictedVals
  return(matrices)
}




getConfusionMatrixNZnew <- function(model,test_md,validate_md,neutralZone,transAll,algorithm){
  results <- list()
  if(dim(test_md)[1]>0){
    
    if(strsplit(algorithm,"_")[[1]][1] == "randomForest") predicted <- predict(model,test_md,OOB=TRUE)
    else if(strsplit(algorithm,"_")[[1]][1] == "cforest") predicted <- predict(model,test_md,OOB=TRUE)[,1] # predictions
    

    #predicted <- rep(0.1,dim(test_md)[1]) #TEST
    real <- test_md$response #real accuracies
    real.before <- validate_md$response #the real response before transformation was applied
    #weight <- 1/length(real)
    if(transAll)
      weight <- test_md$weightsAll
    else
      weight <- test_md$weights # I need the weight vector for all the testing datasets and 
    # I need to make sure that this vector will be properly multiplyid down there
    
    l <- length(neutralZone)
    TPr <- numeric(l)
    TNr <- numeric(l)
    FPr <- numeric(l)
    FNr <- numeric(l)
    FNb <- numeric(l)
    TNb <- numeric(l)
    FPb <- numeric(l)
    
    for(i in 1:length(neutralZone)){
      nz <- neutralZone[i]
      positivePredictions <- real[predicted > nz] # the neutral zone both in the predicted and the real cases!
      negativePredictions <- real[(predicted < 0) & (abs(predicted) > nz)]
      #print(c(positivePredictions,negativePredictions))
      
      positiveWeights <- weight[predicted >nz]
      negativeWeights <- weight[(predicted < 0) & (abs(predicted) > nz)]
      
      TPr[i] <- sum(positiveWeights[positivePredictions > 0]) #since the weight will be a vector in 10Fold I need to make sure that the proper weights are multiples
      # I need to take the indexes of the positivePredictions >0 for instance and take their corresponding weights, from the weight vector
      # e.g., 
      FPr[i] <- sum(positiveWeights[positivePredictions <0])
      FNr[i] <- sum(negativeWeights[negativePredictions >0])
      TNr[i] <- sum(negativeWeights[negativePredictions <0])
      if(nz==0){
        neutralPredictions <- real[predicted ==0]
        neutralWeights <- weight[predicted ==0]
      }
      else {
        neutralPredictions <- real[((predicted < nz) & (predicted >0)) | ((predicted < 0) & (abs(predicted) <nz))]
        neutralWeights <- weight[((predicted < nz) & (predicted >0)) | ((predicted < 0) & (abs(predicted)<nz))]
      }
      FNb[i] <- sum(neutralWeights[neutralPredictions !=0])
      TNb[i] <- sum(neutralWeights[neutralPredictions ==0])
      
      nonNeutralPredictions <- real[((predicted > nz) & (predicted >0)) | ((predicted < 0) & (abs(predicted)>nz))]
      nonNeutralWeights <- weight[((predicted > nz) & (predicted >0)) | ((predicted < 0) & (abs(predicted)>nz))]
      #FPb[i] <- sum(positiveWeights[positivePredictions ==0]) + sum(negativeWeights[negativePredictions ==0])
      FPb[i] <- sum(nonNeutralWeights[nonNeutralPredictions == 0])
    }
    confMatrix <- rbind(TPr,FPr,FNr,TNr,FNb,FPb,TNb)
  }
  else confMatrix <- rep(0,7)
  
  results$confMatrix <- confMatrix #new
  predictionResults <- as.data.frame(cbind(Dataset=test_md$Dataset,Transformation=test_md$Transformation,Attributes=test_md$Attributes,predicted)) #rename also the columns
  results$predictedVals <- predictionResults #new
  return(results) #new
  #return(confMatrix)
}

