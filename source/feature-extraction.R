library(FactoMineR)
library(factoextra)
library(ppcor)
source("utilityFunctions.R")

performPCA <- function(df,variance=90,old,deltas=FALSE){
  pca <- list()
  if(old) mf <- df[,formula17]
  else {
    if(!deltas){
    mf <- df[,meta_features] # keep only meta-features
    mf <- mf[,-which(colnames(mf) %in% nonTransFeatures)] #TO DO : remove non transformable features from the feature space
    #mf <- mf[,which(colnames(mf) %in% restChars)]
    } else {
      mf <- df[,c(meta_features,delta_meta_features)] # keep only delta meta-features or whatever we decide to keep
      mf <- mf[,-which(colnames(mf) %in% delta_nonTransFeatures)] #TO DO : remove non transformable features from the feature space
    }
  }
  print(dim(mf))
#   if(!is.na(type)){
#     if(type=="numerical")
#       mf <- mf[,-which(colnames(mf) %in% pureCategoricalChars)]
#     else if(type=="categorical"){
#       print("I am here")
#       mf <- mf[,-which(colnames(mf) %in% pureNumericalChars)]
#       print(colnames(mf))
#       }
#     else if(type=="combined")
#       mf <- mf[,-which(colnames(mf) %in% c(pureNumericalChars,pureCategoricalChars))]
#     
#   } 
  
  #scaled.mf <- scale(mf,center = TRUE,scale=TRUE)
  scaled.mf <- mf
  print(dim(scaled.mf))
  md.pca <- PCA(scaled.mf,axes = c(1,2),ncp = 30, graph=FALSE) #do the pca and varimax only with the transformable set of features
  pca.ds.eig <- md.pca$eig
  firstPCwithGTninty <- rownames(pca.ds.eig[which(pca.ds.eig$`cumulative percentage of variance` > variance)[1],])
  ncomp <- strsplit(firstPCwithGTninty," ")[[1]][2]
  
  pca$md.pca <- md.pca
  pca$ncomp <- as.numeric(ncomp)
  return(pca)
}

performVarimax<-function(pca.ds,ncomp){
  return(rot.pca.ds <- varimax(pca.ds$var$cor[,1:ncomp])) #varimax
}


performFeatureExtraction <-function(rot.md,md.ds,md.trans,measure="pa",sign=0.01,withWeights,union){
  latent.md <- list()
  latent <- prepareLatentSpaceAutomatically(rot.md,md.ds,md.trans,measure=measure,0.7,NULL,withWeights=withWeights,sign,union)
  #return(latent)
  #print(colnames(latent$latent.trans.space))
  #return(latent)
  if(union){
    extraMetadata <- c("Dataset","Transformation","Attributes")
    latent.ds <- cbind(md.ds[,extraMetadata],latent$latent.ds.space)
    latent.trans <- cbind(md.trans[,extraMetadata],latent$latent.trans.space)
  } else {
    print("I am here Besim!!!")
    partial.corr <- calculateAndPrintCorrelations(latent$latent.ds.space,sign=sign) #non delta
    #partial.corr <- calculateAndPrintCorrelations(latent$latent.trans.space,sign=sign) # delta
    relevantFeatures <- partial.corr$partial.correlation$metadata #get the names of relevant dimensions
    #return(relevantFeatures)
    extraMetadata <- c("Dataset","Transformation","Attributes")
    latent.ds <- cbind(md.ds[,extraMetadata],latent$latent.ds.space[,relevantFeatures],md.ds[,measure])
    colnames(latent.ds)[ncol(latent.ds)] <- measure #"response"
  
    #latent.trans <- cbind(md.trans[,extraMetadata],latent$latent.trans.space[,relevantFeatures],md.trans[,measure]) #transformations switched to the latent space
    relevantdims.trans <- latent$latent.trans.space[,relevantFeatures]
    latent.delta.trans <- latent$latent.delta.space[,relevantFeatures]
    colnames(latent.delta.trans) <- unname(sapply(colnames(relevantdims.trans),function(x) paste(c(x,"delta"),collapse="_")))
    latent.trans <- cbind(md.trans[,extraMetadata],relevantdims.trans,latent.delta.trans,md.trans[,measure])
    colnames(latent.trans)[ncol(latent.trans)] <- measure #"response"
  }
  latent.md$relevantFeatures <- colnames(latent.ds)
  latent.md$latent.ds <- latent.ds
  latent.md$latent.trans <- insertRealWeights(latent.trans,latent.ds,measure)
  return(latent.md)
}

insertRealWeights <- function(md.trans,md.ds,measure){
  weights <- numeric()
  weightsAll <- numeric()
  ds <- md.ds$Dataset
  for(i in 1:length(ds)){
    trans.ds <- md.trans[md.trans$Dataset == ds[i],]
    
    nr.trans.ds <- dim(trans.ds)[1]
    weightsAll <- c(weightsAll,rep(1/nr.trans.ds,nr.trans.ds)) #this is to give uniform weight to all transformations
    
    lengthsOfTransformations <- list()
    for(i in 1:length(transformations)){
      lengthsOfTransformations[transformations[i]] <-  dim(trans.ds[trans.ds$Transformation == transformations[i],])[1]
    }
    for(j in 1:dim(trans.ds)[1]){
      tr <- trans.ds[j,'Transformation']
      l <- lengthsOfTransformations[[tr]][1]
      weights <- c(weights,rep(1/l,1))
    }
  }
  
  md.trans <- cbind(md.trans,weights,weightsAll)
  return(md.trans)
}

insertWeights <- function(md.trans,md.ds,measure){
  #weights <- numeric(dim(md.trans)[1])
  zone <- numeric()
  ds <- md.ds$Dataset
  response <- md.ds[,measure]
  for(i in 1:length(ds)){
    trans.ds <- md.trans[md.trans$Dataset==ds[i],]
    nr.trans.ds <- dim(trans.ds)[1]
    #weights <- c(weights,rep(1/nr.trans.ds,nr.trans.ds))
    zn <-getZone(response[i])
    zone <- c(zone,rep(zn,nr.trans.ds))
    }
  md.trans <- cbind(md.trans,zone)
  return(md.trans)
}

getZone <- function(response){
  ranges <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
  result <- 0
  for(i in 1:(length(ranges)-1)){
    k <- i+1
    if(response >= ranges[i] & response <= ranges[k]){
      return(i)
    }
  }
}

prepareLatentSpaceAutomatically <-function(rot.ds,ds,ds.trans,measure,corrThreshold = 0.7, dimNames=NULL,withWeights,sign,union=FALSE){ #we need the dim names, otherwise they will be numeric
  
  dim <- list()
  loadingsMatrix <- as.matrix(rot.ds$loadings) #get the values from the loading matrix (rotated pca)
  
  metaFeatures.all <- rownames(loadingsMatrix)
  metaFeatures.available <- colnames(ds)
  
  #splits <- splitDatasets(ds)
  splits <- NULL
    
  print(dim(loadingsMatrix))
  numberOfMetaFeaturesPerDim <- apply(loadingsMatrix,2,function(x) {length(which(x<(-corrThreshold) | x>(corrThreshold)))})
  dimsWithoutMetaFeatures <- grep("0",numberOfMetaFeaturesPerDim)
  if(length(dimsWithoutMetaFeatures) != 0)
    loadingsMatrix <- loadingsMatrix[,-dimsWithoutMetaFeatures] #remove the dimensions that do not have any significantly correlated meta-features
  print(dim(loadingsMatrix))
  for(i in 1:ncol(loadingsMatrix)){ #at the end depending on the dimension we will have a list of length as many dimensions however some of them may be null
    negative.corr <- which(loadingsMatrix[,i]<(-corrThreshold))
    positive.corr <- which(loadingsMatrix[,i]>corrThreshold)
    metaFeatures.perDimension.negCorr <- metaFeatures.all[negative.corr] #names of the meta features are actually in the row names of the loading matrix
    metaFeatures.perDimension.posCorr <- metaFeatures.all[positive.corr]
    negative.weights <- loadingsMatrix[negative.corr,i] * (-1) #we multiply by -1... ?!
    positive.weights <- loadingsMatrix[positive.corr,i]
    
    if(length(metaFeatures.perDimension.negCorr) != 0){ #if there are negatively correlated, transform them to positively correlated by multiplying with -1.
      if(all(metaFeatures.perDimension.negCorr %in% metaFeatures.available)){ #check if the features exist in the ds (because ds can be only numeric or categorical or combined)
        if (length(metaFeatures.perDimension.posCorr) != 0){ #check if there are also positively correlated features, if not do not multiply with (-1), because all of the meta-faetures of a latent feature are negatively correlated
          if(all(metaFeatures.perDimension.posCorr %in% metaFeatures.available)){
            ds[,metaFeatures.perDimension.negCorr] <- ds[,metaFeatures.perDimension.negCorr] * (-1) 
          }
        }
      } else next;         
    } else if (length(metaFeatures.perDimension.posCorr) != 0) {
      if(all(metaFeatures.perDimension.posCorr %in% metaFeatures.available)){ # TO DO : check if the features exist in the ds
        
        #don't do anything just continue in the loop
      } else next;
    } else next; #if both positively corr and negatively corr meta-features do not exist then it means that we have less dimensions then we should, or just remove the columns with such properties from the loading matrix
    
    metaFeature.perDimension <- c(metaFeatures.perDimension.negCorr,metaFeatures.perDimension.posCorr)
    metaFeature.weights <- c(negative.weights,positive.weights)
    metaFeature.type <- checkType(metaFeature.perDimension)

    
    if(length(metaFeature.weights)==0 | !withWeights) metaFeature.weights <- 1
    
    properties.perDimension <- list()
    properties.perDimension$metaFeatures <- metaFeature.perDimension
    properties.perDimension$metaFeature.weights <- metaFeature.weights
    properties.perDimension$metaFeature.type <- metaFeature.type
    
    if(is.null(dimNames)){
      properties.perDimension$dim.name <- paste(c("Dim.",i),collapse = "")
    } else {
      properties.perDimension$dim.name <- dimNames[i]
    }
    #dimName <- properties.perDimension$dim.name
    dim[[i]] <- properties.perDimension
  }
  if(union){ #if the relevant features are taken as unions of the partial correlations of the splits
    relevantDimensions <- createLatentMatricesAndGetRelevantDimensions(ds,dim,measure,dimNames,sign) #datasets only
    new.ds <- createLatentMatrixForLearning(ds,dim,measure,relevantDimensions)
    new.trans.ds <- createLatentMatrixForLearning(ds.trans,dim,measure,relevantDimensions) #transformations only
  } else {
    #it seems we are not scaling the data !!! they are inserted here in their original form
    new.ds <- combineLatentMatrix(ds,dim,measure,1:length(dim))
    new.trans.ds <- combineLatentMatrix(ds.trans,dim,measure,1:length(dim))
    latent.delta <- combineDeltaLatentMatrix(ds.trans,dim,1:length(dim))
  }

  dim$latent.ds.space <- new.ds
  dim$latent.trans.space <- new.trans.ds
  dim$latent.delta.space <- latent.delta
  dim$splits <- splits
  return(dim)
}

#INPUT
# ds: the matrix for which the correlations need to be found
# type: the type of the datasets (we split the datasets used for meta-learning into three sets: Categorical, Continuous, Combined) 
# alg: 
# repsonse: the performance measure used
# finds the correlations and partial correlations of a given matrix
# we select only the columns we are interested in: the corr/partial corr of the latent features and the response
#OUTPUT
# a list containing the corr,partial corr, alg type, measure.
calculateAndPrintCorrelations <- function(ds,sign=0.05,printToFile = FALSE,pcor=FALSE){
  result<- list()
  response = "response"
  if(!pcor){
  ds <- ds[complete.cases(ds),] #for calculating the partial correlations we use only the rows withouth nans
  pcor.matrix <- ourpcor(ds,tol=6.99412e-33)
  } else {
  pcor.matrix <- pcor(ds)
  }
  proba <- which(pcor.matrix$p.value[,grep(response,colnames(ds))]<=sign & pcor.matrix$estimate[,grep(response,colnames(ds))]!=1) 
  correlation <- pcor.matrix$estimate[,grep(response,colnames(ds))][proba]
  p.value <- pcor.matrix$p.value[,grep(response,colnames(ds))][proba]
  
  metadata <- colnames(ds)[proba]
  partial.correlation <- as.data.frame(cbind(metadata,correlation,p.value))
  rownames(partial.correlation) <- NULL
  indx <- sapply(partial.correlation, is.factor)
  partial.correlation[indx] <- lapply(partial.correlation[indx], function(x) as.character(x))
  
  result$partial.correlation <- partial.correlation
  
  return(result)
}

createLatentMatrixForLearning <- function(ds,dim,measure,relevantDimensions){

  relevantDims <- numeric(0) #the indexes of relevant dimensions , because we had their names
  
  for(i in 1:length(dim)){
    for(j in 1:length(relevantDimensions)){
      if(dim[[i]]$dim.name == relevantDimensions[j])
        relevantDims <- c(relevantDims,i)
    }
  }
  new.ds <- combineLatentMatrix(ds,dim,measure,relevantDims)
  
  return(new.ds)
}


createLatentMatricesAndGetRelevantDimensions <- function(ds,dim,measure,dimNames=NULL,sign){
  latent <- list()
  
  splits <- splitDatasets(ds)
  
  numerical.dim <- numeric() #indexes of dimensions of numerical type...
  categorical.dim <- numeric()
  combined.dim <- numeric()
  
  for(i in 1:length(dim)){
    type = dim[[i]]$metaFeature.type
    print(paste(c(i,type, dim[[i]]$metaFeatures),collapse = ","))
    if(type == "numerical"){
      numerical.dim <- c(numerical.dim,i)
    } else if(type == "categorical"){
      categorical.dim <- c(categorical.dim,i)
    } else if(type == "combined"){
      combined.dim <- c(combined.dim,i)
    }
  }
  print(c(numerical.dim))
  #numerical.dim <- numerical.dim[-2]
  print(c(numerical.dim))
  
  latent$categoric.ds <- combineLatentFeaturesAndCalcCorrelations(ds,splits$categorical,dim,categorical.dim,measure,sign)
  latent$combined.ds <- combineLatentFeaturesAndCalcCorrelations(ds,splits$combined,dim,combined.dim,measure,sign)
  latent$numeric.ds <- combineLatentFeaturesAndCalcCorrelations(ds,splits$numerical,dim,numerical.dim,measure,sign)
  return(c(latent$numeric.ds,latent$categoric.ds,latent$combined.ds))
}

combineLatentFeaturesAndCalcCorrelations <-function(ds,dsType,dim,dimType,measure,sign){
  subset <- ds[rownames(dsType),] #get the rownames of the split
  new.ds <- combineLatentMatrix(subset,dim,measure,dimType)
  partial.corr <- calculateAndPrintCorrelations(new.ds,sign) #perform partial correlation and find relevant dimensions
  relevantFeatures <- partial.corr$partial.correlation$metadata #get the names of relevant dimensions

  return(relevantFeatures)
}

combineLatentMatrix <-function (ds,dim,measure,relevantDims){
  #maybe we need to scale the features here before taking their mean
  if(length(relevantDims)>0){
    new.ds <- apply(ds[,dim[[relevantDims[1]]]$metaFeatures] * dim[[relevantDims[1]]]$metaFeature.weights,MARGIN=1,FUN=mean,na.rm=TRUE)
    for(i in 2:length(relevantDims)){
      if(length(dim[[relevantDims[i]]]$metaFeatures) >1){
        new.ds <- cbind(new.ds,apply(ds[,dim[[relevantDims[i]]]$metaFeatures] * dim[[relevantDims[i]]]$metaFeature.weights,MARGIN=1,FUN=mean,na.rm=TRUE))
      }
      else
        new.ds <- cbind(new.ds,ds[,dim[[relevantDims[i]]]$metaFeatures])
    }
  } else message("NO RELEVANT DIMENSIONS!")
  
  message(sum(is.na(new.ds)))
  
  dimensionNames <- character(0)
  for(i in 1:length(relevantDims)){
    dimensionNames <- c(dimensionNames,dim[[relevantDims[i]]]$dim.name)
  }
  
  colnames(new.ds)[c(1:ncol(new.ds))] <- dimensionNames
  new.ds <- cbind(new.ds,ds[,which(colnames(ds)==measure)])#add the response to the latent space to use it in the correlation analysis
  #new.ds <- scale(new.ds)
  colnames(new.ds)[ncol(new.ds)] <- "response"
  return(new.ds)
}

#new function for computing the delta of the latent features, we will do this only for the transformed instances
combineDeltaLatentMatrix <-function (ds,dim,relevantDims){
  #take only the delta features ..
  ds_delta <- ds[,delta_meta_features]
  colnames(ds_delta) <- unname(sapply(colnames(ds_delta),function(x) strsplit(x,"_")[[1]][1])) #rename the delta features to meta_features to help the selection and computation of the means
  #return(ds_delta)
  #maybe we need to scale the features here before taking their mean
  if(length(relevantDims)>0){
    new.ds <- apply(ds[,dim[[relevantDims[1]]]$metaFeatures] * dim[[relevantDims[1]]]$metaFeature.weights,MARGIN=1,FUN=mean,na.rm=TRUE)
    new.delta.ds <-  apply(ds_delta[,dim[[relevantDims[1]]]$metaFeatures] * dim[[relevantDims[1]]]$metaFeature.weights,MARGIN=1,FUN=mean,na.rm=TRUE)
    for(i in 2:length(relevantDims)){
      if(length(dim[[relevantDims[i]]]$metaFeatures) >1){
        new.ds <- cbind(new.ds,apply(ds[,dim[[relevantDims[i]]]$metaFeatures] * dim[[relevantDims[i]]]$metaFeature.weights,MARGIN=1,FUN=mean,na.rm=TRUE))
        new.delta.ds <-  cbind(new.delta.ds,apply(ds_delta[,dim[[relevantDims[i]]]$metaFeatures] * dim[[relevantDims[i]]]$metaFeature.weights,MARGIN=1,FUN=mean,na.rm=TRUE))
      }
      else{
        new.ds <- cbind(new.ds,ds[,dim[[relevantDims[i]]]$metaFeatures])
        new.delta.ds <- cbind(new.delta.ds,ds_delta[,dim[[relevantDims[i]]]$metaFeatures])
      }
    }
  } else message("NO RELEVANT DIMENSIONS!")
  
  message(sum(is.na(new.ds)))
  
  dimensionNames <- character(0)
  for(i in 1:length(relevantDims)){
    dimensionNames <- c(dimensionNames,dim[[relevantDims[i]]]$dim.name)
  }
  
  colnames(new.ds)[c(1:ncol(new.ds))] <- dimensionNames
  colnames(new.delta.ds)[c(1:ncol(new.delta.ds))] <- dimensionNames
  
  #new.ds <- scale(new.ds)
  
  #new.delta.ds <- scale(new.delta.ds, center = attributes(new.ds)$'scaled:center', scale = attributes(new.ds)$'scaled:scale') #the scaled latent deltas
  
  #latent.delta <- new.ds - new.delta.ds
  #latent.delta <- matrix(0,nrow(new.ds),ncol(new.ds))
  # for(j in 1:nrow(new.ds)){
  #   latent.delta[j,] <- new.ds[j,]-new.delta.ds[j,]
  # }
  #return(new.delta.ds)
  #print(head(new.ds))
  #print(head(new.delta.ds))
  return(new.delta.ds)
}





prepareMetaFeatures <- function(latent.ds,latent.trans,md.ds,md.trans,measure){
  #new.md.ds <- cbind(latent.ds,md.ds[setdiff(delta_meta_features,delta_nonTransFeatures)],md.ds[delta_measures[which(delta_measures==measure)]])
  new.md.ds <- cbind(latent.ds,md.ds[delta_measures[which(delta_measures==measure)]])
  colnames(new.md.ds)[length(colnames(new.md.ds))] <- "response"
  #new.md.trans <- cbind(latent.trans,md.trans[setdiff(delta_meta_features,delta_nonTransFeatures)],md.trans[delta_measures[which(delta_measures==measure)]])
  new.md.trans <- cbind(latent.trans,md.trans[delta_measures[which(delta_measures==measure)]])
  colnames(new.md.trans)[length(colnames(new.md.trans))] <- "response"
  results <- list()
  results$md.ds <- new.md.ds
  results$md.trans <- new.md.trans
  return(results)
}


