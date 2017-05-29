transformations <- c("weka.filters.unsupervised.attribute.Discretize",
                    "weka.filters.supervised.attribute.Discretize", 
                    "weka.filters.unsupervised.attribute.NominalToBinary",
                    "weka.filters.supervised.attribute.NominalToBinary", 
                    "weka.filters.unsupervised.attribute.Normalize",
                    "weka.filters.unsupervised.attribute.Standardize",
                    "weka.filters.unsupervised.attribute.PrincipalComponents",
                    "weka.filters.unsupervised.attribute.ReplaceMissingValues")
transformationsShort <- c("u.Discretize",
                          "s.Discretize", 
                          "u.NomToBin",
                          "s.NomToBin", 
                          "u.Normalize",
                          "u.Stand",
                          "u.PCA",
                          "u.MissVal")


transformationsLog <- c("weka.filters.unsupervised.attribute.Discretize",
                    "weka.filters.supervised.attribute.Discretize", 
                    "weka.filters.unsupervised.attribute.PrincipalComponents")



transformationsPART <- c("weka.filters.unsupervised.attribute.Discretize",
                    "weka.filters.supervised.attribute.Discretize", 
                    "weka.filters.unsupervised.attribute.NominalToBinary",
                    "weka.filters.supervised.attribute.NominalToBinary", 
                    "weka.filters.unsupervised.attribute.PrincipalComponents",
                    "weka.filters.unsupervised.attribute.ReplaceMissingValues")

transformationsJ48 <- c("weka.filters.unsupervised.attribute.Discretize",
                    "weka.filters.supervised.attribute.Discretize", 
                    "weka.filters.unsupervised.attribute.NominalToBinary",
                    "weka.filters.supervised.attribute.NominalToBinary", 
                    "weka.filters.unsupervised.attribute.PrincipalComponents",
                    "weka.filters.unsupervised.attribute.ReplaceMissingValues")

transformationsIBk <- c("weka.filters.unsupervised.attribute.Discretize",
                    "weka.filters.supervised.attribute.Discretize", 
                    "weka.filters.unsupervised.attribute.NominalToBinary",
                    "weka.filters.supervised.attribute.NominalToBinary", 
                    "weka.filters.unsupervised.attribute.PrincipalComponents",
                    "weka.filters.unsupervised.attribute.ReplaceMissingValues")

nonTransFeatures <- c("DefaultAccuracy","NumberOfInstances","IncompleteInstanceCount","NumberOfClasses","MinorityClassSize","MajorityClassSize","MinorityClassPerentage","MajorityClassPercentage","ClassEntropy")
delta_nonTransFeatures <- paste(nonTransFeatures,"_delta",sep="")

#trans <- c("All",transformations)

measures <- c("pa","precision","recall","roc","time")
delta_measures <- paste(measures,"_delta",sep="")

formula17old <- c("Dimensionality",	"EquivalentNumberOfAtts",	"NumberOfFeatures",	"PercentageOfNumericAtts",	"NoiseToSignalRatio",	"MeanKurtosisOfNumericAtts",	"MeanMeansOfNumericAtts",	"MeanStdDevOfNumericAtts",	"MeanMutualInformation",	"MaxNominalAttDistinctValues",	"StdvNominalAttDistinctValues",	"MeanNominalAttDistinctValues",	"MeanSkewnessOfNumericAtts",	"PercentageOfNominalAtts",	"MeanAttributeEntropy",	"PercentageOfBinaryAtts",	"PercentageOfMissingValues")
formula17 <- c("Dimensionality",	"EquivalentNumberOfAtts",	"NumberOfFeatures",	"PercentageOfNumericFeatures",	"NoiseToSignalRatio",	"MeanKurtosisOfNumericAtts",	"MeanMeansOfNumericAtts",	"MeanStdDevOfNumericAtts",	"MeanMutualInformation",	"MaxNominalAttDistinctValues",	"StdvNominalAttDistinctValues",	"MeanNominalAttDistinctValues",	"MeanSkewnessOfNumericAtts",	"PercentageOfSymbolicFeatures",	"MeanAttributeEntropy",	"PercentageOfBinaryFeatures",	"PercentageOfMissingValues")

pureNumericalChars <- c("NumberOfNumericFeatures","MeanMeansOfNumericAtts","MeanStdDevOfNumericAtts","MeanKurtosisOfNumericAtts","MeanSkewnessOfNumericAtts","MinMeansOfNumericAtts","MinStdDevOfNumericAtts", "MinKurtosisOfNumericAtts","MinSkewnessOfNumericAtts","MaxMeansOfNumericAtts","MaxStdDevOfNumericAtts", "MaxKurtosisOfNumericAtts","MaxSkewnessOfNumericAtts","Quartile1MeansOfNumericAtts","Quartile1StdDevOfNumericAtts","Quartile1KurtosisOfNumericAtts","Quartile1SkewnessOfNumericAtts","Quartile2MeansOfNumericAtts","Quartile2StdDevOfNumericAtts", "Quartile2KurtosisOfNumericAtts","Quartile2SkewnessOfNumericAtts","Quartile3MeansOfNumericAtts","Quartile3StdDevOfNumericAtts", "Quartile3KurtosisOfNumericAtts","Quartile3SkewnessOfNumericAtts","PercentageOfNumericFeatures")
pureCategoricalChars <-c("PercentageOfSymbolicFeatures","PercentageOfBinaryFeatures","NumberOfSymbolicFeatures","NumberOfBinaryFeatures","MeanAttributeEntropy", "MeanMutualInformation","EquivalentNumberOfAtts","NoiseToSignalRatio","MinAttributeEntropy","MinMutualInformation","MaxAttributeEntropy","MaxMutualInformation","Quartile1AttributeEntropy","Quartile1MutualInformation","Quartile2AttributeEntropy","Quartile2MutualInformation","Quartile3AttributeEntropy","Quartile3MutualInformation","MaxNominalAttDistinctValues","MinNominalAttDistinctValues","MeanNominalAttDistinctValues","StdvNominalAttDistinctValues")
restChars <- c("ClassEntropy","NumberOfInstances","NumberOfFeatures","NumberOfInstancesWithMissingValues","NumberOfMissingValues","PercentageOfInstancesWithMissingValues","PercentageOfMissingValues","Dimensionality","NumberOfClasses","MajorityClassSize","MinorityClassSize","MajorityClassPercentage","MinorityClassPerentage")

meta_features <- c(pureNumericalChars,pureCategoricalChars,restChars)
delta_meta_features <- paste(meta_features,"_delta",sep="")


#read the file which consists either of latent features or normal features,dataset,transformation,metadata/latent,response
readFile<-function(algName,readDelta=FALSE){
  if(!readDelta){
    file <- paste(c("D://csi//",algName,"_MD.csv"),collapse="")
    md <- read.csv(file, header=TRUE, dec=".", sep=",",colClasses= c(rep("character",4),rep("numeric",61),rep("numeric",6)))
    return(md)
  } else {
    #file <- paste(c("D://csi//md-delta//replaceMissingVals//relativeImp//",algName,"_MD_DELTA.csv"),collapse="")
    file <- paste(c("..//md//relativeImp//",algName,"_MD_DELTA.csv"),collapse="")
    md <- read.csv(file, header=TRUE, dec=".", sep=",",colClasses= c(rep("character",4),rep("numeric",61),rep("numeric",6),rep("numeric",61),rep("numeric",6)))
    return(md)
  }
}

readClassificationFile <- function(alg){
  file <- paste(c("..//md//relativeImp//asClassification//zeroAsRange//0.0001//",alg,"_MD_classification.csv"),collapse="")
  if(alg == "weka.IBk" | alg =="weka.PART"){
    md <- read.csv(file, header=TRUE, dec=".", sep=",",colClasses= c(rep("character",3),rep("numeric",63),rep("character",1)))
  } else if (alg =="weka.J48" | alg =="weka.Logistic" | alg=="weka.NaiveBayes"){
    md <- read.csv(file, header=TRUE, dec=".", sep=",",colClasses= c(rep("character",3),rep("numeric",62),rep("character",1)))
  } else if(alg=="weka.JRip"){
    md <- read.csv(file, header=TRUE, dec=".", sep=",",colClasses= c(rep("character",3),rep("numeric",64),rep("character",1)))
  }
  return(md)
}

getDS <-function(algName,readDelta){
  if(class(algName)=="character"){
      df <- readFile(algName,readDelta)
      return(df[df$Transformation=="None",])
  } else {
    return(algName[algName$Transformation=="None",])
  }
}

getTransformations <- function(algName,readDelta,regOrClass){
  if(class(algName)=="character"){
    if(regOrClass == "classification"){ 
      df <- readClassificationFile(algName)
      return(df)
      } else {
        df <- readFile(algName,readDelta)
        return(df[-which(df$Transformation=="None"),])
      }
  } else {
    return(algName[-which(algName$Transformation=="None"),])
  }
}

getPredictionsFromFile <- function(algName,trans){
  file <- paste(c("D://csi//md-delta//replaceMissingVals//relativeImp//predictions//",algName,"//",algName,"_",trans,".csv"),collapse="")
  md <- read.csv(file, header=TRUE, dec=".", sep=",",colClasses= c(rep("character",3),rep("character",1)))
  md[,4] <- as.numeric(as.character(md[,4]))
  return(md)
}

getFormulaFromTrainingData <- function(train_md){
  latent.features <- colnames(train_md[,sapply(train_md, is.numeric)])
  if(length(latent.features>0)){
    latent.features <- head(latent.features,-1) #remove the last feature (the response)
    formula <- paste(latent.features, collapse="+")
    formula <- as.formula(paste("response ~",formula,sep=""))
    return(formula)
  } else print("No latent features!")
}

replaceMeaninglessValsWithNA <- function(x){
  x[x$NumberOfNumericFeatures==0,pureNumericalChars]<-NaN
  x[x$NumberOfSymbolicFeatures ==1,pureCategoricalChars]<-NaN
  return(x)
}

getPureDatasets <- function(x,type = "numerical"){
  if(type=="numerical"){
    datasets <- x[!is.nan(x$NumberOfNumericFeatures) & is.nan(x$NumberOfSymbolicFeatures),]$Dataset
  }else if(type=="categorical"){
    datasets <- x[is.nan(x$NumberOfNumericFeatures) & !is.nan(x$NumberOfSymbolicFeatures),]$Dataset
  }else if(type=="combined"){
    datasets <- setdiff(x$Dataset,union(getPureDatasets(x,type="numerical"),getPureDatasets(x,type="categorical")))
  }
  return(datasets)
}

getDSMEDI <- function(alg){
  old <- list()
  filePath <-paste(c("C:/Users/Besim/Dropbox/PhD/eclipse/workspaceDropbox/newWeka/results/appliedTransformations/complete/small/",alg,"_complete.csv"),collapse="")
  df <- read.csv(filePath, header=TRUE, dec=".", sep=",",colClasses= c("character",rep("numeric",33),rep("character",2)))
  #colnames(df)[30] <- "Transformation"
  old$md.ds <- df[df$Transformation=="None",]
  old$md.trans <- df[-which(df$Transformation=="None"),]
  return(old)
}

pureNumericalChars <- c("NumberOfNumericFeatures","MeanMeansOfNumericAtts","MeanStdDevOfNumericAtts","MeanKurtosisOfNumericAtts","MeanSkewnessOfNumericAtts","MinMeansOfNumericAtts","MinStdDevOfNumericAtts", "MinKurtosisOfNumericAtts","MinSkewnessOfNumericAtts","MaxMeansOfNumericAtts","MaxStdDevOfNumericAtts", "MaxKurtosisOfNumericAtts","MaxSkewnessOfNumericAtts","Quartile1MeansOfNumericAtts","Quartile1StdDevOfNumericAtts","Quartile1KurtosisOfNumericAtts","Quartile1SkewnessOfNumericAtts","Quartile2MeansOfNumericAtts","Quartile2StdDevOfNumericAtts", "Quartile2KurtosisOfNumericAtts","Quartile2SkewnessOfNumericAtts","Quartile3MeansOfNumericAtts","Quartile3StdDevOfNumericAtts", "Quartile3KurtosisOfNumericAtts","Quartile3SkewnessOfNumericAtts","PercentageOfNumericFeatures")
pureCategoricalChars <-c("PercentageOfSymbolicFeatures","PercentageOfBinaryFeatures","NumberOfSymbolicFeatures","NumberOfBinaryFeatures","MeanAttributeEntropy", "MeanMutualInformation","EquivalentNumberOfAtts","NoiseToSignalRatio","MinAttributeEntropy","MinMutualInformation","MaxAttributeEntropy","MaxMutualInformation","Quartile1AttributeEntropy","Quartile1MutualInformation","Quartile2AttributeEntropy","Quartile2MutualInformation","Quartile3AttributeEntropy","Quartile3MutualInformation","MaxNominalAttDistinctValues","MinNominalAttDistinctValues","MeanNominalAttDistinctValues","StdvNominalAttDistinctValues")
restChars <- c("ClassEntropy","NumberOfInstances","NumberOfFeatures","NumberOfInstancesWithMissingValues","NumberOfMissingValues","PercentageOfInstancesWithMissingValues","PercentageOfMissingValues","Dimensionality","NumberOfClasses","MajorityClassSize","MinorityClassSize","MajorityClassPercentage","MinorityClassPerentage")


checkType <- function(metaFeatures){
  if(all(metaFeatures %in% pureNumericalChars))
    return("numerical")
  else if(all(metaFeatures %in% pureCategoricalChars))
    return("categorical")
  else if(all(metaFeatures %in% restChars))
    return("combined")
  else return("combined")
}

splitDatasets <- function(ds){
  splits <- list()
  splits$numerical <- ds[ds$Dataset %in% getPureDatasets(ds,type="numerical"),]
  splits$categorical <- ds[ds$Dataset %in% getPureDatasets(ds,type="categorical"),]
  splits$combined <- ds[ds$Dataset %in% getPureDatasets(ds,type="combined"),]
  responseAsFactor <- character(dim(ds)[1])
  responseAsFactor[ds$response >0] <- "IMP"
  responseAsFactor[ds$response < 0] <- "NONIMP"
  responseAsFactor[ds$response == 0] <- "SAME"
  responseAsFactor <- as.factor(responseAsFactor)
  ds <- cbind(ds,responseAsFactor)
  write.arff(ds,paste(c("C://Users//Besim//Dropbox//PhD//ourML//Change in Accuracy//md_weka//three_class//relativeImp//",alg,".arff"),collapse = ""))
  return(ds)
}

ourpcor <- function(x,tol,method=c("pearson","kendall","spearman")){
  method <- match.arg(method)
  if (is.data.frame(x)) 
    x <- as.matrix(x)
  if (!is.matrix(x)) 
    stop("supply a matrix-like 'x'")
  if (!(is.numeric(x) || is.logical(x))) 
    stop("'x' must be numeric")
  stopifnot(is.atomic(x))
  n <- dim(x)[1]
  gp <- dim(x)[2] - 2
  cvx <- cov(x, method = method)
  if (det(cvx) < .Machine$double.eps) {
    warning("The inverse of variance-covariance matrix is calculated using Moore-Penrose generalized matrix invers due to its determinant of zero.")
    icvx <- ginv(cvx)
  }
  else icvx <- solve(cvx,tol = tol)
  pcor <- -cov2cor(icvx)
  diag(pcor) <- 1
  if (method == "kendall") {
    statistic <- pcor/sqrt(2 * (2 * (n - gp) + 5)/(9 * (n - 
                                                          gp) * (n - 1 - gp)))
    p.value <- 2 * pnorm(-abs(statistic))
  }
  else {
    statistic <- pcor * sqrt((n - 2 - gp)/(1 - pcor^2))
    p.value <- 2 * pt(-abs(statistic), (n - 2 - gp))
  }
  diag(statistic) <- 0
  diag(p.value) <- 0
  list(estimate = pcor, p.value = p.value, statistic = statistic, 
       n = n, gp = gp, method = method)
}

printForArrayInJava <- function(datasets){
  str <- paste(c(datasets[1],","),collapse="")
  if(length(datasets)>1){
    for(i in 2:length(datasets)){
      str <-paste(c(str,datasets[i],","),collapse="")
    }
  }
  return(str)
}

getClassAsFactor <- function(x){
  responseAsFactor <- character(length(x))
  responseAsFactor[x ==2] <- "POS"
  responseAsFactor[x ==1] <- "NEG"
  responseAsFactor[x ==3] <- "ZERO"
  responseAsFactor <- as.factor(responseAsFactor)
  return(responseAsFactor)
}


getImpactAsFactor <- function(x){
  responseAsFactor <- character(length(x))
  responseAsFactor[x >0] <- "Pos"
  responseAsFactor[x < 0] <- "Neg"
  responseAsFactor[x == 0] <- "Zero"
  responseAsFactor <- as.factor(responseAsFactor)
  return(responseAsFactor)
}

convertDFtoARFF <- function(ds,alg){
  responseAsFactor <- character(dim(ds)[1])
  responseAsFactor[ds$response >0] <- "IMP"
  responseAsFactor[ds$response < 0] <- "NONIMP"
  responseAsFactor[ds$response == 0] <- "SAME"
  responseAsFactor <- as.factor(responseAsFactor)
  ds <- cbind(ds,responseAsFactor)
  write.arff(ds,paste(c("C://Users//Besim//Dropbox//PhD//ourML//Change in Accuracy//md_weka//three_class//relativeImp//",alg,".arff"),collapse = ""))
  return(ds)
}

writeToFile <- function(table,alg,fileName,whatToWrite){
  #write.csv(table,paste(c(getwd(),fileName,".csv"),collapse=""))
  if(whatToWrite == "predictions")
  {path  <- "..//results//predictions//"}
  else {
  path  <- "..//results//"}
  
  if(!dir.exists(paste(c(path,alg,"//"),collapse=""))){
  	folderCreated <- dir.create(paste(c(path,alg,"//"),collapse=""))
	folder <- paste(c(path,alg,"//"),collapse="")
  }
  else{
	folder <- paste(c(path,alg,"//"),collapse="")
  }
if(whatToWrite == "predictions"){
  write.csv(table,paste(c(folder,fileName,".csv"),collapse=""),row.names=FALSE)}
  else{
  write.csv(table,paste(c(folder,fileName,".csv"),collapse=""))}
}
