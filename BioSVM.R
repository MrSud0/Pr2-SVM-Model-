
#####This script creates and train a SVM model based on GDS4057, then predicts the classes for GDS4056 and GDS3716####
#####and creates the confusion matrix/accuracy for each gds based on our model###########################################
#####output printed on screen the 3 confusion matrices  and 3 accuracy values############################################

  #Machine Learning model SVM


  
  #install packages function only need to run it the first time  running the programm on our pc
  #DLB()
  
  
  library(Biobase)
  library(GEOquery)
  library(crossval)
  library(pathClass)
  library(caret)
  library(affy)
  library(e1071)
  library(C50)
  
  
  ##################################
  ########### GDS4057 ##############
  
  #Downloads "GDSXXXX" and saves it in a global variable GEOdata
  GDSdownload("GDS4057") 
  #converting GDS to express set
  eset<-GDS2eSet(GEOdata,do.log2=TRUE) 
  #we take only the samples needed
  genoData<-eset@phenoData@data$`genotype/variation` 
  esetReduced<-eset[,genoData %in% c("ER (IHC): -","ER (IHC): +")]
  
  #we create the factor genoData for crossval
  genoData<-factor(genoData[genoData %in% c("ER (IHC): -","ER (IHC): +")]) 
  #we create our reduced data matrix
  esetMat<-(exprs(esetReduced))  
  #we transpose the matrix for the crossval input
  esetMatT<-t(esetMat) 
  #we replace the NaN values with zeroes to avoid nan errors, although I am not sure thats the best approach
  esetMatT[!is.finite(esetMatT)] <- 0 
  
  
  ##################################
  ########### GDS4056 ##############
  
  GDSdownload("GDS4056")
  eset2<-GDS2eSet(GEOdata,do.log2=TRUE)
  genoData2<-eset2@phenoData@data$`genotype/variation`
  #we dont need to reduce the samples on this one as they dont contain unwanted values
  esetMat2<-exprs(eset2)
  esetMat2T<-t(esetMat2)
  esetMat2T[!is.finite(esetMat2T)] <- 0
  
  ##################################
  ########### GDS3716 ##############
  
  GDSdownload("GDS3716")
  eset3<-GDS2eSet(GEOdata,do.log2=TRUE)
  genoData3<-eset3@phenoData@data$specimen
  
  eset3Reduced<-eset3[,genoData3 %in% c("ER- breast cancer","ER+ breast cancer")]
  genoData3<-factor(genoData3[genoData3 %in% c("ER- breast cancer","ER+ breast cancer")])
  
  esetMat3<-exprs(eset3Reduced)
  esetMat3T<-t(esetMat3)
  esetMat3T[!is.finite(esetMat3T)] <- 0
  
  ##################################
  ##################################
  
  
  #this function will take as input a transposed mat and a factor with genoData
  #it will return the selected genes on a matrix based on rfe feature selection using function crossval

  selectedMat <- FSelection(esetMatT,genoData)
  
  #we create and train out SVM model based on our selectedMat and the genoData
  
  control<-trainControl(method = "repeatedcv",number = 10, repeats = 3)
  modelSvm<-train(x=selectedMat,y=genoData ,method="svmLinearWeights",trControl = control)
  
  cm1<-0
  
  ####predict data for GDS4057####
  pr <- predict(modelSvm, newdata = esetMatT)
  #####Confusion matrix + accuracy finder function#####
  #creates a matrix "cm1" containing the confusion matrix
  #and a variable "accuracy" containing the accuracy
  CMAdjusted(pr,genoData)
  
  #printing on the screen confusion matrix and accuracy
  cm1
  accuracy
  
  ####predict data for GDS4056####
  pr <- predict(modelSvm, newdata = esetMat2T) 
  CMAdjusted(pr,genoData2)
  #prints confusion matrix and accuracy
  cm1
  accuracy
  
  
  ####predict data for GDS3716####
  pr <- predict(modelSvm, newdata = esetMat3T)
  CMAdjusted(pr,genoData3)
  #prints confusion matrix and accuracy
  cm1
  accuracy

  
  