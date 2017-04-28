#Machine Learning model

#install packages function
DLB()

install.packages("caret")

install.packages("pathClass")
biocLite("crossval")
library(e1071)
library(C50)
library(affy)
library(crossval)
library(caret)
library(pathClass)

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

#finds the most important features/genes
res.rfe<-crossval(esetMatT,genoData,DEBUG=TRUE, theta.fit=fit.rfe)

#we save the most important features/genes on a matrix 
x<-as.matrix(extractFeatures(res.rfe,toFile=FALSE))


temp<-colnames(esetMatT)  #columns' names for if statements below

selectedMat<-0 #this will be the selected genes matrix

#checks which genes are the important ones and creates a matrix with only those
for( y in 1  : nrow(x))
{
  for (i in 1 :ncol(esetMatT))
  {
    if(temp[i]==x[y] & y == 1)
    {selectedMat<-as.matrix(esetMatT[,i])
    break} 
    if(temp[i]==x[y] & y != 1)
    {selectedMat<-cbind(selectedMat,esetMatT[,i]) 
    break}
  }
}
#we put the names of the genes to the columns
colnames(selectedMat)<-t(x[,1]);

#we create and train out SVM model

control<-trainControl(method = "repeatedcv",number = 10, repeats = 3)
modelSvm<-train(x=selectedMat,y=genoData ,method="svmLinearWeights",trControl = control)

#predict data for GDS4056
pr1<- predict(modelSvm, newdata = esetMat2T) 

#predict data for GDS3716
pr2 <- predict(modelSvm, newdata = esetMat3T)

#####Confusion matrix + accuracy finder function#####
#creates a matrix "cm1" containing the confusion matrix
#and a variable "accuracy" containing the accuracy
CMAdjusted(pr4056,genoData2)
View(cm1)
View(accuracy)
CMAdjusted(pr2,genoData3)
View(cm1)
View(accuracy)


####################################################################################
####################### Kmeans and rerun the code above ############################




