library(deepnet)
library(caret)

#Implement a simple neural network using nnet package
# Author: Roshan K R

# Read from Dataset
dataset<-read.csv('D:/Others/R/data/adult.csv',header=T)

#change the input features (X) and actual output (Y) to numerical values 
dataset$workclass <-lapply(dataset$workclass, as.numeric)
dataset$education <-lapply(dataset$education, as.numeric)
dataset$marital_status <-lapply(dataset$marital_status, as.numeric)
dataset$occupation <-lapply(dataset$occupation, as.numeric)
dataset$relationship <-lapply(dataset$relationship, as.numeric)
dataset$race <-lapply(dataset$race, as.numeric)
dataset$sex <-lapply(dataset$sex, as.numeric)
dataset$native_country <-lapply(dataset$native_country, as.numeric)

#After loading the library, we will divide the population in two sets:
#Training and validation.
train <- createDataPartition(y=dataset$Class,p=0.7,list=FALSE)

training <- dataset[train,]
training$Class <-lapply(training$Class, as.numeric)

Validation <- dataset[-train,]
Validation$Class <-lapply(Validation$Class, as.numeric)

#Change it to a numeric matrix
training <- matrix(as.numeric(unlist(training)),nrow=nrow(training))
Validation <- matrix(as.numeric(unlist(Validation)),nrow=nrow(Validation))

#Normalize the value to be predicted , use that attribute of the 
#dataset , that you want to predict
Y <- matrix(training[,15],nrow(X),1)

#Train the model using nnet
modfit = nn.train(training[,-15], Y, hidden=c(50),learningrate=0.01, numepochs = 10)

#test1 <- nn.test(modfit, Validation[,-7], class.ind(Validation[,7]), t = 0.5)
#print(test1)

# print(table(Pred_Trn,training$Class))
testfit = nn.predict(modfit,Validation[,-15])
print(testfit)
# print(Validation[1:100,15])
# print(round(testfit[1:100,]))



