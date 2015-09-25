library(deepnet)
library(caret)

#Implement a simple neural network using nnet package
# Author: Roshan K R

# Read from Dataset
dataset<-read.csv('D:/Others/R/data/Cars_Changed.csv',header=T)

#change the input features (X) and actual output (Y) to numerical values 
dataset$buyingprice <-lapply(dataset$buyingprice, as.numeric)
dataset$mainprice <-lapply(dataset$mainprice, as.numeric)
dataset$percapacity <-lapply(dataset$percapacity, as.numeric)
dataset$lug_boot <-lapply(dataset$lug_boot, as.numeric)

#After loading the library, we will divide the population in two sets:
#Training and validation.
train <- createDataPartition(y=dataset$Safety,p=0.7,list=FALSE)

training <- dataset[train,]
training$Safety <-lapply(training$Safety, as.numeric)

Validation <- dataset[-train,]
Validation$Safety <-lapply(Validation$Safety, as.numeric)

#Change it to a numeric matrix
training <- matrix(as.numeric(unlist(training)),nrow=nrow(training))
Validation <- matrix(as.numeric(unlist(Validation)),nrow=nrow(Validation))

#Normalize the value to be predicted , use that attribute of the 
#dataset , that you want to predict
Y <- class.ind(training[,7])

#Train the model using nnet
modfit = nn.train(training[,-7], Y, hidden=c(10),learningrate=0.5, numepochs = 100)


#test1 <- nn.test(modfit, Validation[,-7], class.ind(Validation[,7]), t = 0.5)
#print(test1)

# print(table(Pred_Trn,training$Safety))
testfit = nn.predict(modfit,Validation[,-7])

print(round(testfit))
#print(class.ind(training[,7]))
# print(sum(round(testfit[,1])))
# print(sum(round(testfit[,2])))
# print(sum(round(testfit[,3])))
# print(sum(round(testfit[,4])))
# 
# print(sum(round(Y[,1])))
# print(sum(round(Y[,2])))
# print(sum(round(Y[,3])))
# print(sum(round(Y[,4])))



