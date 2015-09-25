library(deepnet)
library(caret)
options(max.print=1000000) 

#Implement a simple neural network using nnet package
# Author: Roshan K R

# Read from Dataset
dataset<-read.csv('D:/Others/R/data/Cars_quadratic.csv',header=T)

#change the input features (X) and actual output (Y) to numerical values 
# dataset$buyingprice <-lapply(dataset$buyingprice, as.numeric)
# dataset$mainprice <-lapply(dataset$mainprice, as.numeric)
# dataset$percapacity <-lapply(dataset$percapacity, as.numeric)
# dataset$lug_boot <-lapply(dataset$lug_boot, as.numeric)

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
Y <- class.ind(training[,13])

#Train the model using nnet
modfit = nn.train(training[,-13], Y, hidden=c(20),learningrate=0.9, numepochs = 100)

#test1 <- nn.test(modfit, Validation[,-7], class.ind(Validation[,7]), t = 0.5)
#print(test1)

# print(table(Pred_Trn,training$Safety))
testfit = nn.predict(modfit,Validation[,-13])

#print(round(testfit))

z <- class.ind(Validation[,13]) 
z<-matrix(z,nrow(z),ncol(z))

checksuccess <- apply(round(testfit)==z,1,all) 
print(table(checksuccess)["TRUE"])
print(table(checksuccess)["FALSE"])
print ( (table(checksuccess)["TRUE"])*100/((table(checksuccess)["TRUE"]) + (table(checksuccess)["FALSE"]))    )


