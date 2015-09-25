library(nnet)

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
Validation <- dataset[-train,]

#Normalize the value to be predicted , use that attribute of the 
#dataset , that you want to predict
Y <- class.ind(training$Safety)

#Train the model using nnet
modfit = nnet(training[,-7], Y, size=20, maxit=2000,censored=TRUE) 

#dataset$Safety <-lapply(dataset$Safety, as.numeric)
Pred_Trn <- predict(modfit,training[,-7],type="class")

print(table(Pred_Trn,training$Safety))

#Having built such an accurate model, we will like to make sure that we are not over fitting the model on the training data. This is done by validating the same model on an independent data set. We use the following code to do the same :

Pred_Val <- predict(modfit,Validation[,-7],type="class")
print(table(Pred_Val,Validation$Safety))

