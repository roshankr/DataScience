library(randomForest)
library(randomForestSRC)
library(caret)
library(datasets)

#Implementation of Random Forest algorithm using bagging and
#random subspace methods. Another key concept is Out-of-bag error (OOB)
#Car data accepatble or unacceptable

# Read from Dataset
dataset <-read.csv('D:/Others/R/data/Cars.csv',header=T)

#After loading the library, we will divide the population in two sets:
#Training and validation.

train <- createDataPartition(y=dataset$Safety,p=0.7,list=FALSE)

training <- dataset[train,]
Validation <- dataset[-train,]

#You can use the following code to generate a random forest model on the training dataset.
#modfit <- train(Species~.,method="rf",data=training) 
modfit <- randomForest(training[,-7], training[,7], ntree=1000,prox=TRUE)

Pred_Trn <- predict(modfit,training)
print(table(Pred_Trn,training$Safety))

#Having built such an accurate model, we will like to make sure that we are not over fitting the model on the training data. This is done by validating the same model on an independent data set. We use the following code to do the same :

Pred_Val <- predict(modfit,Validation)
print(table(Pred_Val,Validation$Safety))
    