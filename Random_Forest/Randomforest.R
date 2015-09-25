library(randomForest)
library(randomForestSRC)
library(caret)
library(datasets)

#Implementation of Random Forest algorithm using bagging and
#random subspace methods. Another key concept is Out-of-bag error (OOB)

# look at the dataset
data(iris)
#assignment <-write.csv(iris,"iris.csv")
# visually look at the dataset
#qplot(Petal.Length,Petal.Width,colour=Species,data=iris)

#After loading the library, we will divide the population in two sets:
#Training and validation.

train <- createDataPartition(y=iris$Species,p=0.5,list=FALSE)
training <- iris[train,]
Validation <- iris[-train,]

#You can use the following code to generate a random forest model on the training dataset.

#modfit <- train(Species~.,method="rf",data=training) 

modfit <- randomForest(training[,-5], training[,5], ntree=10000,prox=TRUE)
pred <- predict(modfit,training)
print(table(pred,training$Species))

#Having built such an accurate model, we will like to make sure that we are not over fitting the model on the training data. This is done by validating the same model on an independent data set. We use the following code to do the same :
pred.val<-predict(modfit,newdata=Validation)
print(table(pred.val,Validation$Species))

# predict a single val 
#ValFinal = Validation[42,]
#pred.val<-predict(modfit,newdata=ValFinal)
#print(table(pred.val,ValFinal$Species))
    
    