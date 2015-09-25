library(randomForest)
library(randomForestSRC)
library(caret)
library(datasets)

#Implementation of Random Forest algorithm using bagging and
#random subspace methods. Another key concept is Out-of-bag error (OOB)
#Adult data to predcit salary <= 50k or > 50K

# Read from Dataset
dataset <-read.csv('D:/Others/R/data/adult.csv',header=T)

#After loading the library, we will divide the population in two sets:
#Training and validation.

train <- createDataPartition(y=dataset$Class,p=0.7,list=FALSE)

training <- dataset[train,]
Validation <- dataset[-train,]
training <- training[1:2000,]

#You can use the following code to generate a random forest model on the training dataset.
#modfit <- train(Species~.,method="rf",data=training) 
modfit <- randomForest(training[,-15], training[,15], ntree=100,prox=TRUE)

pred <- predict(modfit,training)

for (i in 1:nrow(training))
{    
    if (pred[i] >= 0.5)
    {
        
        pred[i]= 1
    }else
    {
        pred[i]= 0
    }
}

print(table(pred,training$Class))

#Having built such an accurate model, we will like to make sure that we are not over fitting the model on the training data. This is done by validating the same model on an independent data set. We use the following code to do the same :
Validation <- Validation[1:2000,]
pred.val<-predict(modfit,newdata=Validation)
for (i in 1:nrow(Validation))
{    
    if (pred.val[i] >= 0.5)
    {
        
        pred.val[i]= 1
    }else
    {
        pred.val[i]= 0
    }
}
print(table(pred.val,Validation$Class))

# predict a single val 
#ValFinal = Validation[42,]
#pred.val<-predict(modfit,newdata=ValFinal)
#print(table(pred.val,ValFinal$Species))
    
    