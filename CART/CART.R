library(rpart)
library(caret)
library(rattle)

#CART - Classificaiton and Regression using rpart library
#Author - Roshan K R

data(iris)
# look at the dataset
#print(iris)
# visually look at the dataset
#qplot(Petal.Length,Petal.Width,colour=Species,data=iris)

#After loading the library, we will divide the population in two sets:
#Training and validation.

train <- createDataPartition(y=iris$Species,p=0.5,list=FALSE)
training <- iris[train,]
Validation <- iris[-train,]

# Once we have the two data sets and have got a basic understanding 
# of data, we now build a CART model. We have used "caret" and "rpart"
# package to build this model. However, the traditional representation
# of the CART model is not graphically appealing on R. Hence, we have 
# used a package called "rattle" to make this decision tree. "Rattle" 
# builds a more fancy and clean trees, which can be easily interpreted. 
# Use the following code to build a tree and graphically check this 
# tree:


modfit <- train(Species~.,method="rpart",data=training) 
#fancyRpartPlot(modfit$finalModel)
print(modfit)

# Now, we need to check the predictive power of the CART model, we just
# built. Here, we are looking at a discordance rate (which is the number
# of misclassifications in the tree) as the decision criteria. We use 
# the following code to do the same :

train.cart<-predict(modfit,newdata=training)
print(table(train.cart,training$Species))

#Only 3 misclassified observations out of 75, signifies good predictive power. In general, a model with misclassification rate less than 30% is considered to be a good model. But, the range of a good model depends on the industry and the nature of the problem. Once we have built the model, we will validate the same on a separate data set. This is done to make sure that we are not over fitting the model. In case we do over fit the model, validation will show a sharp decline in the predictive power. It is also recommended to do an out of time validation of the model. This will make sure that our model is not time dependent. For instance, a model built in festive time, might not hold in regular time. For simplicity, we will only do an in-time validation of the model. We use the following code to do an in-time validation:
    
pred.cart<-predict(modfit,newdata=Validation)
print(table(pred.cart,Validation$Species))


#As we see from the above calculations that the predictive power decreased in validation as compared to training. This is generally true in most cases. The reason being, the model is trained on the training data set, and just overlaid on validation training set. But, it hardly matters, if the predictive power of validation is lesser or better than training. What we need to check is that they are close enough. In this case, we do see the misclassification rate to be really close to each other. Hence, we see a stable CART model in this case study.
#Let's now try to visualize the cases for which the prediction went wrong. Following is the code we use to find the same :
    
correct <- pred.cart == Validation$Species
qplot(Petal.Length,Petal.Width,colour=correct,data=Validation)

# predict a single val
ValFinal = Validation[1,]
pred.val<-predict(modfit,newdata=ValFinal)
print(pred.val)    
    
    
    
    
    
    