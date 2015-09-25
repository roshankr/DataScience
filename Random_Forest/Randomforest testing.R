library(randomForest)
library(randomForestSRC)
library(caret)
library(datasets)
 

data(iris)
iris.rf <- randomForest(iris[,-5], iris[,5], ntree=1000,prox=TRUE)
iris.p <- classCenter(iris[,-5], iris[,5], iris.rf$prox)
print(iris.rf)

# iris.rf <- randomForest(Species ~ ., iris, proximity=TRUE,
#                         keep.forest=FALSE)
# MDSplot(iris.rf, iris$Species)
# ## Using different symbols for the classes:
# MDSplot(iris.rf, iris$Species, palette=rep(1, 3), pch=as.numeric(iris$Species))