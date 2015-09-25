library(nnet)

#Implement a simple neural network using nnet package
# Author: Roshan K R

# Read from Dataset
seeds<-read.csv('D:/Others/R/data/seeds_dataset.csv',header=T)

#Setting training set index ,  210 is the dataset size, 147 is 70 % of that 
seedstrain<- sample(1:210,147)

#Setting test set index
seedstest <- setdiff(1:210,seedstrain)

#Normalize the value to be predicted , use that attribute of the 
#dataset , that you want to predict
Y <- class.ind(seeds$Class)

#Train the model using nnet
#seedsANN = nnet(seeds[seedstrain,-8], Y[seedstrain,], size=10, maxit=5000, softmax=TRUE)    
seedsANN = nnet(seeds[seedstrain,-8], Y[seedstrain,], size=20, maxit=1000,censored=TRUE)    

#Normalize the predicted output and compare with Y
trainoutput <- predict(seedsANN, seeds[seedstrain,-8], type="class")
print(table(trainoutput,seeds[seedstrain,]$Class))

#Calculate Classification accuracy using test variables
testoutput <- predict(seedsANN, seeds[seedstest,-8], type="class")
print(table(testoutput,seeds[seedstest,]$Class))
