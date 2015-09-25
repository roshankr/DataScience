library(nnet)

#Implement a simple neural network using nnet package
# Author: Roshan K R

# Read from Dataset
dataset<-read.csv('D:/Others/R/data/letter-recognition.csv',header=T)

#After loading the library, we will divide the population in two sets:
#Training and validation.
train <- createDataPartition(y=dataset$capletter,p=0.8,list=FALSE)

training <- dataset[train,]
Validation <- dataset[-train,]

#Normalize the value to be predicted , use that attribute of the 
#dataset , that you want to predict
Y <- class.ind(training$capletter)

#Train the model using nnet
#modfit = nnet(training[,-17], Y, size=20, maxit=2000,censored=TRUE) 

#dataset$capletter <-lapply(dataset$capletter, as.numeric)
Pred_Trn <- predict(modfit,training[,-17],type="class")
print(table(Pred_Trn,training$capletter))

Pred_Match <- 0
Pred_UnMatch <- 0
for (i in 1:nrow(training))
{   
    if (Pred_Trn[i] == training$capletter[i])
    {
        Pred_Match <- Pred_Match + 1
    }else
    {
        Pred_UnMatch <- Pred_UnMatch + 1
    }
}

print ("Training final count")
print (Pred_Match)
print (Pred_UnMatch)
print ((Pred_Match*100)/(Pred_Match+Pred_UnMatch))
print ((Pred_UnMatch*100)/(Pred_Match+Pred_UnMatch))

#Having built such an accurate model, we will like to make sure that we are not over fitting the model on the training data. This is done by validating the same model on an independent data set. We use the following code to do the same :

Pred_Val <- predict(modfit,Validation[,-17],type="class")
print(table(Pred_Val,Validation$capletter))

Pred_Match <- 0
Pred_UnMatch <- 0
for (i in 1:nrow(Validation))
{   
    if (Pred_Val[i] == Validation$capletter[i])
    {
        Pred_Match <- Pred_Match + 1
    }else
    {
        Pred_UnMatch <- Pred_UnMatch + 1
    }
}

print ("Validation final count")
print (Pred_Match)
print (Pred_UnMatch)
print ((Pred_Match*100)/(Pred_Match+Pred_UnMatch))
print ((Pred_UnMatch*100)/(Pred_Match+Pred_UnMatch))
