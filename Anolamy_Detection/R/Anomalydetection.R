library(caret)
library(datasets)
library(nnet)
library(ggplot2)

################################################################
#Anomaly Detection                                             #
#Example - BC_benign_malignant P(x) < epsilon                  #
# 2- benign , 4 - malignant                                    #
# It uses Gaussion distribution                                #
#Author - Roshan K R                                           #
################################################################

# Read the training data set, with all positive values
dataset<-read.csv('D:/Others/ML/data/BC_benign.csv',header=T)

# Move dataset to X and do feature scaling
X_Train <- dataset[,2:10]

# Read the CV and Test  data set, with  positive and negative values
dataset<-read.csv('D:/Others/ML/data/BC_benign_malignant.csv',header=T)

CV_T <- createDataPartition(y=dataset$Class,p=0.6,list=FALSE)
X_CV_Full <- dataset[CV_T,]
X_Test_Full <- dataset[-CV_T,]

X_CV <- X_CV_Full[,1:11]
X_Test <- X_Test_Full[,1:11]


################################################################
#Calculate Mean                                                #
################################################################

Meantrain <- function(Train)
{
    Meantrain_Val <- matrix(rep(0,ncol(Train)) , 1, ncol(Train))
    
    for (i in 1:ncol(Train))
    {
        Meantrain_Val [i] <- sum(Train[,i]) / nrow(Train)
    }
    return(Meantrain_Val)
}

################################################################
#Calculate the Variance                                        #
################################################################

Variancetrain <- function(Train,Meantrain_Val)
{
    Train_Temp <- matrix(rep(0,nrow(Train)*ncol(Train)) , nrow(Train), ncol(Train))
    Variancetrain_Val <- matrix(rep(0,ncol(Train)) , 1, ncol(Train))
    
    for (i in 1:nrow(Train))
    {
        Train_Temp [i,] <- (Train[i,] - Meantrain_Val) ^2
    }

    Variancetrain_Val <- Meantrain(Train_Temp)
     
    return(Variancetrain_Val)
}

################################################################
#Calculate the Probability                                     #
################################################################

Calc_Prob_CV <- function(DS,Meantrain_Val,Variancetrain_Val)
{
    Prob_X_CV <- matrix(rep(0,nrow(DS)) , nrow(DS), 1)
    DS_Temp <- matrix(rep(0,nrow(DS)*ncol(DS)) , nrow(DS), ncol(DS))
    
    Prob_Const <- 1/((2*3.14*Variancetrain_Val)^0.5)
    
    for (i in 1:nrow(DS))
    {
        DS_Temp [i,] <-  Prob_Const *(exp(-(((DS[i,] - Meantrain_Val) ^2)/(2*Variancetrain_Val))))
        Prob_X_CV [i] <- prod(DS_Temp [i,])
    }
    
    
    return(Prob_X_CV)
}

################################################################
#Calculate the probability of CV and Test datas                #
################################################################

Check_CV_Test <- function(Prob_output,CV_Test_Input,Epsilon)
{
    Epsilon <- 0.0000007836733
    New_CV <- matrix(rep(0,nrow(CV_Test_Input)*4) , nrow(CV_Test_Input), 4)
    
    New_CV[,1] <- Prob_output
    New_CV[,2] <- CV_Test_Input[,11]
    New_CV[,3] <- CV_Test_Input[,1]
   
    
    for (i in 1:nrow(New_CV))
    {
        if (New_CV[i,1] < Epsilon)
        {
            New_CV[i,4] <- 4
        }else
        {
            New_CV[i,4] <- 2
        }
    }
    return(New_CV)
}

################################################################
#Main prgram starts here                                       #
################################################################

Meantrain_Val <- Meantrain(X_Train)

Variancetrain_Val <- Variancetrain(X_Train,Meantrain_Val)

################################################################
#check the cross validation set                                #
################################################################

Prob_X_CV <- Calc_Prob_CV(X_CV[,2:10],Meantrain_Val,Variancetrain_Val)

New_CV <- Check_CV_Test(Prob_X_CV,X_CV,Epsilon)

print(New_CV[order(New_CV[,1]),])

#print(table(New_CV[,4],New_CV[,2]))

################################################################
#check the test set                                            #
################################################################

Prob_X_Test <- Calc_Prob_CV(X_Test[,2:10],Meantrain_Val,Variancetrain_Val)

New_CV <- Check_CV_Test(Prob_X_Test,X_Test,Epsilon)

#print(New_CV[order(New_CV[,1]),])

#print(table(New_CV[,4],New_CV[,2]))
