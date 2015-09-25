library(caret)
library(datasets)
library(nnet)
library(ggplot2)

################################################################
#K Means Clustering                                            #
#Author - Roshan K R                                           #
################################################################

# Read from Dataset
dataset<-read.csv('D:/Others/ML/data/bank-data2.csv',header=T)

# Move dataset to X and do feature scaling
X <- dataset
X[,2] <- X[,2] / 1000

# Assign random initial values for K - cluster centroids
K <- X[sample(nrow(X),size=5,replace=TRUE),]
numclusters <- nrow(K)

Totiter <- 30

Finalerr <- matrix(rep(0,Totiter) , Totiter, 1)

for (iter in 1:Totiter)
{   
   
    # Initialize diff vectors / matrices
    KcentTot <- matrix(rep(0,numclusters*2) , numclusters, 2)
    KcentCnt <- matrix(rep(0,numclusters*1) , numclusters, 1)
    KcentErr <- KcentCnt
    C <- matrix(rep(0,nrow(X)) , nrow(X), 1)
    Err <- matrix(rep(0,nrow(X)) , nrow(X), 1)
    #start loop for number of training examples (i)
    for (i in 1:nrow(X))
    {   

        Errorrate <- 10000000
        
        # for each traing sample, check it is near to which centroid
        for (j in 1:numclusters)
        {
            Y <- sqrt((sum( X[i,] - K[j,] ))^ 2)
            if(Errorrate > Y)
            {
                Errorrate <- Y
                C[i] <- j
                Err[i] <- Y
            }
        }
        
        # once the centrod is identified move it to a vector , also keep the 
        # error values 
        # KcentTot = total value of features (x1,x2 etc) which is nearer to a
        # centroid, KcentCnt is total samples for that centrod and KcentErr is 
        # its error value (erro should be minimum)
        
        for (l in 1:numclusters)  
        {
            if (C[i] == l)
            {
                KcentTot[l,1] <- KcentTot[l,1] + X[i,1]
                KcentTot[l,2] <- KcentTot[l,2] + X[i,2]
                KcentCnt[l]  <- KcentCnt[l]  + 1
                KcentErr[l]  <- KcentErr[l]  + Err[i]               
            }

        }

    }
    
    # calculate the new centroid by getting the K-mean
    for (newcen in 1:numclusters)
    {
        K[newcen,] <- KcentTot[newcen,]/ KcentCnt[newcen]
        Finalerr[iter] <- Finalerr[iter] + KcentErr[newcen]
    }
   
}

#plot the x input and latest centroid
cols <- c("tan2", "purple3", "grey80","pink","cyan2","black")
data.df <- data.frame(cent=c(C),axis1=c(X[,1]),axis2=c(X[,2]))
data.ds <- data.frame(axis3=c(K[,1]),axis4=c(K[,2]))

print(ggplot() + 
       geom_point(data=data.df,aes(x=axis1,y=axis2,
                  colour=factor(cent),fill=factor(cent)),
                  shape=21,size=4)                        +
       scale_fill_manual(values = cols)                   +
       scale_colour_manual(values = cols)                 +
       geom_point(data=data.ds,aes(x=axis3,y=axis4,
                  colour='centroid',fill='centroid'),
                  shape=21,size=4)                        +
       xlim(0,70) + ylim(0,70))

       


# p <-ggplot()
# 
# for (newcen in 1:nrow(X))
# {
#   if (C[newcen] == 1)
#   {
#     p <-  p + geom_point(data=X[i,],aes(x=X[i,1],y=X[i,2]),colour = qsec,size=2)
#   }else if (C[newcen] == 2)
#   {
#     p <-  p + geom_point(data=X[i,],aes(x=X[i,1],y=X[i,2]),colour = "red",size=2)
#   } else if (C[newcen] == 3)
#   {
#     p <-  p + geom_point(data=X[i,],aes(x=X[i,1],y=X[i,2]),colour = "blue",size=2)
#   }else if (C[newcen] == 4)
#   {
#     p <-  p + geom_point(data=X[i,],aes(x=X[i,1],y=X[i,2]),colour = "black",size=2)
#   }else
#   {
#     p <-  p + geom_point(data=X[i,],aes(x=X[i,1],y=X[i,2]),colour = "pink",size=2)
#   }
#   
                                                               





