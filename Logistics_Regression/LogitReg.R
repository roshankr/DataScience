library(LogicReg)
library(caret)
library(datasets)

#Logistics Regression - with two outputs (1 or 0)
#Pending Item - Need to find out a way to display decision boundary
#Author - Roshan K R

# look at the dataset
data(iris)

#After loading the library, we will divide the population in two sets:
#Training and validation.so
#train <- createDataPartition(y=iris$Species,p=0.5,list=FALSE)
#training <- iris[train,]
#Validation <- iris[-train,]

#using only two outputs
training <- iris[51:150,]
#print(attributes(training)
#print(training)
#visually look at the dataset
#qplot(Petal.Length,Petal.Width,colour=Species,data=training)

#Predictor variables
X <- as.matrix(training[,-5])

#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable

Y <- as.matrix(rep(0,nrow(X)))

for (i in 1:nrow(X))
{    
    if (training$Species[i] == "versicolor")
    {
        
        Y[i] <- 1
    }else
    {
        Y[i] <- 0
    }
}

#Sigmoid function
sigmoid <- function(z)
{
    g <- 1/(1+exp(-z))
    return(g)
}

#Cost Function
cost <- function(theta)
{
    m <- nrow(X)
    g <- sigmoid(X%*%theta)
    J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
    return(J)
}

#Intial theta
initial_theta <- as.matrix(rep(0,ncol(X)))

#Cost at inital theta
cost1 <-cost(initial_theta)

# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta

hypothesis1 <- sigmoid(X%*%theta)
hypothesis <- hypothesis1
# check the hypthesis values  

for (i in 1:nrow(X))
{    
    if (hypothesis1[i] >= 0.5)
    {
        
        hypothesis[i] <- 1
    }else
    {
        hypothesis[i] <- 0
    }
}

# identify mismatches 

for (i in 1:nrow(X))
{    
    if (Y[i] != hypothesis[i])
    {
        
        hypothesis[i] <- hypothesis[i] + 2
    }
}

print(hypothesis)
#print(table(hypothesis,Y))
qplot(training$Sepal.Length*training$Sepal.Width,training$Petal.Length*training$Petal.Width,colour=hypothesis,label=hypothesis,size=1, geom="text")
#qplot(training$Sepal.Length*training$Sepal.Width,training$Petal.Length[34]*training$Petal.Width[34],colour=2)
#qplot(training$Sepal.Length[84]*training$Sepal.Width[84],training$Petal.Length[84]*training$Petal.Width[84],colour=3)