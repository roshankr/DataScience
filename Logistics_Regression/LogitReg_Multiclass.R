library(LogicReg)
library(caret)
library(datasets)

#Logistics Regression - multi class (more than 2 output) using one-vs-
#All model
#Pending Item - Need to find out a way to display decision boundary
#Author - Roshan K R

# look at the dataset
data(iris)


#use the whole dataset as training set
training <- iris[,]


#visually look at the dataset
#print(qplot(Petal.Length,Petal.Width,colour=Species,data=training))

#Predictor variables
X <- as.matrix(iris[,-5])

#Add ones to X for theta0
X <- cbind(rep(1,nrow(X)),X)

#change the degree of X to 3 as linear equation not solving 2nd category "versicolor" 
X <- cbind(X,iris[,1]^2,iris[,2]^2,iris[,3]^2,iris[,4]^2,iris[,1]^3,iris[,2]^3,iris[,3]^3,iris[,4]^3)

# Get unique Y values (since i have more than 2 predictions)
distinctspecies <- unique(iris[,5], incomparables = FALSE)

#Initialize theta and hypotheis 
theta <- matrix(rep(0,ncol(X)*length(distinctspecies)),nrow=ncol(X),ncol=length(distinctspecies),byrow=TRUE)
hypothesis <- matrix(rep(0,nrow(X)*length(distinctspecies)),nrow=nrow(X),ncol=length(distinctspecies),byrow=TRUE)

#Initialize function for setting Y value as 1 or 0
Initialize <- function(SpeciesVal)
{
    
  #Response variable
  Y <- as.matrix(rep(0,nrow(X)))

  #changin multiclass to 1 and 0
  for (i in 1:nrow(X))
  {    
      if (iris$Species[i] == SpeciesVal)
      {
        
        Y[i] <- 1
      }else
      {
        Y[i] <- 0
      }
  }
  return(Y)
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
#################################################################
#Run LogitReg as one vs rest for all 3 specis 
#
for (j in 1:length(distinctspecies))
{ 
  SpeciesVal <- distinctspecies[j]
  
  #Intial theta
  initial_theta <- as.matrix(rep(0,ncol(X)))
  
  #get Y value for 1 to n cases
  Y=Initialize(SpeciesVal)

  # Derive theta using gradient descent using optim function
  theta_optim <- optim(par=initial_theta,fn=cost)

  #set theta to a matrix
  theta[1:ncol(X),j] <- theta_optim$par
  
  #calaculate hypothesis and move to a matrix
  hypothesis[1:nrow(X),j] <- sigmoid(X%*%theta[1:ncol(X),j])
}

# Get max(hypotheis value)
hypothesismax <- apply(hypothesis, 1, max)

#check the max hypthesis values and assign 1 if g(z)>= 0.5 , else 0
for (i in 1:nrow(X))
{    
    if (hypothesismax[i] >= 0.5)
    {
        
        hypothesismax[i] <- 1
    }else
    {
        hypothesismax[i] <- 0
    }
}

#qplot(training$Sepal.Length*training$Sepal.Width,training$Petal.Length*training$Petal.Width,colour=hypothesismax,label=hypothesismax,size=1, geom="text")

###################################################################33
#training Example

Z <- matrix(c(1,5.7,4.3,1.5,0.3,5.7^2,4.3^2,1.5^2,0.3^2,5.7^3,4.3^3,1.5^3,0.3^3),nrow=1,ncol=ncol(X),byrow=TRUE)
#Z <- matrix(c(1,6.7,3.0,4.3,1.3,6.7^2,3.0^2,4.3^2,1.3^2,6.7^3,3.0^3,4.3^3,1.3^3),nrow=1,ncol=ncol(X),byrow=TRUE)
#Z <- matrix(c(1,6.4,3.0,5.1,2.5,6.4^2,3.0^2,5.1^2,2.5^2,6.4^3,3.0^3,5.1^3,2.5^3),nrow=1,ncol=ncol(X),byrow=TRUE)

#check all 3 category output and consider the max 
hypothesistrain <- sigmoid(Z%*%theta[1:ncol(X),1])
print(hypothesistrain)
hypothesistrain <- sigmoid(Z%*%theta[1:ncol(X),2])
print(hypothesistrain)
hypothesistrain <- sigmoid(Z%*%theta[1:ncol(X),3])
print(hypothesistrain)


#plot the x input and latest centroid
cols <- c("tan2", "purple3", "grey80","black")
data.df <- data.frame(cent=c(iris$Species),axis1=c(iris$Petal.Length),axis2=c(iris$Petal.Width))
data.ds <- data.frame(axis3=c(1.5),axis4=c(0.3))

print(ggplot() + 
          geom_point(data=data.df,aes(x=axis1,y=axis2,
                                      colour=factor(cent),fill=factor(cent)),
                     shape=21,size=4)                        +
          scale_fill_manual(values = cols)                   +
          scale_colour_manual(values = cols)                 +
          geom_point(data=data.ds,aes(x=axis3,y=axis4,
                                      colour='centroid',fill='centroid'),
                     shape=21,size=4) )                   


