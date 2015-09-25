library(caret)
library(datasets)
library(nnet)
options(max.print=1000000) 

################################################################
#Neural Network with 1 hidden unit                             #
#Author - Roshan K R                                           #
################################################################

# Read from Dataset
dataset<-read.csv('D:/Others/ML/data/Cars_quadratic.csv',header=T)

#change the input features (X) and actual output (Y) to numerical values 
# dataset$buyingprice <-lapply(dataset$buyingprice, as.numeric)
# dataset$mainprice <-lapply(dataset$mainprice, as.numeric)
# dataset$percapacity <-lapply(dataset$percapacity, as.numeric)
# dataset$lug_boot <-lapply(dataset$lug_boot, as.numeric)
# dataset$COMFORT <-lapply(dataset$COMFORT, as.numeric)
# dataset$doors <-lapply(dataset$doors, as.numeric)

#After loading the library, we will divide the population in two sets:
#Training and validation.
train <- createDataPartition(y=dataset$Safety,p=0.7,list=FALSE)
training <- dataset[train,]
Validation <- dataset[-train,]

#Predictor variables add 1 as first col , change it a matrix
X <- as.matrix(training[,-13])
X <- cbind(rep(1,nrow(X)),X)
X <- matrix(as.numeric(unlist(X)),nrow=nrow(X))

#Normalize the value to be predicted , use that attribute of the 
#dataset , that you want to predict
Y <- class.ind(training$Safety)

# Set the hidden unit value , total iterations
hiddenunits <- 20 + 1 # 1 for biased unit
Totaliter <- 1000

# Initialize Cost function matrix. Set is as global variable
CostVal <<- matrix(rep(0,Totaliter) , Totaliter,1)

#Setting the input layer as the a1 activation
a1<-X


################################################################
#Sigmoid function                                              #
################################################################
sigmoid <- function(z)
{
    g <- 1/(1+exp(-z))
    return(g)
}

################################################################
#Initialize Theta function                                     #        
################################################################
Inittheta <- function()
{   
    Theta1 <- matrix(runif(((ncol(X))*hiddenunits), -0.1, 0.1) , (ncol(X)), hiddenunits)
    Theta2 <- matrix(runif((hiddenunits*ncol(Y)), -0.1, 0.1) , hiddenunits,ncol(Y) )
    initial_theta <- c(as.vector(Theta1),as.vector(Theta2))
    return(initial_theta)
}

################################################################
#NeuralNetwork main function that performs                     #
# a) Forward Propagation for h(x)                              #
# b) Back Propagation for partial derivatives                  #
# c) Calculate the Cost function                               #
################################################################
NeuralNet <- function(initial_theta,iter)
{    

    #Initialize activation and Partial_Der matrices
    
    a2 <- matrix(rep(0,nrow(X)*hiddenunits) , (nrow(X)), hiddenunits)
    Partial_der_a2 <- matrix(rep(0,nrow(X)*hiddenunits) , (nrow(X)), hiddenunits)
    
    a3 <- matrix(rep(0,nrow(X)*ncol(Y)) , (nrow(X)), ncol(Y))
    Partial_der_a3 <- matrix(rep(0,nrow(X)*ncol(Y)) , (nrow(X)), ncol(Y))
    
    Gradient1 <- matrix(rep(0,ncol(X)*hiddenunits),ncol(X), hiddenunits)
    Gradient2 <- matrix(rep(0,ncol(Y)*hiddenunits),hiddenunits, ncol(Y))
    J<-0
    
    # Move initial_theta to Theta1 and Theta2
    Theta1 <- matrix(initial_theta[1:((ncol(X))*hiddenunits)] , (ncol(X)), hiddenunits)
    Theta2 <- matrix(initial_theta[(((ncol(X))*hiddenunits)+1):(((ncol(X))*hiddenunits)+(ncol(Y)*hiddenunits))] , hiddenunits,ncol(Y) )
    
    
    for (i in 1:nrow(X))
    {  
       ###Forward propagation#######
       #Calculate the activation for hidden unit 
       a2[i,]<- sigmoid(X[i,] %*% (Theta1))
       
       # add ao hidden neuron value as 1 
       a2[i,1] <- 1
       
       #Calculate the activation for output unit 
       a3[i,] <- sigmoid(a2[i,] %*% (Theta2)) 
       
       # Calculate the cost of the ith sample
       
       Y_output <- matrix(Y[i,],ncol(Y),1)
       
       J <- J + (( (log(a3[i,])) %*% Y_output ) +  ((log(1-a3[i,])) %*% (1-Y_output))  )
       
       ###Back propagation#######
       # Ge the output nodes error details (3rd layer) (1 X 4)
       Partial_der_a3[i,] <- a3[i,] - Y[i,]
              
       # Ge the hidden nodes error details (2nd layer) (1 X 7)
       Partial_der_a2[i,] <- (Partial_der_a3[i,] %*% t(Theta2))*((a2[i,]*(1-a2[i,])))       
              
       # Getting the sum of partial deratives (it shud be of same
       # size as that of Theta2 and Theta1 )
       
       Partial_a3 <- matrix((Partial_der_a3[i,]),1,ncol(Y))
       Partial_a2 <- matrix((Partial_der_a2[i,]),1,hiddenunits)
       new_a1 <- matrix(a1[i,],ncol(X),1)
       
       Gradient2 <- Gradient2 + (a2[i,] %*% Partial_a3 )

       Gradient1 <- Gradient1 + (new_a1 %*% Partial_a2 )
       
    }
     
     #lambda is a regularization parameter. Weight decay is an 
     #additional term in the weight update rule that causes the 
     #weights to exponentially decay to zero
     lambda <- 0.75
     
     #learning rate is used for calcualting the new theta.The 
     #learning rate is a parameter that determines how much an 
     #updating step influences the current value of the weights
     learningrate <- 0.9
     momentum <- 0.5
    
     m <- nrow(X)
     
     #Get final cost function including regularization
     CostVal[iter] <<- ((-1/m)*J) + ((lambda/(2*m))*(sum(initial_theta*initial_theta)))

     # Get new Theta values 
     Theta1 <- Theta1 - ( (learningrate/nrow(X))* Gradient1 )
     Theta2 <- Theta2 - ( (learningrate/nrow(X))* Gradient2 )

     #Theta1 <- ( (learningrate/nrow(X))* Gradient1 ) + (momentum*Theta1)
     #Theta2 <- ( (learningrate/nrow(X))* Gradient2 ) + (momentum*Theta2)
     
     #return new theta values as vector 
     return(c(as.vector(Theta1),as.vector(Theta2)))
}

################################################################
#Main Program starts here                                      #
################################################################

# Call Inittheta function to initialize the Theta values
initial_theta <- Inittheta()

for (iter in 1:Totaliter)
{   
    # Call main lofic function
    newtheta <- NeuralNet(initial_theta,iter)
    # Set the new theta values as initial_theta
    initial_theta <- newtheta
}

# Plot Cost function to see the convergence
plot(CostVal,y=NULL,main="test",type="l")

################################################################
#test the validaton set                                        #
################################################################

# Move initial_theta to Theta1 and Theta2
Theta1 <- matrix(initial_theta[1:((ncol(X))*hiddenunits)] , (ncol(X)), hiddenunits)
Theta2 <- matrix(initial_theta[(((ncol(X))*hiddenunits)+1):(((ncol(X))*hiddenunits)+(ncol(Y)*hiddenunits))] , hiddenunits,ncol(Y) )

#Predictor variables
testdata <- as.matrix(Validation[,-13])
#Add ones to testdata
testdata <- cbind(rep(1,nrow(testdata)),testdata)
testdata <- matrix(as.numeric(unlist(testdata)),nrow=nrow(testdata))

a2 <- matrix(rep(0,nrow(testdata)*hiddenunits) , (nrow(testdata)), hiddenunits)
a3 <- matrix(rep(0,nrow(testdata)*ncol(Y)) , (nrow(testdata)), ncol(Y))

for (i in 1:nrow(testdata))
{  
    
    #Calculate the activation for hidden unit 
    a2[i,]<- sigmoid(testdata[i,] %*% Theta1)
    
    # add a0 hidden neuron value as 1 
    a2[i,1] <- 1
    
    #Calculate the activation for output unit 
    a3[i,] <- sigmoid(a2[i,] %*% Theta2)  
    
}
z <- class.ind(Validation[,13]) 
z<-matrix(z,nrow(z),ncol(z))

checksuccess <- apply(round(a3)==z,1,all) 
print(table(checksuccess)["TRUE"])
print(table(checksuccess)["FALSE"])
#print ( (table(checksuccess)["TRUE"])*100/((table(checksuccess)["TRUE"]) + (table(checksuccess)["FALSE"]))    )

############End of Program########################################
