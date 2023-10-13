#=====================================================================#
# loading requird packages
require (ggplot2)
require(corrplot)
require (corrgram)
require (tidyverse)
require(dplyr)
require(car)
require(neuralnet)
require (caret)
#=====================================================================#

set.seed (69)
#=====================================================================#

# data
drying <- read.csv('~/Files/My docs/Github/drying-pred/ANN/Training Functions/Drying.new.csv')

drying$PR [drying$PR == 1505] <- 1
drying$PR [drying$PR == 1005] <- 2
drying$PR [drying$PR == 1002] <- 3

drying$PR <- as.integer(drying$PR)
#=====================================================================#

# shuffling the data
set.seed (69)
drying.shuffled <- drying [sample (nrow (drying)) , ]
#=====================================================================#

# Number of folds
k <- 10

# creating k equal sized folds
folds <- cut (seq(1,nrow (drying.shuffled)) , breaks = k , labels = F)

# creating data object to hold the performance metrics of models
rpropminus.metrics <- data.frame (matrix (vector () , 5 , 20 ,
                                          dimnames = list (c("MSE","RMSE","SSE","SST","R2") , 
                                                           c ("n1" , "n2" , "n3" , "n4" ,
                                                              "n5" , "n6" , "n7" , "n8" ,
                                                              "n9" , "n10" , "n11" , "n12" ,
                                                              "n13" , "n14" , "n15" , "n16" ,
                                                              "n17" , "n18" , "n19" , "n20"))) ,
                                  stringsAsFactors = F)

# creating data object to hold the performance metrics of models
mse <- matrix (data = NA , nrow = k , ncol = 20)
rmse <- matrix (data = NA , nrow = k , ncol = 20)
sse <- matrix (data = NA , nrow = k , ncol = 20)
sst <- matrix (data = NA , nrow = k , ncol = 20)
R2 <- matrix (data = NA , nrow = k , ncol = 20)

#=====================================================================#

#Sigmoidal  Ativation functon
sigmoid <- function (x) {
  (1 / (1 + exp (-x)))
}

# k fold cv


for (i in 1:k) {
  
  # training and testData data
  testDataIndexes <- which (folds == i , arr.ind = T)
  trainData <- drying.shuffled [-testDataIndexes , ]
  testData <- drying.shuffled [testDataIndexes , ]
  
  # Normalising the training and testData data
  maxs.train <- apply (trainData , 2 , max)
  mins.train <- apply (trainData , 2 , min)
  maxs.testData <- apply (testData , 2, max)
  mins.testData <- apply (testData , 2 , min)
  trainData.scaled <- scale (trainData , center = mins.train , scale = maxs.train - mins.train)
  testData.scaled <- scale (testData , center = mins.testData , scale = maxs.testData - mins.testData)
  
  # using k fold cv to evaluate the models
  for (m in 1:20) {
    model <- neuralnet (formula = MR ~ Time + DT + VP + PR , data = trainData.scaled , 
                        hidden = m , act.fct = sigmoid, algorithm = "rprop-" , linear.output = T)
    rpropminus.predict <- neuralnet::compute(model , testData.scaled)
    rpropminus.predict <- rpropminus.predict$net.result
    
    #unnormalize
    unnormalize <- function (x) {
      return ((x* (max(testData$MR)) - min (testData$MR)) + min (testData$MR))
    }
    
    rpropminus.predict <- unnormalize(rpropminus.predict)
    
    mse [i , m] <- mean ((rpropminus.predict - testData$MR)^2)
    rmse [i , m] <- mse [i , m] ^ 0.5
    sse [i , m] <- sum ((rpropminus.predict - testData$MR) ^ 2)
    sst [i , m] <- sum ((testData$MR - mean(trainData$MR))^2)
    R2 [i , m] <- 1 - ((sse[i , m]) / sst [i , m])
    
  }
  
}
#=====================================================================#

mse <- data.frame(mse)
rmse <- data.frame (rmse)
sse <- data.frame (sse)
sst <- data.frame(sst)
R2 <- data.frame (R2)
#=====================================================================#

for (l in 1:20) {
  rpropminus.metrics ["MSE" , l] <- mean (mse [ , l])
  rpropminus.metrics ["RMSE" , l] <- mean (rmse [ , l])
  rpropminus.metrics ["SSE" , l] <- mean (sse [ , l])
  rpropminus.metrics ["SST" , l] <- mean (sst [ , l])
  rpropminus.metrics ["R2" , l] <- mean (R2 [ , l])
}  
#=====================================================================#

rpropminus.metrics <- round (rpropminus.metrics,5)
rpropminus.metrics  

rpropminus.metrics1 <- rpropminus.metrics
rpropminus.metrics1 <- as.matrix (rpropminus.metrics1)
rpropminus.metrics1 <- t (rpropminus.metrics1)
rpropminus.metrics1 <- data.frame (rpropminus.metrics1)
rpropminus.metrics1$NeuronNumber <- 1:20
#=====================================================================#

#plot (rpropminus.metrics1$RMSE , rpropminus.metrics1$R2)
rpropminus.metrics1 <- rpropminus.metrics1 [, c ('RMSE' , 'R2' , 'NeuronNumber')]
write.csv(rpropminus.metrics1 , "rpropminus.metrics.csv")
#=====================================================================#


















