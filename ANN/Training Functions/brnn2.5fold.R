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
require(brnn) # for activation function
#=====================================================================#
set.seed (69)
#=====================================================================#
# data
drying <- read.csv('~/Files/My docs/Github/drying-pred/ANN/Training Functions/Drying.new.csv')
#=====================================================================#
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
k <- 5

# creating k equal sized folds
folds <- cut (seq(1,nrow (drying.shuffled)) , breaks = k , labels = F)

# creating data object to hold the performance metrics of models
brnn.metrics <- data.frame (matrix (vector () , 5 , 20 ,
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
  x_train <- trainData.scaled [ , c(1:4)]
  x_train <- as.matrix (x_train , byrow = T)
  y_train <- trainData.scaled[ , ncol(trainData.scaled)]
  
  x_test <- testData.scaled [ , c(1:4)]
  y_test <- testData.scaled [, ncol(testData.scaled)]
  
  
  # using k fold cv to evaluate the models
  for (m in 1:20) {
    model <- brnn (x_train , y_train ,normalize = F ,neurons = m , verbose = F)
    brnn.predict <- predict.brnn(model , newdata = x_test)
    #brnn.predict <- brnn.predict$net.result
    
    #unnormalize
    unnormalize <- function (x) {
      return ((x* (max(testData$MR)) - min (testData$MR)) + min (testData$MR))
    }
    
    brnn.predict <- unnormalize(brnn.predict)
    
    mse [i , m] <- mean ((brnn.predict - testData$MR)^2)
    rmse [i , m] <- mse [i , m] ^ 0.5
    sse [i , m] <- sum ((brnn.predict - testData$MR) ^ 2)
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
  brnn.metrics ["MSE" , l] <- mean (mse [ , l])
  brnn.metrics ["RMSE" , l] <- mean (rmse [ , l])
  brnn.metrics ["SSE" , l] <- mean (sse [ , l])
  brnn.metrics ["SST" , l] <- mean (sst [ , l])
  brnn.metrics ["R2" , l] <- mean (R2 [ , l])
}  

brnn.metrics <- round (brnn.metrics,5)
brnn.metrics  

brnn.metrics1 <- brnn.metrics
brnn.metrics1 <- as.matrix (brnn.metrics1)
brnn.metrics1 <- t (brnn.metrics1)
brnn.metrics1 <- data.frame (brnn.metrics1)
brnn.metrics1$NeuronNumber <- 1:20

#=====================================================================#
plot (brnn.metrics1$RMSE , brnn.metrics1$R2)
brnn.metrics1 <- brnn.metrics1 [, c ('RMSE' , 'R2' , 'NeuronNumber')]
write.csv(brnn.metrics1 , "brnn.metrics5fold.csv")
brnn.metrics1
#=====================================================================#

















