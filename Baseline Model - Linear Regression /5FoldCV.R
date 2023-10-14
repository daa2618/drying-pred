#=============================================================#
# packages
require (dplyr)
require(corrplot)
require (corrgram)
require (tidyverse)
require(ggplot2)
require(car)
#=============================================================#
set.seed(69)
#=============================================================#
# reading the data
Data <- read.csv('Drying.new.csv')
#=============================================================#
# # Copying the values into another variable
drying <- Data

drying$PR [drying$PR == 1505] <- 1
drying$PR [drying$PR == 1005] <- 2
drying$PR [drying$PR == 1002] <- 3

str (drying)
drying$PR <- as.integer(drying$PR)
#=============================================================#
#ggplot (drying , aes (x = Time , y = DR)) + geom_point()
#=============================================================#
# Randomly shuffling data
set.seed (69)
drying.shuffled <- drying [sample (nrow(drying)) , ]
#=============================================================#
# Number of folds for k-fold cross validation
k <- 5

# creating k equal-sized folds
folds <- cut (seq(1,nrow(drying.shuffled)) , breaks = k , labels = F)
folds
#=============================================================#
# creating data object to hold MSE'sof models
train.metrics <- data.frame (matrix (vector () , 5 , 1 ,
                                     dimnames = list (c("MSE","RMSE","SSE","SST","R2") , 
                                                      c ("value"))) ,
                             stringsAsFactors = F)

test.metrics <- data.frame (matrix (vector () , 5 , 1 ,
                                    dimnames = list (c("MSE","RMSE","SSE","SST","R2") , 
                                                     c ("value"))) ,
                            stringsAsFactors = F)
#=============================================================#
# creating data object to hold the performance metrics of models
mse.train <- matrix (data = NA , nrow = k , ncol = 1)
rmse.train <- matrix (data = NA , nrow = k , ncol = 1)
sse.train <- matrix (data = NA , nrow = k , ncol = 1)
sst.train <- matrix (data = NA , nrow = k , ncol = 1)
R2.train <- matrix (data = NA , nrow = k , ncol = 1)

mse.test <- matrix (data = NA , nrow = k , ncol = 1)
rmse.test <- matrix (data = NA , nrow = k , ncol = 1)
sse.test <- matrix (data = NA , nrow = k , ncol = 1)
sst.test <- matrix (data = NA , nrow = k , ncol = 1)
R2.test <- matrix (data = NA , nrow = k , ncol = 1)
#=============================================================#
# k fold cross validation
for (i in 1:k) {
  
  # defining trainign and testing data
  testIndexes <- which (folds == i , arr.ind = T)
  testData <- drying.shuffled [testIndexes , ]
  trainData <- drying.shuffled [-testIndexes , ]
  
  # use k fold cv to evaluate models
  #for (j in 1:degree) {
  model <- lm (MR ~ Time + VP + DT + PR, data = trainData)
  train.predict <- predict (model , trainData)
  test.predict <- predict (model, newdata = testData)
  
  mse.train [i , 1] <- mean ((train.predict - trainData$MR)^2)
  rmse.train [i , 1] <- mse.train [i , 1] ^ 0.5
  sse.train[i , 1] <- sum ((train.predict - trainData$MR) ^ 2)
  sst.train [i , 1] <- sum ((trainData$MR - mean(trainData$MR))^2)
  R2.train [i , 1] <- 1 - ((sse.train[i , 1]) / sst.train [i , 1])
  
  mse.test [i , 1] <- mean ((test.predict - testData$MR)^2)
  rmse.test [i , 1] <- mse.test [i , 1] ^ 0.5
  sse.test [i , 1] <- sum ((test.predict - testData$MR) ^ 2)
  sst.test [i , 1] <- sum ((testData$MR - mean(trainData$MR))^2)
  R2.test [i , 1] <- 1 - ((sse.test[i , 1]) / sst.test [i , 1])
  
  
}
#=============================================================#
summary (model)
mse.train <- data.frame(mse.train)
rmse.train <- data.frame (rmse.train)
sse.train<- data.frame (sse.train)
sst.train <- data.frame(sst.train )
R2.train <- data.frame (R2.train )


mse.test <- data.frame(mse.test)
rmse.test <- data.frame (rmse.test)
sse.test <- data.frame (sse.test)
sst.test <- data.frame(sst.test )
R2.test <- data.frame (R2.test )
#=============================================================#
#=============================================================#

for (l in 1) {
  test.metrics ["MSE" , l] <- mean (mse.test [ , l])
  test.metrics ["RMSE" , l] <- mean (rmse.test [ , l])
  test.metrics ["SSE" , l] <- mean (sse.test  [ , l])
  test.metrics ["SST" , l] <- mean (sst.test  [ , l])
  test.metrics ["R2" , l] <- mean (R2.test  [ , l])
}  


test.metrics <- round (test.metrics,5)
test.metrics  

for (l in 1) {
  train.metrics ["MSE" , l] <- mean (mse.train [ , l])
  train.metrics ["RMSE" , l] <- mean (rmse.train [ , l])
  train.metrics ["SSE" , l] <- mean (sse.train  [ , l])
  train.metrics ["SST" , l] <- mean (sst.train  [ , l])
  train.metrics ["R2" , l] <- mean (R2.train  [ , l])
}  


train.metrics <- round (train.metrics,5)
train.metrics  
#=============================================================#
# Plots
p6 <- ggplot (data = df.MR.80 , aes (x = DryingTime , y = value , shape = variable , color = variable)) +
  geom_line (aes (linetype = variable)) +
  geom_point (size = 2) +
  scale_color_hue (name = "variable" , l = 30) +
  xlab ("Drying Time (min)") + ylab ("Mositure Ratio") +
  ggtitle ("B") + theme_bw() +
  scale_x_continuous("Drying Time (min)" , breaks = seq(0 , 600 , 50))

p6
results
#=============================================================#
