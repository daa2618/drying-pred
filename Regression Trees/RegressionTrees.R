#=============================================================#
# required packages
require (rpart)# recursive partioning for regression trees
install.packages('rpart.plot')
require (rpart.plot)
install.packages('Cubist')
require(Cubist)
require (psych)
#=============================================================#
# Data
Data <- read.csv('Drying.new.csv')

drying <- Data
#=============================================================#
# Examine the data
str (drying)
pairs (drying [c("Time" , "DT" , "VP" ,"PR" , "DR")])
pairs.panels(drying [c("Time" , "DT" , "VP" ,"PR" , "DR")])
# Exploring and Preparing the data
drying$PR [drying$PR == 1505] <- 1
drying$PR [drying$PR == 1005] <- 2
drying$PR [drying$PR == 1002] <- 3
drying$PR <- as.integer(drying$PR)
#=============================================================#
# Plotting
hist (drying$PR)
hist (drying$Time)
hist (drying$DR)
hist (drying$MR)
hist (drying$ST)
#=============================================================#
# Training and Testing data
Data.train <- read.csv('Data.train.csv')
Data.train <- Data.train [-1]
Data.test <- read.csv('Data.test.csv')
Data.test <- Data.test [-1]
#=============================================================#
# Building the models
# Using All variables
model1 <- rpart (formula = DR ~ . , data = Data.train)
summary (model1)
model1
#=============================================================#
# All 105 examples begin at root node, of which 38 have Time >= 285 and 67 have Time < 285
# Because Time is used first in the tree, it is the single most important predictor of DR
# Nodes indicated by * are terminal or leaf nodes
# They mean that result in a prediction (yval)
# i.e. Node 11 has yval of 0.013668260 for Time >= 285 and MR >= 0.1001576
#=============================================================#
# Training a model on the data

# Tree diagram
rpart.plot(model1 , digits = 3)

# Adjusting the viz
rpart.plot(model1 , digits = 4 , fallen.leaves = T ,
           type = 3 , extra = 101)

# Type and extra parameters affect the ay the decisions and nodes are labelled
# number 3 and 101 refer to a style format
# fallen.leaves forces the leaf nodes to be aligned at the bottom of the plot
#=============================================================#
# Evaluating model performance
predTest1 <- predict (model1 , Data.test )
summary (predTest1)
summary (Data.test$DR)

# Predictions fall on a much narrower range than true values (min to max)
# The model is not correctly identifying the extreme cases, in particular the, min and max DR
# However between the first and third quantiles, the model is doing well
#=============================================================#
# Comparing how well the predicted values corresponf to true values
cor(predTest1 , Data.test$DR) # 0.9568034 highly correlated

# The correlation only measures how strongly the predictions are related to the true value;
# it is not a measure of how far off the predictions were from the true values.
#=============================================================#
# MAE
MAE <- function (x, y) {
  mean (abs(x - y))
}

MAE (predTest1 , Data.test$DR) # 0.001973841
#=============================================================#
# In the training data
mean (Data.train$DR) # 0.01675558

MAE (0.01675558 , Data.test$DR) # 0.007174535

# If we predicted the value 0.01675558 for every observation, we would have a mean absolute error of only about 0.007174535
#=============================================================#
min (predTest1)
mse <- mean ((Data.test$DR - predTest1) ^ 2)
mse # 5.526955e-06

rmse <- mse ^ 0.5
rmse  # 0.002350948

sse <- sum((Data.test$DR - predTest1) ^ 2)
sst <- sum((Data.test$DR - mean (Data.train$DR)) ^ 2)
R2 <- 1 - (sse/sst) # 0.9256899
#=============================================================#
# Improving model performance by applying model tree algorithm
# It is  a more complex application of trees to numeric prediction

model2 <- cubist(x = Data.train[-7] , y = Data.train$DR)
model2 # Just shows 1 rule

summary (model2)

#=============================================================#
# A key difference between this model tree output and the earlier regression tree output, however, is that the nodes here terminate not in a numeric prediction, but rather a linear model.
# Fon one unit increase in time, the drying rate reduces by 5.75e-05
#=============================================================#
# Model performace
predTest2 <- predict (model2 , Data.test)
summary (predTest1)
summary (predTest2)

# The model tree appears to be predicting a wider range of values than regression tree
#=============================================================#
# Improving model performance
cor (predTest2 , Data.test$DR) # 0.9249124
# less correlation than regression tree -> predictions are related to the true value in a weaker manner 
#=============================================================#
# MAE
MAE (Data.test$DR , predTest2) #0.002576186 has less MAE 

mse <- mean ((Data.test$DR - predTest2) ^ 2)
mse # 1.044125e-05

rmse <- mse ^ 0.5
rmse  # 0.003231292

sse <- sum((Data.test$DR - predTest2) ^ 2)
sst <- sum((Data.test$DR - mean (Data.train$DR)) ^ 2)
R2 <- 1 - (sse/sst) # 0.859617
#=============================================================#


















