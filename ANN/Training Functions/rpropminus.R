#=====================================================================#
# libraries
require (neuralnet)
?neuralnet
#=====================================================================#

set.seed(69)
drying <- read.csv("~/Files/My docs/Github/drying-pred/ANN/Training Functions/Drying.new.csv")
#=====================================================================#
# shuffling the data
drying <- drying [sample(nrow(drying)) , ]
head (drying)

drying$PR [drying$PR == 1505] <- 1
drying$PR [drying$PR == 1005] <- 2
drying$PR [drying$PR == 1002] <- 3

drying$PR <- as.integer(drying$PR)
str (drying)
head (drying)
#=====================================================================#
# Train test split of original data
train_test_split_index_unscaled <- 0.6 * nrow (drying)
train <- drying [1:train_test_split_index_unscaled , ]
test <- drying [(train_test_split_index_unscaled+1) : nrow (drying) , ]
head (train)
head (test)
#=====================================================================#
# Normalise the data using scale function
maxs <- apply (drying , 2 , max)
maxs
mins <- apply (drying , 2, min)
mins

scaled.data <- scale (drying , center = mins , scale = maxs - mins)
scaled.data <- as.data.frame(scaled.data)
#=====================================================================#
# Train test split of scaled data
train_test_split_index <- 0.6 * nrow (scaled.data)
train.scaled <- scaled.data [1:train_test_split_index , ]
test.scaled <- scaled.data [(train_test_split_index+1) : nrow (scaled.data) , ]
head (train)
head (train.scaled)
head (test)
head (test.scaled)
#=====================================================================#
#x_train <- train.scaled [ , c(1:4)]
#y_train <- train.scaled$MR
#head (x_train)
#head (y_train)
#str (y_train)

#test.scaled <- test.scaled [ , c(1:4)]
#y_test <- test.scaled$MR
#head (test.scaled)
#head (y_test)

#head (scaled.data)

#table (train.scaled$PR)
#table (test.scaled$PR)

#dim (y_test) <- c(length(y_test) , 1)
#dim (y_train) <- c (length(y_train) , 1)

# converting all to matrix
#x_train <- as.matrix (x_train , byrow = T)
#x_train <- t(x_train)
#y_train <- as.matrix (y_train , byrow = T)
#y_train <- t(y_train)
#y_train <- data.frame (y_train)

#test.scaled <- as.matrix (test.scaled , byrow = T)
#test.scaled <- t (test.scaled)
#y_test <- as.matrix (y_test , byrow = T)
#y_test <- t (y_test)

#install.packages("neuralnet") # choose no
#require(neuralnet)

#head (x_train)
#dim(x_train)
#head (y_train)
#dim (y_train)
#=====================================================================#
# Activation function for hidden layer

#Sigmoidal  Ativation functon
sigmoid <- function (x) {
  (1 / (1 + exp (-x)))
}

#=====================================================================#
# list to hold models
rpropminus.list <- list()

# iterating over the dataframe with varying number of hidden layers for the neural net
for (i in 1:20) 
  {
  
  model <- neuralnet (
    formula = MR ~ Time + VP + DT + PR , data = train.scaled ,
    hidden  = i , act.fct = sigmoid , linear.output = T ,
    algorithm = "rprop-")
  
  rpropminus.list[[i]] <- model
  
  model.name <- paste("rpropminus.model", i, sep = "")
  
  names(rpropminus.list)[i] <- model.name
}

#str (x_train)
#=====================================================================#
nrow(test.scaled)
#=====================================================================#
predict.df <- data.frame(matrix(data = NA,
                                nrow = 120,
                                ncol = length(rpropminus.list)))

for (i in 1:length(rpropminus.list))
{
  predict.name <- paste("rpropminus.predict", i, sep = "")
  
  pred.result <- neuralnet::compute(rpropminus.list[[i]], 
                                    covariate = test.scaled)
  
  pred.result <- pred.result$net.result
  
  pred.result <- data.frame(pred.result)
  
  predict.df[i] <- pred.result
  
  names(predict.df)[i] <- predict.name
}
#=====================================================================#
#unnormalize

unnormalize <- function (x) {
  return ((x* (max(test$MR)) - min (test$MR)) + min (test$MR))
}
# apply the function
predict.df <- unnormalize(predict.df)
#=====================================================================#

rpropminus_results <- cbind (test$Time , predict.df, test$MR)

colnames(rpropminus_results) <- c ('DryingTime' , 'PredictedMR1' , 'PredictedMR2' , 
                                   'PredictedMR3' , 'PredictedMR4' , 'PredictedMR5' , 
                                   'PredictedMR6' , 'PredictedMR7' ,'PredictedMR8' , 'PredictedMR9' ,
                                   'PredictedMR10' , 'PredictedMR11' , 'PredictedMR12' ,
                                   'PredictedMR13' , 'PredictedMR14' , 'PredictedMR15' ,
                                   'PredictedMR16', 'PredictedMR17' , 'PredictedMR18' ,
                                   'PredictedMR19' , 'PredictedMR20' , 'ActualMR') 

rpropminus_results <- data.frame(rpropminus_results)

rpropminus.metrics <- data.frame (matrix (vector () , 5 , 20 ,
                                          dimnames = list (c("MSE","RMSE","SSE","SST","R2") , 
                                                           c ("n1" , "n2" , "n3" , "n4" ,
                                                              "n5" , "n6" , "n7" , "n8" ,
                                                              "n9" , "n10" , "n11" , "n12" ,
                                                              "n13" , "n14" , "n15" , "n16" ,
                                                              "n17" , "n18" , "n19" , "n20"))) ,
                                  stringsAsFactors = F)

#=====================================================================#
# Performance matrices

for (i in 1:ncol(predict.df))
{
  # Mean Square Error
  rpropminus.metrics["MSE", i] <- mean((predict.df[, i] - test$MR))
  
  # Root Mean Square Error
  rpropminus.metrics["RMSE", i] <- (rpropminus.metrics ["MSE" , i]) ^ 2
  
  #Sum of Square Error
  rpropminus.metrics["SSE", i] <- sum ((test$MR - predict.df[, i]) ^ 2)
  
  #SST
  rpropminus.metrics ["SST" , i] <- sum ((test$MR - mean (train$MR)) ^ 2)
  
  #R-squared
  rpropminus.metrics ["R2" , i] <- 1 - 
    ((rpropminus.metrics["SSE" , i]) / (rpropminus.metrics["SST" , i]))
  
}

#=====================================================================#
rpropminus.metrics <- round (rpropminus.metrics,5)
rpropminus.metrics

rpropminus.metrics1 <- rpropminus.metrics
rpropminus.metrics1 <- as.matrix (rpropminus.metrics1)
rpropminus.metrics1 <- t (rpropminus.metrics1)
rpropminus.metrics1 <- data.frame (rpropminus.metrics1)
rpropminus.metrics1$NeuronNumber <- 1:20

plot (rpropminus.metrics1$RMSE , rpropminus.metrics1$R2)
rpropminus.metrics1 <- rpropminus.metrics1 [, c ('RMSE' , 'R2' , 'NeuronNumber')]
write.csv(rpropminus.metrics1 , "rpropminus.metrics.csv")

#=====================================================================#






