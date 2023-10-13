#=====================================================================#
# libraries
#install.packages("brnn") # choose no
require(brnn)
#=====================================================================#
# set the seed for reproducability
set.seed(69)
#=====================================================================#
# read the csv file into drying
drying <- read.csv("~/Files/My docs/Github/drying-pred/ANN/Training Functions/Drying.new.csv")
#=====================================================================#
# processing the data
# shuffling the data
drying <- drying [sample(nrow(drying)) , ]
head (drying)
#=====================================================================#
# Data wrangling
drying$PR [drying$PR == 1505] <- 1
drying$PR [drying$PR == 1005] <- 2
drying$PR [drying$PR == 1002] <- 3

drying$PR <- as.integer(drying$PR)
str (drying)
head (drying)
#=====================================================================#
# Train test split of original data
train_test_split_index_unscaled <- 0.6 * nrow (drying)
train<- drying [1:train_test_split_index_unscaled , ]
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

# Train test split of scaled data
train_test_split_index <- 0.6 * nrow (scaled.data)
train.scaled <- scaled.data [1:train_test_split_index , ]
test.scaled <- scaled.data [(train_test_split_index+1) : nrow (scaled.data) , ]
head (train)
head (train.scaled)
head (test)
head (test.scaled)

x_train <- train.scaled [ , c(1:4)]
y_train <- train.scaled$MR
head (x_train)
head (y_train)
str (y_train)

x_test <- test.scaled [ , c(1:4)]
y_test <- test.scaled$MR
head (x_test)
head (y_test)

head (scaled.data)

table (train.scaled$PR)
table (test.scaled$PR)

#dim (y_test) <- c(length(y_test) , 1)
#dim (y_train) <- c (length(y_train) , 1)

# converting all to matrix
x_train <- as.matrix (x_train , byrow = T)
#x_train <- t(x_train)
#y_train <- as.matrix (y_train , byrow = T)
#y_train <- t(y_train)
#y_train <- data.frame (y_train)

#x_test <- as.matrix (x_test , byrow = T)
#x_test <- t (x_test)
#y_test <- as.matrix (y_test , byrow = T)
#y_test <- t (y_test)


#=====================================================================#

head (x_train)
dim(x_train)
head (y_train)
dim (y_train)
#=====================================================================#
# initialise an empty list to hold the values 

brnn.list <- list()
  
for (j in 1:20) 
{
  model.name <- paste("brnn.model", j, sep="")
    
  brnn.model <- brnn(x_train, y_train, normalize = F,
                       neurons = j, verbose = T)
    
  brnn.list[[j]] <- brnn.model
    
  names(brnn.list)[j] <- model.name
    
}

#=====================================================================#

str (y_train)
str (x_train)
#=====================================================================#
# initialise an empty dataframe to hold the predicted values 

#brnn.predict.list <- list()

brnn.predict.df <- data.frame(matrix(data = NA,
                                     nrow = 120,
                                     ncol = length(brnn.list)))

for (i in 1:length(brnn.list))
{
  predict.name <- paste("brnn.predict", i, sep = "")
  
  pred.result <- predict.brnn(brnn.list[[i]], newdata = x_test)
  
  pred.result <- data.frame(pred.result)
  
  brnn.predict.df[i] <- pred.result
  
  names(brnn.predict.df)[i] <- predict.name
}

head(brnn.predict.df)

#=====================================================================#
# UNNORMALIZING THE PREDICTED RESULTS

# create the function to unnormalize 
unnormalize <- function (x) {
  return ((x* (max(test$MR)) - min (test$MR)) + min (test$MR))
}

# apply the function
brnn.predict.df <- unnormalize(brnn.predict.df)

# create the results data frame

brnn_results <- cbind (test$Time , brnn.predict.df ,test$MR)

colnames(brnn_results) <- c ('DryingTime' , 'PredictedMR1' , 'PredictedMR2' , 
                             'PredictedMR3' , 'PredictedMR4' , 'PredictedMR5' , 
                             'PredictedMR6' , 'PredictedMR7' ,'PredictedMR8' , 'PredictedMR9' ,
                             'PredictedMR10' , 'PredictedMR11' , 'PredictedMR12' ,
                             'PredictedMR13' , 'PredictedMR14' , 'PredictedMR15' ,
                             'PredictedMR16', 'PredictedMR17' , 'PredictedMR18' ,
                             'PredictedMR19' , 'PredictedMR20' , 'ActualMR') 

#brnn_results <- data.frame(brnn_results)

brnn.metrics <- data.frame (matrix (vector () , 5 , 20 ,
                                    dimnames = list (c("MSE","RMSE","SSE","SST","R2") , 
                                                     c ("n1" , "n2" , "n3" , "n4" ,
                                                        "n5" , "n6" , "n7" , "n8" ,
                                                        "n9" , "n10" , "n11" , "n12" ,
                                                        "n13" , "n14" , "n15" , "n16" ,
                                                        "n17" , "n18" , "n19" , "n20"))) ,
                            stringsAsFactors = F)


# MSE
for (i in 1:ncol(brnn.metrics))
{
  # Mean Square Error
  brnn.metrics["MSE", i] <- mean((brnn.predict.df[, i] - test$MR))
  
  # Root Mean Square Error
  brnn.metrics["RMSE", i] <- (brnn.metrics ["MSE" , i]) ^ 2
  
  #Sum of Square Error
  brnn.metrics["SSE", i] <- sum ((test$MR - brnn.predict.df[, i]) ^ 2)
  
  #SST
  brnn.metrics ["SST" , i] <- sum ((test$MR - mean (train$MR)) ^ 2)
  
  #R-squared
  brnn.metrics ["R2" , i] <- 1 - 
    ((brnn.metrics["SSE" , i]) / (brnn.metrics["SST" , i]))
  
  
  
}


brnn.metrics <- round (brnn.metrics,5)
brnn.metrics

brnn.metrics1 <- brnn.metrics
brnn.metrics1 <- as.matrix (brnn.metrics1)
brnn.metrics1 <- t (brnn.metrics1)
brnn.metrics1 <- data.frame (brnn.metrics1)
brnn.metrics1$NeuronNumber <- 1:20

#plot (brnn.metrics1$RMSE , brnn.metrics1$R2)
brnn.metrics1 <- brnn.metrics1 [, c ('RMSE' , 'R2' , 'NeuronNumber')]
write.csv(brnn.metrics1 , "brnn.metrics.csv")




