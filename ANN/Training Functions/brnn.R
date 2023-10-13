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

for (i in 1:20) 
{
  model.name <- paste("brnn.model", i, sep="")
  names(brnn.list)[i] <- model.name
  
  model.name <- brnn(x_train, y_train, normalize = F,
                     neurons = i, verbose = T)
    
  brnn.list[[i]] <- model.name
  
}

brnn.model1 <- brnn (x_train , y_train ,normalize = F ,neurons = 1 , verbose = T)
brnn.model2 <- brnn (x_train , y_train , normalize = F ,neurons = 2 , verbose = T)
brnn.model3 <- brnn (x_train , y_train , normalize = F ,neurons = 3 , verbose = T)
brnn.model4 <- brnn (x_train , y_train , normalize = F ,neurons = 4 , verbose = T)
brnn.model5 <- brnn (x_train , y_train , normalize = F ,neurons = 5 , verbose = T)
brnn.model6 <- brnn (x_train , y_train ,normalize = F , neurons = 6 , verbose = T)
brnn.model7 <- brnn (x_train , y_train , normalize = F ,neurons = 7 , verbose = T)
brnn.model8 <- brnn (x_train , y_train , normalize = F ,neurons = 8 , verbose = T)
brnn.model9 <- brnn (x_train , y_train , normalize = F ,neurons = 9 , verbose = T)
brnn.model10 <- brnn (x_train , y_train ,normalize = F , neurons = 10 , verbose = T)
brnn.model11 <- brnn (x_train , y_train , normalize = F ,neurons = 11 , verbose = T)
brnn.model12 <- brnn (x_train , y_train ,normalize = F , neurons = 12 , verbose = T)
brnn.model13 <- brnn (x_train , y_train ,normalize = F , neurons = 13 , verbose = T)
brnn.model14 <- brnn (x_train , y_train , normalize = F ,neurons = 14 , verbose = T)
brnn.model15 <- brnn (x_train , y_train , normalize = F ,neurons = 15 , verbose = T)
brnn.model16 <- brnn (x_train , y_train , normalize = F ,neurons = 16 , verbose = T)
brnn.model17 <- brnn (x_train , y_train , normalize = F ,neurons = 17 , verbose = T)
brnn.model18 <- brnn (x_train , y_train , normalize = F ,neurons = 18 , verbose = T)
brnn.model19 <- brnn (x_train , y_train , normalize = F ,neurons = 19 , verbose = T)
brnn.model20 <- brnn (x_train , y_train , normalize = F ,neurons = 20 , verbose = T)
str (y_train)
str (x_train)

brnn.predict1 <- predict.brnn(brnn.model1 , newdata = x_test)
brnn.predict2 <- predict.brnn(brnn.model2 , newdata = x_test)
brnn.predict3 <- predict.brnn(brnn.model3 , newdata = x_test)
brnn.predict4 <- predict.brnn(brnn.model4 , newdata = x_test)
brnn.predict5 <- predict.brnn(brnn.model5 , newdata = x_test)
brnn.predict6 <- predict.brnn(brnn.model6 , newdata = x_test)
brnn.predict7 <- predict.brnn(brnn.model7 , newdata = x_test)
brnn.predict8 <- predict.brnn(brnn.model8 , newdata = x_test)
brnn.predict9 <- predict.brnn(brnn.model9 , newdata = x_test)
brnn.predict10 <- predict.brnn(brnn.model10 , newdata = x_test)
brnn.predict11 <- predict.brnn(brnn.model11 , newdata = x_test)
brnn.predict12 <- predict.brnn(brnn.model12 , newdata = x_test)
brnn.predict13 <- predict.brnn(brnn.model13 , newdata = x_test)
brnn.predict14 <- predict.brnn(brnn.model14 , newdata = x_test)
brnn.predict15 <- predict.brnn(brnn.model15 , newdata = x_test)
brnn.predict16 <- predict.brnn(brnn.model16 , newdata = x_test)
brnn.predict17 <- predict.brnn(brnn.model17 , newdata = x_test)
brnn.predict18 <- predict.brnn(brnn.model18 , newdata = x_test)
brnn.predict19 <- predict.brnn(brnn.model19 , newdata = x_test)
brnn.predict20 <- predict.brnn(brnn.model20 , newdata = x_test)

#brnn.predict

unnormalize <- function (x) {
  return ((x* (max(test$MR)) - min (test$MR)) + min (test$MR))
}

brnn.predict1 <- unnormalize (brnn.predict1)
brnn.predict2 <- unnormalize (brnn.predict2)
brnn.predict3 <- unnormalize (brnn.predict3)
brnn.predict4 <- unnormalize (brnn.predict4)
brnn.predict5 <- unnormalize (brnn.predict5)
brnn.predict6 <- unnormalize (brnn.predict6)
brnn.predict7 <- unnormalize (brnn.predict7)
brnn.predict8 <- unnormalize (brnn.predict8)
brnn.predict9 <- unnormalize (brnn.predict9)
brnn.predict10 <- unnormalize (brnn.predict10)

brnn.predict11 <- unnormalize (brnn.predict11)
brnn.predict12 <- unnormalize (brnn.predict12)
brnn.predict13 <- unnormalize (brnn.predict13)
brnn.predict14 <- unnormalize (brnn.predict14)
brnn.predict15 <- unnormalize (brnn.predict15)
brnn.predict16 <- unnormalize (brnn.predict16)
brnn.predict17 <- unnormalize (brnn.predict17)
brnn.predict18 <- unnormalize (brnn.predict18)
brnn.predict19 <- unnormalize (brnn.predict19)
brnn.predict20 <- unnormalize (brnn.predict20)

brnn_results <- cbind (test$Time , brnn.predict1 , brnn.predict2 ,brnn.predict3,brnn.predict4,
                       brnn.predict5,brnn.predict6,brnn.predict7,brnn.predict8,brnn.predict9,
                       brnn.predict10,brnn.predict11,brnn.predict12,brnn.predict13,brnn.predict14,
                       brnn.predict15,brnn.predict16,brnn.predict17,brnn.predict18,brnn.predict19,
                       brnn.predict20,test$MR)

colnames(brnn_results) <- c ('DryingTime' , 'PredictedMR1' , 'PredictedMR2' , 
                             'PredictedMR3' , 'PredictedMR4' , 'PredictedMR5' , 
                             'PredictedMR6' , 'PredictedMR7' ,'PredictedMR8' , 'PredictedMR9' ,
                             'PredictedMR10' , 'PredictedMR11' , 'PredictedMR12' ,
                             'PredictedMR13' , 'PredictedMR14' , 'PredictedMR15' ,
                             'PredictedMR16', 'PredictedMR17' , 'PredictedMR18' ,
                             'PredictedMR19' , 'PredictedMR20' , 'ActualMR') 
brnn_results <- data.frame(brnn_results)

brnn.metrics <- data.frame (matrix (vector () , 5 , 20 ,
                                    dimnames = list (c("MSE","RMSE","SSE","SST","R2") , 
                                                     c ("n1" , "n2" , "n3" , "n4" ,
                                                        "n5" , "n6" , "n7" , "n8" ,
                                                        "n9" , "n10" , "n11" , "n12" ,
                                                        "n13" , "n14" , "n15" , "n16" ,
                                                        "n17" , "n18" , "n19" , "n20"))) ,
                            stringsAsFactors = F)


# MSE
brnn.metrics ["MSE" , "n1"] <- mean ((brnn_results$PredictedMR1 - test$MR))
brnn.metrics ["MSE" , "n2"] <- mean ((brnn_results$PredictedMR2 - test$MR))
brnn.metrics ["MSE" , "n3"] <- mean ((brnn_results$PredictedMR3 - test$MR))
brnn.metrics ["MSE" , "n4"] <- mean ((brnn_results$PredictedMR4 - test$MR))
brnn.metrics ["MSE" , "n5"] <- mean ((brnn_results$PredictedMR5 - test$MR))

brnn.metrics ["MSE" , "n6"] <- mean ((brnn_results$PredictedMR6 - test$MR))
brnn.metrics ["MSE" , "n7"] <- mean ((brnn_results$PredictedMR7 - test$MR))
brnn.metrics ["MSE" , "n8"] <- mean ((brnn_results$PredictedMR8 - test$MR))
brnn.metrics ["MSE" , "n9"] <- mean ((brnn_results$PredictedMR9 - test$MR))
brnn.metrics ["MSE" , "n10"] <- mean ((brnn_results$PredictedMR10 - test$MR))

brnn.metrics ["MSE" , "n11"] <- mean ((brnn_results$PredictedMR11 - test$MR))
brnn.metrics ["MSE" , "n12"] <- mean ((brnn_results$PredictedMR12 - test$MR))
brnn.metrics ["MSE" , "n13"] <- mean ((brnn_results$PredictedMR13 - test$MR))
brnn.metrics ["MSE" , "n14"] <- mean ((brnn_results$PredictedMR14 - test$MR))
brnn.metrics ["MSE" , "n15"] <- mean ((brnn_results$PredictedMR15 - test$MR))

brnn.metrics ["MSE" , "n16"] <- mean ((brnn_results$PredictedMR16 - test$MR))
brnn.metrics ["MSE" , "n17"] <- mean ((brnn_results$PredictedMR17 - test$MR))
brnn.metrics ["MSE" , "n18"] <- mean ((brnn_results$PredictedMR18 - test$MR))
brnn.metrics ["MSE" , "n19"] <- mean ((brnn_results$PredictedMR19 - test$MR))
brnn.metrics ["MSE" , "n20"] <- mean ((brnn_results$PredictedMR20 - test$MR))

# RMSE
brnn.metrics ["RMSE" , "n1"] <- (brnn.metrics ["MSE" , "n1"]) ^ 2
brnn.metrics ["RMSE" , "n2"] <- (brnn.metrics ["MSE" , "n2"]) ^ 2
brnn.metrics ["RMSE" , "n3"] <- (brnn.metrics ["MSE" , "n3"]) ^ 2
brnn.metrics ["RMSE" , "n4"] <- (brnn.metrics ["MSE" , "n4"]) ^ 2
brnn.metrics ["RMSE" , "n5"] <- (brnn.metrics ["MSE" , "n5"]) ^ 2

brnn.metrics ["RMSE" , "n6"] <- (brnn.metrics ["MSE" , "n6"]) ^ 2
brnn.metrics ["RMSE" , "n7"] <- (brnn.metrics ["MSE" , "n7"]) ^ 2
brnn.metrics ["RMSE" , "n8"] <- (brnn.metrics ["MSE" , "n8"]) ^ 2
brnn.metrics ["RMSE" , "n9"] <- (brnn.metrics ["MSE" , "n9"]) ^ 2
brnn.metrics ["RMSE" , "n10"] <- (brnn.metrics ["MSE" , "n10"]) ^ 2

brnn.metrics ["RMSE" , "n11"] <- (brnn.metrics ["MSE" , "n11"]) ^ 2
brnn.metrics ["RMSE" , "n12"] <- (brnn.metrics ["MSE" , "n12"]) ^ 2
brnn.metrics ["RMSE" , "n13"] <- (brnn.metrics ["MSE" , "n13"]) ^ 2
brnn.metrics ["RMSE" , "n14"] <- (brnn.metrics ["MSE" , "n14"]) ^ 2
brnn.metrics ["RMSE" , "n15"] <- (brnn.metrics ["MSE" , "n15"]) ^ 2

brnn.metrics ["RMSE" , "n16"] <- (brnn.metrics ["MSE" , "n16"]) ^ 2
brnn.metrics ["RMSE" , "n17"] <- (brnn.metrics ["MSE" , "n17"]) ^ 2
brnn.metrics ["RMSE" , "n18"] <- (brnn.metrics ["MSE" , "n18"]) ^ 2
brnn.metrics ["RMSE" , "n19"] <- (brnn.metrics ["MSE" , "n19"]) ^ 2
brnn.metrics ["RMSE" , "n20"] <- (brnn.metrics ["MSE" , "n20"]) ^ 2


# SSE
brnn.metrics ["SSE" , "n1"] <- sum ((test$MR - brnn_results$PredictedMR1) ^ 2)
brnn.metrics ["SSE" , "n2"] <- sum ((test$MR - brnn_results$PredictedMR2) ^ 2)
brnn.metrics ["SSE" , "n3"] <- sum ((test$MR - brnn_results$PredictedMR3) ^ 2)
brnn.metrics ["SSE" , "n4"] <- sum ((test$MR - brnn_results$PredictedMR4) ^ 2)
brnn.metrics ["SSE" , "n5"] <- sum ((test$MR - brnn_results$PredictedMR5) ^ 2)

brnn.metrics ["SSE" , "n6"] <- sum ((test$MR - brnn_results$PredictedMR6) ^ 2)
brnn.metrics ["SSE" , "n7"] <- sum ((test$MR - brnn_results$PredictedMR7) ^ 2)
brnn.metrics ["SSE" , "n8"] <- sum ((test$MR - brnn_results$PredictedMR8) ^ 2)
brnn.metrics ["SSE" , "n9"] <- sum ((test$MR - brnn_results$PredictedMR9) ^ 2)
brnn.metrics ["SSE" , "n10"] <- sum ((test$MR - brnn_results$PredictedMR10) ^ 2)

brnn.metrics ["SSE" , "n11"] <- sum ((test$MR - brnn_results$PredictedMR11) ^ 2)
brnn.metrics ["SSE" , "n12"] <- sum ((test$MR - brnn_results$PredictedMR12) ^ 2)
brnn.metrics ["SSE" , "n13"] <- sum ((test$MR - brnn_results$PredictedMR13) ^ 2)
brnn.metrics ["SSE" , "n14"] <- sum ((test$MR - brnn_results$PredictedMR14) ^ 2)
brnn.metrics ["SSE" , "n15"] <- sum ((test$MR - brnn_results$PredictedMR15) ^ 2)

brnn.metrics ["SSE" , "n16"] <- sum ((test$MR - brnn_results$PredictedMR16) ^ 2)
brnn.metrics ["SSE" , "n17"] <- sum ((test$MR - brnn_results$PredictedMR17) ^ 2)
brnn.metrics ["SSE" , "n18"] <- sum ((test$MR - brnn_results$PredictedMR18) ^ 2)
brnn.metrics ["SSE" , "n19"] <- sum ((test$MR - brnn_results$PredictedMR19) ^ 2)
brnn.metrics ["SSE" , "n20"] <- sum ((test$MR - brnn_results$PredictedMR20) ^ 2)

# SST 
brnn.metrics ["SST" , "n1"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n2"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n3"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n4"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n5"] <- sum ((test$MR - mean (train$MR)) ^ 2)

brnn.metrics ["SST" , "n6"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n7"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n8"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n9"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n10"] <- sum ((test$MR - mean (train$MR)) ^ 2)

brnn.metrics ["SST" , "n11"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n12"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n13"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n14"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n15"] <- sum ((test$MR - mean (train$MR)) ^ 2)

brnn.metrics ["SST" , "n16"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n17"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n18"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n19"] <- sum ((test$MR - mean (train$MR)) ^ 2)
brnn.metrics ["SST" , "n20"] <- sum ((test$MR - mean (train$MR)) ^ 2)



# R2
brnn.metrics ["R2" , "n1"] <- 1 - ((brnn.metrics["SSE" , "n1"]) / (brnn.metrics["SST" , "n1"]))
brnn.metrics ["R2" , "n2"] <- 1 - ((brnn.metrics["SSE" , "n2"]) / (brnn.metrics["SST" , "n2"]))
brnn.metrics ["R2" , "n3"] <- 1 - ((brnn.metrics["SSE" , "n3"]) / (brnn.metrics["SST" , "n3"]))
brnn.metrics ["R2" , "n4"] <- 1 - ((brnn.metrics["SSE" , "n4"]) / (brnn.metrics["SST" , "n4"]))
brnn.metrics ["R2" , "n5"] <- 1 - ((brnn.metrics["SSE" , "n5"]) / (brnn.metrics["SST" , "n5"]))

brnn.metrics ["R2" , "n6"] <- 1 - ((brnn.metrics["SSE" , "n6"]) / (brnn.metrics["SST" , "n6"]))
brnn.metrics ["R2" , "n7"] <- 1 - ((brnn.metrics["SSE" , "n7"]) / (brnn.metrics["SST" , "n7"]))
brnn.metrics ["R2" , "n8"] <- 1 - ((brnn.metrics["SSE" , "n8"]) / (brnn.metrics["SST" , "n8"]))
brnn.metrics ["R2" , "n9"] <- 1 - ((brnn.metrics["SSE" , "n9"]) / (brnn.metrics["SST" , "n9"]))
brnn.metrics ["R2" , "n10"] <- 1 - ((brnn.metrics["SSE" , "n10"]) / (brnn.metrics["SST" , "n10"]))

brnn.metrics ["R2" , "n11"] <- 1 - ((brnn.metrics["SSE" , "n11"]) / (brnn.metrics["SST" , "n11"]))
brnn.metrics ["R2" , "n12"] <- 1 - ((brnn.metrics["SSE" , "n12"]) / (brnn.metrics["SST" , "n12"]))
brnn.metrics ["R2" , "n13"] <- 1 - ((brnn.metrics["SSE" , "n13"]) / (brnn.metrics["SST" , "n13"]))
brnn.metrics ["R2" , "n14"] <- 1 - ((brnn.metrics["SSE" , "n14"]) / (brnn.metrics["SST" , "n14"]))
brnn.metrics ["R2" , "n15"] <- 1 - ((brnn.metrics["SSE" , "n15"]) / (brnn.metrics["SST" , "n15"]))

brnn.metrics ["R2" , "n16"] <- 1 - ((brnn.metrics["SSE" , "n16"]) / (brnn.metrics["SST" , "n16"]))
brnn.metrics ["R2" , "n17"] <- 1 - ((brnn.metrics["SSE" , "n17"]) / (brnn.metrics["SST" , "n17"]))
brnn.metrics ["R2" , "n18"] <- 1 - ((brnn.metrics["SSE" , "n18"]) / (brnn.metrics["SST" , "n18"]))
brnn.metrics ["R2" , "n19"] <- 1 - ((brnn.metrics["SSE" , "n19"]) / (brnn.metrics["SST" , "n19"]))
brnn.metrics ["R2" , "n20"] <- 1 - ((brnn.metrics["SSE" , "n20"]) / (brnn.metrics["SST" , "n20"]))

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




