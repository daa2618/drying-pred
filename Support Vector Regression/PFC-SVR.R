#=============================================================#
# loading requird packages
require (ggplot2)
require(corrplot)
require (corrgram)
require (tidyverse)
require(dplyr)
require(car)
require(e1071)
require (caret)
#=============================================================#
# data
set.seed (69)
#=============================================================#

drying <- read.csv('Drying.new.csv')
drying$PR [drying$PR == 1505] <- 1
drying$PR [drying$PR == 1005] <- 2
drying$PR [drying$PR == 1002] <- 3

drying$PR <- as.integer(drying$PR)
#=============================================================#
drying <-  drying [sample (nrow (drying)) , ]
#=============================================================#

#drying.shuffled <- drying [sample (nrow (drying)) , ]
#=============================================================#

pf <- data.frame(matrix(vector(), 9, 5, dimnames=list(c("TR-10", "TR-20", "TR-30" ,
                                                        "TR-40" , "TR-50" , "TR-60" ,
                                                        "TR-70" , "TR-80" , "TR-90"),
                                                      c("MSE","RMSE","SSE","SST","R2"))),
                 stringsAsFactors=F)

pfc <- 0
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)
training_data_percentages

# 

for (t in training_data_percentages) {
  pfc <- pfc + 1
  
  indx_partition <- createDataPartition(drying$DR , p = t , list = F)
  trainData <- drying [indx_partition , ]
  testData <- drying [-indx_partition , ]
  
  # model building
  
  model <- svm (MR ~ Time + VP + DT + PR , data = trainData)
  predictedMR <- predict (model, newdata = testData)
  
  pf [pfc , "MSE"] <- mean ((predictedMR - testData$MR)^2)
  pf [pfc , "RMSE"] <- (pf [pfc , "MSE"]) ^ 0.5
  pf [pfc , "SSE"] <- sum ((testData$MR - predictedMR) ^ 2)
  pf [pfc , "SST"] <- sum ((testData$MR - mean (trainData$MR)) ^ 2)
  pf [pfc , "R2"] <- 1 - ((pf [pfc , "SSE"]) / (pf [pfc , "SST"]))
  
}

pf
#write.csv (pf , 'performance metrics10.csv')
# Lowest RMSE and highest R 2 occured when 70% is assigned for training and 30% for testing

# tuning the model
for (t in training_data_percentages) {
  pfc <- pfc + 1
  
  indx_partition <- createDataPartition(drying$DR , p = t , list = F)
  trainData <- drying [indx_partition , ]
  testData <- drying [-indx_partition , ]
  
  # model building
  
  model.tune <- tune(svm , MR ~ Time + VP + DT + PR , data = trainData ,
                     ranges = list (epsilon = seq(0 , 1 , 0.1) , cost = 2 ^ (2:9)))
  print (model.tune) # prints mse values
  
  plot(model.tune)
  
  #pf [pfc , "MSE"] <- mean ((predictedMR - testData$MR)^2)
  #pf [pfc , "RMSE"] <- (pf [pfc , "MSE"]) ^ 0.5
  #pf [pfc , "SSE"] <- sum ((testData$MR - predictedMR) ^ 2)
  #pf [pfc , "SST"] <- sum ((testData$MR - mean (trainData$MR)) ^ 2)
  #pf [pfc , "R2"] <- 1 - ((pf [pfc , "SSE"]) / (pf [pfc , "SST"]))
  
}

# tuning the model

pf2 <- data.frame(matrix(vector(), 9, 5, dimnames=list(c("TR-10", "TR-20", "TR-30" ,
                                                         "TR-40" , "TR-50" , "TR-60" ,
                                                         "TR-70" , "TR-80" , "TR-90"),
                                                       c("MSE","RMSE","SSE","SST","R2"))),
                  stringsAsFactors=F)

pfc2 <- 0

for (t in training_data_percentages) {
  pfc2 <- pfc2 + 1
  
  indx_partition <- createDataPartition(drying$DR , p = t , list = F)
  trainData <- drying [indx_partition , ]
  testData <- drying [-indx_partition , ]
  
  # model building
  
  model.tune <- tune(svm , MR ~ Time + VP + DT + PR , data = trainData ,
                     ranges = list (epsilon = seq(0 , 0.2 , 0.01) , cost = 2 ^ (2:9)))
  print (model.tune) # prints mse values
  
  plot(model.tune)
  
  model.tuned <- model.tune$best.model
  predictedMR.tune <- predict (model.tuned , newdata = testData)
  
  pf2 [pfc2 , "MSE"] <- mean ((predictedMR.tune - testData$MR)^2)
  pf2 [pfc2 , "RMSE"] <- (pf2 [pfc2 , "MSE"]) ^ 0.5
  pf2 [pfc2 , "SSE"] <- sum ((testData$MR - predictedMR.tune) ^ 2)
  pf2 [pfc2 , "SST"] <- sum ((testData$MR - mean (trainData$MR)) ^ 2)
  pf2 [pfc2 , "R2"] <- 1 - ((pf2 [pfc2 , "SSE"]) / (pf2 [pfc2 , "SST"]))
  
}

pf
pf2
# EXTRACTING THE TRAINING AND TEST DATA FOR 60% TRANING
#set.seed (123)
indx_partition60 <- createDataPartition(drying$DR , p = 0.6 , list = F)
trainData60 <- drying [indx_partition60 , ]
testData60 <- drying [-indx_partition60 , ]

write.csv (trainData60 , 'trainData60.csv')
write.csv (testData60 , 'testData60.csv')

# scaling the data
maxs.train60 <- apply (trainData60 , 2 , max)
mins.train60 <- apply (trainData60 , 2 , min)
maxs.test60 <- apply (testData60 , 2, max)
mins.test60 <- apply (testData60 , 2 , min)
trainData60.scaled <- scale (trainData60 , center = mins.train60 , scale = maxs.train60 - mins.train60)
testData60.scaled <- scale (testData60 , center = mins.test60 , scale = maxs.test60 - mins.test60)

# model building
model60 <- neuralnet(DR ~ ., data = trainData60.scaled , hidden = c(5,3) , linear.output = T)
plot (model60)

#undo the scaling
unnormalize <- function (x) {
  return ((x * (max(testData$DR)) - min (testData$DR)) + min (testData$DR))
}

# getting predictions
pred60 <- neuralnet :: compute (model60 , testData60.scaled)

predictedDR60 <- pred60$net.result
predictedDR60 <- unnormalize(predictedDR60)

results60 <- cbind (testData60$Time , predictedDR60 , testData60$DR)
colnames(results60) <- c ('DryingTime' , 'PredictedDR' , 'ActualDR')
results60 <- as.data.frame(results60)

df60 <- results60 %>% gather (key = "variable" , value = "value" , - DryingTime)
head (df60)

ggplot (df60 , aes (x = DryingTime , y = value)) +
  geom_line (aes(color = variable , linetype = variable)) +
  scale_color_manual(values = c ("darkred" , "steelblue")) + 
  xlab ('Drying Time (min)') + ylab ('Drying Rate (g/g.dm)') + theme_bw() +
  ggtitle('Drying Rates Predicted by ANN') + stat_smooth()

ggplot (df60 , aes (x = DryingTime , y = value)) +
  geom_point (aes(color = variable , linetype = variable)) +
  scale_color_manual(values = c ("darkred" , "steelblue")) + 
  xlab ('Drying Time (min)') + ylab ('Drying Rate (g/g.dm)') + theme_bw() +
  ggtitle('Drying Rates Predicted by ANN') + stat_smooth()

mse60 <- mean ((testData60$DR - predictedDR60) ^ 2) # 1.294299e-05
rmse60 <- mse60^0.5 # 0.003597636
sse60 <- sum ((testData60$DR - predictedDR60) ^ 2)  # 0.001553158
sst60 <- sum ((testData60$DR - mean (trainData60$DR)) ^ 2) # 0.008741237
r2.60 <- 1 - (sse60 / sst60) # 0.8223183
#pf






