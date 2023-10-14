#=============================================================#
require (caret)
require (kernlab)
#install.packages("rBayesianOptimization")
require (rBayesianOptimization)
require (dplyr)
#=============================================================#

set.seed (69)
#=============================================================#
drying <- read.csv('Drying.new.csv')
drying$PR [drying$PR == 1505] <- 1
drying$PR [drying$PR == 1005] <- 2
drying$PR [drying$PR == 1002] <- 3

drying$PR <- as.integer(drying$PR)

drying <-  drying [sample (nrow (drying)) , ]

set.seed(69)
indx_partition <- createDataPartition(drying$DR , p = 0.8 , list = F)
trainData <- drying [indx_partition , ]
testData <- drying [-indx_partition , ]

rand_ctrl <- trainControl(method = "repeatedcv" , repeats = 5 ,
                          search = "random")
rand_search <- train (MR ~ Time + VP + DT + PR , 
                      data = trainData ,
                      method = "svmRadial" ,
                      # create 20 random parameter values 
                      tuneLength = 20,
                      metric = "RMSE" ,
                      preProc = c("center" , "scale") ,
                      trControl = rand_ctrl)
rand_search

ggplot (rand_search , color = "blue") + scale_x_log10() + scale_y_log10() + theme_bw()
getTrainPerf(rand_search)

## Define the resampling method

ctrl <- trainControl(method = "repeatedcv", repeats = 5)

svm_fit_bayes <- function (logC , logSigma) {
  ## Use the same model code but for a single (C, sigma) pair.
  txt <- capture.output(
    mod <- train (MR ~ Time + VP + DT + PR , 
                  data = trainData ,
                  method = "svmRadial" ,
                  # create 20 random parameter values 
                  metric = "RMSE" ,
                  preProc = c("center" , "scale") ,
                  trControl = ctrl,
                  tuneGrid = data.frame (C = exp (logC) , sigma = exp(logSigma)))
  )
  
  list (Score = - getTrainPerf(mod) [ , "TrainRMSE"] , Pred =0)
}

lower_bounds <- c(logC = -5 , logSigma = -9)
upper_bounds <- c (logC = 20 , logSigma = -0.75)
bounds <- list (logC = c(lower_bounds[1], upper_bounds[1]),
                logSigma = c(lower_bounds[2], upper_bounds[2]))

initial_grid <- rand_search$results[, c("C", "sigma", "RMSE")]
initial_grid$C <- log(initial_grid$C)
initial_grid$sigma <- log(initial_grid$sigma)
initial_grid$RMSE <- -initial_grid$RMSE
names(initial_grid) <- c("logC", "logSigma", "Value")



set.seed(69)
ba_search <- BayesianOptimization(svm_fit_bayes ,
                                  bounds = bounds ,
                                  init_grid_dt = initial_grid ,
                                  init_points = 0,
                                  n_iter = 30 ,
                                  acq = "ucb" ,
                                  kappa = 1,
                                  eps = 0.0,
                                  verbose = T)

final_search <- train (MR ~ Time + VP + DT + PR , 
                       data = trainData ,
                       method = "svmRadial" ,
                       # create 20 random parameter values 
                       tuneGrid = data.frame(C = exp(ba_search$Best_Par["logC"]) ,
                                             sigma = exp (ba_search$Best_Par["logSigma"])),
                       metric = "RMSE" ,
                       preProc = c("center" , "scale") ,
                       trControl = ctrl)

compare_models(final_search , rand_search)

postResample(predict(rand_search , testData) , testData$MR)
postResample(predict(final_search , testData) , testData$MR)

predictedMR <- predict (final_search , testData)

results <- cbind(testData$Time , testData$VP , testData$MR , predictedMR)
colnames(results) <- c ('DryingTime' , 'VacuumPressure' , 'ActualMR' , 'PredictedMR')
results <- data.frame(results)


# t test

ActualMR <- results$ActualMR
PredictedMR <- results$PredictedMR

my_data <- data.frame(
  group = rep(c("ActualMR" , "PredictedMR") , each = 10) ,
  MR = c(ActualMR , PredictedMR)
)

my_data
str (my_data)
# Computing summary statistics by groups
group_by (my_data , group) %>% 
  summarise (
    count = n(),
    mean = mean (MR , na.rm = T),
    sd = sd (MR , na.rm = T)
  )

#if (!require (devtools)) install.packages("devtools")
install.packages("ggpubr")
require (ggpubr)

ggboxplot (my_data , x = "group" , y = "MR" ,
           color = "group" , pallete = "paired" ,
           order = c ("ActualMR" , "PredictedMR") ,
           ylab = "Moisture Ratio" , xlab = "Groups")


install.packages("PairedData")
require (PairedData)

ActualMR <- subset(my_data,  group == "ActualMR", MR,
                   drop = TRUE)
PredictedMR <- subset(my_data,  group == "PredictedMR", MR,
                      drop = TRUE)

pd <- paired(ActualMR , PredictedMR)
plot (pd , type = "profile") + theme_bw()

# check normality
#Null hypothesis: the data are normally distributed
#Alternative hypothesis: the data are not normally distributed

# compute the difference
d <- with (my_data , 
           MR[group == "ActualMR"] - MR [group == "PredictedMR"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # p-value = 0.08073

# if p > 0.05 , alternate hypothesis is rejected
#From the output, the p-value is more than the significance level 0.05 
# implying that the distribution of the differences (d) 
# are not significantly different from normal distribution. 
# In other words, we can assume the normality.

# if the data are not normally distributed, 
# it’s recommended to use the non parametric paired two-samples Wilcoxon test.

# Paired sample wilcox test method 1
res <- wilcox.test(ActualMR , PredictedMR , paired = T)
res

# Paired sample wilcox test method 2
res <- wilcox.test(MR ~ group , data = my_data , paired = T)
res

res$p.value # 0.1501
# the two methods give the same results.

# The p-value of the test is 0.1501, which is more than the significance level alpha = 0.05
# median MR from experiment is not significantly different from the MR predicted form linear regression with a p-value = 0.2120981

# checking for paired t test 
res <- t.test(ActualMR , PredictedMR , paired = T)
res

# t is the t-test statistic value (t = 1.2588,)
# df is the degrees of freedom (df= 59)
# p-value is the significance level of the t-test (p-value = 0.213)
# conf.int is the confidence interval (conf.int) of the mean differences at 95% (conf.int = [ -0.03102935 , 0.13629397])
# sample estimates is the mean differences between pairs (mean = 0.05263231)

# The p-value of the test is 0.213 , which is greater than the significance level alpha = 0.05
# Therefore accept null hypothesis, 
# and conclude that the average MR from experiments is not significantly different from the average MR predicted with a p value = 0.3338 


# F test is very sensitive to departure from the normal assumption
# If the sample size is large enough (n > 30), we can ignore the distribution of the data and use parametric tests.
# The central limit theorem tells us that no matter what distribution things have, the sampling distribution tends to be normal if the sample is large enough (n > 30).

# however chcenking for nornality
# 1 Density plot
#  density plot provides a visual judgment about whether the distribution is bell shaped.
ggdensity (my_data$MR , main = "Density plot of Moisture Ratio" , 
           xlab = "Moisture Ratio")

# 2 Q-Q plot

# Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. A 45-degree reference line is also plotted.
ggqqplot(my_data$MR)

# or
require (car)
qqPlot (my_data$MR)

# As all the points fall approximately along this reference line, we can assume normality.


# F test
res.ftest <- var.test(MR ~ group , data = my_data)
res.ftest

# The p-value of F-test is p = 0.8272 , greater than the significance level 0.05.
# In conclusion, there is no significant difference between the two variances.
# 95% CI [ 0.6324151 1.7724812]



# Levene's test
# Levene’s test is an alternative to Bartlett’s test when the data is not normally distributed.
leveneTest(MR ~ group , data = my_data) # p value = 0.7993
