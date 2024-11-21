install.packages("neuralnet")
install.packages("boot")
install.packages("plyr")
install.packages("matrixStats")

# Load libraries
library(boot)
library(plyr)
library(neuralnet)
library(matrixStats)

## Creating index variable
#https://www.analyticsvidhya.com/blog/2017/09/creating-visualizing-neural-network-in-r/
library(readxl)
# Read the Data
data = readr::read_csv("cereals.csv")
#data = read.csv("cereals.csv", header=T)
#cereals <- read.csv("C:/Users/zugui/Desktop/work/UDFall2020/Module10/cereals.csv")

# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample( seq_len (nrow ( data ) ), size = samplesize )

# Create training and test set
datatrain = data[index, ]
datatest = data[-index, ]

## Scale data for neural network
max = apply(data, 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
scaled <- as.data.frame(lapply(data, normalize))

## Fit neural network 

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
set.seed(2)
NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 3 , linear.output = T )
pred <- predict(NN, testNN)
table(testNN$rating, apply(pred, 1, which.max))

NN10 = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 10 , linear.output = T )

# plot neural network
plot(NN)
plot(NN10)

## Prediction using neural network
predict_testNN = compute(NN, testNN[,c(1:5)])
predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)

plot(datatest$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$rating - predict_testNN)^2) / nrow(datatest)) ^ 0.5

# Initialize variables
set.seed(50)
k = 100
RMSE.NN = NULL

List = list( )

# Fit neural network model within nested for loop
for(j in 10:65){
  for (i in 1:k) {
    index = sample(1:nrow(data),j )
    
    trainNN = scaled[index,]
    testNN = scaled[-index,]
    datatest = data[-index,]
    
    NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 3, linear.output= T)
    predict_testNN = compute(NN,testNN[,c(1:5)])
    predict_testNN = (predict_testNN$net.result*(max(data$rating)-min(data$rating)))+min(data$rating)
    
    RMSE.NN [i]<- (sum((datatest$rating - predict_testNN)^2)/nrow(datatest))^0.5
  }
  List[[j]] = RMSE.NN
}

Matrix.RMSE = do.call(cbind, List)
## Prepare boxplot
boxplot(Matrix.RMSE[,56], ylab = "RMSE", main = "RMSE BoxPlot (length of traning set = 65)")

## Variation of median RMSE 
med = colMedians(Matrix.RMSE)

X = seq(10,65)

plot (med~X, type = "l", xlab = "length of training set", ylab = "median RMSE", 
      main = "Variation of RMSE with length of training set")


#set up different number of hidden
set.seed(50)
k = 20
RMSE.NN = NULL

List = list( )

# Fit neural network model within nested for loop
for(j in 1:k){
    samplesize = 0.60 * nrow(data)
    index = sample( seq_len (nrow ( data ) ), size = samplesize )
    #index = sample(1:nrow(data),j )
    
    trainNN = scaled[index,]
    testNN = scaled[-index,]
    datatest = data[-index,]
    
    NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = j, linear.output= T)
    predict_testNN = compute(NN,testNN[,c(1:5)])
    predict_testNN = (predict_testNN$net.result*(max(data$rating)-min(data$rating)))+min(data$rating)
    
    RMSE.NN<- (sum((datatest$rating - predict_testNN)^2)/nrow(datatest))^0.5
  
  List[[j]] = RMSE.NN
}

Matrix.RMSE = do.call(cbind, List)
## Prepare boxplot
boxplot(Matrix.RMSE[,20], ylab = "RMSE", main = "RMSE BoxPlot (length of traning set = 65)")

## Variation of median RMSE 
med = colMedians(Matrix.RMSE)

X = seq(1,20)

plot (med~X, type = "l", xlab = "length of training set", ylab = "median RMSE", 
      main = "Variation of RMSE with length of training set")

