install.packages("deepnet")
install.packages("neuralnet")
install.packages("kerasR")
install.packages("xlsx")
install.packages("expss")

library(deepnet)
library(neuralnet)
library(keras)
library(kerasR)
library(xlsx)
library(expss)

library(mlbench)
data("BreastCancer")
#Clean off rows with missing data
str(BreastCancer)

y = as.matrix(BreastCancer[,11])
y[which(y=="benign")] = 0
y[which(y=="malignant")] = 1
y = as.numeric(y)
x = as.numeric(as.matrix(BreastCancer[,2:10]))
x = matrix(as.numeric(x),ncol=9)

nn <- nn.train(x, y, hidden = c(5))
yy = nn.predict(nn, x)
print(head(yy))

yhat = matrix(0,length(yy),1)
yhat[which(yy > mean(yy))] = 1
yhat[which(yy <= mean(yy))] = 0
cm = table(y,yhat)
print(cm)

print(sum(diag(cm))/sum(cm))

df = data.frame(cbind(x,y))
names(df) = c("thickness", "cell_size", "cell_shape", "adhesion",
                  "epithelial_size", "bare_nuclei", "bland_cromatin", "normal_nucleoli", "mitoses",
                  "class") 

nn = neuralnet(class~.,data=df,hidden = 5)
nn = neuralnet(y~V1+V2+V3+V4+V5+V6+V7+V8+V9,data=df,hidden = 5)
yy = nn$net.result[[1]]
yhat = matrix(0,length(y),1)
yhat[which(yy > mean(yy))] = 1
yhat[which(yy <= mean(yy))] = 0
print(table(y,yhat))
plot(nn)

df = na.omit(df)
set.seed(80817) # Set a random seed so that repeated analyses have the same outcome. Seeds are saved on thr PC only and will not allow analyses to be repeated precicesly on other machines.
index = 1:nrow(df) #Create an index vector with as many sequential variables as there are rows in the cancer dataset.
testindex = sample(index, trunc(length(index)/3)) #Take a sample of 33.3% of the variables from the index vector.
testset = df[testindex, ] #Create a test (validation) dataset with 33.3$ of the data.
trainset = df[-testindex, ] #Create a trainig dataset with 66.6% of the data.
x_train = data.matrix(trainset[, 1:9]) # Take the features (x) from the training dataset.
y_train = as.numeric(trainset[, 10]) # Take the outcomes (y) from the training dataset.
x_test = data.matrix(testset[, 1:9]) # Take the features (x) from the testing/validation dataset.
y_test = as.numeric(testset[, 10]) # Take the outcomes (y) from the testing/validation dataset.

#install.packages("nnet") # Install latest verison of `nnet`. Only necessary once.
require(nnet) 
## Loading required package: nnet
nnet_model = nnet(x_train, y_train, size=5) #Fit a single-layer neural network to the data with 5 units

nnet_pred = round(predict(nnet_model, x_test, type="raw"),0) #Prediction vector for the neural network.
predictions = data.frame(nnet_pred) # Collate the prediction vectors into a data frame.
names(predictions) = c("nnet") #Name the columns of the dataframe.

confusionMatrix(as.factor(nnet_pred),as.factor(y_test)) # Create a confusion matrix for the neural network.

#install.packages("pROC") # Install the `pROC` package. Only necessary once.
require(pROC) # Load the caret package into this R session.

roc_nnet = roc(as.vector(y_test), as.vector(nnet_pred))

plot.roc(roc_nnet, ylim=c(0,1), xlim=c(1,0)) #Plot the ROC curves
lines(roc_nnet, col="green")
legend("bottomright", legend=c("Neural Net"), col=c("green"), lwd=2)

auc_nnet = auc(roc_nnet)#Calculate the area under the ROC curve

# The code below sets the values for the features to be evaluated by the trained and validated model.
thickness = 8
cell_size = 5
cell_shape = 8
adhesion = 5
epithelial_size = 5
bare_nuclei = 7
bland_cromatin = 9
normal_nucleoli = 8
mitoses = 3

set.seed(80817) 
new_pred_nnet = predict(nnet_model ,data.matrix(t(new_data)),type="raw")

print(new_pred_nnet) #Print the prediction for the new data from the nnet.


# libraries
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(caret))
install.packages("digest")

library(digest)
library(caret)

# set up data
id <- sample(rep(1:4, 2), 8)
X <- matrix(c(0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1), nrow = 4, byrow = FALSE)
X <- X[id,]
y <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1), nrow = 4)
y <- y[id,]

# activation function
# sigmoid
sigmoid <- function(x) return(1/(1+exp(-x)))
d.sigmoid <- function(x) return(x*(1-x))

# neural net function with 1 hidden layer - user specifies number of nodes
myNeuralNet <- function(X, y, hl, niters, learning.rate){
  
  # add in intercept
  X <- cbind(rep(1, nrow(X)), X)
  
  # set error array
  error <- rep(0, niters)
  
  # set up weights
  # the +1 is to add in the intercept/bias parameter
  W1 <- matrix(runif(ncol(X)*hl[1], -1, 1), nrow = ncol(X))
  W2 <- matrix(runif((hl[1]+1)*hl[2], -1, 1), nrow = hl[1]+1)
  W3 <- matrix(runif((hl[2]+1)*ncol(y), -1, 1), nrow = hl[2]+1)
  
  for(k in 1:niters){
    
    # calculate the hidden and output layers using X and hidden layer as inputs
    # hidden layer 1 and 2 have a column of ones appended for the bias term
    hidden1 <- cbind(matrix(1, nrow = nrow(X)), sigmoid(X %*% W1))
    hidden2 <- cbind(matrix(1, nrow = nrow(X)), sigmoid(hidden1 %*% W2))
    y_hat <- sigmoid(hidden2 %*% W3)
    
    # calculate the gradient and back prop the errors
    # see theory above
    y_hat_del <- (y-y_hat)*(d.sigmoid(y_hat))
    hidden2_del <- y_hat_del %*% t(W3)*d.sigmoid(hidden2)
    hidden1_del <- hidden2_del[,-1] %*% t(W2)*d.sigmoid(hidden1)
    
    # update the weights
    W3 <- W3 + learning.rate*t(hidden2) %*% y_hat_del
    W2 <- W2 + learning.rate*t(hidden1) %*% hidden2_del[,-1]
    W1 <- W1 + learning.rate*t(X) %*% hidden1_del[,-1]
    
    # storing error (MSE)
    error[k] <- 1/nrow(y)*sum((y-y_hat)^2)
    if((k %% (10^4+1)) == 0) cat("mse:", error[k], "\n")
  }
  
  # plot loss
  xvals <- seq(1, niters, length = 1000)
  print(qplot(xvals, error[xvals], geom = "line", main = "MSE", xlab = "Iteration"))
  
  return(y_hat)
}

# set parameters
hidden.layers <- c(6, 6)
iter <- 50000
lr <- 0.02

# run neural net
out <- myNeuralNet(X, y, hl = hidden.layers, niters= iter, learning.rate = lr)

pred <- apply(out, 1, which.max)
true <- apply(y, 1, which.max)
cbind(true, pred)

# on iris data set
data(iris)
Xiris <- as.matrix(iris[, -5])
yiris <- model.matrix(~ Species - 1, data = iris)
out.iris <- myNeuralNet(Xiris, yiris, hl = hidden.layers, niters = iter, learning.rate = lr)

labels <- c("setosa", "versicolor", "virginica")
pred.iris <- as.factor(labels[apply(out.iris, 1, which.max)])
confusionMatrix(table(iris$Species, pred.iris))

# comparing with the neuralnet package
suppressPackageStartupMessages(library(neuralnet))

df <- data.frame(X1 = X[,1], X2 = X[,2], X3 = X[,3], y1 = y[,1], y2 = y[,2])
nn.mod <- neuralnet(y1 + y2 ~ X1 + X2 + X3, data = df, hidden = hidden.layers, 
                    algorithm = "backprop", learningrate = lr, act.fct = "logistic")
nn.pred <- apply(nn.mod$net.result[[1]], 1, which.max)
cbind(true, nn.pred)

# and on the iris package
iris.df <- cbind(iris[,-5], setosa = yiris[,1], versicolor = yiris[,2], virginica = yiris[,3])
nn.iris <- neuralnet(setosa + versicolor + virginica ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width, 
                     data = iris.df, hidden = c(6, 6), algorithm = "backprop", learningrate = lr, act.fct = "logistic", 
                     linear.output = FALSE)
pred.iris <- labels[apply(nn.iris$net.result[[1]], 1, which.max)]
confusionMatrix(table(iris$Species, pred.iris))

plot(nn.iris)

#Using H2O
library(h2o)
library(h2o)
localH2O = h2o.init(ip="localhost", port = 54321, 
                    startH2O = TRUE, nthreads=-1)

train <- h2o.importFile("BreastCancer.csv")
test <- h2o.importFile("BreastCancer.csv")

train<-h2o.importFile("C:/Users/zugui/Desktop/work/UDFall2020/Module10/BreastCancer.csv")
test <-h2o.importFile("C:/Users/zugui/Desktop/work/UDFall2020/Module10/BreastCancer.csv")

y = names(train)[11]
x = names(train)[1:10]

train[,y] = as.factor(train[,y])
test[,y] = as.factor(train[,y])

model = h2o.deeplearning(x=x, 
                         y=y, 
                         training_frame=train, 
                         validation_frame=test, 
                         distribution = "multinomial",
                         activation = "RectifierWithDropout",
                         hidden = c(10,10,10,10),
                         input_dropout_ratio = 0.2,
                         l1 = 1e-5,
                         epochs = 50)

print(model)

install.packages("devtools")
require(devtools)
install.packages("reticulate")
library(reticulate)
install.packages("yaml")
library(yaml)
install.packages("tensorflow")
library(tensorflow)
install_github("rstudio/keras")
library(keras)

library(reticulate)
library(h2o)
install.packages("devtools")
library(devtools)
require(devtools)
install_github("rstudio/reticulate")
install_github("rstudio/tensorflow")
install_github("rstudio/keras")





library(reticulate)
library(h2o)
install.packages("devtools")
library(devtools)
require(devtools)
install_github("rstudio/reticulate")
install_github("rstudio/tensorflow")
install_github("rstudio/keras")
library(tensorflow)
library(kerasR)

library(tensorflow)
library(keras)

tf_train <- read.csv("BreastCancer.csv")
tf_test <- read.csv("BreastCancer.csv")

tf_train <- read.csv("C:/Users/zugui/Desktop/work/UDFall2020/Module10/BreastCancer.csv")
tf_test<- read.csv("C:/Users/zugui/Desktop/work/UDFall2020/Module10/BreastCancer.csv")


X_train = as.matrix(tf_train[,2:10])
X_test = as.matrix(tf_test[,2:10])
y_train = as.matrix(tf_train[,11])
y_test = as.matrix(tf_test[,11])

idx = which(y_train=="benign"); y_train[idx]=0; y_train[-idx]=1; y_train=as.integer(y_train)
idx = which(y_test=="benign"); y_test[idx]=0; y_test[-idx]=1; y_test=as.integer(y_test)

Y_train <- to_categorical(y_train,2)



library(mxnet)

library(h2o)
localH2O = h2o.init(ip="localhost", port = 54321, 
                    startH2O = TRUE)

## Import MNIST CSV as H2O
train <- h2o.importFile("train.csv")
test <- h2o.importFile("test.csv")

print(dim(train))
print(dim(test))



library(tensorflow)
library(keras)
library(kerasR)

devtools::install_github("rstudio/tensorflow")
devtools::install_github("rstudio/keras")
tensorflow::install_tensorflow()
tensorflow::tf_config()

reticulate::py_discover_config()
reticulate::use_condaenv("r-tensorflow")
reticulate::py_config()




tf_train <-read.csv("C:/Users/zugui/Desktop/work/UDFall2020/Module10/BreastCancer.csv")
tf_test <-read.csv("C:/Users/zugui/Desktop/work/UDFall2020/Module10/BreastCancer.csv")
#tf_train <- read.csv("BreastCancer.csv")
#tf_test <- read.csv("BreastCancer.csv")

X_train = as.matrix(tf_train[,2:10])
X_test = as.matrix(tf_test[,2:10])
y_train = as.matrix(tf_train[,11])
y_test = as.matrix(tf_test[,11])

idx = which(y_train=="benign"); y_train[idx]=0; y_train[-idx]=1; y_train=as.integer(y_train)
idx = which(y_test=="benign"); y_test[idx]=0; y_test[-idx]=1; y_test=as.integer(y_test)

Y_train <- to_categorical(y_train,2)

n_units = 512 

mod <- Sequential()
mod$add(Dense(units = n_units, input_shape = dim(X_train)[2]))
mod$add(LeakyReLU())
mod$add(Dropout(0.25))

mod$add(Dense(units = n_units))
mod$add(LeakyReLU())
mod$add(Dropout(0.25))

mod$add(Dense(units = n_units))
mod$add(LeakyReLU())
mod$add(Dropout(0.25))

mod$add(Dense(units = n_units))
mod$add(LeakyReLU())
mod$add(Dropout(0.25))

mod$add(Dense(units = n_units))
mod$add(LeakyReLU())
mod$add(Dropout(0.25))

mod$add(Dense(2))
mod$add(Activation("softmax"))

keras_compile(mod, loss = 'categorical_crossentropy', optimizer = RMSprop())

keras_fit(mod, X_train, Y_train, batch_size = 32, epochs = 15, verbose = 2, validation_split = 1.0)

#Validation
Y_test_hat <- keras_predict_classes(mod, X_test)
table(y_test, Y_test_hat)
print(c("Mean validation accuracy = ",mean(y_test == Y_test_hat)))

library(data.table)
train = as.data.frame(fread("train.csv"))
test = as.data.frame(fread("test.csv"))
#mnist <- load_mnist()
X_train <- as.matrix(train[,-1])
Y_train <- as.matrix(train[,785])
X_test <- as.matrix(test[,-1])
Y_test <- as.matrix(test[,785])
dim(X_train)

library(magrittr)
library(keras)
model <- keras_model_sequential() 
