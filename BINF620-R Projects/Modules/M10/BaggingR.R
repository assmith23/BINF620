X <- as.data.frame(matrix(rnorm(1000), ncol=10))
y <- factor(ifelse(apply(X, 1, mean) > 0, 1, 0))
learn <- cbind(y, X)

mt <- bagging(y ~., data=learn, coob=TRUE)
mt

X <- as.data.frame(matrix(rnorm(1000), ncol=10))
y <- factor(ifelse(apply(X, 1, mean) > 0, 1, 0))

cls <- predict(mt, newdata=X)

cat("Misclass error est: ", mean(y != cls), "")
cat("Misclass error oob: ", mt$err, "")

X <- as.data.frame(matrix(rnorm(1000), ncol=10))
y <- apply(X, 1, mean) + rnorm(nrow(X))

learn <- cbind(y, X)

mt <- bagging(y ~., data=learn, coob=TRUE)
mt

X <- as.data.frame(matrix(rnorm(1000), ncol=10))
y <- apply(X, 1, mean) + rnorm(nrow(X))

haty <- predict(mt, newdata=X)

cat("MSE error: ", mean((haty - y)^2) , "")

install.packages(ipred)
library(ipred)
library(tree)
library(mlbench)

data(BreastCancer)
BreastCancer$Id <- NULL

# Test set error bagging (nbagg = 50): 3.7\% (Breiman, 1998, Table 5)

modelBreast<-bagging(Class ~ Cl.thickness + Cell.size 
        + Cell.shape + Marg.adhesion
        + Epith.c.size + Bare.nuclei
        + Bl.cromatin + Normal.nucleoli
        + Mitoses, data=BreastCancer, coob=TRUE)
summary(modelBreast)


require(data.table)
library(rpart)
require(ggplot2)
set.seed(456)
##Reading data
bagging_data=data.table(airquality)
ggplot(bagging_data,aes(Wind,Ozone))+geom_point()+ggtitle("Ozone vs wind speed")
data_test=na.omit(bagging_data[,.(Ozone,Wind)])
##Training data
train_index=sample.int(nrow(data_test),size=round(nrow(data_test)*0.8),replace = F)
data_test[train_index,train:=TRUE][-train_index,train:=FALSE]
##Model without bagging
no_bag_model=rpart(Ozone~Wind,data_test[train_index],control=rpart.control(minsplit=6))
result_no_bag=predict(no_bag_model,bagging_data)
##Training of the bagged model
n_model=100
bagged_models=list()
for (i in 1:n_model)
{
  new_sample=sample(train_index,size=length(train_index),replace=T)
  bagged_models=c(bagged_models,list(rpart(Ozone~Wind,data_test[new_sample],control=rpart.control(minsplit=6))))
}
##Getting estimate from the bagged model
bagged_result=NULL
i=0
for (from_bag_model in bagged_models)
{
  if (is.null(bagged_result))
    bagged_result=predict(from_bag_model,bagging_data)
  else
    bagged_result=(i*bagged_result+predict(from_bag_model,bagging_data))/(i+1)
  i=i+1
}
##Plot
require(ggplot2)
gg=ggplot(data_test,aes(Wind,Ozone))+geom_point(aes(color=train))
for (tree_model in bagged_models[1:100])
{
  prediction=predict(tree_model,bagging_data)
  data_plot=data.table(Wind=bagging_data$Wind,Ozone=prediction)
  gg=gg+geom_line(data=data_plot[order(Wind)],aes(x=Wind,y=Ozone),alpha=0.2)
}
data_bagged=data.table(Wind=bagging_data$Wind,Ozone=bagged_result)
gg=gg+geom_line(data=data_bagged[order(Wind)],aes(x=Wind,y=Ozone),color='green')
data_no_bag=data.table(Wind=bagging_data$Wind,Ozone=result_no_bag)
gg=gg+geom_line(data=data_no_bag[order(Wind)],aes(x=Wind,y=Ozone),color='red')
gg

suppressMessages(library(caret))

head(airquality)
index = createDataPartition(y=airquality$Temp, p=0.7, list=FALSE)

train = airquality[index,]
test = airquality[-index,]

train.predictors = data.frame(ozone = train$Ozone)
train.temperature = train$Temp

test.predictors = data.frame(ozone = test$Ozone)
test.temperature = test$Temp
# fit the model
treebag = bag(train.predictors, train.temperature, B=100,
              bagControl = bagControl(fit = ctreeBag$fit,
                                      predict = ctreeBag$pred,
                                      aggregate = ctreeBag$aggregate),
              trControl = trainControl(method = "oob"))

treebag

#in above model fitting we have used OOB approach for estimating test-error instead of CV. when you have large dataset, 
#OOB (out-of-bag) is better due to CV's computationally expensive.

temp.pred = predict(treebag, test.predictors)
MSE = mean(test.temperature - temp.pred)^2
model.SE = sqrt(MSE)
model.SE

# original observed data
plot(test.predictors$ozone, test.temperature, col='lightgrey', pch=19) 
# predicted temperature from one model
points(test.predictors$ozone, predict(treebag$fits[[1]]$fit, test.predictors), pch=19, col="red") 
# predicted temperature from 10 model's average
points(test.predictors$ozone, predict(treebag, test.predictors), pch=19, col="blue") 

ctreeBag$fit

ctreeBag$pred

ctreeBag$aggregate
