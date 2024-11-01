#https://rich-d-wilkinson.github.io/docs/Teaching/G12SMM/CS9_ModelSelection_Prostate.html
#Ridge Regression for prost data
install.packages("C:/Users/zugui/Downloads/ElemStatLearn_2015.6.26.1.tar.gz", repos = NULL, type = "source")
library(ElemStatLearn)
data(prostate)
str(prostate)

library(dplyr)
prostTrain <- filter(prostate, train)
prostTest <- filter(prostate, !train)
prostTrain <-select(prostTrain, -train)
prostTest <-select(prostTest, -train)


library(glmnet)
x =   select(prostTrain, c(lcavol, lweight, age, lbph, svi, lcp, gleason, pgg45))
x=as.matrix(x)
y =   select(prostTrain, lpsa)
y=as.matrix(y)
ridge=glmnet(x,y, alpha=0)
ridge.cv=cv.glmnet(x,y, alpha=0)
ridge.cv$lambda.min

ridge.cv$lambda.1se

plot(ridge.cv)

xnew = select(prostTest,  c(lcavol, lweight, age, lbph, svi, lcp, gleason, pgg45))
xnew = as.matrix(xnew)
ridge.prediction1 = predict(ridge.cv, xnew, s = "lambda.1se")
ridge.prediction2 = predict(ridge.cv, xnew, s = "lambda.min")
sumridge1=sum((ridge.prediction1 - select(prostTest, lpsa))^2)
mse_ridge1 <-sumridge1/length(ridge.prediction1)
#mse_ridge1 <-  mean((ridge.prediction1 - select(prostTest, lpsa))^2)
#mse_ridge2 <-  mean((ridge.prediction2 - select(prostTest, lpsa))^2)

sumridge2=sum((ridge.prediction2 - select(prostTest, lpsa))^2)
mse_ridge2 <-sumridge1/length(ridge.prediction2)

mse_ridge1

mse_ridge2




# load packages
library(caret)
library(mlbench)
library(glmnet)
# Load the dataset
data(PimaIndiansDiabetes)
# train
set.seed(7)
trainControl <- trainControl(method="cv", number=5)
fit.glmnet <- train(diabetes~., data=PimaIndiansDiabetes, method="glmnet",
                    metric="Accuracy", preProcess=c("center", "scale"), trControl=trainControl)
# summarize fit
print(fit.glmnet)

#install.packages(c("mplot","ISLR","glmnet","MASS","readr","dplyr"))

url = "https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data"
dat = readr::read_csv(url,col_names = FALSE)

# View(dat)
dim(dat)
dat = dplyr::rename(dat, y = X35)
# names(dat)[35] = "y" # an alternative
dat$y = as.factor(dat$y)

x = as.matrix(dat[,-c(2,35)])
dim(x)
y = dat$y
is.factor(y)
table(y)

set.seed(1)
lasso.cv = cv.glmnet(x, y, family = "binomial", type.measure = "class")
c(lasso.cv$lambda.min,lasso.cv$lambda.1se)
round(log(c(lasso.cv$lambda.min,lasso.cv$lambda.1se)),2)
plot(lasso.cv)

n = length(y)
n
set.seed(2015)
train = sample(1:n, 2*n/3) # by default replace=FALSE in sample()
length(train)/n
max(table(train)) # if 1, then no ties in the sampled observations
test = (1:n)[-train]

lasso.cv = cv.glmnet(x[train,], y[train], family="binomial", type.measure="class")
lasso.fit = glmnet(x[train,], y[train], family="binomial")
# calculate predicted probability to be 'g' - second level of categorical y - through type="response"
lasso.predict.prob = predict(lasso.fit, s=lasso.cv$lambda.min, newx=x[test,], type="response") # check help(predict.glmnet) 
predict.g = lasso.predict.prob > 0.5
table(y[test],predict.g)
sum(table(y[test],predict.g)[c(2,3)])/length(test)
# calculate predicted y through type="class"
lasso.predict.y = predict(lasso.fit, s=lasso.cv$lambda.min,
                          newx=x[test,], type="class") # check help(predict.glmnet) 
mean(lasso.predict.y!=y[test])

set.seed(300)
n = 300
p = 1000
p.nonzero = 10
x = matrix(rnorm(n*p),nrow=n,ncol=p)
b.nonzero = rexp(p.nonzero)*(sign(rnorm(p.nonzero)))
b.nonzero
beta = c(b.nonzero,rep(0,p-p.nonzero))
y = x%*%beta + rnorm(n)

train = sample(1:n,2*n/3)
x.train = x[train,]
x.test  = x[-train,]
y.train = y[train]
y.test  = y[-train]
#.Fit the lasso, elastic-net (with ??=0.5) and ridge regression.
lasso.fit = glmnet(x.train, y.train, family="gaussian", alpha=1)
ridge.fit = glmnet(x.train, y.train, family="gaussian", alpha=0)
enet.fit  = glmnet(x.train, y.train, family="gaussian", alpha=.5)

fit = list()
for (i in 0:10) {
  fit[[i+1]] = cv.glmnet(x.train, y.train, type.measure="mse", alpha=i/10,family="gaussian")
}
names(fit) = 0:10

par(mfrow=c(3,2))
plot(lasso.fit, xvar="lambda")
plot(fit[["10"]], main="lasso")

plot(ridge.fit, xvar="lambda")
plot(fit[["5"]], main="elastic-net")

plot(enet.fit, xvar="lambda")
plot(fit[["0"]], main="ridge")
#.Calculate the MSE on the test set.
yhat0  = predict(fit[["0"]], s=fit[["0"]]$lambda.1se, newx=x.test)
yhat1  = predict(fit[["1"]], s=fit[["1"]]$lambda.1se, newx=x.test)
yhat2  = predict(fit[["2"]], s=fit[["2"]]$lambda.1se, newx=x.test)
yhat3  = predict(fit[["3"]], s=fit[["3"]]$lambda.1se, newx=x.test)
yhat4  = predict(fit[["4"]], s=fit[["4"]]$lambda.1se, newx=x.test)
yhat5  = predict(fit[["5"]], s=fit[["5"]]$lambda.1se, newx=x.test)
yhat6  = predict(fit[["6"]], s=fit[["6"]]$lambda.1se, newx=x.test)
yhat7  = predict(fit[["7"]], s=fit[["7"]]$lambda.1se, newx=x.test)
yhat8  = predict(fit[["8"]], s=fit[["8"]]$lambda.1se, newx=x.test)
yhat9  = predict(fit[["9"]], s=fit[["9"]]$lambda.1se, newx=x.test)
yhat10 = predict(fit[["10"]], s=fit[["10"]]$lambda.1se, newx=x.test)

mse0  = mean((y.test - yhat0)^2)
mse1  = mean((y.test - yhat1)^2)
mse2  = mean((y.test - yhat2)^2)
mse3  = mean((y.test - yhat3)^2)
mse4  = mean((y.test - yhat4)^2)
mse5  = mean((y.test - yhat5)^2)
mse6  = mean((y.test - yhat6)^2)
mse7  = mean((y.test - yhat7)^2)
mse8  = mean((y.test - yhat8)^2)
mse9  = mean((y.test - yhat9)^2)
mse10 = mean((y.test - yhat10)^2)
c(mse0,mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10)
