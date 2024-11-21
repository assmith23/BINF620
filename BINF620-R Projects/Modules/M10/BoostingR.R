install.packages("adabag")
library(adabag)
library(tree)

data(iris)
iris.adaboost <- boosting(Species~., data=iris, boos=TRUE, mfinal=3)
iris.adaboost

## Data Vehicle (four classes) 
library(mlbench)
data(Vehicle)
l <- length(Vehicle[,1])
sub <- sample(1:l,2*l/3)
mfinal <- 3 
maxdepth <- 5

Vehicle.rpart <- rpart(Class~.,data=Vehicle[sub,],maxdepth=maxdepth)
Vehicle.rpart.pred <- predict(Vehicle.rpart,newdata=Vehicle[-sub, ],type="class")
tb <- table(Vehicle.rpart.pred,Vehicle$Class[-sub])
error.rpart <- 1-(sum(diag(tb))/sum(tb))
tb
error.rpart

Vehicle.adaboost <- boosting(Class ~.,data=Vehicle[sub, ],mfinal=mfinal, coeflearn="Zhu",
                             control=rpart.control(maxdepth=maxdepth))
Vehicle.adaboost.pred <- predict.boosting(Vehicle.adaboost,newdata=Vehicle[-sub, ])
Vehicle.adaboost.pred$confusion
Vehicle.adaboost.pred$error

#comparing error evolution in training and test set
errorevol(Vehicle.adaboost,newdata=Vehicle[sub, ])->evol.train
errorevol(Vehicle.adaboost,newdata=Vehicle[-sub, ])->evol.test

plot.errorevol(evol.test,evol.train)

install.packages("gbm")
install.packages("viridis")

library(gbm)
library(viridis)

# Simulate data
set.seed(101)  # for reproducibility
N <- 1000
X1 <- runif(N)
X2 <- 2 * runif(N)
X3 <- ordered(sample(letters[1:4], N, replace = TRUE), levels = letters[4:1])
X4 <- factor(sample(letters[1:6], N, replace = TRUE))
X5 <- factor(sample(letters[1:3], N, replace = TRUE))
X6 <- 3 * runif(N) 
mu <- c(-1, 0, 1, 2)[as.numeric(X3)]
SNR <- 10  # signal-to-noise ratio
Y <- X1 ^ 1.5 + 2 * (X2 ^ 0.5) + mu
sigma <- sqrt(var(Y) / SNR)
Y <- Y + rnorm(N, 0, sigma)
X1[sample(1:N,size=500)] <- NA  # introduce some missing values
X4[sample(1:N,size=300)] <- NA  # introduce some missing values
data <- data.frame(Y, X1, X2, X3, X4, X5, X6)

# Fit a GBM
set.seed(102)  # for reproducibility
gbm1 <- gbm(Y ~ ., data = data, var.monotone = c(0, 0, 0, 0, 0, 0),
            distribution = "gaussian", n.trees = 100, shrinkage = 0.1,             
            interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)  

# Check performance using the out-of-bag (OOB) error; the OOB error typically
# underestimates the optimal number of iterations
best.iter <- gbm.perf(gbm1, method = "OOB")
print(best.iter)

# Check performance using the 50% heldout test set
best.iter <- gbm.perf(gbm1, method = "test")
print(best.iter)

# Check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm1, method = "cv")
print(best.iter)

# Plot relative influence of each variable
par(mfrow = c(1, 2))
summary(gbm1, n.trees = 1)          # using first tree
summary(gbm1, n.trees = best.iter)  # using estimated best number of trees

# Compactly print the first and last trees for curiosity
print(pretty.gbm.tree(gbm1, i.tree = 1))
print(pretty.gbm.tree(gbm1, i.tree = gbm1$n.trees))

# Simulate new data
set.seed(103)  # for reproducibility
N <- 1000
X1 <- runif(N)
X2 <- 2 * runif(N)
X3 <- ordered(sample(letters[1:4], N, replace = TRUE))
X4 <- factor(sample(letters[1:6], N, replace = TRUE))
X5 <- factor(sample(letters[1:3], N, replace = TRUE))
X6 <- 3 * runif(N) 
mu <- c(-1, 0, 1, 2)[as.numeric(X3)]
Y <- X1 ^ 1.5 + 2 * (X2 ^ 0.5) + mu + rnorm(N, 0, sigma)
data2 <- data.frame(Y, X1, X2, X3, X4, X5, X6)

# Predict on the new data using the "best" number of trees; by default,
# predictions will be on the link scale
Yhat <- predict(gbm1, newdata = data2, n.trees = best.iter, type = "link")

# least squares error
print(sum((data2$Y - Yhat)^2))

# Construct univariate partial dependence plots
plot(gbm1, i.var = 1, n.trees = best.iter)
plot(gbm1, i.var = 2, n.trees = best.iter)
plot(gbm1, i.var = "X3", n.trees = best.iter)  # can use index or name

# Construct bivariate partial dependence plots
plot(gbm1, i.var = 1:2, n.trees = best.iter)
plot(gbm1, i.var = c("X2", "X3"), n.trees = best.iter)
plot(gbm1, i.var = 3:4, n.trees = best.iter)

# Construct trivariate partial dependence plots
plot(gbm1, i.var = c(1, 2, 6), n.trees = best.iter, 
     continuous.resolution = 20)
plot(gbm1, i.var = 1:3, n.trees = best.iter)
plot(gbm1, i.var = 2:4, n.trees = best.iter)
plot(gbm1, i.var = 3:5, n.trees = best.iter)

# Add more (i.e., 100) boosting iterations to the ensemble
gbm2 <- gbm.more(gbm1, n.new.trees = 100, verbose = FALSE)

install.packages("dismo")
library(dismo)
data(Anguilla_train)
head(Anguilla_train)

#fitting the model
angaus.tc5.lr01 <- gbm.step(data=Anguilla_train, gbm.x = 3:13, gbm.y = 2,
                            family = "bernoulli", tree.complexity = 5,
                            learning.rate = 0.01, bag.fraction = 0.5)
names(angaus.tc5.lr01)
summary(angaus.tc5.lr01)

angaus.tc5.lr005 <- gbm.step(data=Anguilla_train, gbm.x = 3:13, gbm.y = 2,
                             family = "bernoulli", tree.complexity = 5,
                             learning.rate = 0.005, bag.fraction = 0.5)
names(angaus.tc5.lr005)
summary(angaus.tc5.lr005)

angaus.simp <- gbm.simplify(angaus.tc5.lr005, n.drops = 5)
summary(angaus.simp)

angaus.tc5.lr005.simp <- gbm.step(Anguilla_train,
                                  gbm.x=angaus.simp$pred.list[[1]], gbm.y=2,
                                tree.complexity=5, learning.rate=0.005)
summary(angaus.tc5.lr005.simp)

gbm.plot(angaus.tc5.lr005, n.plots=11, plot.layout=c(4, 3), write.title = FALSE)
gbm.plot.fits(angaus.tc5.lr005)

find.int <- gbm.interactions(angaus.tc5.lr005)
gbm.perspec(angaus.tc5.lr005, 7, 1, y.range=c(15,20), z.range=c(0,0.6))

data(Anguilla_test)
library(gbm)
preds <- predict.gbm(angaus.tc5.lr005, Anguilla_test,
                     n.trees=angaus.tc5.lr005$gbm.call$best.trees, type="response")
calc.deviance(obs=Anguilla_test$Angaus_obs, pred=preds, calc.mean=TRUE)
## [1] 0.7417041
d <- cbind(Anguilla_test$Angaus_obs, preds)
pres <- d[d[,1]==1, 2]
abs <- d[d[,1]==0, 2]
e <- evaluate(p=pres, a=abs)
e

angaus.5000 <- gbm.fixed(data=Anguilla_train, gbm.x=3:13, gbm.y=2,
                         learning.rate=0.005, tree.complexity=5, n.trees=5000)
## [1] fitting gbm model with a fixed number of 5000 trees for Angaus
## [1] total deviance = 1006.33
## [1] residual deviance = 203.73
tree.list <- seq(100, 5000, by=100)
pred <- predict.gbm(angaus.5000, Anguilla_test, n.trees=tree.list, "response")

angaus.pred.deviance <- rep(0,50)
for (i in 1:50) {
  angaus.pred.deviance[i] <- calc.deviance(Anguilla_test$Angaus_obs,
                                           pred[,i], calc.mean=TRUE)
}
plot(tree.list, angaus.pred.deviance, ylim=c(0.7,1), xlim=c(-100,5000),
     type='l', xlab="number of trees", ylab="predictive deviance",
     cex.lab=1.5)
data(Anguilla_grids)
plot(Anguilla_grids)

Method <- factor('electric', levels = levels(Anguilla_train$Method))
add <- data.frame(Method)
p <- predict(Anguilla_grids, angaus.tc5.lr005, const=add,
             n.trees=angaus.tc5.lr005$gbm.call$best.trees, type="response")
p <- mask(p, raster(Anguilla_grids, 1))
plot(p, main='Angaus - BRT prediction')
