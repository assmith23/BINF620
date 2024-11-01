library(mlbench)
data(PimaIndiansDiabetes)
dim(PimaIndiansDiabetes)
levels(PimaIndiansDiabetes$diabetes)
head(PimaIndiansDiabetes)

library(e1071) #SVM
library(caret) #select tuning parameters
library(reshape2) #assist in creating boxplots
library(ggplot2) #create boxplots
library(kernlab) #assist with SVM feature selection
library(pROC)
pima.scale = as.data.frame(scale(PimaIndiansDiabetes[,-9]))
str(pima.scale)
pima.scale$diabetes = PimaIndiansDiabetes$diabetes

pima.scale.melt = melt(pima.scale, id.var="diabetes")
ggplot(data=pima.scale.melt, aes(x=diabetes, y=value)) +geom_boxplot()+facet_wrap(~variable, ncol=2)
cor(pima.scale[-9])
table(pima.scale$diabetes)

set.seed(123)
ind = sample(2, nrow(pima.scale), replace=TRUE, prob=c(0.6,0.4))
train = pima.scale[ind==1,]
test = pima.scale[ind==2,]
str(train)
str(test)

#SVM modelling
linear.tune = tune.svm(diabetes~., data=train, kernel="linear", cost=c(0.001, 0.01, 0.1, 1,5,10))
summary(linear.tune)

best.linear = linear.tune$best.model
tune.test = predict(best.linear, newdata=test)
table(tune.test, test$diabetes)

set.seed(123)
poly.tune = tune.svm(diabetes~., data=train, kernel="polynomial",
                     degree=c(3,4,5), coef0=c(0.1,0.5,1,2,3,4))
summary(poly.tune)

best.poly = poly.tune$best.model
poly.test = predict(best.poly, newdata=test)
table(poly.test, test$diabetes)

(163+51)/300

set.seed(123)
rbf.tune = tune.svm(diabetes~., data=train, kernel="radial",
                    gamma=c(0.1,0.5,1,2,3,4))
summary(rbf.tune)

best.rbf = rbf.tune$best.model
rbf.test = predict(best.rbf, newdata=test)
table(rbf.test, test$diabetes)
(164+64)/300

#https://rpubs.com/Kushan/296706
set.seed(123)
sigmoid.tune = tune.svm(diabetes~., data=train, kernel="sigmoid",
                        gamma=c(0.1,0.5,1,2,3,4), coef0=c(0.1,0.5,1,2,3,4))
summary(sigmoid.tune)

best.sigmoid = sigmoid.tune$best.model
sigmoid.test = predict(best.sigmoid, newdata=test)
table(sigmoid.test, test$diabetes)
(163+54)/300

confusionMatrix(sigmoid.test, test$diabetes, positive="pos")

confusionMatrix(rbf.test, test$diabetes, positive="pos")
confusionMatrix(tune.test, test$diabetes, positive="pos")

#feature selection for SVM
set.seed(123)
rfeCNTL = rfeControl(functions=lrFuncs, method="cv", number=10)
svm.features = rfe(train[,1:8], train[,9],sizes = c(8, 7, 6, 5, 4),
                   rfeControl = rfeCNTL, method = "svmLinear")

svm.features

svm.5 <- svm(diabetes~glucose+mass+pedigree+pressure+age, data=train, kernel="linear")
svm.5.predict <- predict(svm.5, newdata=test[c(2,6,7,3,8)])
table(svm.5.predict, test$diabetes)

(166+64)/300
