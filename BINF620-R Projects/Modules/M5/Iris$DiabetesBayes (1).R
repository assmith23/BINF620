install.packages("caret")
library(caret)
data(iris)
names(iris)
set.seed(430)
iris_obs = nrow(iris)
iris_idx = sample(iris_obs, size = trunc(0.50 * iris_obs))
# iris_index = sample(iris_obs, size = trunc(0.10 * iris_obs))
iris_trn = iris[iris_idx, ]
iris_tst = iris[-iris_idx, ]
caret::featurePlot(x = iris_trn[, c("Sepal.Length", "Sepal.Width", 
                                    "Petal.Length", "Petal.Width")], 
                   y = iris_trn$Species,
                   plot = "density", 
                   scales = list(x = list(relation = "free"), 
                                 y = list(relation = "free")), 
                   adjust = 1.5, 
                   pch = "|", 
                   layout = c(2, 2), 
                   auto.key = list(columns = 3))

caret::featurePlot(x = iris_trn[, c("Sepal.Length", "Sepal.Width", 
                                    "Petal.Length", "Petal.Width")], 
                   y = iris_trn$Species,
                   plot = "ellipse",
                   auto.key = list(columns = 3))

caret::featurePlot(x = iris_trn[, c("Sepal.Length", "Sepal.Width", 
                                    "Petal.Length", "Petal.Width")], 
                   y = iris_trn$Species,
                   plot = "box",
                   scales = list(y = list(relation = "free"),
                                 x = list(rot = 90)),
                   layout = c(4, 1))

library(e1071)
library(mlbench)
# Load the dataset
data(PimaIndiansDiabetes)
# fit model
fit <- naiveBayes(diabetes~., data=PimaIndiansDiabetes)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, PimaIndiansDiabetes[,1:8])
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)

library(caret)
library(mlbench)
# Load the dataset
data(PimaIndiansDiabetes)
# train
set.seed(7)
trainControl <- trainControl(method="cv", number=5)
fit.nb <- train(diabetes~., data=PimaIndiansDiabetes, method="nb", metric="Accuracy",
                trControl=trainControl)
# summarize fit
print(fit.nb)


training <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/iris_train.csv')
test <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/iris_test.csv')

str(training)
str(test)
# Using klaR for Naive Bayes
library(klaR)
nb_mod <- NaiveBayes(Species ~ ., data=training)
pred <- predict(nb_mod, test)

# Confusion Matrix
tab <- table(pred$class, test$Species)
caret::confusionMatrix(tab)  
#> Confusion Matrix and Statistics

# Plot density of each feature using nb_mod
opar = par(mfrow=c(2, 2), mar=c(4,0,0,0))
plot(nb_mod, main="")  
par(opar)

# Plot the Confusion Matrix
library(ggplot2)
test$pred <- pred$class
ggplot(test, aes(Species, pred, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title="Confusion Matrix", 
       subtitle="Predicted vs. Observed from Iris dataset", 
       y="Predicted", 
       x="Truth",
       caption="machinelearningplus.com")


# load the packages
library(MASS)
library(mlbench)
# Load the dataset
data(PimaIndiansDiabetes)
# fit model
fit <- lda(diabetes~., data=PimaIndiansDiabetes)
# summarize the fit
print(fit)
plot(fit)
# make predictions
predictions <- predict(fit, PimaIndiansDiabetes[,1:8])$class
# summarize accuracy
table(predictions, PimaIndiansDiabetes$diabetes)


# load packages
library(caret)
library(mlbench)
set.seed(7)
trainControl <- trainControl(method="cv", number=5)
fit.lda <- train(diabetes~., data=PimaIndiansDiabetes, method="lda", metric="Accuracy",
                 preProcess=c("center", "scale"), trControl=trainControl)
# summarize fit
print(fit.lda)


library(MASS)
data(iris)
iris_lda = lda(Species ~ ., data = iris_trn)

