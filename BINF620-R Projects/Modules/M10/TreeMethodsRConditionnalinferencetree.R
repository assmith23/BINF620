install.packages("party")
install.packages("dplyr")
install.packages("rvest")
install.packages("partykit") 

library(dplyr)
library(party)
library(rpart)
library(rvest)
library(caret)
library(partykit) 


model <- rpart(Species ~., data = iris)
par(xpd = NA) # otherwise on some devices the text is clipped
plot(model)
text(model, digits = 3)
print(model, digits = 2)

newdata <- data.frame(
  Sepal.Length = 6.5, Sepal.Width = 3.0,
  Petal.Length = 5.2, Petal.Width = 2.0
)
model %>% predict(newdata, "class") 

# Load the data
data("Boston", package = "MASS")
# Inspect the data
sample_n(Boston, 3)
# Split the data into training and test set
set.seed(123)
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

# Fit the model on the training set
set.seed(123)
model <- train(
  medv ~., data = train.data, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Plot model error vs different values of
# cp (complexity parameter)
plot(model)
# Print the best tuning parameter cp that
# minimize the model RMSE
model$bestTune

# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(model$finalModel)
text(model$finalModel, digits = 3)

# Decision rules in the model
model$finalModel
# Make predictions on the test data
predictions <- model %>% predict(test.data)
head(predictions)
# Compute the prediction error RMSE
RMSE(predictions, test.data$medv)


#Conditionnal inference tree
# Load the data
data("PimaIndiansDiabetes2", package = "mlbench")
pima.data <- na.omit(PimaIndiansDiabetes2)
str(pima.data)
# Split the data into training and test set
set.seed(123)
training.samples <- pima.data$diabetes %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- pima.data[training.samples, ]
test.data <- pima.data[-training.samples, ]

library(party)
set.seed(123)
model <- train(
  diabetes ~., data = train.data, method = "ctree2",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(maxdepth = 3, mincriterion = 0.95 )
)
plot(model$finalModel)

# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
# Compute model accuracy rate on test data
mean(predicted.classes == test.data$diabetes)


data(airquality)
str(airquality)

set.seed(888)
airquality[is.na(airquality$Ozone),1]<-
  sample(airquality[!is.na(airquality$Ozone),1],37)
 summary(airquality$Ozone)
 
 airct <- ctree(Ozone ~ ., data = airquality,controls =
                  ctree_control(maxsurrogate = 3))
 airct
 plot(airct)
 
 plot(airct, inner_panel = node_boxplot, edge_panel =
        function(...) invisible(),tnex = 1)

 airConInfTree <- ctree(Ozone ~ .,  
                        data = airquality) 
 print(airConInfTree) 
 plot(airConInfTree)
  