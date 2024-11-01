install.packages("boot")
library(boot)
# Load the data
data("swiss")

#str(swiss)
# Define training control
train.control <- trainControl(method = "boot", number = 100)
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

model_coef <- function(data, index){
  coef(lm(Fertility ~., data = data, subset = index))
}
model_coef(swiss, 1:47)

library(boot)
boot(swiss, model_coef, 500)
summary(lm(Fertility ~., data = swiss))$coef

library(tidyverse)
library(caret)
data("swiss")
# Inspect the data
#sample_n(swiss, 3)

# Split the data into training and test set
set.seed(123)
training.samples <- swiss$Fertility %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- swiss[training.samples, ]
test.data <- swiss[-training.samples, ]
# Build the model
model <- lm(Fertility ~., data = train.data)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Fertility),
            RMSE = RMSE(predictions, test.data$Fertility),
            MAE = MAE(predictions, test.data$Fertility))
RMSE(predictions, test.data$Fertility)/mean(test.data$Fertility)

# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

#K-fold cross-validation
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

#Repeated K-fold cross-validation
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

#Bootstrap
set.seed(125)
train.control <- trainControl(method = "boot", 
                              number = 100)
# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

##Using abalone data
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data("abalone")
head(abalone)
#Bootstrap
set.seed(125)
train.control <- trainControl(method = "boot", 
                              number = 100)
# Train the model
model <- train(Rings ~., data = abalone, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)


training.samples <- abalone$Rings %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- abalone[training.samples, ]
test.data <- abalone[-training.samples, ]

model <- lm(Rings ~., data = train.data)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Rings),
            RMSE = RMSE(predictions, test.data$Rings),
            MAE = MAE(predictions, test.data$Rings))
RMSE(predictions, test.data$Rings)/mean(test.data$Rings)

# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model <- train(Rings ~., data = abalone, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

#K-fold cross-validation
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Rings ~., data = abalone, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

#Repeated K-fold cross-validation
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
model <- train(Rings ~., data = abalone, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)



##BOotstrapping method
train.control <- trainControl(method = "boot", number = 100)
# Train the model
model <- train(Rings ~., data = abalone, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

model_coef <- function(data, index){
  coef(lm(Rings ~., data = data, subset = index))
}
model_coef(abalone, 1:4177)

library(boot)
boot(abalone, model_coef, 500)
summary(lm(Rings ~., data = abalone))$coef
