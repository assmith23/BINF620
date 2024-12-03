```{r p2_buildNN, echo=TRUE}
# Fit neural network model with 1 hidden neuron
nn1 <- neuralnet(formula,
                 data = train_data, linear.output = FALSE)

# Fit neural network model with 5 hidden neurons
nn5 <- neuralnet(formula,
                 data = train_data, hidden = 5, linear.output = FALSE)

# Fit neural network model with 10 hidden neurons
nn10 <- neuralnet(formula,
                  data = train_data, hidden = 10, linear.output = FALSE)
```

```{r p2_buildNN, echo=TRUE}
str(nn1$data)

# Make predictions for each model
pred1 <- compute(nn1$net.result, test_data)
pred5 <- compute(nn5, test_data[, -9])
pred10 <- compute(nn10, test_data[, -9])$net.result

# Convert predictions to factors (as the target variable is binary)
pred1 <- as.factor(ifelse(pred1 > 0.5, 1, 0))
pred5 <- as.factor(ifelse(pred5 > 0.5, 1, 0))
pred10 <- as.factor(ifelse(pred10 > 0.5, 1, 0))

# Confusion matrices and performance metrics
conf1 <- table(pred1, test_data$class)
conf5 <- table(pred5, test_data$class)
conf10 <- table(pred10, test_data$class)

# Accuracy calculation
accuracy1 <- sum(diag(conf1)) / sum(conf1)
accuracy5 <- sum(diag(conf5)) / sum(conf5)
accuracy10 <- sum(diag(conf10)) / sum(conf10)

# False Positive Rate (FPR) calculation
FPR1 <- conf1[2,1] / (conf1[2,1] + conf1[2,2])
FPR5 <- conf5[2,1] / (conf5[2,1] + conf5[2,2])
FPR10 <- conf10[2,1] / (conf10[2,1] + conf10[2,2])

# Print the results
cat("Accuracy for 1 hidden neuron: ", accuracy1, "\n")
cat("Accuracy for 5 hidden neurons: ", accuracy5, "\n")
cat("Accuracy for 10 hidden neurons: ", accuracy10, "\n")
cat("False Positive Rate for 1 hidden neuron: ", FPR1, "\n")
cat("False Positive Rate for 5 hidden neurons: ", FPR5, "\n")
cat("False Positive Rate for 10 hidden neurons: ", FPR10, "\n")

```

```{r p2_rmse, echo=TRUE}
# Function to calculate RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Predictions on the test set for each model
nn_pred1 <- predict(nn_model, test_data)
nn_pred2 <- predict(nn_model2, test_data)
nn_pred3 <- predict(nn_model3, test_data)
nn_pred4 <- predict(nn_model4, test_data)

# Calculate RMSE for each model
rmse_nn1 <- rmse(test_data$pedigree, nn_pred1)
rmse_nn2 <- rmse(test_data$pedigree, nn_pred2)
rmse_nn3 <- rmse(test_data$pedigree, nn_pred3)
rmse_nn4 <- rmse(test_data$pedigree, nn_pred4)

# Store the RMSEs for each model
rmse_values <- c(rmse_nn1, rmse_nn2, rmse_nn3, rmse_nn4)
```

```{r p2_buildNN, eval=FALSE, include=FALSE}
formula <- diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + age + pedigree

nn_model_h1 <- neuralnet(formula, 
                         data = train_data, 
                         linear.output = FALSE,
                         rep = 3,
                         stepmax = 1e5)#,
#lifesign = "full")

nn_model_h5 <- neuralnet(formula, 
                         data = train_data, 
                         linear.output = FALSE,
                         hidden = 5,
                         rep = 3,
                         stepmax = 1e5)#,
#lifesign = "full")

nn_model_h10 <- neuralnet(formula, 
                          data = train_data, 
                          linear.output = FALSE,
                          hidden = 10,
                          rep = 3,
                          stepmax = 1e5)#,
#lifesign = "full")
```

```{r p2_acc, echo=TRUE}
predictors <- test_data[, colnames(test_data) != "diabetes"]
# Make predictions using each neural network
pred1 <- compute(nn_model_h1, predictors)$net.result
pred2 <- compute(nn_model_h5, predictors)$net.result
pred3 <- compute(nn_model_h10, predictors)$net.result
pred4 <- compute(nn_model4, predictors)$net.result
```