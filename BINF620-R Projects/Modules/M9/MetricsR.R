# load libraries
#Accuracy and Kappa
install.packages("mlbench")
library(caret)
library(mlbench)
# load the dataset
data(PimaIndiansDiabetes)
# prepare resampling method
control <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(diabetes~., data=PimaIndiansDiabetes, method="glm", metric="Accuracy", trControl=control)
# display results
print(fit)

# load data
#RMSE and R^2
data(longley)
# prepare resampling method
control <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(Employed~., data=longley, method="lm", metric="RMSE", trControl=control)
# display results
print(fit)

# load the dataset
#log loss
data(iris)
# prepare resampling method
control <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(7)
fit <- train(Species~., data=iris, method="rpart", metric="logLoss", trControl=control)
# display results
print(fit)

#http://r-statistics.co/Information-Value-With-R.html
install.packages("InformationValue")  # For stable CRAN version
library(InformationValue)
install.packages("githubinstall")
library(githubinstall)
install.packages("devtools")
library(devtools)
devtools::install_github("InformationValue")  # For latest dev version.
install.packages("Information")
install.packages("gridExtra")
library(Information)
library(gridExtra)
data(ActualsAndScores)

data('ActualsAndScores')
plotROC(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)
sensMat <- plotROC(actuals=ActualsAndScores$Actuals,  predictedScores=ActualsAndScores$PredictedScores, returnSensitivityMat = TRUE)
sensitivity(actuals = ActualsAndScores$Actuals, predictedScores = ActualsAndScores$PredictedScores)

max_sens_cutoff <- optimalCutoff(actuals=ActualsAndScores$Actuals, predictedScores = ActualsAndScores$PredictedScores, optimiseFor='Ones')  # determine cutoff to maximise sensitivity.

print(max_sens_cutoff)  # This would be cut-off score that achieved maximum sensitivity.

#> [1] 0.5531893

sensitivity(actuals = ActualsAndScores$Actuals, predictedScores = ActualsAndScores$PredictedScores, threshold=max_sens_cutoff)
#> [1] 1

specificity(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)
#> [1] 0.1411765

specificity(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores, threshold = 0.35)
#> [1] 0.01176471

precision(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)

npv(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)  
youdensIndex(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)  

misClassError(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores, threshold=0.5)
#> [1] 0.4294
Concordance(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)
somersD(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)

ks_stat(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)
#> [1] 0.6118
ks_plot(actuals=ActualsAndScores$Actuals, predictedScores=ActualsAndScores$PredictedScores)

optimalCutoff(actuals = ActualsAndScores$Actuals, predictedScores = ActualsAndScores$PredictedScores)  # returns cutoff that gives minimum misclassification error.
optimalCutoff(actuals = ActualsAndScores$Actuals, predictedScores = ActualsAndScores$PredictedScores, 
              optimiseFor = "Both")  # returns cutoff that gives maximum of Youden's J Index
> # > [1] 0.6431893
  
  sens_table <- optimalCutoff(actuals = ActualsAndScores$Actuals, predictedScores = ActualsAndScores$PredictedScores, 
                              optimiseFor = "Both", returnDiagnostics = TRUE)$sensitivityTable
WOE(X=SimData$X.Cat, Y=SimData$Y.Binary)

options(scipen = 999, digits = 2)
WOETable(X=SimData$X.Cat, Y=SimData$Y.Binary)

options(scipen = 999, digits = 4)
IV(X=SimData$X.Cat, Y=SimData$Y.Binary)

