library(caret)
set.seed(123)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3 / 4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
modelFitRF <- train(diagnosis ~ ., data = training, method = "rf")
modelFitGBM <- train(diagnosis ~ ., data = training, method = "gbm",verbose=F)
modelFitLDA <- train(diagnosis ~ ., data = training, method = "lda")

predRF <- predict(modelFitRF,newdata=testing)
predGBM <- predict(modelFitGBM, newdata = testing)
prefLDA <- predict(modelFitLDA, newdata = testing)

confusionMatrix(predRF, testing$diagnosis)$overall[1]

confusionMatrix(predGBM, testing$diagnosis)$overall[1]
#Accuracy 
#0.7926829 

confusionMatrix(prefLDA, testing$diagnosis)$overall[1]
#Accuracy 
#0.7682927

predDF <- data.frame(predRF, predGBM, prefLDA, diagnosis = testing$diagnosis, stringsAsFactors = F)

modelStack <- train(diagnosis ~ ., data = predDF, method = "rf")
combPred <- predict(modelStack, predDF)
confusionMatrix(combPred, testing$diagnosis)$overall[1] 
#Accuracy 
#0.804878

predDF <- data.frame(predRF, predGBM, prefLDA, diagnosis = testing$diagnosis, stringsAsFactors = F)
modelStack <- train(diagnosis ~ ., data = predDF, method = "rf")
combPred <- predict(modelStack, predDF)
confusionMatrix(combPred, testing$diagnosis)$overall[1] 

# Generate predictions on the test set
testPredRF <- predict(modelFitRF, newdata = testing)
testPredGBM <- predict(modelFitGBM, newdata = testing)
testPredLDA <- predict(modelFitLDA, newdata = testing)

# Using the base learner test set predictions, 
# create the level-one dataset to feed to the ensemble
testPredLevelOne <- data.frame(testPredRF, testPredGBM, testPredLDA, diagnosis = testing$diagnosis, stringsAsFactors = F)
combPred <- predict(modelStack, testPredLevelOne)

# Evaluate ensemble test performance
confusionMatrix(combPred, testing$diagnosis)$overall[1]

# Evaluate base learner test performance 
confusionMatrix(testPredRF, testing$diagnosis)$overall[1]
confusionMatrix(testPredGBM, testing$diagnosis)$overall[1]
confusionMatrix(testPredLDA, testing$diagnosis)$overall[1]

library(h2o)
h2o.init()

# Import a sample binary outcome train/test set into H2O
train <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_train_10k.csv")
test <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_test_5k.csv")

# Identify predictors and response
y <- "response"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])

# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5

# There are a few ways to assemble a list of models to stack toegether:
# 1. Train individual models and put them in a list
# 2. Train a grid of models
# 3. Train several grids of models
# Note: All base models must have the same cross-validation folds and
# the cross-validated predicted values must be kept.


# 1. Generate a 2-model ensemble (GBM + RF)

# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train,
                  distribution = "bernoulli",
                  ntrees = 10,
                  max_depth = 3,
                  min_rows = 2,
                  learn_rate = 0.2,
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train,
                          ntrees = 50,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)

# Train a stacked ensemble using the GBM and RF above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                base_models = list(my_gbm, my_rf))

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)

# Compare to base learner performance on the test set
perf_gbm_test <- h2o.performance(my_gbm, newdata = test)
perf_rf_test <- h2o.performance(my_rf, newdata = test)
baselearner_best_auc_test <- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test))
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))
# [1] "Best Base-learner Test AUC:  0.76979821502548"
# [1] "Ensemble Test AUC:  0.773501212640419"

# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(ensemble, newdata = test)


# 2. Generate a random grid of models and stack them together

# GBM Hyperparamters
learn_rate_opt <- c(0.01, 0.03)
max_depth_opt <- c(3, 4, 5, 6, 9)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt,
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt)

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 3,
                        seed = 1)

gbm_grid <- h2o.grid(algorithm = "gbm",
                     grid_id = "gbm_grid_binomial",
                     x = x,
                     y = y,
                     training_frame = train,
                     ntrees = 10,
                     seed = 1,
                     nfolds = nfolds,
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

# Train a stacked ensemble using the GBM grid
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                base_models = gbm_grid@model_ids)

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)

# Compare to base learner performance on the test set
.getauc <- function(mm) h2o.auc(h2o.performance(h2o.getModel(mm), newdata = test))
baselearner_aucs <- sapply(gbm_grid@model_ids, .getauc)
baselearner_best_auc_test <- max(baselearner_aucs)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))
# [1] "Best Base-learner Test AUC:  0.748146530400473"
# [1] "Ensemble Test AUC:  0.773501212640419"

# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(ensemble, newdata = test)