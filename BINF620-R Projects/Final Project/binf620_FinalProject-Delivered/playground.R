# *************************************
# *  A Manning Smith
# *  Playground - Final Report
# *
# *
# *
# *
# *************************************


# Setup ####

## Load Libraries ----
library(knitr)
library(RColorBrewer)
library(ggplot2)
library(readxl)
library(tibble)
library(dplyr)
library(tidyverse)
library(writexl)
library(png)
library(tinytex)
library(bookdown)
library(ROCR)
library(randomForest)
library(gridExtra)
library(caret)
library(mlbench)
library(kableExtra)
library(neuralnet)
library(naivebayes)
library(tidyr)
library(fastDummies)

library(mice)
library(corrplot)
library(car)
library(glmnet)
library(MASS)
library(pROC)
library(gbm)

# Set Working Directory ----
setwd("~/Documents/BINF620/BINF620-R Projects/Final Project/binf620_FinalProject - Delivered")

# Set Seed
set.seed(1776)


# Load Data ####

## Filter for Selected Columns ----
columns_to_keep <- unique(c(
  "SC_AGE_YEARS", "PLACESLIVED", "HHCOUNT", "FAMCOUNT", 
  "K2Q33A", "K2Q33B", "K2Q32A", "K2Q32B", "FASD", "K2Q37A", 
  "K2Q31A", "K2Q31B", "K2Q31D", "ENGAGE_FAST", "ENGAGE_INTEREST", 
  "ENGAGE_PICKY", "ENGAGE_BINGE", "ENGAGE_PURG", "ENGAGE_PILLS", 
  "ENGAGE_EXERCISE", "ENGAGE_NOEAT", "BORNUSA", "K8Q35", 
  "EMOSUPSPO", "EMOSUPFAM", "EMOSUPHCP", "EMOSUPWOR", "EMOSUPADV", 
  "EMOSUPPEER", "EMOSUPMHP", "EMOSUPOTH", "ACE3", "ACE4", "ACE5", 
  "ACE6", "ACE7", "ACE8", "ACE9", "ACE10", "ACE12", "ACE11", 
  "K10Q40_R", "K7Q02R_R", "K7Q04R_R", "PHYSACTIV", "HOURSLEEP05", 
  "HOURSLEEP", "OUTDOORSWKDAY", "OUTDOORSWKEND", "SCREENTIME", 
  "GRADES", "SC_ENGLISH", "FPL_I1", "FPL_I2", "FPL_I3", "FPL_I4", 
  "FPL_I5", "FPL_I6", "FWC", "HEIGHT", "WEIGHT", "INQ_RESSEG", 
  "INQ_EDU", "INQ_EMPLOY", "INQ_INCOME", "INQ_HOME", "hrsareg", 
  "age3_22", "age5_22", "sex_22", "race4_22", "raceASIA_22", 
  "race7_22", "PrntNativity_22", "HHLanguage_22", "hisplang_22", 
  "famstruct5_22", "povlev4_22", "povSCHIP_22", "AdultEduc_22", 
  "SugarDrink_22", "anxiety_22", "depress_22", "behavior_22", 
  "DevDelay_22", "MotherMH_22", "FatherMH_22", "MotherHSt_22", 
  "FatherHSt_22", "ScreenTime_22", "ACEdivorce_22", "ACEdeath_22", 
  "ACEjail_22", "ACEdomviol_22", "ACEneighviol_22", "ACEmhealth_22", 
  "ACEdrug_22", "ACEdiscrim_22", "ACESexDiscrim_22", 
  "ACEHealthDiscrim_22", "ACEct11_22", "PlacesLived_22", "K2Q33C", 
  "K2Q32C", "K6Q70_R", "K6Q73_R", "K6Q71_R", "K6Q72_R", "K7Q84_R", 
  "K7Q85_R", "K7Q82_R", "K7Q83_R", "K7Q70_R", "BULLIED_R", "BULLY", 
  "SIMPLEADDITION", "STARTNEWACT", "DISTRACTED", "SC_ASIAN", 
  "SC_AIAN", "SC_NHPI", "OutdrsWkend_22", "OutdrsWkDay_22", 
  "AnxietSev_22", "DepresSev_22", "BehavSev_22", "MEDB10ScrQ5_22", 
  "cares_22", "homework_22", "grades6to11_22", "grades12to17_22", 
  "ACE4ctCom_22", "EmSSpouse_22", "EmSFamily_22", "EmSProvider_22", 
  "EmSWorship_22", "EmSSupGrp_22", "EmSPeer_22", "EmSMental_22", 
  "TOTKIDS_R", "K11Q43R", "K7Q30", "K7Q31", "K7Q32", "K7Q37", 
  "K7Q38", "K9Q96", "ENGAGECONCERN", "A1_SEX", "A1_BORN", 
  "A1_EMPLOYED", "A1_GRADE", "A1_MARITAL", "A1_RELATION", "A2_SEX", 
  "A2_BORN", "A2_EMPLOYED", "A2_GRADE", "A2_MARITAL", "A2_RELATION", 
  "A1_ACTIVE", "A2_ACTIVE", "A1_PHYSHEALTH", "A1_MENTHEALTH", 
  "A2_PHYSHEALTH", "A2_MENTHEALTH", "K5Q40", "K5Q41", "K5Q42", 
  "K5Q43", "K5Q44", "DISCUSSOPT", "RAISECONC", "BESTFORCHILD", 
  "TALKABOUT", "WKTOSOLVE", "STRENGTHS", "HOPEFUL", "K10Q30", 
  "K10Q31", "GOFORHELP", "K10Q41_R", "K8Q31", "K8Q32", "K8Q34", 
  "K8Q11", "FOODSIT", "K4Q22_R", "TREATNEED", "K8Q21", "K7Q33", 
  "MAKEFRIEND", "SC_RACE_R", "SC_HISPANIC_R", "SC_RACER", 
  "PrntCncrn_22", "BodyImage_22", "PhysAct_22", "WgtConcn_22", 
  "bully_22", "bullied_22", "curious6to17_22", "flrish6to17_22", 
  "argue_22", "MakeFriend_22", "Transition_22", "SchlEngage_22", 
  "sports_22", "clubs_22", "lessons_22", "AftSchAct_22", "EventPart_22", 
  "volunteer_22", "workpay_22", "mentor_22", "WrkngPoorR_22", 
  "ShareIdeas_22", "TalkAbout_22", "WrkToSolve_22", "strengths_22", 
  "hopeful_22", "ACE2more11_22", "ACE6ctHH_22", "ACE2more6HH_22", 
  "ACE1more4Com_22", "EmSupport_22", "NbhdSupp_22", "NbhdSafe_22", 
  "SchlSafe_22", "SideWlks_22", "park_22", "RecCentr_22", "library_22", 
  "NbhdAmenities_22", "litter_22", "housing_22", "vandal_22", 
  "NbhdDetract_22", "PHYSACTIV", "FAMILY_R", "MHealthConcern", "MHealthConcernC",
  "mentor_22", "ShareIdeas_22", "sex_22", "SC_AGE_YEARS", 
  "SC_RACE_R", "HHCOUNT", "MotherMH_22", "FatherMH_22", 
  "bullied_22", "ACE12", "ACEct11_22", "ACE4ctCom_22", 
  "FAMILY_R", "NbhdSupp_22", "NbhdSafe_22"))

# Change if need to rerun raw data
if(FALSE){
  # Load Original Dataset
  rawData <- read.csv('NSCH_2022e_Topical_CSV_CAHMI_DRCv2.csv')
  
  # Create MHealthConcern
  rawData$MHealthConcernC <- factor(
    ifelse(rawData$K2Q33B == 1 | rawData$K2Q32B == 1, 'Yes', 'No'),
    levels = c('No', 'Yes')
  )
  # Numeric
  rawData$MHealthConcern <- ifelse(rawData$K2Q33A == 1 | rawData$K2Q32A == 1, 1, 0)
  
  # Filter Columns
  currData <- rawData[, columns_to_keep]
  # Save Data
  save(currData, file = 'currData_NSCH.RData')
}

## Load Pre-saved R Data ----
load("currData_NSCH.RData")
load("modelData_NSCH.RData")
load("imputed_NSCH.RData") # Ensure that this data is in your working directory


# Data Preparation ####

# Make sure all categorical variables are properly formatted as factors
factor_variables <- c("MHealthConcern", "BORNUSA", "K8Q35", "ACE12", 
                      "PHYSACTIV", "sex_22", "MotherMH_22",  
                      "FatherMH_22", "ScreenTime_22", "ACEct11_22", "ACE4ctCom_22", 
                      "SC_RACE_R", "bully_22", "bullied_22", "AftSchAct_22", 
                      "EventPart_22", "mentor_22", "ShareIdeas_22", 
                      "NbhdSupp_22", "NbhdSafe_22", "FAMILY_R")

currData <- currData %>%
  mutate(across(all_of(factor_variables), as.factor))

# currData Columns
colnames(currData)

# Define analysis variables for easier reference
analysis_vars <- c("MHealthConcern", "AftSchAct_22", "PHYSACTIV", "EventPart_22", 
                   "mentor_22", "ShareIdeas_22", "sex_22", "SC_AGE_YEARS", 
                   "SC_RACE_R", "HHCOUNT", "MotherMH_22", "FatherMH_22", 
                   "bullied_22", "ACE12", "ACEct11_22", "ACE4ctCom_22", 
                   "FAMILY_R", "NbhdSupp_22", "NbhdSafe_22")


analysis_data <- currData %>%
  dplyr::select(all_of(analysis_vars)) %>%
  filter(complete.cases(.))


## Imputation ----

if(FALSE){
  imputation_model <- mice(currData %>% dplyr::select(all_of(analysis_vars)), m=5, maxit=50, method='pmm')
  imputed_data <- complete(imputation_model, 1)

  # Save imputed data
  save(imputed_data, file = "imputed_NSCH.RData")
}



# Descriptive Statistics & Exploration ####

# Summarize MH Concerns
mh_by_activity <- currData %>%
  group_by(AftSchAct_22, MHealthConcern) %>%
  summarise(count = n()) %>%
  group_by(AftSchAct_22) %>%
  mutate(proportion = count / sum(count))

mh_by_physical <- currData %>%
  group_by(PHYSACTIV, MHealthConcern) %>%
  summarise(count = n()) %>%
  group_by(PHYSACTIV) %>%
  mutate(proportion = count / sum(count))

mh_by_sex <- currData %>%
  group_by(sex_22, MHealthConcern) %>%
  summarise(count = n()) %>%
  group_by(sex_22) %>%
  mutate(proportion = count / sum(count))

# Create Plots
mh_concernsBY_afterSchoolActPart <- ggplot(mh_by_activity, aes(x=AftSchAct_22, y=proportion, fill=MHealthConcern)) +
  geom_col(position="dodge") +
  labs(title="Mental Health Concerns by After-School Activity Participation",
       x="After-School Activity Participation", y="Proportion") +
  theme_minimal()

mh_concernsBYPhysicalActLevel <- ggplot(mh_by_physical, aes(x=PHYSACTIV, y=proportion, fill=MHealthConcern)) +
  geom_col(position="dodge") +
  labs(title="Mental Health Concerns by Physical Activity Level",
       x="Physical Activity Level", y="Proportion") +
  theme_minimal()


# Logistic Regression ####

# Prepare data for glmnet (convert factors to dummy variables)
x_vars <- model.matrix(~ AftSchAct_22 + PHYSACTIV + EventPart_22 + 
                         mentor_22 + ShareIdeas_22 + sex_22 + SC_AGE_YEARS + 
                         SC_RACE_R + bullied_22 + ACE12 - 1, 
                       data = imputed_data)
y_var <- as.numeric(imputed_data$MHealthConcern) - 1  # Convert to 0/1

# Set up cross-validation for lambda selection
set.seed(123)
cv_fit <- cv.glmnet(x_vars, y_var, family = "binomial", alpha = 0.5, nfolds = 5)

# Plot cross-validation results
plot(cv_fit)

# Fit regularized model with optimal lambda
# Alpha = 0.5 for elastic net (mix of ridge and lasso)
reg_model <- glmnet(x_vars, y_var, family = "binomial", 
                    alpha = 0.5, lambda = cv_fit$lambda.min)

# Get coefficients for the regularized model
reg_coefs <- coef(reg_model)
print(reg_coefs)

# Make predictions using the regularized model
reg_probs <- predict(reg_model, newx = x_vars, type = "response")
reg_preds <- ifelse(reg_probs > 0.5, 1, 0)

# Create confusion matrix
reg_conf_matrix <- table(Predicted = reg_preds, Actual = y_var)
print(reg_conf_matrix)

# Calculate accuracy, sensitivity, specificity
reg_accuracy <- sum(diag(reg_conf_matrix)) / sum(reg_conf_matrix)
reg_sensitivity <- reg_conf_matrix[2,2] / sum(reg_conf_matrix[,2])
reg_specificity <- reg_conf_matrix[1,1] / sum(reg_conf_matrix[,1])

print(paste("Regularized Model Accuracy:", round(reg_accuracy, 3)))
print(paste("Regularized Model Sensitivity:", round(reg_sensitivity, 3)))
print(paste("Regularized Model Specificity:", round(reg_specificity, 3)))

# Plot ROC curve
if(requireNamespace("pROC", quietly = TRUE)) {
  roc_reg <- roc(y_var, as.vector(reg_probs))
  plot(roc_reg, main = "ROC Curve - Regularized Logistic Regression")
  auc_value <- auc(roc_reg)
  text(0.7, 0.3, paste("AUC =", round(auc_value, 3)))
}


# Random Forest Classification ####

# Create training and testing sets (70/30 split)
train_index <- createDataPartition(imputed_data$MHealthConcern, p = 0.7, list = FALSE)
train_data <- imputed_data[train_index, ]
test_data <- imputed_data[-train_index, ]

train_data$MHealthConcern <- factor(train_data$MHealthConcern, levels = c(0, 1))
test_data$MHealthConcern <- factor(test_data$MHealthConcern, levels = c(0, 1))


# Train random forest model
rf_model <- randomForest(MHealthConcern ~ AftSchAct_22 + PHYSACTIV + 
                           EventPart_22 + mentor_22 + ShareIdeas_22 + 
                           sex_22 + SC_AGE_YEARS + SC_RACE_R + 
                           bullied_22 + ACE12,
                         data = train_data,
                         ntree = 500,
                         importance = TRUE)

print(rf_model)

# Variable importance plot
varImpPlot(rf_model, main = "Variable Importance (Random Forest)")

# Evaluate on test data
rf_pred <- predict(rf_model, test_data)

# Ensure both prediction and reference are factors with same levels
rf_pred <- factor(rf_pred, levels = c(0, 1))
reference <- factor(test_data$MHealthConcern, levels = c(0, 1))

rf_conf_matrix <- confusionMatrix(rf_pred, reference)
print(rf_conf_matrix)

# ROC curve for random forest

rf_pred_prob <- predict(rf_model, test_data, type = "prob")[,2]
roc_rf <- roc(test_data$MHealthConcern, rf_pred_prob)
plot(roc_rf, main = "ROC Curve - Random Forest", col = "blue")
text(0.7, 0.3, paste("AUC =", round(auc(roc_rf), 3)), col = "blue")


# Gradient Boosting Machine (GBM) ####

# Convert outcome to numeric for GBM (0/1)
train_data$MHealthConcern_num <- as.numeric(train_data$MHealthConcern)-1
test_data$MHealthConcern_num <- as.numeric(test_data$MHealthConcern)-1

# Train GBM model with fewer iterations and larger shrinkage for faster computation
gbm_model <- gbm(MHealthConcern_num ~ AftSchAct_22 + PHYSACTIV + 
                   EventPart_22 + mentor_22 + ShareIdeas_22 + 
                   sex_22 + SC_AGE_YEARS + SC_RACE_R + bullied_22 + ACE12,
                 distribution = "bernoulli",
                 n.trees = 500,
                 interaction.depth = 3,
                 shrinkage = 0.05,
                 data = train_data)

# Find best iteration based on test data performance
best_iter <- gbm.perf(gbm_model, method = "OOB")
print(paste("Best number of trees:", best_iter))

# Summary of variable importance
summary(gbm_model, n.trees = best_iter, plotit = TRUE)

# Make predictions on test data
gbm_pred_prob <- predict(gbm_model, test_data, n.trees = best_iter, type = "response")
gbm_pred <- ifelse(gbm_pred_prob > 0.5, 1, 0)

# Create confusion matrix
gbm_conf_matrix <- table(predicted = gbm_pred, actual = test_data$MHealthConcern_num)
print(gbm_conf_matrix)

# Calculate metrics
gbm_accuracy <- sum(diag(gbm_conf_matrix)) / sum(gbm_conf_matrix)
print(paste("GBM Accuracy:", round(gbm_accuracy, 3)))

# ROC curve for GBM
roc_gbm <- roc(test_data$MHealthConcern_num, gbm_pred_prob)
plot(roc_gbm, main="ROC Curve - Gradient Boosting Machine", col="red")
text(0.7, 0.3, paste("AUC =", round(auc(roc_gbm), 3)), col="red")


# Focused Analysis on Key Relationships ####

# For social connectedness & after-school activities, create simpler models
# that are less likely to have separation issues

# Model 1: After-school activities and physical activity
activity_model <- glm(MHealthConcern ~ AftSchAct_22 + PHYSACTIV,
                      family = binomial(link = "logit"), 
                      data = imputed_data)
summary(activity_model)

# Model 2: Social connection indicators
social_model <- glm(MHealthConcern ~ mentor_22 + ShareIdeas_22 + EventPart_22,
                    family = binomial(link = "logit"), 
                    data = imputed_data)
summary(social_model)

# Model 3: Demographic factors
demographic_model <- glm(MHealthConcern ~ sex_22 + SC_AGE_YEARS + SC_RACE_R,
                         family = binomial(link = "logit"), 
                         data = imputed_data)
summary(demographic_model)

# Model 4: Adverse experiences
adverse_model <- glm(MHealthConcern ~ bullied_22 + ACE12,
                     family = binomial(link = "logit"), 
                     data = imputed_data)
summary(adverse_model)

# Calculate odds ratios for each model
or_activity <- exp(cbind(OR = coef(activity_model), confint(activity_model)))
or_social <- exp(cbind(OR = coef(social_model), confint(social_model)))
or_demographic <- exp(cbind(OR = coef(demographic_model), confint(demographic_model)))
or_adverse <- exp(cbind(OR = coef(adverse_model), confint(adverse_model)))

# Print odds ratios
print("Odds Ratios for Activity Model:")
print(or_activity)
print("Odds Ratios for Social Connection Model:")
print(or_social)
print("Odds Ratios for Demographic Model:")
print(or_demographic)
print("Odds Ratios for Adverse Experiences Model:")
print(or_adverse)

# Compare model performance
models <- list(activity_model, social_model, demographic_model, adverse_model)
model_names <- c("Activity", "Social", "Demographic", "Adverse")

auc_values <- numeric(length(models))
for (i in 1:length(models)) {
  probs <- predict(models[[i]], type = "response")
  roc_obj <- roc(imputed_data$MHealthConcern, probs)
  auc_values[i] <- auc(roc_obj)
}

model_comparison <- data.frame(
  Model = model_names,
  AUC = auc_values,
  AIC = sapply(models, AIC)
)

print(model_comparison)

# Create a bar chart comparing AUC values across simplified models
ggplot(model_comparison, aes(x = reorder(Model, AUC), y = AUC, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AUC, 3)), vjust = -0.3, size = 3.5) +
  labs(title = "Model Comparison - Area Under the ROC Curve",
       x = "Model", y = "AUC") +
  theme_minimal() +
  ylim(0, 1) +
  coord_flip()


# Interactive Effects Analysis ####

# Examine interactions between activities and sex
sex_interaction <- glm(MHealthConcern ~ AftSchAct_22*sex_22 + PHYSACTIV,
                       family = binomial(link = "logit"), 
                       data = imputed_data)
summary(sex_interaction)

# Interaction between physical activity and after-school activities
phys_activity_interaction <- glm(MHealthConcern ~ AftSchAct_22*PHYSACTIV,
                                 family = binomial(link = "logit"), 
                                 data = imputed_data)
summary(phys_activity_interaction)

# Create interaction plots using predicted probabilities
# For AftSchAct_22 and sex_22 interaction
interaction_data <- expand.grid(
  AftSchAct_22 = levels(imputed_data$AftSchAct_22),
  sex_22 = levels(imputed_data$sex_22),
  PHYSACTIV = levels(imputed_data$PHYSACTIV)[1]  # Use first level or most common
)

interaction_data$predicted <- predict(sex_interaction, newdata = interaction_data, type = "response")

ggplot(interaction_data, aes(x = AftSchAct_22, y = predicted, group = sex_22, color = sex_22)) +
  geom_line() +
  geom_point() +
  labs(title = "Interaction between After-School Activities and Sex",
       x = "After-School Activities", 
       y = "Predicted Probability of Mental Health Concern") +
  theme_minimal()

# For AftSchAct_22 and PHYSACTIV interaction
interaction_data2 <- expand.grid(
  AftSchAct_22 = levels(imputed_data$AftSchAct_22),
  PHYSACTIV = levels(imputed_data$PHYSACTIV)
)

interaction_data2$predicted <- predict(phys_activity_interaction, 
                                       newdata = interaction_data2, 
                                       type = "response")

ggplot(interaction_data2, aes(x = AftSchAct_22, y = predicted, group = PHYSACTIV, color = PHYSACTIV)) +
  geom_line() +
  geom_point() +
  labs(title = "Interaction between After-School Activities and Physical Activity",
       x = "After-School Activities", 
       y = "Predicted Probability of Mental Health Concern") +
  theme_minimal()


# Compare Machine Learning Models ####

# Create a comparison of the best models from each approach
model_comparison_ml <- data.frame(
  Model = c("Regularized Logistic", "Random Forest", "GBM"),
  AUC = c(auc_value, auc(roc_rf), auc(roc_gbm))
)

# Print comparison
print(model_comparison_ml)

# Create a visual comparison
ggplot(model_comparison_ml, aes(x = reorder(Model, AUC), y = AUC, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AUC, 3)), vjust = -0.3, size = 3.5) +
  labs(title = "Model Comparison - Machine Learning Approaches",
       x = "Model", y = "AUC") +
  theme_minimal() +
  ylim(0, 1) +
  coord_flip()

# Compare variable importance across models
# Extract variable importance from random forest
rf_importance <- importance(rf_model)
rf_importance_df <- data.frame(
  Variable = rownames(rf_importance),
  Importance = rf_importance[, "MeanDecreaseGini"]
)

# Extract variable importance from GBM
gbm_importance <- summary(gbm_model, n.trees = best_iter, plotit = FALSE)
gbm_importance_df <- data.frame(
  Variable = gbm_importance$var,
  Importance = gbm_importance$rel.inf
)

# Extract variable importance from regularized model
# (Absolute value of coefficients, excluding intercept)
reg_importance <- as.matrix(coef(reg_model))[-1, ]
reg_importance_df <- data.frame(
  Variable = rownames(reg_importance),
  Importance = abs(as.vector(reg_importance))
)

# Display top 10 variables from each model
rf_top10 <- rf_importance_df %>% 
  arrange(desc(Importance)) %>% 
  head(10)

gbm_top10 <- gbm_importance_df %>% 
  arrange(desc(Importance)) %>% 
  head(10)

reg_top10 <- reg_importance_df %>% 
  filter(!is.na(Importance)) %>%
  arrange(desc(Importance)) %>% 
  head(10)

print("Top 10 variables - Random Forest:")
print(rf_top10)

print("Top 10 variables - GBM:")
print(gbm_top10)

print("Top 10 variables - Regularized Logistic:")
print(reg_top10)

# Plot variable importance for Random Forest
ggplot(rf_top10, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance - Random Forest",
       x = "Variable", y = "Importance") +
  theme_minimal()

# Plot variable importance for GBM
ggplot(gbm_top10, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Variable Importance - GBM",
       x = "Variable", y = "Importance") +
  theme_minimal()