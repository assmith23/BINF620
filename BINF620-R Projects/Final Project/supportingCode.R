# *************************************
# *  A Manning Smith
# *
# *
# *
# *
# *
# *************************************

## Load Libraries
library(RColorBrewer)
library(ROCR)
library(randomForest)
library(gridExtra)
library(caret)
library(mlbench)
library(knitr)
library(kableExtra)
library(tidyverse)
library(neuralnet)
library(naivebayes)
library(tidyr)
library(fastDummies)



library(dplyr)
library(rpart)
library(rpart.plot)
library(survival)
library(ranger)
library(ggplot2)
library(ggfortify)
library("survminer")
library(corrplot)
library(ggcorrplot)
library(mice)
library(class)
library(VIM)
library(MASS)
library(boot)
library(pROC)
library(MatchIt)
library(tableone)
library(WeightIt)
library(survey)


## Functions ##
getAccuracy_glm <- function(modelResults, type = "Binary") {
  
  accuracy = 0
  
  if (type == "Binary") {
    pred_prob <- predict(modelResults, test_data, type = "response")
    pred_binary <- ifelse(pred_prob > 0.5, 1, 0)
    accuracy <- mean(pred_binary == test_data$MHealthConcern)
  }
  else {
    pred <- predict(modelResults, test_data)
    pred_rounded <- round(pred)
    accuracy <- mean(pred_rounded == test_data$MHealthScore)
  }
  
  return(accuracy)
}

calculate_metrics <- function(conf_matrix) {
  
  if (nrow(conf_matrix) < 2 || ncol(conf_matrix) < 2) {
    conf_matrix <- matrix(c(conf_matrix, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)
    rownames(conf_matrix) <- c("0", "1")
    colnames(conf_matrix) <- c("0", "1")
  }
  
  TP <- conf_matrix["1", "1"]
  TN <- conf_matrix["0", "0"]
  FP <- conf_matrix["1", "0"]
  FN <- conf_matrix["0", "1"]
  
  accuracy <- (TP + TN) / sum(conf_matrix)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  FPR <- FP / (FP + TN)
  
  metrics <- list(
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    FPR = FPR)
  
  # Create a data frame for metrics
  metrics_df <- data.frame(
    Metric = c("Accuracy", "Sensitivity", "Specificity", "False Positive Rate (FPR)"),
    Value = c(metrics$Accuracy, metrics$Sensitivity, metrics$Specificity, metrics$FPR))
  
  return(metrics_df)
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

nn_stats <- function(nn, test_data) {
  pred <- as.numeric(predict(nn, test_data))
  rmse <- rmse(test_data$MHealthConcern, pred)
  pred_class <- factor(ifelse(pred > 0.5, 1, 0), levels = c(0, 1))
  test_data$MHealthConcern <- factor(test_data$MHealthConcern, levels = c(0, 1))
  conf <- table(Predicted = pred_class, Actual = test_data$MHealthConcern)
  
  return(list(rmse = rmse, confusion_matrix = conf))
}

## Load Data ##

# Load Original Dataset
  #rawData <- read.csv("Final Project/NSCH_2022e_Topical_CSV_CAHMI_DRCv2.csv")

# Filter for Selected Columns
selectedColumns <- c(
  "SC_AGE_YEARS", "PLACESLIVED", "HHCOUNT", "K2Q33A", "K2Q33B", "K2Q32A", "K2Q32B", 
  "ENGAGE_BINGE", "ENGAGE_PURG", "ENGAGE_EXERCISE", "ENGAGE_NOEAT", "BORNUSA", "K8Q35", 
  "EMOSUPSPO", "EMOSUPFAM", "EMOSUPWOR", "EMOSUPPEER", "EMOSUPMHP", "ACE4", "ACE12", 
  "ACE11", "K10Q40_R", "PHYSACTIV", "OUTDOORSWKDAY", "OUTDOORSWKEND", "SCREENTIME", 
  "GRADES", "FWC", "HEIGHT", "WEIGHT", "age3_22", "age5_22", "sex_22", "povlev4_22", 
  "anxiety_22", "depress_22", "behavior_22", "DevDelay_22", "MotherMH_22", "FatherMH_22", 
  "MotherHSt_22", "FatherHSt_22", "ScreenTime_22", "ACEct11_22", "PlacesLived_22", 
  "K2Q33C", "K2Q32C", "BULLIED_R", "BULLY", "SC_ASIAN", "OutdrsWkend_22", 
  "OutdrsWkDay_22", "AnxietSev_22", "DepresSev_22", "BehavSev_22", "MEDB10ScrQ5_22", 
  "ACE4ctCom_22", "ENGAGECONCERN", "K8Q21", "K7Q33", "MAKEFRIEND", "SC_RACE_R", 
  "SC_RACER", "PrntCncrn_22", "BodyImage_22", "PhysAct_22", "WgtConcn_22", "bully_22", 
  "bullied_22", "AftSchAct_22", "EventPart_22", "mentor_22", "WrkngPoorR_22", 
  "ShareIdeas_22", "TalkAbout_22", "ACE2more11_22", "ACE6ctHH_22", "ACE2more6HH_22", 
  "ACE1more4Com_22", "NbhdSupp_22", "NbhdSafe_22", "SchlSafe_22"
)
  #filteredData <- rawData[, selectedColumns]

# Save Filtered Data
  #save(filteredData, file = "Final Project/filtered_NSCH.RData")

# Load Filtered Data
load("Final Project/filtered_NSCH.RData")
currData <- filteredData


## Data Imputations / Mutations ##

# Create Mental Health Variable
currData$MHealthConcern <- factor(
  # K2Q33B = "Anxiety Currently", K2Q32B = "Depression Currently"
  ifelse(currData$K2Q33B == 1 | currData$K2Q32B == 1, 1, 0),
  levels = c(0, 1))

currData <- currData %>% filter(MHealthConcern %in% c(0, 1))

# Create Mental Health Score
currData <- currData %>% mutate(
  # Past Mental Health Indicators | K2Q33A = "Anxiety, K2Q32A = "Depression"
  anxiety_past = ifelse(K2Q33A == 1, 1, 0),
  depression_past = ifelse(K2Q32A == 1, 1, 0),
  
  # Current Mental Health Indicators | K2Q33B = "Anxiety Currently", K2Q32B = "Depression Currently"
  anxiety_current = ifelse(K2Q33B == 1, 1, 0),
  depression_current = ifelse(K2Q32B == 1, 1, 0),
  
  MHealthScore = case_when(
    # Highest severity: Both past and current mental health issues
    anxiety_past == 1 & anxiety_current == 1 & depression_past == 1 & depression_current == 1 ~ 3,
    
    # High severity: Past mental health issues with current symptoms
    (anxiety_past == 1 | depression_past == 1) & (anxiety_current == 1 | depression_current == 1) ~ 2,
    
    # Moderate severity: Either past or current issues
    anxiety_past == 1 | anxiety_current == 1 | depression_past == 1 | depression_current == 1 ~ 1,
    
    # No mental health issues (Level 0)
    TRUE ~ 0
  )
)

## Clean Data ##

# Factor Variables
currData$SC_RACE_R <- as.factor(currData$SC_RACE_R)
currData$sex_22 <- as.factor(currData$sex_22)
currData$K8Q35 <- as.factor(currData$K8Q35)
currData$TalkAbout_22 <- as.factor(currData$TalkAbout_22)
currData$TalkAbout_22 <- as.factor(currData$MHealthConcern)

# Race Data
raceFilter <- c("SC_RACE_R", "age3_22", "MHealthConcern", "MHealthScore")
raceData <- currData[, raceFilter]

# Label Race Variable
raceData$RACE <- factor(raceData$SC_RACE_R,
                        levels = c(1, 2, 3, 4, 5, 7),
                        labels = c("White alone",
                                   "Black or African American alone", 
                                   "American Indian or Alaska Native alone", 
                                   "Asian alone", 
                                   "Native Hawaiian and Other Pacific Islander alone", 
                                   "Two or More Races"))


## Data Exploration ##

# Explore Health Score by Age Group
mhealth_by_age <- currData %>%
  group_by(age3_22, MHealthScore) %>%
  summarise(Count = n(), .groups = 'drop_last') %>%
  group_by(age3_22) %>%
  mutate(Proportion = Count / sum(Count) * 100)

mhealth_by_age$age3_22 <- as.factor(mhealth_by_age$age3_22)
mhealth_by_age$MHealthScore <- as.factor(mhealth_by_age$MHealthScore)

fig1 <- ggplot(mhealth_by_age, aes(x = factor(age3_22), y = Proportion, fill = factor(MHealthScore))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues", 
                    name = "Mental Health\nSeverity Level",
                    labels = c("0: No Issues", 
                               "1: Low Severity", 
                               "2: High Severity", 
                               "3: Highest Severity")) +
  labs(
    title = "Figure 0.0 | Mental Health Severity Levels",
    subtitle = "Proportion of Levels by Age Group",
    x = "Age Group",
    y = "Proportion"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right") +
  scale_fill_brewer(palette = "Dark2")

currData_age23 <- currData %>% filter(age3_22 == 2 | age3_22 == 3)

# Explore Proportions of Health Score Filtered by Age group 2 & 3
score_summary <- currData_age23 %>%
  group_by(MHealthScore) %>%
  summarise(
    Count = n(),
    Percentage = round(n() / nrow(currData_age23) * 100, 2)
  )

fig2 <- ggplot(score_summary, aes(x = factor(MHealthScore), y = Count, fill = factor(MHealthScore))) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(Count, " (", Percentage, "%)")), 
            vjust = -0.5, 
            fontface = "bold") +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = " Figure 0.0 | Distribution of Mental Health Scores",
    subtitle = "Breakdown of Mental Health Indicators (Age Group 2 & 3)",
    x = "Mental Health Score",
    y = "Number of Individuals",
    fill = "Score Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  scale_fill_brewer(palette = "Dark2")

# Race Exploration by Age 2 and 3
raceData$age3_22 <- as.numeric(as.character(raceData$age3_22))

fig3_1 <- raceData %>% 
  filter(age3_22 == 2 | age3_22 == 3) %>%
  ggplot(aes(x = RACE, fill = RACE)) +  # Add fill aesthetic for color
  geom_bar() +  # Add geom_bar to create the bar plot
  labs(
    title = "Figure 0.0 | Distribution of Racial Categories",
    subtitle = "Age Group 2 & 3",
    x = "Race",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  scale_fill_brewer(palette = "Dark2")

fig3_2 <- raceData %>% 
  filter(age3_22 == 2 | age3_22 == 3) %>%
  ggplot(aes(x = RACE, fill = MHealthConcern)) +
  geom_bar(position = "fill") +
  labs(title = "Figure 0.0 | Mental Health Concerns by Race",
       subtitle = "Age Group 2 & 3",
       x = "Race",
       y = "Proportion",
       fill = "Mental Health Concern") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  scale_fill_brewer(palette = "Dark2")

## Define Models ##

# Neighborhood / Activities / Support Model | Model1
model1 <- "MHealthConcern ~ HHCOUNT+PHYSACTIV+K8Q35+mentor_22+ShareIdeas_22+
          ScreenTime_22+ACE4ctCom_22+ AftSchAct_22+NbhdSupp_22+NbhdSafe_22"
model12 <- "MHealthScore ~ HHCOUNT+PHYSACTIV+K8Q35+mentor_22+ShareIdeas_22+
          ScreenTime_22+ACE4ctCom_22+ AftSchAct_22+NbhdSupp_22+NbhdSafe_22"
model1_filter <- c("MHealthConcern", "HHCOUNT", "PHYSACTIV", "K8Q35", "mentor_22", "ShareIdeas_22",
                   "ScreenTime_22", "ACE4ctCom_22", "AftSchAct_22", "NbhdSupp_22", "NbhdSafe_22")

# Family / History Model | Model2
model2 <- "MHealthConcern ~ BORNUSA+ACE12+ACE11+K10Q40_R+
          age3_22+sex_22+MotherMH_22+FatherMH_22+SC_RACE_R+
          EventPart_22+mentor_22+ACE2more11_22+ACE6ctHH_22"
model22 <- "MHealthScore ~ BORNUSA+ACE12+ACE11+K10Q40_R+
          age3_22+sex_22+MotherMH_22+FatherMH_22+SC_RACE_R+
          EventPart_22+mentor_22+ACE2more11_22+ACE6ctHH_22"
model2_filter <- c("MHealthConcern", "BORNUSA", "ACE12", "ACE11", "K10Q40_R",
                   "age3_22", "sex_22", "MotherMH_22", "FatherMH_22", "SC_RACE_R",
                   "EventPart_22", "mentor_22", "ACE2more11_22", "ACE6ctHH_22")

# Adverse Events / Friends Model | Model3
model3 <- "MHealthConcern ~ HHCOUNT+BORNUSA+K8Q35+ACE12+ACE11+
          age3_22+SC_RACE_R+bully_22+bullied_22+AftSchAct_22+
          EventPart_22+mentor_22+ShareIdeas_22"
model32 <- "MHealthScore ~ HHCOUNT+BORNUSA+K8Q35+ACE12+ACE11+
          age3_22+SC_RACE_R+bully_22+bullied_22+AftSchAct_22+
          EventPart_22+mentor_22+ShareIdeas_22"
model3_filter <- c("MHealthConcern", "HHCOUNT", "BORNUSA", "K8Q35", "ACE12", "ACE11",
                     "age3_22", "SC_RACE_R", "bully_22", "bullied_22", "AftSchAct_22",
                     "EventPart_22", "mentor_22", "ShareIdeas_22")

# Combined Model | Model4
model4 <- "MHealthConcern_1 ~ HHCOUNT+BORNUSA+K8Q35+ACE12+ACE11+K10Q40_R+PHYSACTIV+
          age3_22+sex_22+MotherMH_22+FatherMH_22+ScreenTime_22+
          ACEct11_22+ACE4ctCom_22+SC_RACE_R+bully_22+bullied_22+AftSchAct_22+
          EventPart_22+mentor_22+ShareIdeas_22+ACE2more11_22+ACE6ctHH_22+
          NbhdSupp_22+NbhdSafe_22"
model42 <- "MHealthScore ~ HHCOUNT+BORNUSA+K8Q35+ACE12+ACE11+K10Q40_R+PHYSACTIV+
          age3_22+sex_22+MotherMH_22+FatherMH_22+ScreenTime_22+
          ACEct11_22+ACE4ctCom_22+SC_RACE_R+bully_22+bullied_22+AftSchAct_22+
          EventPart_22+mentor_22+ShareIdeas_22+ACE2more11_22+ACE6ctHH_22+
          NbhdSupp_22+NbhdSafe_22"
model4_filter <- c("MHealthConcern", "HHCOUNT", "BORNUSA", "K8Q35", "ACE12", "ACE11", 
                   "K10Q40_R", "PHYSACTIV", "age3_22", "sex_22", "MotherMH_22", 
                   "FatherMH_22", "ScreenTime_22", "ACEct11_22", "ACE4ctCom_22", 
                   "SC_RACE_R", "bully_22", "bullied_22", "AftSchAct_22", 
                   "EventPart_22", "mentor_22", "ShareIdeas_22", "ACE2more11_22", 
                   "ACE6ctHH_22", "NbhdSupp_22", "NbhdSafe_22")


## Data Setup ##

# Split Data
n <- nrow(currData_age23)
train_size <- floor(0.8 * n)
train_indices <- sample(seq_len(n), train_size)

train_data <- currData_age23[train_indices, ]
test_data <- currData_age23[-train_indices, ]
test_data$MHealthConcern <- factor(test_data$MHealthConcern)
train_data$MHealthConcern <- factor(train_data$MHealthConcern)
test_data$K8Q35<- as.numeric(as.character(test_data$K8Q35))
train_data$K8Q35 <- as.numeric(as.character(train_data$K8Q35))

# Logistic Regression

# Model 1 LR
logreg_1 <- glm(model1,
  data = train_data, 
  family = binomial()
)
summary(logreg_1)
logreg_1_accuracy <- getAccuracy_glm(logreg_1)

logreg_12 <- glm(model12, data = train_data,)
summary(logreg_12)
logreg_12_accuracy <- getAccuracy_glm(logreg_12, "Binomial")

# Model 2 LR
logreg_2 <- glm(model2,
                data = train_data, 
                family = binomial()
)
summary(logreg_2)
logreg_2_accuracy <- getAccuracy_glm(logreg_2)

logreg_22 <- glm(model22, data = train_data,)
summary(logreg_22)
logreg_22_accuracy <- getAccuracy_glm(logreg_22, "Binomial")

# Model 3 LR
logreg_3 <- glm(model3,
                data = train_data, 
                family = binomial()
)
summary(logreg_3)
logreg_3_accuracy <- getAccuracy_glm(logreg_3)

logreg_32 <- glm(model32, data = train_data,)
summary(logreg_32)
logreg_32_accuracy <- getAccuracy_glm(logreg_32, "Binomial")

# Model 4 LR
logreg_4 <- glm(model4,
                data = train_data, 
                family = binomial()
)
summary(logreg_4)
logreg_4_accuracy <- getAccuracy_glm(logreg_4)

logreg_42 <- glm(model42, data = train_data,)
summary(logreg_42)
logreg_42_accuracy <- getAccuracy_glm(logreg_42, "Binomial")

accuracy_df <- data.frame(
  Model = c(
    "Model 1", "Model 1.2", 
    "Model 2", "Model 2.2", 
    "Model 3", "Model 3.2", 
    "Model 4", "Model 4.2"
  ),
  Accuracy = c(
    logreg_1_accuracy, logreg_12_accuracy,
    logreg_2_accuracy, logreg_22_accuracy,
    logreg_3_accuracy, logreg_32_accuracy,
    logreg_4_accuracy, logreg_42_accuracy
  )
)

accuracy_df$ModelGroup <- substr(accuracy_df$Model, 1, 6)

fig4 <- ggplot(accuracy_df, aes(x = Model, y = Accuracy, fill = ModelGroup)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "Figure 0.0 | Logistic Model Accuracies",
       x = "Model",
       y = "Accuracy") +
  scale_fill_brewer(palette = "Dark2")

# K Means

clusterFilter <- c("HHCOUNT", "BORNUSA", "K8Q35", "ACE12", "ACE11", "K10Q40_R",
                  "PHYSACTIV", "age3_22", "sex_22", "MotherMH_22", "FatherMH_22",
                  "ScreenTime_22", "ACEct11_22", "ACE4ctCom_22", "SC_RACE_R", "bully_22",
                  "bullied_22", "AftSchAct_22", "EventPart_22", "mentor_22", "ShareIdeas_22",
                  "ACE2more11_22", "ACE6ctHH_22", "NbhdSupp_22", "NbhdSafe_22", "MHealthConcern")

clusterData <- currData %>% filter(age3_22 == 2 | age3_22 == 3)
clusterData <- clusterData[, clusterFilter]

model <- kmeans(clusterData[, -1], centers = 2, iter.max = 100)

clusterData$Cluster <- model$cluster - 1
#clusterData$Cluster <- ifelse(clusterData$Cluster == 2, 1, 2)

accuracy <- mean(clusterData$Cluster == clusterData$MHealthConcern)

combined_data <- clusterData %>%
  mutate(MHealthConcern = as.integer(as.character(MHealthConcern))) %>%
  dplyr::select(MHealthConcern, Cluster) %>%
  pivot_longer(cols = c(MHealthConcern, Cluster), names_to = "Type", values_to = "Value")

fig5 <- ggplot(combined_data, aes(x = Value, fill = Type)) +
  geom_histogram(position = "dodge", binwidth = 1, color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, max(combined_data$Value), 1)) +
  labs(title = "Figure 0.0 | Histogram of Clusters and Mental Health Concern Scores",
       x = "Value",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
       plot.subtitle = element_text(hjust = 0.5),
       legend.position = "right") +
  scale_fill_manual(labels = c("Mental Health Concern", "Cluster")) +
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Dark2")


# Neural Network

# Model 1 NN1_1
scaled_data <- train_data[, model1_filter] %>%
  mutate(across(where(is.numeric) & !matches("MHealthConcern"), scale)) %>%
  mutate(MHealthConcern = train_data$MHealthConcern) %>%
  mutate(MHealthConcern = as.numeric(as.factor(MHealthConcern)) - 1)

nn_model11 <- neuralnet(model1,
                      data = scaled_data, 
                      linear.output = FALSE,
                      likelihood = TRUE,
                      #hidden = c(2, 1),
                      algorithm = "rprop+",
                      err.fct = "ce")
nn_results <- nn_stats(nn_model11, test_data)
rmse_nn11 <- nn_results$rmse
metrics_nn11 <- calculate_metrics(nn_results$confusion_matrix)
knitr::kable(metrics_nn11, col.names = c("Metric", "Value"), caption = "Neural Network Model 1 Performance Metrics | NN1_1")

# Model 1 NN1_2
nn_model12 <- neuralnet(model1,
                       data = scaled_data, 
                       linear.output = FALSE,
                       likelihood = TRUE,
                       hidden = c(2, 1),
                       algorithm = "rprop+",
                       err.fct = "ce")
nn_results <- nn_stats(nn_model12, test_data)
rmse_nn12 <- nn_results$rmse
metrics_nn12 <- calculate_metrics(nn_results$confusion_matrix)
knitr::kable(metrics_nn12, col.names = c("Metric", "Value"), caption = "Neural Network Model 2 Performance Metrics | NN1_2")

# Model 4 NN4_1
scaled_data <- train_data[, model4_filter] %>%
  mutate(across(where(is.numeric) & !matches("MHealthConcern"), scale)) %>%
  mutate(MHealthConcern = train_data$MHealthConcern) %>%
  mutate(MHealthConcern = as.numeric(as.factor(MHealthConcern)) - 1)


train_data <- train_data[, model4_filter]
test_data <- test_data[, model4_filter]
train_data <- dummy_cols(train_data, 
                         remove_selected_columns = TRUE, 
                         remove_first_dummy = TRUE)
test_data <- dummy_cols(test_data, 
                         remove_selected_columns = TRUE, 
                         remove_first_dummy = TRUE)
model4_string <- paste(colnames(train_data), collapse = " + ")
model4_string <- gsub("MHealthConcern_1", "", model4_string)
model4_string <- as.formula(paste("MHealthConcern_1 ~", model4_string))

nn_model41 <- neuralnet(model4_string,
                        data = train_data, 
                        linear.output = FALSE,
                        likelihood = TRUE,
                        #hidden = c(2, 1),
                        algorithm = "rprop+",
                        err.fct = "ce")
nn_results <- nn_stats(nn_model41, test_data)
rmse_nn41 <- nn_results$rmse
metrics_nn41 <- calculate_metrics(nn_results$confusion_matrix)
knitr::kable(metrics_nn41, col.names = c("Metric", "Value"), caption = "Neural Network Model 4 Performance Metrics | NN4_1")

nn_model42 <- neuralnet(model4_string,
                        data = train_data, 
                        linear.output = FALSE,
                        likelihood = TRUE,
                        hidden = c(2, 1),
                        algorithm = "rprop+",
                        err.fct = "ce")
nn_results <- nn_stats(nn_model42, test_data)
rmse_nn42 <- nn_results$rmse
metrics_nn42 <- calculate_metrics(nn_results$confusion_matrix)
knitr::kable(metrics_nn42, col.names = c("Metric", "Value"), caption = "Neural Network Model 4 Performance Metrics | NN4_2")

done = FALSE
while(done == FALSE){
  nn_model43 <- neuralnet(model4_string,
                          data = train_data, 
                          linear.output = FALSE,
                          likelihood = TRUE,
                          hidden = c(5, 5, 3),
                          algorithm = "rprop+",
                          err.fct = "ce")
  if (!is.null(nn_model43$result.matrix)) {
    done <- TRUE
  }
}
nn_results <- nn_stats(nn_model43, test_data)
rmse_nn43 <- nn_results$rmse
metrics_nn43 <- calculate_metrics(nn_results$confusion_matrix)
knitr::kable(metrics_nn43, col.names = c("Metric", "Value"), caption = "Neural Network Model 4 Performance Metrics | NN4_3")





library(vip)
library(NeuralNetTools)
vip(nn_model43)




### Show Figures ###
# Figure 1 | Figure 0.0 --> Health Score by Age Group Filled by Score
grid.arrange(fig1)
# Figure 2 | Figure 0.0 --> Mental Health Score Proportion filtered by Age Group 2/3
grid.arrange(fig2)
# Figure 3 | Figure 0.0 --> Mental Health Score by Race
grid.arrange(fig3_1, fig3_2, ncol = 2, nrow = 1)
# Figure 4 | Figure 0.0 --> Logistic Models Accuracies
grid.arrange(fig4)
# Figure 5 | Figure 0.0 --> Kmeans results
grid.arrange(fig5)



# Tree
tree_model <- rpart(MHealthConcern ~ HHCOUNT + K8Q35 + TalkAbout_22 + SC_RACE_R +
                      NbhdSupp_22 + NbhdSafe_22 + ACE2more6HH_22 + INQ_INCOME,
                    data = train_data,
                    method = "anova",
                    control = rpart.control(cp = 0.001))

rpart.plot(tree_model, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)

# Fit a random forest model
rf_model <- randomForest(MHealthConcern ~ HHCOUNT + K8Q35 + TalkAbout_22 + SC_RACE_R +
                           NbhdSupp_22 + NbhdSafe_22 + ACE2more6HH_22 + INQ_INCOME,
                         data = train_data, importance = TRUE)

plot(rf_model)
rf_predictions <- predict(rf_model, test_data)
rf_mse <- mean((rf_predictions - test_data$MHealthConcern)^2)