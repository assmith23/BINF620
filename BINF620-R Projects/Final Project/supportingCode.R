# *************************************
# *  A Manning Smith
# *
# *
# *
# *
# *
# *************************************

## Load Libraries ##
library(dplyr)
library(ggplot2)
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
library(vip)
library(NeuralNetTools)
library(rpart)
library(rpart.plot)


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

get_model_metrics <- function(model, test_data, target_column) {
  predictions <- predict(model, newdata = test_data, type = "response")
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$MHealthConcern)
  
  # Calculate metrics using the previously defined function
  metrics_df <- calculate_metrics(conf_matrix)
  return(metrics_df)
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
  "ACE1more4Com_22", "NbhdSupp_22", "NbhdSafe_22", "SchlSafe_22", "FAMILY_R"
)
  #filteredData <- rawData[, selectedColumns]

# Save Filtered Data
  #save(filteredData, file = "Final Project/filtered_NSCH.RData")

# Load Filtered Data
load("Final Project/filtered_NSCH.RData")
currData <- filteredData

# Load Model Data
  #load("Final Project/modelData_NSCH.RData")

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


## Model Data Mutations ##
modelFilter <- c("SC_AGE_YEARS", "HHCOUNT", "BORNUSA", "K8Q35", "ACE12", 
                 "PHYSACTIV", "age3_22", "sex_22", "MotherMH_22", 
                 "FatherMH_22", "ScreenTime_22", "ACEct11_22", "ACE4ctCom_22", 
                 "SC_RACE_R", "bully_22", "bullied_22", "AftSchAct_22", 
                 "EventPart_22", "mentor_22", "ShareIdeas_22", 
                 "ACE6ctHH_22", "NbhdSupp_22", "NbhdSafe_22", "FAMILY_R",
                 "K2Q32B", "K2Q33B", "K2Q32A", "K2Q33A", "SC_AGE_YEARS", "MHealthConcern")

modelData <- currData[, modelFilter]

# Mutation 1
modelData$MotherMH_22 <- ifelse(modelData$MotherMH_22 == 3, 1, 
                                ifelse(modelData$MotherMH_22 %in% c(1, 2), 0, 99))
modelData$FatherMH_22 <- ifelse(modelData$FatherMH_22 == 3, 1, 
                                ifelse(modelData$FatherMH_22 %in% c(1, 2), 0, 99))
modelData <- modelData %>%
  filter(MotherMH_22 < 10 & FatherMH_22 < 10)

# Mutation 2
modelData$EventPart_22 <- ifelse(modelData$EventPart_22 %in% c(1, 2), 0, 
                                 ifelse(modelData$EventPart_22 %in% c(3, 4), 1, 99))
modelData <- modelData %>%
  filter(EventPart_22 < 10)

# Mutation 3
modelData$NbhdSafe_22 <- ifelse(modelData$NbhdSafe_22 %in% c(1, 2), 0, 
                                ifelse(modelData$NbhdSafe_22 == 3, 1, 99))
modelData <- modelData %>%
  filter(NbhdSafe_22 < 10)

# Mutation 4
modelData$PHYSACTIV <- ifelse(modelData$PHYSACTIV %in% c(1,0), 0, 
                              ifelse(modelData$PHYSACTIV %in% c(3,4), 1,99))

# Mutation 5
modelData$ACE4ctCom_22 <- ifelse(modelData$ACE4ctCom_22 == 1, 0, 
                                 ifelse(modelData$ACE4ctCom_22 == 2, 1, 99))

modelData$AftSchAct_22 <- ifelse(modelData$AftSchAct_22 == 2, 0, 
                                 ifelse(modelData$AftSchAct_22 == 1, 1, 99))
modelData <- modelData %>%
  filter(AftSchAct_22 < 10)

# Mutation 6
modelData$ACE12 <- modelData$ACE12 %% 2

# Mutation 7
modelData$AftSchAct_22 <- ifelse(modelData$AftSchAct_22 == 2, 0, 
                                 ifelse(modelData$AftSchAct_22 == 1, 1, 99))
# Mutation 8
modelData <- modelData %>%
  filter(AftSchAct_22 < 10)

# Mutation 9
modelData <- modelData %>%
  filter(ScreenTime_22 < 10)

# Mutation 10
modelData <- modelData %>%
  filter(bully_22 < 10)
modelData <- modelData %>%
  filter(bullied_22 < 10)

# Mutation 11
modelData$K8Q35 <- modelData$K8Q35 %% 2

# Mutation 12
modelData <- modelData %>%
  filter(NbhdSupp_22 < 10)

# Mutation 13
modelData$ShareIdeas_22 <- ifelse(modelData$ShareIdeas_22 %in% c(1,2), 0, 
                                  ifelse(modelData$ShareIdeas_22 == 3, 0, 99))
modelData <- modelData %>%
  filter(ShareIdeas_22 < 10)

# Mutation 14
modelData <- modelData %>%
  filter(mentor_22 < 10)

modelData$mentor_22 <- modelData$mentor_22 %% 2

  #save(modelData, file = "modelData_NSCH.RData")


## Data Exploration ##

# Explore Mother/Father Mental Health History
long_data <- currData %>%
  pivot_longer(cols = c(MotherMH_22, FatherMH_22), 
               names_to = "Parent", 
               values_to = "Category") %>%
  group_by(Parent, Category) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportion = count / sum(count))
  
# Plot proportions
fig6 <- ggplot(long_data, aes(x = factor(Category), y = proportion, fill = Parent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Category",
    y = "Proportion",
    title = "Figure 1.1 | Proportions of Mental Health Severity by Categories of Parent",
    fill = "Parent"
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Excellent",
      "2" = "Good",
      "3" = "Fair or Poor",
      "95" = "No Primary Caregiver",
      "99" = "Missing Values"
    )
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right") +
  scale_fill_brewer(palette = "Dark2")

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
    title = "Figure 2.1 | Mental Health Severity Levels",
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
    title = " Figure 3.1 | Distribution of Mental Health Scores",
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
    title = "Figure 4.1 | Distribution of Racial Categories",
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
  labs(title = "Figure 4.2 | Mental Health Concerns by Race",
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
          ScreenTime_22+ACE4ctCom_22+AftSchAct_22+NbhdSupp_22+NbhdSafe_22"
model1_filter <- c("MHealthConcern", "HHCOUNT", "PHYSACTIV", "K8Q35", "mentor_22", "ShareIdeas_22",
                   "ScreenTime_22", "ACE4ctCom_22", "AftSchAct_22", "NbhdSupp_22", "NbhdSafe_22")

# Family / History Model | Model2
model2 <- "MHealthConcern ~ BORNUSA+ACE12+SC_AGE_YEARS+sex_22+MotherMH_22+FatherMH_22+SC_RACE_R+
          EventPart_22+mentor_22+ACE6ctHH_22+FAMILY_R"
model2_filter <- c("MHealthConcern", "BORNUSA", "ACE12", "ACE11", "K10Q40_R",
                   "age3_22", "sex_22", "MotherMH_22", "FatherMH_22", "SC_RACE_R",
                   "EventPart_22", "mentor_22", "ACE2more11_22", "ACE6ctHH_22")

# Adverse Events / Friends Model | Model3
model3 <- "MHealthConcern ~ HHCOUNT+BORNUSA+K8Q35+ACE12+
          SC_AGE_YEARS+SC_RACE_R+bully_22+bullied_22+AftSchAct_22+
          EventPart_22+mentor_22+ShareIdeas_22"
model3_filter <- c("MHealthConcern", "HHCOUNT", "BORNUSA", "K8Q35", "ACE12", "ACE11",
                     "age3_22", "SC_RACE_R", "bully_22", "bullied_22", "AftSchAct_22",
                     "EventPart_22", "mentor_22", "ShareIdeas_22")

# Combined Model | Model4
model4 <- "MHealthConcern ~ HHCOUNT+BORNUSA+K8Q35+ACE12+PHYSACTIV+
          SC_AGE_YEARS+sex_22+MotherMH_22+FatherMH_22+ScreenTime_22+
          ACEct11_22+ACE4ctCom_22+SC_RACE_R+bully_22+bullied_22+AftSchAct_22+
          EventPart_22+mentor_22+ShareIdeas_22+ACE6ctHH_22+
          NbhdSupp_22+NbhdSafe_22+FAMILY_R"
model4_filter <- c("MHealthConcern", "HHCOUNT", "BORNUSA", "K8Q35", "ACE12", 
                   "PHYSACTIV", "SC_AGE_YEARS", "sex_22", "MotherMH_22", 
                   "FatherMH_22", "ScreenTime_22", "ACEct11_22", "ACE4ctCom_22", 
                   "SC_RACE_R", "bully_22", "bullied_22", "AftSchAct_22", 
                   "EventPart_22", "mentor_22", "ShareIdeas_22", 
                   "ACE6ctHH_22", "NbhdSupp_22", "NbhdSafe_22", "FAMILY_R")

# Final Model
final_model <- "MHealthConcern ~ SC_AGE_YEARS * sex_22 * SC_RACE_R + PHYSACTIV *
                                 AftSchAct_22 * ACEct11_22  + ACE4ctCom_22 +
                                 MotherMH_22 * FatherMH_22 + FAMILY_R + HHCOUNT + bullied_22 + ACE12 + 
                                 ShareIdeas_22 + mentor_22"

# Factor Variables
factor_Variables <- c("MHealthConcern", "BORNUSA", "K8Q35", "ACE12", 
                      "PHYSACTIV", "sex_22", "MotherMH_22",  
                      "FatherMH_22", "ScreenTime_22", "ACEct11_22", "ACE4ctCom_22", 
                      "SC_RACE_R", "bully_22", "bullied_22", "AftSchAct_22", 
                      "EventPart_22", "mentor_22", "ShareIdeas_22", 
                      "NbhdSupp_22", "NbhdSafe_22", "FAMILY_R")

## Data Setup ##

# Split Data
n <- nrow(modelData)
train_size <- floor(0.7 * n)
train_indices <- sample(seq_len(n), train_size)

train_data <- currData_age23[train_indices, ]
test_data <- currData_age23[-train_indices, ]
test_data$MHealthConcern <- factor(test_data$MHealthConcern)
train_data$MHealthConcern <- factor(train_data$MHealthConcern)
test_data$K8Q35<- as.numeric(as.character(test_data$K8Q35))
train_data$K8Q35 <- as.numeric(as.character(train_data$K8Q35))

train_data[factor_Variables] <- lapply(train_data[factor_Variables], as.factor)
test_data[factor_Variables] <- lapply(test_data[factor_Variables], as.factor)

# Logistic Regression

# Model 1 LR
logreg_1 <- glm(model1,
  data = train_data, 
  family = binomial()
)
summary(logreg_1)
logreg_1_accuracy <- getAccuracy_glm(logreg_1)

# Model 2 LR
logreg_2 <- glm(model2,
                data = train_data, 
                family = binomial()
)
summary(logreg_2)
logreg_2_accuracy <- getAccuracy_glm(logreg_2)

# Model 3 LR
logreg_3 <- glm(model3,
                data = train_data, 
                family = binomial()
)
summary(logreg_3)
logreg_3_accuracy <- getAccuracy_glm(logreg_3)

# Model 4 LR
logreg_4 <- glm(model4,
                data = train_data, 
                family = binomial()
)
summary(logreg_4)
logreg_4_accuracy <- getAccuracy_glm(logreg_4)

# Final Model
logreg_final <- glm(final_model,
                data = train_data, 
                family = binomial()
)
summary(logreg_final)
logreg_final_accuracy <- getAccuracy_glm(logreg_final)

plot(logreg_final)

# Combine Results
models <- list(logreg_1, logreg_2, logreg_3, logreg_4, logreg_final)
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4", "Final Model")
all_metrics <- lapply(models, get_model_metrics, test_data = test_data, target_column = "MHealthConcern")
combined_metrics <- do.call(rbind, all_metrics)
combined_metrics$Model <- rep(model_names, each = nrow(combined_metrics) / length(model_names))
logisticReg_metrics <- combined_metrics[, c("Model", "Metric", "Value")]

# K Means

clusterFilter <- c("HHCOUNT", "BORNUSA", "ACE12",
                  "PHYSACTIV", "SC_AGE_YEARS", "sex_22", "MotherMH_22", "FatherMH_22",
                  "ScreenTime_22", "ACEct11_22", "ACE4ctCom_22", "SC_RACE_R", "bully_22",
                  "bullied_22", "AftSchAct_22", "EventPart_22", "mentor_22", "ShareIdeas_22",
                  "ACE6ctHH_22", "NbhdSupp_22", "NbhdSafe_22", "FAMILY_R", "MHealthConcern")

clusterData <- modelData
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

# Final Model
n <- nrow(modelData)
train_size <- floor(0.7 * n)
train_indices <- sample(seq_len(n), train_size)

train_data <- modelData[train_indices, ]
test_data <- modelData[-train_indices, ]
train_data <- train_data[, model4_filter]
test_data <- test_data[, model4_filter]
train_data[factor_Variables] <- lapply(train_data[factor_Variables], as.factor)
test_data[factor_Variables] <- lapply(test_data[factor_Variables], as.factor)
train_data <- dummy_cols(train_data, 
                         remove_selected_columns = TRUE, 
                         remove_first_dummy = TRUE)
test_data <- dummy_cols(test_data, 
                        remove_selected_columns = TRUE, 
                        remove_first_dummy = TRUE)
model_final_string <- paste(colnames(train_data), collapse = " + ")
model_final_string <- gsub("MHealthConcern_1", "", model_final_string)
model_final_string <- as.formula(paste("MHealthConcern_1 ~", model_final_string))

nn_model_final <- neuralnet(model_final_string,
                        data = train_data, 
                        linear.output = FALSE,
                        likelihood = TRUE,
                        algorithm = "rprop-",
                        stepmax = 1e+06,
                        err.fct = "ce")

nn_model_final <- neuralnet(model_final_string,
                            data = train_data, 
                            linear.output = FALSE,
                            likelihood = TRUE,
                            algorithm = "rprop+",
                            err.fct = "ce",
                            rep = 5)
nn_results <- nn_stats(nn_model_final, test_data)
rmse_nnFINAL <- nn_results$rmse
metrics_nnFINAL <- calculate_metrics(nn_results$confusion_matrix)
vip(nn_model_final)
plot(nn_model_final)


### Show Figures ###
# Figure 1 | Figure 2.1 --> Health Score by Age Group Filled by Score
grid.arrange(fig1)
# Figure 2 | Figure 3.1 --> Mental Health Score Proportion filtered by Age Group 2/3
grid.arrange(fig2)
# Figure 3 | Figure 4.0 --> Mental Health Score by Race
grid.arrange(fig3_1, fig3_2, ncol = 2, nrow = 1)
# Figure 5 | Figure 5.1 --> Kmeans results
grid.arrange(fig5)
# Figure 6 | Figure 1.1 --> Mother/Father Mental Health History
grid.arrange(fig6)

# Table 1 | Table 1.1 --> Logistic Regression Results
knitr::kable(logisticReg_metrics, col.names = c("Model", "Metric", "Value"), caption = "Table 1.1 | Logistic Regression Model Performance Metrics")
# Table 2 | Table 2.1 --> Final Neural Network
knitr::kable(metrics_nnFINAL, col.names = c("Metric", "Value"), caption = "Table 2.1 | Neural Network Final Model Performance Metrics | NN5_1")