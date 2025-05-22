library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)

rawData <- read.csv("Final Project/NSCH_2022e_DRC.csv")

summary(rawData$SC_AGE_YEARS)

cols <- colnames(rawData)

columns_to_keep <- c(
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
  "NbhdDetract_22", "PHYSACTIV"
)

# Create a new data frame with only the specified columns
filteredData <- rawData[, columns_to_keep]
save(filteredData, file = "filtered_NSCH.RData")

## Race Factors
raceFacotrs <- c("INQ_RESSEG", "INQ_EDU", "INQ_EMPLOY", "INQ_INCOME", "INQ_HOME", 
                 "race4_22", "raceASIA_22", "race7_22", "SC_ASIAN", "SC_AIAN", 
                 "SC_NHPI", "SC_RACE_R", "SC_HISPANIC_R", "K2Q32B", "K2Q33B", "age3_22")

raceFiltered <- rawData[, raceFacotrs]

raceFiltered$RACE <- factor(raceFiltered$SC_RACE_R, 
                            levels = c(1, 2, 3, 4, 5, 7),
                            labels = c("White alone", 
                                       "Black or African American alone", 
                                       "American Indian or Alaska Native alone", 
                                       "Asian alone", 
                                       "Native Hawaiian and Other Pacific Islander alone", 
                                       "Two or More Races"))

ggplot(raceFiltered, aes(x = RACE)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Racial Categories",
       x = "Race",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

raceFiltered$MHealthConcern <- factor(
  ifelse(raceFiltered$K2Q33B == 1 | raceFiltered$K2Q32B == 1, "Yes", "No"),
  levels = c("No", "Yes")
)

raceFiltered <- raceFiltered %>% filter(age3_22 == 3)

ggplot(raceFiltered, aes(x = RACE, fill = MHealthConcern)) +
  geom_bar(position = "fill") +
  labs(title = "Mental Health Concerns by Race | Age Group 3",
       x = "Race",
       y = "Proportion",
       fill = "Mental Health Concern") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

raceFiltered <- raceFiltered %>% filter(age3_22 == 2)

ggplot(raceFiltered, aes(x = RACE, fill = MHealthConcern)) +
  geom_bar(position = "fill") +
  labs(title = "Mental Health Concerns by Race | Age Group 2",
       x = "Race",
       y = "Proportion",
       fill = "Mental Health Concern") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

currentFilter <- c("anxiety_22", "depress_22", "SC_RACE_R", "hrsareg", "age3_22", "AnxietSev_22", "DepresSev_22")

currData <- filteredData[, currentFilter]

currData$RACE <- factor(currData$SC_RACE_R,
                            levels = c(1, 2, 3, 4, 5, 7),
                            labels = c("White alone", 
                                       "Black or African American alone", 
                                       "American Indian or Alaska Native alone", 
                                       "Asian alone", 
                                       "Native Hawaiian and Other Pacific Islander alone", 
                                       "Two or More Races"))

currData$MHealthConcern <- factor(
  ifelse(currData$anxiety_22 == 1 | currData$depress_22 == 1, "Yes", "No"),
  levels = c("No", "Yes"))

currData$MHealthConcern_parent <- factor(
  ifelse(currData$AnxietSev_22 == 1 | currData$DepresSev_22 == 1, "Yes", "No"),
  levels = c("No", "Yes"))

plot_data <- currData %>%
  group_by(RACE, MHealthConcern, MHealthConcern_parent) %>%
  summarise(count = n()) %>%
  group_by(RACE) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Create a long-format dataframe for easier plotting
plot_data_long <- plot_data %>%
  pivot_longer(cols = c(MHealthConcern, MHealthConcern_parent), 
               names_to = "Concern_Type", 
               values_to = "Concern_Status")

# Plot
ggplot(plot_data_long, aes(x = RACE, y = prop, fill = interaction(Concern_Type, Concern_Status))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mental Health Concerns by Race: Child vs Parent Reporting",
       x = "Race",
       y = "Proportion",
       fill = "Concern Type") +
  scale_fill_manual(
    values = c("MHealthConcern.No" = "lightblue", 
               "MHealthConcern.Yes" = "blue", 
               "MHealthConcern_parent.No" = "lightgreen", 
               "MHealthConcern_parent.Yes" = "darkgreen"),
    labels = c("Child No Concern", "Child Concern", 
               "Parent No Concern", "Parent Concern")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format())

# Alternative visualization - side-by-side comparison
ggplot(plot_data_long, aes(x = RACE, y = prop, fill = Concern_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Concern_Type, labeller = labeller(
    Concern_Type = c("MHealthConcern" = "Child-Reported", 
                     "MHealthConcern_parent" = "Parent-Reported")
  )) +
  labs(title = "Mental Health Concerns by Race: Child vs Parent Reporting",
       x = "Race",
       y = "Proportion",
       fill = "Concern Status") +
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format())

# Chi-square test to check statistical significance
library(stats)
chisq_child <- chisq.test(table(currData$RACE, currData$MHealthConcern))
chisq_parent <- chisq.test(table(currData$RACE, currData$MHealthConcern_parent))

print("Chi-square test for child-reported mental health concerns:")
print(chisq_child)
print("Chi-square test for parent-reported mental health concerns:")
print(chisq_parent)








currentFilter <- c("K2Q33A", "K2Q32A", "SC_RACE_R", "sex_22", "SC_AGE_YEARS", "age3_22")

currData <- filteredData[, currentFilter]

currData$SC_RACE_R <- as.factor(currData$SC_RACE_R)
currData$sex_22 <- as.factor(currData$sex_22)

currData$MHealthConcern <- factor(
  ifelse(currData$K2Q33A == 1 | currData$K2Q32A == 1, "Yes", "No"),
  levels = c("No", "Yes"))

model <- "MHealthConcern ~ SC_RACE_R + sex_22 + SC_AGE_YEARS"

logistic_model <- glm(
  # MHealthConcern ~ SC_RACE_R + sex_22 + SC_AGE_YEARS
  MHealthConcern ~ sex_22, 
  data = currData, 
  family = binomial()
)

summary(logistic_model)

library(broom)

# Tidy output with odds ratios and confidence intervals
tidy_model <- tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE)
print(tidy_model)

library(pscl)
pR2 <- pR2(logistic_model)
print(pR2)

# Model fit assessment
# Likelihood Ratio Test
anova(logistic_model, test = "LRT")

# Check for multicollinearity
library(car)
vif_values <- vif(logistic_model)
print("Variance Inflation Factors:")
print(vif_values)

# Prediction and model performance
# Predict probabilities
currData$predicted_prob <- predict(logistic_model, type = "response")

# Classification table
library(caret)
predicted_classes <- ifelse(currData$predicted_prob > 0.5, "Yes", "No")
confusion_matrix <- confusionMatrix(
  as.factor(predicted_classes), 
  as.factor(currData$MHealthConcern)
)
print(confusion_matrix)

currData <- currData %>% filter(age3_22 == 3)

currData$sex_22 <- factor(currData$sex_22,
                        levels = c(1, 2),
                        labels = c("Male", 
                                   "Female"))

ggplot(currData, aes(x = sex_22, fill = MHealthConcern)) +
  geom_bar(position = "fill") +
  labs(title = "Mental Health Concerns by Sex",
       x = "Sex",
       y = "Proportion",
       fill = "Mental Health Concern") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")



perform_chi_square <- function(data, depression_var, activity_var) {
  # Create contingency table
  cont_table <- table(data[[depression_var]], data[[activity_var]])
  
  # Perform chi-square test
  chi_test <- chisq.test(cont_table)
  
  # Print results
  cat("Chi-Square Test for", depression_var, "and", activity_var, "\n")
  cat("Contingency Table:\n")
  print(cont_table)
  cat("\nChi-Square Results:\n")
  print(chi_test)
  cat("\nEffect Size (Cramer's V):\n")
  
  # Calculate Cramer's V for effect size
  cramer_v <- sqrt(chi_test$statistic / 
                     (length(data[[depression_var]]) * 
                        (min(dim(cont_table)) - 1)))
  print(cramer_v)
  cat("\n---\n")
}

# List of activity variables to test
activity_vars <- c(
  "PHYSACTIV",      # Physical activity
  "SCREENTIME",     # Screen time
  "OUTDOORSWKDAY",  # Weekday outdoor time
  "OUTDOORSWKEND",  # Weekend outdoor time
  "K7Q31",          # Clubs or Organizations
  "K7Q32",          # Organized Activities
  "AftSchAct_22"    # After-school activities indicator
)

# Perform chi-square tests
for (var in activity_vars) {
  perform_chi_square(filteredData %>% filter(age3_22 == 3), "depress_22", var)
}

temp <- filteredData %>% filter(age3_22 == 3)

p_values <- sapply(activity_vars, function(var) {
  cont_table <- table(temp$depress_22, temp[[var]])
  chisq.test(cont_table)$p.value
})

# Bonferroni corrected p-values
bonferroni_p <- p.adjust(p_values, method = "bonferroni")
print("Bonferroni Corrected P-values:")
print(bonferroni_p) 



activityData <- filteredData %>% filter(age3_22 == 3)

activityData$MHealthConcern <- factor(
  ifelse(activityData$K2Q33B == 1 | activityData$K2Q32B == 1, "Yes", "No"),
  levels = c("No", "Yes")
)

formula <- "MHealthConcern ~ PHYSACTIV + SCREENTIME + OUTDOORSWKDAY + OUTDOORSWKEND + 
                      K7Q31 + K7Q32 + AftSchAct_22 + sports_22 + volunteer_22"

model <- glm(formula, 
             data = activityData, 
             family = binomial(link = "logit"))

# Tidy model results
tidy_results <- tidy(model, conf.int = TRUE, exponentiate = TRUE)

# Print model summary
cat("Logistic Regression Model Summary:\n")
print(summary(model))

# Print tidy results (odds ratios with confidence intervals)
cat("\nOdds Ratios and Confidence Intervals:\n")
print(tidy_results)










activityData <- filteredData %>% filter(age3_22 == 3)

activityData$MHealthConcern <- factor(
  ifelse(activityData$K2Q33B == 1 | activityData$K2Q32B == 1, "Yes", "No"),
  levels = c("No", "Yes")
)

activityData$K2Q33C <- as.factor(activityData$K2Q33C)

formula <- "K2Q33C ~ PHYSACTIV + SCREENTIME + OUTDOORSWKDAY + OUTDOORSWKEND + 
                      K7Q31 + K7Q32 + AftSchAct_22 + sports_22 + volunteer_22"

# Run ordinal logistic regression
model <- polr(formula, 
              data = activityData, 
              Hess = TRUE)  # Hess=TRUE computes standard errors

# Compute p-values
coef_table <- coef(summary(model))
p_values <- pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE) * 2

# Combine coefficient table with p-values
results_table <- cbind(coef_table, "p.value" = p_values)

# Print model summary
#cat("Ordinal Logistic Regression Results for", activityData$K2Q33C, "\n")
print(results_table)

# Odds ratios with confidence intervals
cat("\nOdds Ratios and Confidence Intervals:\n")
odds_ratios <- exp(cbind(OR = coef(model), 
                         confint(model)))
print(odds_ratios)




library(ordinal)
# Full proportional odds model
full_model <- clm(formula, data = activityData)

# Partial proportional odds model
partial_model <- clm(formula, data = activityData, 
                     nominal = ~ PHYSACTIV + SCREENTIME)  # Allow some predictors to vary across thresholds

# Likelihood ratio test
lr_test <- anova(full_model, partial_model)

print(lr_test)


################################################

## Load R Dataset
load("Final Project/filtered_NSCH.RData")

## Child Experiences
# Depression, Anxiety, Race, sex, age,
# age group (3 groups), Adverse Experiences, Household Experiences, Community Experiences
# Safe Neighborhood, Supportive Neighborhood, Safe Neighborhood, Places Lived, People at Address, Some to turn too,
# Share ideas, family talks, shares ideas,
# Making Friends, bully, bullied
currentFilter <- c("K2Q33A", "K2Q32A", "SC_RACE_R", "sex_22", "SC_AGE_YEARS",
                   "age3_22", "ACEct11_22", "ACE2more6HH_22", "ACE1more4Com_22",
                   "K10Q40_R", "NbhdSupp_22", "NbhdSafe_22", "PLACESLIVED", "HHCOUNT", "K8Q35",
                   "K8Q21", "TalkAbout_22", "ShareIdeas_22", 
                   "MakeFriend_22", "bully_22", "bullied_22")

currData <- filteredData[, currentFilter]
currData$SC_RACE_R <- as.factor(currData$SC_RACE_R)
currData$sex_22 <- as.factor(currData$sex_22)
currData$K8Q35 <- as.factor(currData$K8Q35)
currData$K8Q21 <- as.factor(currData$K8Q21)
currData$TalkAbout_22 <- as.factor(currData$TalkAbout_22)
currData$ShareIdeas_22 <- as.factor(currData$ShareIdeas_22)
currData$MakeFriend_22 <- as.factor(currData$MakeFriend_22)

currData <- currData %>% filter(age3_22 == 3)
#currData <- currData %>% filter(MakeFriend_22 != 90 | MakeFriend_22 != 99)

currData$MHealthConcern <- factor(
  ifelse(currData$K2Q33A == 1 | currData$K2Q32A == 1, "Yes", "No"),
  levels = c("No", "Yes"))

# Neighboorhood Impacts
logistic_model <- glm(
  MHealthConcern ~ NbhdSupp_22 + NbhdSafe_22 + ACE1more4Com_22, 
  data = currData, 
  family = binomial()
)

summary(logistic_model)

# Family Impacts
logistic_model <- glm(
  MHealthConcern ~ ACE2more6HH_22 + HHCOUNT + PLACESLIVED + K8Q35 + TalkAbout_22, 
  data = currData, 
  family = binomial()
)

summary(logistic_model)

# Places Lived
logistic_model <- glm(
  MHealthConcern ~ PLACESLIVED,
  data = currData, 
  family = binomial()
)

summary(logistic_model)

# Friend Impacts / Bully or Bullied
logistic_model <- glm(
  MHealthConcern ~ MakeFriend_22 + bully_22 + bullied_22, 
  data = currData, 
  family = binomial()
)

summary(logistic_model)

ggplot(currData, aes(x = age3_22, y = bully_22)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bully",
       x = "Race",
       y = "Proportion",
       fill = "Concern Type")


# Create bullying status categories
currData <- currData %>%
  mutate(
    BullyingStatus = case_when(
      bully_22 >= 3 & bullied_22 >= 3 ~ "Bully and Bullied",
      bully_22 >= 3 & bullied_22 < 3 ~ "Bully Only",
      bully_22 < 3 & bullied_22 >= 3 ~ "Bullied Only",
      bully_22 > 6 | bullied_22 > 6 ~ "Ignore",
      TRUE ~ "Neither"
    )
  )

currData <- currData %>%
  mutate(
    Bullyied = case_when(
      bullied_22 >= 3 ~ 1,
      bullied_22 < 6 ~ 0,
      TRUE ~ 2
    )
  )

currData <- currData %>% filter(Bullyied == 1 | Bullyied == 0)



# Proportion of Bullying Status
bullying_proportions <- currData %>%
  group_by(BullyingStatus) %>%
  summarise(
    Count = n(),
    Proportion = n() / nrow(currData)
  )

# Boxplot of Depression by Bullying Status
ggplot(bullying_proportions, 
       aes(x = BullyingStatus, y = Proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Proportions of Bullying Status",
    x = "Bullying Status",
    y = "Percentage"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", Proportion)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  theme_minimal()

# ANOVA to test depression differences
depression_anova <- aov(MHealthConcern ~ BullyingStatus, data = currData)

# Print results
print(bullying_proportions)
print(summary(depression_anova))

# Friend Impacts / Bully or Bullied
logistic_model <- glm(
  MHealthConcern ~ MakeFriend_22 + BullyingStatus,
  data = currData, 
  family = binomial()
)

summary(logistic_model)

logistic_model <- glm(
  Bullyied ~ MakeFriend_22 + MHealthConcern, 
  data = currData, 
  family = binomial()
)

summary(logistic_model)



#########################################
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(survival)
library(ranger)
library(ggplot2)
library(ggfortify)
library("survminer")
library(gridExtra)
library(caret)
library(mlbench)
library(knitr)
library(kableExtra)
library(corrplot)
library(ggcorrplot)
library(mice)
library(class)
library(tidyr)
library(VIM)
library(naivebayes)
library(MASS)
library(boot)
library(neuralnet)
library(pROC)
library(MatchIt)
library(tableone)
library(WeightIt)
library(survey)

load("Final Project/filtered_NSCH.RData")

currData$RACE <- factor(currData$SC_RACE_R,
                        levels = c(1, 2, 3, 4, 5, 7),
                        labels = c("White alone",
                                   "Black or African American alone", 
                                   "American Indian or Alaska Native alone", 
                                   "Asian alone", 
                                   "Native Hawaiian and Other Pacific Islander alone", 
                                   "Two or More Races"))

# Neural Network
currentFilter <- c("K2Q33A", "K2Q32A", "SC_RACE_R", "sex_22", "SC_AGE_YEARS",
                   "age3_22", "ACE2more6HH_22", "ACE1more4Com_22",
                   "NbhdSupp_22", "NbhdSafe_22", "PLACESLIVED", "HHCOUNT", "K8Q35",
                   "TalkAbout_22", "bully_22", "bullied_22", "INQ_INCOME",
                   "INQ_HOME", "SC_RACE_R", "K2Q32B", "K2Q33B", "age3_22")
currData <- filteredData[, currentFilter]

#currData$SC_RACE_R <- as.factor(currData$SC_RACE_R)
#currData$sex_22 <- as.factor(currData$sex_22)
#currData$K8Q35 <- as.factor(currData$K8Q35)
#currData$TalkAbout_22 <- as.factor(currData$TalkAbout_22)

currData <- currData %>% filter(age3_22 == 3)

currData$MHealthConcern <- ifelse(currData$K2Q33A == 1 | currData$K2Q32A == 1, 1, 0)

model <- "MHealthConcern ~ HHCOUNT + K8Q35 + TalkAbout_22 + SC_RACE_R + NbhdSupp_22 + NbhdSafe_22 + ACE2more6HH_22 + INQ_INCOME"

n <- nrow(currData)
train_size <- floor(0.8 * n)
train_indices <- sample(seq_len(n), train_size)

train_data <- currData[train_indices, ]
test_data <- currData[-train_indices, ]
test_data$MHealthConcern <- as.numeric(as.character(test_data$MHealthConcern))
train_data$MHealthConcern <- as.numeric(as.character(train_data$MHealthConcern))

logistic_model <- glm(
  MHealthConcern ~ HHCOUNT + K8Q35 + TalkAbout_22 + SC_RACE_R + NbhdSupp_22 + NbhdSafe_22 + ACE2more6HH_22 + INQ_INCOME, 
  data = train_data, 
  family = binomial()
)

summary(logistic_model)

scaled_data <- train_data %>%
  mutate(across(where(is.numeric) & !matches("MHealthConcern"), scale)) %>%
  mutate(MHealthConcern = train_data$MHealthConcern)

nn_model <- neuralnet(model,
                      data = scaled_data, 
                      linear.output = FALSE,
                      likelihood = TRUE,
                      hidden = c(2, 1),
                      algorithm = "rprop+",
                      err.fct = "ce")

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

pred1 <- as.numeric(predict(nn_model, test_data))
rmse_nn1 <- rmse(test_data$MHealthConcern, pred1)

pred1_class <- factor(ifelse(pred1 > 0.5, 1, 0), levels = c(0, 1))
test_data$MHealthConcern <- factor(test_data$MHealthConcern, levels = c(0, 1))
conf1 <- table(Predicted = pred1_class, Actual = test_data$MHealthConcern)

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
  
  accuracy <- (TP + TN) / sum(conf1)
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

metrics1_df <- calculate_metrics(conf1)
knitr::kable(metrics1_df, col.names = c("Metric", "Value"), caption = "Model Performance Metrics | NN1")



############################
currentFilter <- c("K2Q33A", "K2Q32A", "SC_RACE_R", "sex_22", "SC_AGE_YEARS",
                   "age3_22", "ACE2more6HH_22", "ACE1more4Com_22",
                   "NbhdSupp_22", "NbhdSafe_22", "PLACESLIVED", "HHCOUNT", "K8Q35",
                   "TalkAbout_22", "bully_22", "bullied_22", "INQ_INCOME",
                   "INQ_HOME", "SC_RACE_R", "K2Q32B", "K2Q33B", "age3_22")
currData <- filteredData[, currentFilter]

covariates <- c("MHealthConcern", "HHCOUNT", "K8Q35", "TalkAbout_22", "SC_RACE_R", "NbhdSupp_22",
              "NbhdSafe_22", "ACE2more6HH_22", "INQ_INCOME")

currData <- currData %>% filter(age3_22 == 3)

currData$MHealthConcern <- ifelse(currData$K2Q33A == 1 | currData$K2Q32A == 1, 1, 0)

table_one <- CreateTableOne(
  vars = covariates, 
  strata = "MHealthConcern", 
  data = currData, 
  test = TRUE
)

table_one

# MHealthConcern ~ HHCOUNT + K8Q35 + TalkAbout_22 + SC_RACE_R + NbhdSupp_22 + NbhdSafe_22 + ACE2more6HH_22 + INQ_INCOME

ps_model <- glm(MHealthConcern ~ HHCOUNT + K8Q35 + TalkAbout_22 + SC_RACE_R +
                  NbhdSupp_22 + NbhdSafe_22 + ACE2more6HH_22 + INQ_INCOME,
                family = binomial(),
                data = currData
)

currData$propensity_score <- predict(ps_model, type = "response")

match_results <- matchit(MHealthConcern ~ HHCOUNT + K8Q35 + TalkAbout_22 + SC_RACE_R +
                           NbhdSupp_22 + NbhdSafe_22 + ACE2more6HH_22 + INQ_INCOME,
                         method = "nearest",
                         data = currData
)

matched_data <- match.data(match_results)

matched_table_one <- CreateTableOne(
  vars = covariates, 
  strata = "MHealthConcern", 
  data = matched_data, 
  test = TRUE
)

matched_table_one

weight_results <- weightit(MHealthConcern ~ HHCOUNT + K8Q35 + TalkAbout_22 + SC_RACE_R +
                             NbhdSupp_22 + NbhdSafe_22 + ACE2more6HH_22 + INQ_INCOME,
                           data = currData,
                           method = "ps"
)

currData$weights <- weight_results$weights

survey_design <- svydesign(
  ids = ~1,  # No cluster identifier
  weights = ~weights,
  data = currData)

# Create weighted table one
weighted_table_one <- svyCreateTableOne(
  vars = covariates, 
  strata = "MHealthConcern", 
  data = survey_design,
  test = TRUE)

weighted_table_one

currData <- currData %>%
  mutate(
    Bullyied = case_when(
      bullied_22 >= 3 ~ 1,
      bullied_22 < 6 ~ 0,
      TRUE ~ 2
    )
  )

currData <- currData %>% filter(Bullyied == 1 | Bullyied == 0)

og_model <- glm(Bullyied ~ MHealthConcern, 
                data = currData, 
                family = binomial())

matched_model <- glm(Bullyied ~ MHealthConcern, 
                     data = currData, 
                     family = binomial())

weighted_model <- glm(Bullyied ~ MHealthConcern, 
                      data = currData,
                      weights = weights,
                      family = binomial())

# Summarize results
results <- list(
  Original = summary(og_model),
  Matched = summary(matched_model),
  Weighted = summary(weighted_model))

results