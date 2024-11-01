#libraries needed
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)

data("iris")
df <- data(iris) ##load data
head(iris) ## see the studcture
##Generate a random number that is 90% of the total number of rows in dataset.
ran <- sample(1:nrow(iris), 0.9 * nrow(iris)) 
##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))
summary(iris_norm)
##extract training set
iris_train <- iris_norm[ran,] 
##extract testing set
iris_test <- iris_norm[-ran,] 
##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
iris_target_category <- iris[ran,5]
##extract 5th column of test dataset to measure the accuracy
iris_test_category <- iris[-ran,5]


##load the package class
library(class)
##run knn function
pr <- knn(iris_train,iris_test,cl=iris_target_category,k=13)
##create confusion matrix
tab <- table(pr,iris_test_category)
##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

library(ggplot2)##load data
data(diamonds)

##store it as data frame
dia <- data.frame(diamonds)

##create a random number equal 90% of total number of rows
ran <- sample(1:nrow(dia),0.9 * nrow(dia))

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##normalization function is created
dia_nor <- as.data.frame(lapply(dia[,c(1,5,6,7,8,9,10)], nor))

##training dataset extracted
dia_train <- dia_nor[ran,]

##test dataset extracted
dia_test <- dia_nor[-ran,]##the 2nd column of training dataset because that is what we need to predict about testing dataset
##also convert ordered factor to normal factor
dia_target <- as.factor(dia[ran,2])

##the actual values of 2nd couln of testing dataset to compaire it with values that will be predicted
##also convert ordered factor to normal factor
test_target <- as.factor(dia[-ran,2])

##run knn function
library(class)
pr <- knn(dia_train,dia_test,cl=dia_target,k=20)

##create the confucion matrix
tb <- table(pr,test_target)

##check the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)## [1] 71.09752

filepath <- "https://quantdev.ssri.psu.edu/sites/qdev/files/student-mat.csv"
#read in the .csv file using the url() function
data <- read.table(file=url(filepath),sep=";",header=TRUE)

#change all variable names to lowercase
var.names.data <-tolower(colnames(data))
colnames(data) <- var.names.data
head(data)

data_class <- data

# put outcome in its own object
mjob_outcome <- data_class %>% select(mjob)

# remove original variable from the data set
data_class <- data_class %>% select(-mjob)
str(data_class)

data_class[, c("age", "medu", "fedu", "traveltime", "studytime", "failures", "famrel", "freetime", "goout", "dalc", "walc", "health", "absences", "g1", "g2", "g3")] <- scale(data_class[, c("age", "medu", "fedu", "traveltime", "studytime", "failures", "famrel", "freetime", "goout", "dalc", "walc", "health", "absences", "g1", "g2", "g3")])

head(data_class)
str(data_class)

data_class$schoolsup <- ifelse(data_class$schoolsup == "yes", 1, 0)
data_class$famsup <- ifelse(data_class$famsup == "yes", 1, 0)
data_class$paid <- ifelse(data_class$paid == "yes", 1, 0)
data_class$activities <- ifelse(data_class$activities == "yes", 1, 0)
data_class$nursery <- ifelse(data_class$nursery == "yes", 1, 0)
data_class$higher <- ifelse(data_class$higher == "yes", 1, 0)
data_class$internet <- ifelse(data_class$internet == "yes", 1, 0)
data_class$romantic <- ifelse(data_class$romantic == "yes", 1, 0)

data_class$school <- dummy.code(data_class$school)
data_class$sex <- dummy.code(data_class$sex)
data_class$address <- dummy.code(data_class$address)
data_class$famsize <- dummy.code(data_class$famsize)
data_class$pstatus <- dummy.code(data_class$pstatus)


fjob <- as.data.frame(dummy.code(data_class$fjob))
reason <- as.data.frame(dummy.code(data_class$reason))
guardian <- as.data.frame(dummy.code(data_class$guardian))

fjob <- rename(fjob, other_fjob = other)
fjob <- rename(fjob, health_fjob = health)

reason <- rename(reason, other_reason = other)

guardian <- rename(guardian, other_guardian = other)


data_class <- cbind(data_class, fjob, guardian, reason)


data_class <- data_class %>% select(-one_of(c("fjob", "guardian", "reason")))

head(data_class)

set.seed(1234) # set the seed to make the partition reproducible

# 75% of the sample size
smp_size <- floor(0.75 * nrow(data_class))

train_ind <- sample(seq_len(nrow(data_class)), size = smp_size)

# creating test and training sets that contain all of the predictors
class_pred_train <- data_class[train_ind, ]
class_pred_test <- data_class[-train_ind, ]

mjob_outcome_train <- mjob_outcome[train_ind, ]
mjob_outcome_test <- mjob_outcome[-train_ind, ]

mjob_pred_knn <- knn(train = class_pred_train, test = class_pred_test, cl = mjob_outcome_train, k=17)

# put "mjob_outcome_test" in a data frame
mjob_outcome_test <- data.frame(mjob_outcome_test)

# merge "mjob_pred_knn" and "mjob_outcome_test" 
class_comparison <- data.frame(mjob_pred_knn, mjob_outcome_test)

# specify column names for "class_comparison"
names(class_comparison) <- c("PredictedMjob", "ObservedMjob")

# inspect "class_comparison" 
head(class_comparison)

# create table examining model accuracy
CrossTable(x = class_comparison$ObservedMjob, y = class_comparison$PredictedMjob, prop.chisq=FALSE,
           prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

#Use caret package. Run k-NN classification.
#In this package, the function picks the optimal number of neighbors (k) for you.
mjob_pred_caret <- train(class_pred_train, mjob_outcome_train, method = "knn", preProcess = c("center","scale"))
mjob_pred_caret
plot(mjob_pred_caret)

#The confusion matrix output also shows overall model statistics and statistics by class
knnPredict <- predict(mjob_pred_caret, newdata = class_pred_test) 

confusionMatrix(knnPredict, mjob_outcome_test$mjob_outcome_test)


#k-NN regression
data_reg <- data
# put outcome in its own object
absences_outcome <- data_reg %>% select(absences)

# remove original from the data set
data_reg <- data_reg %>% select(-absences)
str(data_reg)

data_reg[, c("age", "medu", "fedu", "traveltime", "studytime", "failures", "famrel", "freetime", "goout", "dalc", "walc", "health", "g1", "g2", "g3")] <- scale(data_reg[, c("age", "medu", "fedu", "traveltime", "studytime", "failures", "famrel", "freetime", "goout", "dalc", "walc", "health", "g1", "g2", "g3")])

head(data_reg)
str(data_reg)

#dummy code variables that have just two levels and are coded 1/0.

data_reg$schoolsup <- ifelse(data_reg$schoolsup == "yes", 1, 0)
data_reg$famsup <- ifelse(data_reg$famsup == "yes", 1, 0)
data_reg$paid <- ifelse(data_reg$paid == "yes", 1, 0)
data_reg$activities <- ifelse(data_reg$activities == "yes", 1, 0)
data_reg$nursery <- ifelse(data_reg$nursery == "yes", 1, 0)
data_reg$higher <- ifelse(data_reg$higher == "yes", 1, 0)
data_reg$internet <- ifelse(data_reg$internet == "yes", 1, 0)
data_reg$romantic <- ifelse(data_reg$romantic == "yes", 1, 0)

#dummy code variables that have two levels, but are not numeric.

data_reg$school <- dummy.code(data_reg$school)
data_reg$sex <- dummy.code(data_reg$sex)
data_reg$address <- dummy.code(data_reg$address)
data_reg$famsize <- dummy.code(data_reg$famsize)
data_reg$pstatus <- dummy.code(data_reg$pstatus)
#dummy code variables that have three or more levels.

mjob <- as.data.frame(dummy.code(data_reg$mjob))
fjob <- as.data.frame(dummy.code(data_reg$fjob))
reason <- as.data.frame(dummy.code(data_reg$reason))
guardian <- as.data.frame(dummy.code(data_reg$guardian))

#Rename "other" columns in "mjob," "fjob," "reason," and "guardian," and rename "health,"
#"at_home," "services," and "teacher" in "mjob" and "fjob" (so we don't have duplicate columns later).

mjob <- rename(mjob, health_mjob = health)
mjob <- rename(mjob, at_home_mjob = at_home)
mjob <- rename(mjob, services_mjob = services)
mjob <- rename(mjob, teacher_mjob = teacher)
mjob <- rename(mjob, other_mjob = other)

fjob <- rename(fjob, health_fjob = health)
fjob <- rename(fjob, at_home_fjob = at_home)
fjob <- rename(fjob, services_fjob = services)
fjob <- rename(fjob, teacher_fjob = teacher)
fjob <- rename(fjob, other_fjob = other)

reason <- rename(reason, other_reason = other)

guardian <- rename(guardian, other_guardian = other)

#Combine new dummy variables with original data set.
data_reg <- cbind(data_reg, mjob, fjob, guardian, reason)
#Remove original variables that had to be dummy coded.
data_reg <- data_reg %>% select(-one_of(c("mjob", "fjob", "guardian", "reason")))

head(data_reg)

set.seed(1234) # set the seed to make the partition reproducible

# 75% of the sample size
smp_size <- floor(0.75 * nrow(data_reg))

train_ind <- sample(seq_len(nrow(data_reg)), size = smp_size)

# creating test and training sets that contain all of the predictors
reg_pred_train <- data_reg[train_ind, ]
reg_pred_test <- data_reg[-train_ind, ]
#Split outcome variable into training and test sets using the same partition as above.
abs_outcome_train <- absences_outcome[train_ind, ]
abs_outcome_test <- absences_outcome[-train_ind, ]

reg_results <- knn.reg(reg_pred_train, reg_pred_test, abs_outcome_train, k = 17)
print(reg_results)
plot(abs_outcome_test, reg_results$pred, xlab="y", ylab=expression(hat(y)))

#mean square prediction error
mean((abs_outcome_test - reg_results$pred) ^ 2)
## [1] 102.5213
#mean absolute error
mean(abs(abs_outcome_test - reg_results$pred))
