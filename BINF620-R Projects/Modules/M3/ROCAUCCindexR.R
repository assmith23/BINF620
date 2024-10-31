library(dplyr)
library(tidyverse)
library(stringr)
library(knitr)
library(leaps)
library(caret)
library(MASS)
library(olsrr)
library(pROC)
library(caret)
library(purrr)
library(epitools)
library(finalfit)

setwd("C:/Users/zugui/Desktop/work/UDIntroBioStatas/Module8")
wcgsmed <- read.csv("C:/Users/zugui/Desktop/work/UDIntroBioStatas/Module8/wcgsmed.csv", 
                    header=TRUE)
names(wcgsmed)
str(wcgsmed)
table(wcgsmed$chd)

alpha=0.05
wcgsmed$chd[wcgsmed$chd=="Yes"] <- 1 
wcgsmed$chd[wcgsmed$chd=="No"] <- 0 
wcgsmed$chd <- as.numeric(wcgsmed$chd)

#full model
wcgsfull<-glm(chd ~., family=binomial, data=wcgsmed)
wcgsmed$prob1=predict(wcgsfull,type=c("response"))
roc<-roc(chd ~ prob1, data = wcgsmed)
roc$auc

#optimal model
optimalwcgs<-glm(chd ~ timechd + chol + age + ncigs + sbp + personality_2L + height,
                 data=wcgsmed)
wcgsmed$proboptimal=predict(optimalwcgs,type=c("response"))
rocoptimal<-roc(chd ~ proboptimal, data = wcgsmed)
rocoptimal$auc

rocauctwo=c(rocfull$auc, rocoptimal$auc)
rocauctwo

#plot ROC, full model
roc<-roc(chd ~ prob1, data = wcgsmed)
ggroc(roc) +
  theme_bw()+
  labs(x="specificity", title="ROC Curve Logistic Regression All Predictors")+
  annotate("text", x = .25, y = .25, label = paste("AUC =", round(roc$auc, 4))) 

#plot ROC with CI
wcgsfull<-glm(chd ~., family=binomial, data=wcgsmed)
wcgsmed$prob1=predict(wcgsfull,type=c("response"))
roc<-roc(chd ~ prob1, data = wcgsmed)

ggroc(roc) +
  theme_minimal() + 
  ggtitle("ROC Curve Logistic Regression All Predictors" ) + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="blue", linetype="dashed")

pROC_obj <- roc(wcgsmed$chd, wcgsmed$prob1,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.90, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC Curve Logistic Regression All Predictors")

sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")
## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
## definition shape.
plot(sens.ci, type="bars")

#plot ROC, optimal model
optimalwcgs<-glm(chd ~ timechd + chol + age + ncigs + sbp + personality_2L + height,
                 data=wcgsmed)
wcgsmed$proboptimal=predict(optimalwcgs,type=c("response"))
rocoptimal<-roc(chd ~ proboptimal, data = wcgsmed)

ggroc(rocoptimal) +
  theme_bw()+
  labs(x="specificity", title="ROC Curve Logistic Regression Optimal Predictors")+
  annotate("text", x = .25, y = .25, label = paste("AUC =", round(rocoptimal$auc, 4))) 

#plot ROC with CI
optimalwcgs<-glm(chd ~ timechd + chol + age + ncigs + sbp + personality_2L + height,
                 data=wcgsmed)
wcgsmed$proboptimal=predict(optimalwcgs,type=c("response"))
rocoptimal<-roc(chd ~ proboptimal, data = wcgsmed)

ggroc(rocoptimal) +
  theme_minimal() + 
  ggtitle("ROC Curve Logistic Regression Optimal Predictors" ) + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="blue", linetype="dashed")

pROC_obj <- roc(wcgsmed$chd, wcgsmed$proboptimal,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.90, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE, main = "ROC Curve Logistic Regression Optimal Predictors")

sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")
## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
## definition shape.
plot(sens.ci, type="bars")