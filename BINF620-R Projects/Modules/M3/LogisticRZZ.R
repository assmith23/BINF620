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
library(aod)
library(ggplot2)

setwd("C:/Users/zugui/Desktop/work/UDIntroBioStatas/Module8")
wcgsmed <- read.csv("C:/Users/zugui/Desktop/work/UDIntroBioStatas/Module8/wcgsmed.csv", 
                    header=TRUE)
names(wcgsmed)

#convert chd into a binary variable, with 1 and 0
wcgsmed$chd[wcgsmed$chd=="Yes"] <- 1 
wcgsmed$chd[wcgsmed$chd=="No"] <- 0 
wcgsmed$chd <- as.numeric(wcgsmed$chd)

wcgssmoking<-glm(chd ~., family=binomial, data=wcgsmed)
summary(wcgssmoking)
anova(wcgssmoking, test="LRT")
coef(wcgssmoking)

wcgsfull<-glm(chd ~., family=binomial, data=wcgsmed)
summary(wcgsfull)
anova(wcgsfull, test="LRT")
wald.test(b = coef(wcgsfull), Sigma = vcov(wcgsfull), Terms = 1:12)
coef(wcgsfull)

#OR and 95% CI:
wcgsfull<-glm(chd ~., family=binomial, data=wcgsmed)
ORwcgsfullwithCI<-exp(cbind(OR = coef(wcgsfull), confint(wcgsfull)))
ORwcgsfullwithCI
kable(ORwcgsfullwithCI)

## CIs using profiled log-likelihood
confint(wcgssmoking)
## CIs using standard errors
confint.default(wcgssmoking)
#We can test for an overall effect of rank using the wald.test function of the aod library.
wald.test(b = coef(wcgssmoking), Sigma = vcov(wcgssmoking), Terms = 1:2)

#OR and 95% CI:
ORSmokingwithCI<-exp(cbind(OR = coef(wcgssmoking), confint(wcgssmoking)))
ORSmokingwithCI
kable(ORSmokingwithCI)
