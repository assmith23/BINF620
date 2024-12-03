install.packages("MatchIt")
install.packages("lmtest")
install.packages("sandwich")
install.packages("ROCR")
install.packages("questionr")
install.packages("crosstable")
install.packages("gmodels")

library(tidyverse)
library(MatchIt)
library(lmtest)
library(sandwich)
library(tableone)
library(ROCR)
library(pROC)
library(questionr)
library(crosstable)
library(gmodels)
setwd("C:/Users/zugui/Desktop/work/UDFall2022/ObservationalStudies")

currentDataset<-read.csv("currentDataset.csv", header=TRUE)
#Propensity score weights
ps <- glm(smoker ~ sex + indigeneity + high_school + partnered + remoteness + language + risky_alcohol + age, 
                  family="binomial", data=currentDataset)
summary(ps)
currentDataset$psvalue<-predict(ps, type="response")
summary(currentDataset$psvalue)
#> summary(currentDataset$psvalue)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01579 0.05941 0.09436 0.12175 0.15905 0.66173
currentDataset$weightPS<-ifelse(currentDataset$smoker ==1, 1/currentDataset$psvalue, 1/(1-currentDataset$psvalue))

#checking balance
AgewithWeigts<-lm(age~smoker, data=currentDataset, weights=(weightPS))
summary(AgewithWeigts)
anova(AgewithWeigts)

Agematch<-lm(age~smoker, data=matched_data)
summary(Agematch)
anova(Agematch)

sexwithweights<-glm(sex~smoker, data=currentDataset,family=binomial(), weights=(weightPS))
summary(sexwithweights)
anova(sexwithweights, test="Chi")

chisq.test(currentDataset$sex, currentDataset$smoker, correct=FALSE)
chisq.test(matched_data$sex, matched_data$smoker, correct=FALSE)

#table with PS weights
sexafterweights<-wtd.table(currentDataset$sex, currentDataset$smoker,weights=(currentDataset$weightPS))
CrossTable(matched_data$sex, matched_data$smoker)

reswithweights1 <- lm(psyc_distress ~ smoker, data = currentDataset, weights =weightPS)
summary(reswithweights1)
coefci(reswithweights1, vcov. = vcovCL,  level = 0.95)

reswithweights2 <- lm(psyc_distress ~ smoker + sex + indigeneity + high_school + partnered + 
                       remoteness + language + risky_alcohol + age, data = currentDataset, weights =weightPS)
summary(reswithweights2)
coefci(reswithweights2, vcov. = vcovCL,  level = 0.95)
