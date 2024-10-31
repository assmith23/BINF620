#https://terpconnect.umd.edu/~egurarie/teaching/Biol709/Topic3/Lab11_GLMandModelSelection.html

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
library(lmtest)

help(wcgs)
#help(wcgs)
#wcgsdata<-finalfit::wcgs

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

#run full model
wcgsfull<-glm(chd ~., family=binomial, data=wcgsmed)
summary(wcgsfull)
anova(wcgsfull, test="LRT")

#run "zero" model
nothing <- glm(chd ~ 1,family=binomial, data=wcgsmed)
summary(nothing)
anova(nothing, test="LRT")

#forward 
forwardswcgs = step(nothing,
                scope=list(lower=formula(nothing),upper=formula(wcgsfull)), 
                direction="forward")
formula(forwardswcgs)
stepAIC(forwardswcgs)
summary(forwardswcgs)

#backwards
backwardswcgs = step(wcgsfull, direction = "backward")
#default is backward
backwards0 = step(wcgsfull,trace=0)
formula(backwardswcgs)
summary(backwardswcgs)

#Stepwise Procedure
bothwayswcgs =step(nothing, list(lower=formula(nothing),upper=formula(wcgsfull)),
               direction="both")
formula(bothwayswcgs)
anova(bothwayswcgs, test="LRT")
summary(bothwayswcgs)

#comparison:  full model vs. optimal model
wcgsfull<-glm(chd ~., family=binomial, data=wcgsmed)
optimalwcgs<-glm(chd ~ timechd + chol + age + ncigs + sbp + personality_2L + height,
                 data=wcgsmed)
anova( wcgsfull, optimalwcgs,test="Chisq")

f1 <- formula(chd ~.)
f2 <- formula(chd ~ timechd + chol + age + ncigs + sbp + personality_2L + height)
AIC(glm(f1, family=binomial, data=wcgsmed), 
    glm(f2, family=binomial, data=wcgsmed))
