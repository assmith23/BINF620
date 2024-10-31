library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

install.packages("survminer")
library("survminer")
library("Rcpp")

data(veteran)
help(veteran)
head(veteran)

# Building build standard survival object.
km <- with(veteran, Surv(time, status))
head(km,80)

#generating the Kaplan-Meier estimates of the probability of survival over time. 
#The times parameter of the summary() function gives some control over which times to print. 
km_fit <- survfit(Surv(time, status) ~ 1, data=veteran)
summary(km_fit, times = c(1,30,60,90*(1:10)))
autoplot(km_fit)

#examining the survival curves by treatment.
km_trt_fit <- survfit(Surv(time, status) ~ trt, data=veteran)
summary(km_trt_fit, times = c(1,30,60,90*(1:10)))
autoplot(km_trt_fit)

#using the survdiff() function from the survival package to do a log-rank test,
surv_diff <- survdiff(Surv(time, status) ~ trt, data=veteran)
surv_diff

#Producing the survival curves in a better way, with number at risk
ggsurvplot(km_trt_fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

