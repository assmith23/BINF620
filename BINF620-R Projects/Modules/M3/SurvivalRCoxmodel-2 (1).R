install.packages(c("survival", "survminer"))
#Load the packages
library("survival")
library("survminer")

library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

library("survminer")
library("Rcpp")

data("lung")
head(lung)

help(lung)

#Univariate Cox regression
res.cox <- coxph(Surv(time, status) ~ sex, data = lung)
res.cox
summary(res.cox)

#the univariate coxph function to multiple covariates
covariates <- c("age", "sex",  "ph.karno", "ph.ecog", "wt.loss")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = lung)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)

res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog +pat.karno +meal.cal + wt.loss, 
                 data =  lung)
summary(res.cox)

#Visualizing the estimated distribution of survival times
#Having fit a Cox model to the data, it’s possible to visualize the predicted survival proportion at any given
#point in time for a particular risk group. The function survfit() estimates the survival proportion, 
#by default at the mean values of covariates.

# Plot the baseline survival function
ggsurvplot(survfit(res.cox), color = "#2E9FDF",
           ggtheme = theme_minimal(), data=lung)

#We may wish to display how estimated survival depends upon the value of a covariate of interest.

#Consider that, we want to assess the impact of the sex on the estimated survival probability. 
#we construct a new data frame with two rows, one for each value of sex; the other covariates are fixed to 
#their average values (if they are continuous variables) or to their lowest level (if they are discrete 
#variables). For a dummy covariate, the average value is the proportion coded 1 in the data set. 
#This data frame is passed to survfit() via the newdata argument:
# Create the new data  
sex_df <- with(lung,
               data.frame(sex = c(1, 2), 
                          age = rep(mean(age, na.rm = TRUE), 2),
                          ph.ecog = c(1, 1),
                          ph.karno = rep(mean(ph.karno, na.rm = TRUE), 2),
                          pat.karno = rep(mean(pat.karno, na.rm = TRUE), 2),
                          meal.cal= rep(mean(meal.cal, na.rm = TRUE), 2),
                          wt.loss = rep(mean(wt.loss, na.rm = TRUE), 2)
               )
)
sex_df

# Survival curves
fit <- survfit(res.cox, newdata = sex_df)
ggsurvplot(fit, conf.int = TRUE, legend.labs=c("Sex=1", "Sex=2"),
           ggtheme = theme_minimal(), data=sex_df)

#Comparing with Raw survival curve
Rawsurv_diff <- survfit(Surv(time, status) ~ sex, data=lung)
Rawsurv_diff
autoplot(Rawsurv_diff)

surv_diff <- survdiff(Surv(time, status) ~ sex, data=lung)
surv_diff

ggsurvplot(Rawsurv_diff,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

#Test:  Cox Model Assumptions
res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog +pat.karno +meal.cal + wt.loss, 
                 data =  lung)
test.ph <- cox.zph(res.cox)
test.ph

#Schoenfeld residual plots
ggcoxzph(test.ph)

