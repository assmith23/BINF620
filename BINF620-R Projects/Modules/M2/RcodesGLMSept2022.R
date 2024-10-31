#GLM 
install.packages("tidyverse")
install.packages("RNHANES")

library(tidyverse)
library(RNHANES)
library(ggplot2)
library(pROC)

#dat <- nhanes_load_data("UHG_G", "2011-2012", demographics = TRUE)

#nhanes_sample_size(dat, "URXUHG", "URDUHGLC")

#epre=nhanes_load_data("EPH_E", "2007-2008")

d07 = nhanes_load_data("DEMO_E", "2007-2008")%>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR) %>%
  transmute(SEQN=SEQN, wave=cycle, RIAGENDR, RIDAGEYR) %>% 
  left_join(nhanes_load_data("VID_E", "2007-2008"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, LBXVIDMS) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD=LBXVIDMS) %>% 
  left_join(nhanes_load_data("BIOPRO_E", "2007-2008"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, LBXSCA) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium = LBXSCA) %>% 
  left_join(nhanes_load_data("OSQ_E", "2007-2008"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium, OSQ060) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium, Osteop = OSQ060)

d09 = nhanes_load_data("DEMO_F", "2009-2010") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR) %>%
  transmute(SEQN=SEQN, wave=cycle, RIAGENDR, RIDAGEYR) %>% 
  left_join(nhanes_load_data("VID_F", "2009-2010"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, LBXVIDMS) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD=LBXVIDMS) %>% 
  left_join(nhanes_load_data("BIOPRO_F", "2009-2010"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, vitD,  LBXSCA) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium = LBXSCA) %>% 
  left_join(nhanes_load_data("OSQ_F", "2009-2010"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium, OSQ060) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium, Osteop = OSQ060)
dat = bind_rows(d07, d09) %>% as.data.frame()

dat1 = dat %>% 
  mutate(
    vitD_group = case_when(
      vitD < 30 ~ "Deficiency",
      vitD >= 30 & vitD < 50 ~ "Inadequacy",
      vitD >= 50 & vitD <= 125 ~ "Sufficiency"))


#Exclude missings
dat2 = dat1 %>% 
  filter(!is.na(vitD_group), !is.na(Calcium), !is.na(Osteop), Osteop!=9) %>% 
  mutate(Gender = recode_factor(RIAGENDR, 
                                `1` = "Men", 
                                `2` = "Women"),
         Osteop = recode_factor(Osteop, 
                                `1` = 1, 
                                `2` = 0))
head(dat2)

#Logit regression model, using the glm() function to run the logistic regression and then summary() command to get the results.

fit <- glm(Osteop ~ vitD_group + Calcium + Gender + RIDAGEYR, 
           data = dat2, 
           family = "binomial")
summary(fit)

#Transforms beta's to the odds ratio. The output of summary() does not provide the odds ratio which are often presented 
#in research papers. The exp of beta's give the odds.

or=round(exp(coef(fit)), 2)
orci=round(exp(confint(fit)), 2)
oddsratio=cbind(or, orci)
oddsratio

# model without vitamin D
fit1 <- glm(Osteop ~ Calcium + Gender + RIDAGEYR, 
            data = dat2, 
            family = "binomial")
# model with vitamin D
fit2 <- glm(Osteop ~ vitD_group + Calcium + Gender + RIDAGEYR, 
            data = dat2, 
            family = "binomial")
dat2$prob1=predict(fit1,type=c("response"))
dat2$prob2=predict(fit2,type=c("response"))
roc(Osteop ~ prob1, data = dat2)
roc(Osteop ~ prob2, data = dat2)

roc(Osteop ~ prob1, data = dat2,plot=TRUE,print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves" )
roc(Osteop ~ prob2, data = dat2, plot=TRUE,print.auc=TRUE,col="blue",lwd = 4,print.auc.y=0.4,legacy.axes=TRUE,add = TRUE)
legend("bottomright",legend=c("model without vitamin D","model with vitamin D"),col=c("green","blue"),lwd=4)