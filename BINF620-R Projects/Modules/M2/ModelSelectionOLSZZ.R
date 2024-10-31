install.packages("olsrr")
library(olsrr)
attach(surgical)

#Surgical data: 54 rows and  9 variables:  A dataset containing data about survival of patients undergoing liver operation
  
#bcs: blood clotting score
#pindex: prognostic index
#enzyme_test: enzyme function test score
#liver_test: liver function test score
#age: age, in years
#gender: indicator variable for gender (0 = male, 1 = female)
#alc_mod: indicator variable for history of alcohol use (0 = None, 1 = Moderate)
#alc_heavy: indicator variable for history of alcohol use (0 = None, 1 = Heavy)
#y: Survival Time

model <- lm(y ~ ., data = surgical)
k <- ols_step_all_possible(model)
plot(k)

model <- lm(y ~ ., data = surgical)
ols_step_best_subset(model)
plot(k)

# stepwise forward regression
model <- lm(y ~ ., data = surgical)
ols_step_forward_p(model)

model <- lm(y ~ ., data = surgical)
k <- ols_step_forward_p(model)
plot(k)

# stepwise forward regression
model <- lm(y ~ ., data = surgical)
ols_step_forward_p(model, details = TRUE)

# stepwise backward regression
model <- lm(y ~ ., data = surgical)
ols_step_backward_p(model)

model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_p(model)
plot(k)

# stepwise backward regression
model <- lm(y ~ ., data = surgical)
ols_step_backward_p(model, details = TRUE)

# stepwise regression
model <- lm(y ~ ., data = surgical)
ols_step_both_p(model)


model <- lm(y ~ ., data = surgical)
k <- ols_step_both_p(model)
plot(k)


# stepwise regression
model <- lm(y ~ ., data = surgical)
ols_step_both_p(model, details = TRUE)

# stepwise aic forward regression
model <- lm(y ~ ., data = surgical)
ols_step_forward_aic(model)

model <- lm(y ~ ., data = surgical)
k <- ols_step_forward_aic(model)
plot(k)

# stepwise aic forward regression
model <- lm(y ~ ., data = surgical)
ols_step_forward_aic(model, details = TRUE)

# stepwise aic backward regression
model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_aic(model)
k

model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_aic(model)
plot(k)

# stepwise aic backward regression
model <- lm(y ~ ., data = surgical)
ols_step_backward_aic(model, details = TRUE)

# stepwise aic regression
model <- lm(y ~ ., data = surgical)
ols_step_both_aic(model)

model <- lm(y ~ ., data = surgical)
k <- ols_step_both_aic(model)
plot(k)

# stepwise aic regression
model <- lm(y ~ ., data = surgical)
ols_step_both_aic(model, details = TRUE)



