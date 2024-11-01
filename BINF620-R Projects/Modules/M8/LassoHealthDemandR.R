install.packages("mpath")
install.packages("zic")

library("mpath")
library("zic")
library("pscl")
data(docvisits)
barplot(with(docvisits, table(docvisits)), ylab = "Frequency",
        xlab = "Doctor office visits")

dt <- docvisits[, -(2:3)]
tmp <- model.matrix(~age30 * health + age35 * health +
                         age40 * health + age45 * health + age50 * health +
                         age55 * health + age60 * health, data = dt)[, -(1:9)]
dat <- cbind(dt, tmp)
#Full ZINB model with all predictor variables.
m1 <- zeroinfl(docvisits ~ . | ., data = dat, dist = "negbin")
summary(m1)
cat("loglik of zero-inflated model", logLik(m1))
cat("BIC of zero-inflated model", AIC(m1, k = log(dim(dat)[1])))
cat("AIC of zero-inflated model", AIC(m1))
#Backward stepwise variable selection with significance level alpha=0.01.
fitbe <- be.zeroinfl(m1, data = dat, dist = "negbin",
                        alpha = 0.01, trace = FALSE)
summary(fitbe)
cat("loglik of zero-inflated model with backward selection",
       logLik(fitbe))
cat("BIC of zero-inflated model with backward selection",
       AIC(fitbe, k = log(dim(dat)[1])))

#Compute LASSO estimates.

fit.lasso <- zipath(docvisits ~ . | ., data = dat, family = "negbin",
                       nlambda = 100, lambda.zero.min.ratio = 0.001, maxit.em = 300,
                       maxit.theta = 25, theta.fixed = FALSE, trace = FALSE,
                       penalty = "enet", rescale = FALSE)
#Estimated coefficient parameters with smallest BIC value.
minBic <- which.min(BIC(fit.lasso))
coef(fit.lasso, minBic)
cat("theta estimate", fit.lasso$theta[minBic])
#Compute standard errors of coefficients:
se(fit.lasso, minBic, log = FALSE)
#Compute AIC, BIC, log-likelihood values of the selected model.
AIC(fit.lasso)[minBic]
BIC(fit.lasso)[minBic]
logLik(fit.lasso)[minBic]
#Compute log-likelihood value via 10-fold cross-validation.
n <- dim(dat)[1]
K <- 10

set.seed(197)
foldid <- split(sample(1:n), rep(1:K, length = n))
fitcv <- cv.zipath(docvisits ~ . | ., data = dat, family = "negbin",
                      nlambda = 100, lambda.count = fit.lasso$lambda.count[1:30],
                      lambda.zero = fit.lasso$lambda.zero[1:30], maxit.em = 300,
                      maxit.theta = 1, theta.fixed = FALSE, penalty = "enet",
                      rescale = FALSE, foldid = foldid)
cat("cross-validated loglik", max(fitcv$cv))

#We compute solution paths for the first 30 pairs
#of shrinkage parameters (the EM algorithm can be slow), and then evaluate
#results as for the LASSO estimates. For cross-validation, set maximum number of iterations in estimating scaling parameter 1 (maxit.theta=1) to reduce
#computation costs.
tmp <- zipath(docvisits ~ . | ., data = dat, family = "negbin",
                 gamma.count = 2.7, gamma.zero = 2.7, lambda.zero.min.ratio = 0.1,
                 maxit = 1, maxit.em = 1, maxit.theta = 2, theta.fixed = FALSE,
                 penalty = "mnet")
fit.mcp <- zipath(docvisits ~ . | ., data = dat, family = "negbin",
                     gamma.count = 2.7, gamma.zero = 2.7, lambda.count = tmp$lambda.count[1:30],
                     lambda.zero = tmp$lambda.zero[1:30], maxit.em = 300,
                     maxit.theta = 25, theta.fixed = FALSE, penalty = "mnet")
#Estimated coefficient parameters with smallest BIC value.
minBic <- which.min(BIC(fit.mcp))
coef(fit.mcp, minBic)
cat("theta estimate", fit.mcp$theta[minBic])

#Compute standard errors of coefficients and theta:
 se(fit.mcp, minBic, log = FALSE)
#Compute AIC, BIC, log-likelihood values of the selected model.
AIC(fit.mcp)[minBic]
BIC(fit.mcp)[minBic]
logLik(fit.mcp)[minBic]

#Compute log-likelihood value via 10-fold cross-validation.
fitcv <- cv.zipath(docvisits ~ . | ., data = dat, family = "negbin",
                      gamma.count = 2.7, gamma.zero = 2.7, lambda.count = tmp$lambda.count[1:30],
                      lambda.zero = tmp$lambda.zero[1:30], maxit.em = 300,
                      maxit.theta = 1, theta.fixed = FALSE, penalty = "mnet",
                      rescale = FALSE, foldid = foldid)
cat("cross-validated loglik", max(fitcv$cv))
#Compute SCAD estimates.
tmp <- zipath(docvisits ~ . | ., data = dat, family = "negbin",
                 gamma.count = 2.5, gamma.zero = 2.5, lambda.zero.min.ratio = 0.01,
                 maxit = 1, maxit.em = 1, maxit.theta = 2, theta.fixed = FALSE,
                 penalty = "snet")
fit.scad <- zipath(docvisits ~ . | ., data = dat, family = "negbin",
                      gamma.count = 2.5, gamma.zero = 2.5, lambda.count = tmp$lambda.count[1:30],
                      lambda.zero = tmp$lambda.zero[1:30], maxit.em = 300,
                      maxit.theta = 25, theta.fixed = FALSE, penalty = "snet")
#Estimated coefficient parameters with smallest BIC value.
minBic <- which.min(BIC(fit.scad))
coef(fit.scad, minBic)
cat("theta estimate", fit.scad$theta[minBic])
#Compute standard errors of coefficients and theta:
 se(fit.scad, minBic, log = FALSE)
#Compute AIC, BIC, log-likelihood values of the selected model.
AIC(fit.scad)[minBic]
BIC(fit.scad)[minBic]
logLik(fit.scad)[minBic]
#Compute log-likelihood value via 10-fold cross-validation.
fitcv <- cv.zipath(docvisits ~ . | ., data = dat, family = "negbin",
                      gamma.count = 2.5, gamma.zero = 2.5, lambda.count = tmp$lambda.count[1:30],
                      lambda.zero = tmp$lambda.zero[1:30], maxit.em = 300,
                      maxit.theta = 1, theta.fixed = FALSE, penalty = "snet",
                      rescale = FALSE, foldid = foldid)
cat("cross-validated loglik", max(fitcv$cv))

print(proc.time() - ptm)
sessionInfo()
