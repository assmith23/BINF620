#https://cran.r-project.org/web/packages/ensr/vignettes/ensr-examples.html
#only run the 

install.packages("ensr")
library(ensr)

library(data.table)
library(magrittr)
library(qwraps2)
library(digest)

install.packages("qwraps2")
library(qwraps2)

set.seed(42)

## Loading required package: glmnet
## Loading required package: Matrix
## Loading required package: foreach
## Loaded glmnet 2.0-16
## 
## Attaching package: 'glmnet'
## The following object is masked from 'package:qwraps2':
## 
##     auc
library(data.table)
library(ggplot2)
## Registered S3 methods overwritten by 'ggplot2':
##   method         from 
##   [.quosures     rlang
##   c.quosures     rlang
##   print.quosures rlang
library(ggforce)
library(doMC)
## Loading required package: iterators
## Loading required package: parallel
registerDoMC(cores = max(c(detectCores() - 2L, 1L)))
options(datatable.print.topn  = 3L,
        datatable.print.nrows = 3L)


vignette("ensr-datasets", package = "ensr")

data(tbi, package = "ensr")
data(landfill, package = "ensr")

TBI_SUBJECTS <- 1323L

tbi <- data.table(age    = round(rweibull(TBI_SUBJECTS, shape = 1.9, scale = 2000)),
                  female = rbinom(TBI_SUBJECTS, 1, 0.10),
                  los    = round(rexp(TBI_SUBJECTS, rate = 1/13)))

pcodes <-
  c(`0.0.0.0.0.0` = 1230, `1.0.0.0.0.0` = 130, `0.1.0.0.0.0` = 40,
    `1.1.0.0.0.0` = 7, `0.0.1.0.0.0` = 120, `1.0.1.0.0.0` = 4, `0.1.1.0.0.0` = 20,
    `1.1.1.0.0.0` = 2, `0.0.0.1.0.0` = 4, `1.0.0.1.0.0` = 4, `0.1.0.1.0.0` = 20,
    `1.1.0.1.0.0` = 4, `0.0.1.1.0.0` = 20, `1.0.1.1.0.0` = 2, `0.1.1.1.0.0` = 2,
    `1.1.1.1.0.0` = 7, `0.0.0.0.1.0` = 5, `1.0.0.0.1.0` = 4, `0.1.0.0.1.0` = 10,
    `1.1.0.0.1.0` = 9, `0.0.1.0.1.0` = 10, `1.0.1.0.1.0` = 7, `0.1.1.0.1.0` = 1,
    `1.1.1.0.1.0` = 7, `0.0.0.1.1.0` = 10, `1.0.0.1.1.0` = 6, `0.1.0.1.1.0` = 5,
    `1.1.0.1.1.0` = 8, `0.0.1.1.1.0` = 5, `1.0.1.1.1.0` = 2, `0.1.1.1.1.0` = 6,
    `1.1.1.1.1.0` = 9, `0.0.0.0.0.1` = 40, `1.0.0.0.0.1` = 7, `0.1.0.0.0.1` = 40,
    `1.1.0.0.0.1` = 10, `0.0.1.0.0.1` = 10, `1.0.1.0.0.1` = 1, `0.1.1.0.0.1` = 20,
    `1.1.1.0.0.1` = 5, `0.0.0.1.0.1` = 10, `1.0.0.1.0.1` = 2, `0.1.0.1.0.1` = 6,
    `1.1.0.1.0.1` = 1, `0.0.1.1.0.1` = 8, `1.0.1.1.0.1` = 1, `0.1.1.1.0.1` = 7,
    `1.1.1.1.0.1` = 2, `0.0.0.0.1.1` = 9, `1.0.0.0.1.1` = 1, `0.1.0.0.1.1` = 1,
    `1.1.0.0.1.1` = 2, `0.0.1.0.1.1` = 6, `1.0.1.0.1.1` = 9, `0.1.1.0.1.1` = 6,
    `1.1.1.0.1.1` = 4, `0.0.0.1.1.1` = 2, `1.0.0.1.1.1` = 9, `0.1.0.1.1.1` = 3,
    `1.1.0.1.1.1` = 1, `0.0.1.1.1.1` = 7, `1.0.1.1.1.1` = 4, `0.1.1.1.1.1` = 4,
    `1.1.1.1.1.1` = 3)

ncodes <-
  c(`0.0.0.0.0.0` = 1240, `1.0.0.0.0.0` = 200, `0.1.0.0.0.0` = 130,
    `1.1.0.0.0.0` = 4, `0.0.1.0.0.0` = 90, `1.0.1.0.0.0` = 9, `0.1.1.0.0.0` = 20,
    `1.1.1.0.0.0` = 7, `0.0.0.1.0.0` = 20, `1.0.0.1.0.0` = 3, `0.1.0.1.0.0` = 10,
    `1.1.0.1.0.0` = 8, `0.0.1.1.0.0` = 7, `1.0.1.1.0.0` = 9, `0.1.1.1.0.0` = 8,
    `1.1.1.1.0.0` = 8, `0.0.0.0.1.0` = 6, `1.0.0.0.1.0` = 9, `0.1.0.0.1.0` = 10,
    `1.1.0.0.1.0` = 3, `0.0.1.0.1.0` = 9, `1.0.1.0.1.0` = 7, `0.1.1.0.1.0` = 6,
    `1.1.1.0.1.0` = 8, `0.0.0.1.1.0` = 6, `1.0.0.1.1.0` = 2, `0.1.0.1.1.0` = 9,
    `1.1.0.1.1.0` = 6, `0.0.1.1.1.0` = 2, `1.0.1.1.1.0` = 2, `0.1.1.1.1.0` = 5,
    `1.1.1.1.1.0` = 7, `0.0.0.0.0.1` = 20, `1.0.0.0.0.1` = 6, `0.1.0.0.0.1` = 4,
    `1.1.0.0.0.1` = 1, `0.0.1.0.0.1` = 2, `1.0.1.0.0.1` = 4, `0.1.1.0.0.1` = 7,
    `1.1.1.0.0.1` = 6, `0.0.0.1.0.1` = 4, `1.0.0.1.0.1` = 1, `0.1.0.1.0.1` = 8,
    `1.1.0.1.0.1` = 6, `0.0.1.1.0.1` = 1, `1.0.1.1.0.1` = 4, `0.1.1.1.0.1` = 2,
    `1.1.1.1.0.1` = 7, `0.0.0.0.1.1` = 3, `1.0.0.0.1.1` = 7, `0.1.0.0.1.1` = 7,
    `1.1.0.0.1.1` = 9, `0.0.1.0.1.1` = 5, `1.0.1.0.1.1` = 1, `0.1.1.0.1.1` = 6,
    `1.1.1.0.1.1` = 5, `0.0.0.1.1.1` = 2, `1.0.0.1.1.1` = 3, `0.1.0.1.1.1` = 1,
    `1.1.0.1.1.1` = 5, `0.0.1.1.1.1` = 6, `1.0.1.1.1.1` = 5, `0.1.1.1.1.1` = 4,
    `1.1.1.1.1.1` = 2)

pcodes <-
  sample(names(pcodes), TBI_SUBJECTS, replace = TRUE, prob = pcodes) %>%
  strsplit("\\.") %>%
  lapply(as.integer) %>%
  do.call(rbind, .) %>%
  as.data.table %>%
  setNames(., paste0("pcode", 1:6))

pcodes

ncodes <-
  sample(names(ncodes), TBI_SUBJECTS, replace = TRUE, prob = ncodes + 0.1) %>%
  strsplit("\\.") %>%
  lapply(as.integer) %>%
  do.call(rbind, .) %>%
  as.data.table %>%
  setNames(., paste0("ncode", 1:6))

ncodes

tbi <- cbind(tbi, pcodes, ncodes)

#The three injury categories are constructed by fitting a simplified logistic regression model. The regression coefficient vectors are defined next.

injury1_coef <-
  matrix(c(-5.80494, -3.03946e-05, 0.773355, 0.00556597, -1.04436, -0.849594,
           0.770122, 0.968153, 16.6315, 0.411286, 4.07926, 5.16926, 2.84976,
           -17.1038, -14.7382, 4.30086),
         ncol = 1)

injury2_coef <-
  matrix(c(-427.083, 0.0742405, -342.072, -8.09704, 299.132, 991.75, -85.0155,
           -170.344, -57.408, 123.201, 161.41, -568.483, -767.944, 706.95,
           10.2964, 148.551),
         ncol = 1)

injury3_coef <-
  matrix(c(-3.54036, -0.00054294, 1.75962, -0.0475071, -17.515, -60.4276,
           4.58458, 0.58551, -19.7312, -1.16923, 2.16091, 63.7699, 39.3569,
           -37.8554, -14.1002, -14.8469),
         ncol = 1)

injury_expression <-
  quote(
    c("injury1", "injury2", "injury3") :=
      list(
        sapply(qwraps2::invlogit(cbind(1, as.matrix(tbi)) %*% injury1_coef),
               function(p) rbinom(1, 1, p)),
        sapply(qwraps2::invlogit(cbind(1, as.matrix(tbi)) %*% injury2_coef),
               function(p) rbinom(1, 1, p)),
        sapply(qwraps2::invlogit(cbind(1, as.matrix(tbi)) %*% injury3_coef),
               function(p) rbinom(1, 1, p))
      )
  )

tbi[, eval(injury_expression)]

ensr_env <- new.env()
data(tbi, package = "ensr", envir = ensr_env)

all.equal(digest::sha1(tbi),
          digest::sha1(ensr_env$tbi))
## [1] TRUE


#There are three outcomes, injuries, in the tbi data set. It would be reasonable to assume that
#there should be common variables with non-zero coefficients for models of each injury. 
#The end user could fit three univariate models or fit one multinomial model using the tools provided by glmnet.

#To illustrate these options we will run ensr five times: three univariate models, one multinomial model with type.multinomial set to the default "ungrouped," and one grouped multinomial model.

#type.multinomial set to the default "ungrouped," and one grouped multinomial model.
names(tbi)

ymat_1 <- matrix(tbi$injury1, ncol = 1)
ymat_2 <- matrix(tbi$injury2, ncol = 1)
ymat_3 <- matrix(tbi$injury3, ncol = 1)
ymat_123 <- as.matrix(tbi[, c("injury1", "injury2", "injury3")], ncol = 3)
xmat     <- as.matrix(tbi[, -c("age", "los", "injury1", "injury2", "injury3")])

foldid <- sample(seq(10), size = nrow(ymat_1), replace = TRUE)

fit_1 <- ensr(y = ymat_1, x = xmat,
              standardize = FALSE, standardize.response = FALSE,
              foldid = foldid, family = "binomial",
              parallel = TRUE)

fit_2 <- ensr(y = ymat_2, x = xmat,
              standardize = FALSE, standardize.response = FALSE,
              foldid = foldid, family = "binomial",
              parallel = TRUE)

fit_3 <- ensr(y = ymat_3, x = xmat,
              standardize = FALSE, standardize.response = FALSE,
              foldid = foldid, family = "binomial",
              parallel = TRUE)

fit_123_ungrouped <-
  ensr(y = ymat_123, x = xmat,
       standardize = FALSE, standardize.response = FALSE,
       foldid = foldid,
       family = "multinomial",
       type.multinomial = "ungrouped",
       parallel = TRUE)

fit_123_grouped <-
  ensr(y = ymat_123, x = xmat,
       standardize = FALSE, standardize.response = FALSE,
       foldid = foldid,
       family = "multinomial",
       type.multinomial = "grouped",
       parallel = TRUE)


gridExtra::grid.arrange(plot(fit_1) + ggplot2::ggtitle("Fit 1"),
                        plot(fit_2) + ggplot2::ggtitle("Fit 2"),
                        plot(fit_3) + ggplot2::ggtitle("Fit 3"),
                        plot(fit_123_ungrouped) + ggplot2::ggtitle("Fit 123 Ungrouped"),
                        plot(fit_123_grouped) + ggplot2::ggtitle("Fit 123 Grouped"),
                        layout_matrix = rbind(c(1, 1, 2, 2, 3, 3),
                                              c(4, 4, 4, 5, 5, 5)))
## Warning: Removed 1 rows containing non-finite values (stat_contour).

## Warning: Removed 1 rows containing non-finite values (stat_contour).

## Warning: Removed 1 rows containing non-finite values (stat_contour).

## Warning: Removed 1 rows containing non-finite values (stat_contour).

## Warning: Removed 1 rows containing non-finite values (stat_contour).

#The summary model output:
  
  all_summaries <-
  rbindlist(list(summary(fit_1),
                 summary(fit_2),
                 summary(fit_3),
                 summary(fit_123_ungrouped),
                 summary(fit_123_grouped)),
            idcol = "fit")

all_summaries[, fit := factor(fit, 1:5, c("Fit 1", "Fit 2", "Fit 3", "Fit 123 Ungrouped", "Fit 123 Grouped"))]

#The models with the lowest mean cross validation error are:
  
  all_summaries[, .SD[cvm == min(cvm)], by = fit]


ggplot(
  all_summaries[, .SD[cvm == min(cvm)], by = c("fit", "nzero")]
) +
  theme_bw() +
  aes(x = nzero, y = cvm, color = factor(fit), linetype = factor(fit)) +
  geom_point() +
  geom_line()

#the models with the lowest mean cross validation error and eight non-zero coefficients:
  
  pref_models <-
  all_summaries[nzero == 8,
                .SD[cvm == min(cvm)],
                by = c("fit")]
pref_models


#the coefficients for these models, starting with the three univariate models

cbind(
  coef(fit_1[[pref_models$l_index[1]]], s = pref_models$lambda[1])
  ,
  coef(fit_2[[pref_models$l_index[2]]], s = pref_models$lambda[2])
  ,
  coef(fit_3[[pref_models$l_index[3]]], s = pref_models$lambda[3])
)
## 14 x 3 sparse Matrix of class "dgCMatrix"


#The following are the non-zero coefficients for the ungrouped models.

do.call(cbind,
        coef(fit_123_ungrouped[[pref_models$l_index[4]]], s = pref_models$lambda[4]))
## 14 x 3 sparse Matrix of class "dgCMatrix"


#The grouped results are:
  
  do.call(cbind,
          coef(fit_123_grouped[[pref_models$l_index[5]]], s = pref_models$lambda[5]))



